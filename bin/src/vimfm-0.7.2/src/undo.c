/* vifm
 * Copyright (C) 2011 xaizek.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 */

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "macros.h"
#include "ops.h"
#include "registers.h"
#include "utils.h"

#include "undo.h"

typedef struct
{
	char *msg;
	int error;
	int balance;
	int can_undone;
	int incomplete;
}group_t;

typedef struct
{
	OPS op;
	const char *src;        /* NULL, buf1 or buf2 */
	const char *dst;        /* NULL, buf1 or buf2 */
	void *data;             /* for uid_t, gid_t and mode_t */
	const char *exists;     /* NULL, buf1 or buf2 */
	const char *dont_exist; /* NULL, buf1 or buf2 */
}op_t;

typedef struct cmd_t
{
	char *buf1;
	char *buf2;
	op_t do_op;
	op_t undo_op;

	group_t *group;
	struct cmd_t *prev;
	struct cmd_t *next;
}cmd_t;

static OPS undo_op[] = {
	OP_NONE,     /* OP_NONE */
	OP_NONE,     /* OP_USR */
	OP_NONE,     /* OP_REMOVE */
	OP_SYMLINK,  /* OP_REMOVESL */
	OP_REMOVE,   /* OP_COPY   */
	OP_MOVE,     /* OP_MOVE */
	OP_MOVETMP0, /* OP_MOVETMP0 */
	OP_MOVETMP1, /* OP_MOVETMP1 */
	OP_MOVETMP2, /* OP_MOVETMP2 */
	OP_MOVETMP3, /* OP_MOVETMP1 */
	OP_MOVETMP4, /* OP_MOVETMP2 */
	OP_CHOWN,    /* OP_CHOWN */
	OP_CHGRP,    /* OP_CHGRP */
#ifndef _WIN32
	OP_CHMOD,    /* OP_CHMOD */
	OP_CHMODR,   /* OP_CHMODR */
#else
	OP_SUBATTR,  /* OP_ADDATTR */
	OP_ADDATTR,  /* OP_SUBATTR */
#endif
	OP_REMOVE,   /* OP_SYMLINK */
	OP_REMOVESL, /* OP_SYMLINK2 */
	OP_RMDIR,    /* OP_MKDIR */
	OP_NONE,     /* OP_RMDIR */
	OP_REMOVE,   /* OP_MKFILE */
};
ARRAY_GUARD(undo_op, OP_COUNT);

static enum
{
	OPER_1ST,
	OPER_2ND,
	OPER_NON,
}opers[][8] = {
	/* 1st arg   2nd arg   exists    absent   */
	{ OPER_NON, OPER_NON, OPER_NON, OPER_NON,    /* do   OP_NONE */
		OPER_NON, OPER_NON, OPER_NON, OPER_NON, }, /* undo OP_NONE */
	{ OPER_NON, OPER_NON, OPER_NON, OPER_NON,    /* do   OP_USR  */
		OPER_NON, OPER_NON, OPER_NON, OPER_NON, }, /* undo OP_NONE */
	{ OPER_1ST, OPER_NON, OPER_1ST, OPER_NON,    /* do   OP_REMOVE */
		OPER_NON, OPER_NON, OPER_NON, OPER_NON, }, /* undo OP_NONE   */
	{ OPER_1ST, OPER_NON, OPER_1ST, OPER_NON,    /* do   OP_REMOVESL */
	  OPER_2ND, OPER_1ST, OPER_NON, OPER_NON, }, /* undo OP_SYMLINK2 */
	{ OPER_1ST, OPER_2ND, OPER_1ST, OPER_2ND,    /* do   OP_COPY   */
		OPER_2ND, OPER_NON, OPER_2ND, OPER_NON, }, /* undo OP_REMOVE */
	{ OPER_1ST, OPER_2ND, OPER_1ST, OPER_2ND,    /* do   OP_MOVE */
		OPER_2ND, OPER_1ST, OPER_2ND, OPER_1ST, }, /* undo OP_MOVE */
	{ OPER_1ST, OPER_2ND, OPER_2ND, OPER_NON,    /* do   OP_MOVETMP0 */
		OPER_2ND, OPER_1ST, OPER_2ND, OPER_NON, }, /* undo OP_MOVETMP0 */
	{ OPER_1ST, OPER_2ND, OPER_2ND, OPER_NON,    /* do   OP_MOVETMP1 */
		OPER_2ND, OPER_1ST, OPER_2ND, OPER_NON, }, /* undo OP_MOVETMP1 */
	{ OPER_1ST, OPER_2ND, OPER_1ST, OPER_NON,    /* do   OP_MOVETMP2 */
		OPER_2ND, OPER_1ST, OPER_1ST, OPER_NON, }, /* undo OP_MOVETMP2 */
	{ OPER_1ST, OPER_2ND, OPER_NON, OPER_2ND,    /* do   OP_MOVETMP3 */
		OPER_2ND, OPER_1ST, OPER_2ND, OPER_1ST, }, /* undo OP_MOVETMP3 */
	{ OPER_1ST, OPER_2ND, OPER_1ST, OPER_2ND,    /* do   OP_MOVETMP4 */
		OPER_2ND, OPER_1ST, OPER_NON, OPER_NON, }, /* undo OP_MOVETMP4 */
	{ OPER_1ST, OPER_NON, OPER_1ST, OPER_NON,    /* do   OP_CHOWN */
		OPER_1ST, OPER_NON, OPER_1ST, OPER_NON, }, /* undo OP_CHOWN */
	{ OPER_1ST, OPER_NON, OPER_1ST, OPER_NON,    /* do   OP_CHGRP */
		OPER_1ST, OPER_NON, OPER_1ST, OPER_NON, }, /* undo OP_CHGRP */
#ifndef _WIN32
	{ OPER_1ST, OPER_NON, OPER_1ST, OPER_NON,    /* do   OP_CHMOD */
		OPER_1ST, OPER_NON, OPER_1ST, OPER_NON, }, /* undo OP_CHMOD */
	{ OPER_1ST, OPER_NON, OPER_1ST, OPER_NON,    /* do   OP_CHMODR */
		OPER_1ST, OPER_NON, OPER_1ST, OPER_NON, }, /* undo OP_CHMODR */
#else
	{ OPER_1ST, OPER_NON, OPER_1ST, OPER_NON,    /* do   OP_ADDATTR */
		OPER_1ST, OPER_NON, OPER_1ST, OPER_NON, }, /* undo OP_SUBATTR */
	{ OPER_1ST, OPER_NON, OPER_1ST, OPER_NON,    /* do   OP_SUBATTR */
		OPER_1ST, OPER_NON, OPER_1ST, OPER_NON, }, /* undo OP_ADDATTR */
#endif
	{ OPER_1ST, OPER_2ND, OPER_NON, OPER_2ND,    /* do   OP_SYMLINK */
		OPER_2ND, OPER_NON, OPER_2ND, OPER_NON, }, /* undo OP_REMOVE  */
	{ OPER_1ST, OPER_2ND, OPER_NON, OPER_NON,    /* do   OP_SYMLINK2 */
		OPER_2ND, OPER_NON, OPER_2ND, OPER_NON, }, /* undo OP_REMOVESL */
	{ OPER_1ST, OPER_NON, OPER_NON, OPER_1ST,    /* do   OP_MKDIR */
		OPER_1ST, OPER_NON, OPER_1ST, OPER_NON, }, /* undo OP_RMDIR */
	{ OPER_1ST, OPER_NON, OPER_1ST, OPER_NON,    /* do   OP_RMDIR */
		OPER_NON, OPER_NON, OPER_NON, OPER_NON, }, /* undo OP_NONE  */
	{ OPER_1ST, OPER_NON, OPER_NON, OPER_1ST,    /* do   OP_MKFILE */
		OPER_1ST, OPER_NON, OPER_1ST, OPER_NON, }, /* undo OP_REMOVE  */
};
ARRAY_GUARD(opers, OP_COUNT);

static int data_is_ptr[] = {
	0, /* OP_NONE */
	1, /* OP_USR */
	0, /* OP_REMOVE */
	0, /* OP_REMOVESL */
	0, /* OP_COPY   */
	0, /* OP_MOVE */
	0, /* OP_MOVETMP0 */
	0, /* OP_MOVETMP1 */
	0, /* OP_MOVETMP2 */
	0, /* OP_MOVETMP3 */
	0, /* OP_MOVETMP4 */
	0, /* OP_CHOWN */
	0, /* OP_CHGRP */
#ifndef _WIN32
	1, /* OP_CHMOD */
	1, /* OP_CHMODR */
#else
	0, /* OP_ADDATTR */
	0, /* OP_SUBATTR */
#endif
	0, /* OP_SYMLINK */
	0, /* OP_SYMLINK2 */
	0, /* OP_MKDIR */
	0, /* OP_RMDIR */
	0, /* OP_MKFILE */
};
ARRAY_GUARD(data_is_ptr, OP_COUNT);

static perform_func do_func;
static const int *undo_levels;

static cmd_t cmds = {
	.prev = &cmds,
};
static cmd_t *current = &cmds;

static size_t trash_dir_len;

static int group_opened;
static long long next_group;
static group_t *last_group;
static char *group_msg;

static int command_count;

static void init_cmd(cmd_t *cmd, OPS op, void *do_data, void *undo_data);
static void init_entry(cmd_t *cmd, const char **e, int type);
static void remove_cmd(cmd_t *cmd);
static int is_undo_group_possible(void);
static int is_redo_group_possible(void);
static int is_op_possible(const op_t *op);
static void change_filename_in_trash(cmd_t *cmd, const char *filename);
static void update_entry(const char **e, const char *old, const char *new);
static char ** fill_undolist_detail(char **list);
static const char * get_op_desc(op_t op);
static char **fill_undolist_nondetail(char **list);

void
init_undo_list(perform_func exec_func, const int* max_levels)
{
	assert(exec_func != NULL);

	do_func = exec_func;
	undo_levels = max_levels;

	trash_dir_len = strlen(cfg.trash_dir);
}

void
reset_undo_list(void)
{
	assert(!group_opened);

	while(cmds.next != NULL)
		remove_cmd(cmds.next);
	cmds.prev = &cmds;

	current = &cmds;
	next_group = 0;
}

void
cmd_group_begin(const char *msg)
{
	assert(!group_opened);

	group_opened = 1;

	free(group_msg);
	group_msg = strdup(msg);
	last_group = NULL;
}

void
cmd_group_continue(void)
{
	assert(!group_opened);
	assert(next_group != 0);

	group_opened = 1;
	next_group--;
}

char *
replace_group_msg(const char *msg)
{
	char *result;

	result = group_msg;
	group_msg = (msg != NULL) ? strdup(msg) : NULL;
	if(last_group != NULL && group_msg != NULL)
	{
		free(last_group->msg);
		last_group->msg = strdup(group_msg);
	}
	return result;
}

int
add_operation(OPS op, void *do_data, void *undo_data, const char *buf1,
		const char *buf2)
{
	int mem_error;
	cmd_t *cmd;

	assert(group_opened);
	assert(buf1 != NULL);
	assert(buf2 != NULL);

	/* free list tail */
	while(current->next != NULL)
		remove_cmd(current->next);

	while(command_count > 0 && command_count >= *undo_levels)
		remove_cmd(cmds.next);

	if(*undo_levels <= 0)
		return 0;

	command_count++;

	/* add operation to the list */
	cmd = calloc(1, sizeof(*cmd));
	if(cmd == NULL)
		return -1;

	cmd->buf1 = strdup(buf1);
	cmd->buf2 = strdup(buf2);
	cmd->prev = current;
	init_cmd(cmd, op, do_data, undo_data);
	if(last_group != NULL)
	{
		cmd->group = last_group;
	}
	else if((cmd->group = malloc(sizeof(group_t))) != NULL)
	{
		cmd->group->msg = strdup(group_msg);
		cmd->group->error = 0;
		cmd->group->balance = 0;
		cmd->group->can_undone = 1;
		cmd->group->incomplete = 0;
	}
	mem_error = cmd->group == NULL || cmd->buf1 == NULL || cmd->buf2 == NULL;
	if(mem_error)
	{
		remove_cmd(cmd);
		return -1;
	}
	last_group = cmd->group;

	if(undo_op[op] == OP_NONE)
		cmd->group->can_undone = 0;

	current->next = cmd;
	current = cmd;
	cmds.prev = cmd;

	return 0;
}

static void
init_cmd(cmd_t *cmd, OPS op, void *do_data, void *undo_data)
{
	cmd->do_op.op = op;
	cmd->do_op.data = do_data;
	cmd->undo_op.op = undo_op[op];
	cmd->undo_op.data = undo_data;
	init_entry(cmd, &cmd->do_op.src, opers[op][0]);
	init_entry(cmd, &cmd->do_op.dst, opers[op][1]);
	init_entry(cmd, &cmd->do_op.exists, opers[op][2]);
	init_entry(cmd, &cmd->do_op.dont_exist, opers[op][3]);
	init_entry(cmd, &cmd->undo_op.src, opers[op][4]);
	init_entry(cmd, &cmd->undo_op.dst, opers[op][5]);
	init_entry(cmd, &cmd->undo_op.exists, opers[op][6]);
	init_entry(cmd, &cmd->undo_op.dont_exist, opers[op][7]);
}

static void
init_entry(cmd_t *cmd, const char **e, int type)
{
	if(type == OPER_NON)
		*e = NULL;
	else if(type == OPER_1ST)
		*e = cmd->buf1;
	else
		*e = cmd->buf2;
}

static void
remove_cmd(cmd_t *cmd)
{
	int last_cmd_in_group = 1;

	if(cmd == current)
		current = cmd->prev;

	if(cmd->prev != NULL)
	{
		cmd->prev->next = cmd->next;
		if(cmd->group == cmd->prev->group)
			last_cmd_in_group = 0;
	}
	if(cmd->next != NULL)
	{
		cmd->next->prev = cmd->prev;
		if(cmd->group == cmd->next->group)
			last_cmd_in_group = 0;
	}
	else /* this is the last command in the list */
	{
		cmds.prev = cmd->prev;
	}

	if(last_cmd_in_group)
	{
		free(cmd->group->msg);
		free(cmd->group);
		if(last_group == cmd->group)
			last_group = NULL;
	}
	else
	{
		cmd->group->incomplete = 1;
	}
	free(cmd->buf1);
	free(cmd->buf2);
	if(data_is_ptr[cmd->do_op.op])
		free(cmd->do_op.data);
	if(data_is_ptr[cmd->undo_op.op])
		free(cmd->undo_op.data);

	free(cmd);

	command_count--;
}

void
cmd_group_end(void)
{
	assert(group_opened);

	group_opened = 0;
	next_group++;

	while(cmds.next != NULL && cmds.next->group->incomplete)
		remove_cmd(cmds.next);
}

int
undo_group(void)
{
	int errors, disbalance, cant_undone;
	int skip;
	assert(!group_opened);

	if(current == &cmds)
		return -1;

	errors = current->group->error != 0;
	disbalance = current->group->balance != 0;
	cant_undone = !current->group->can_undone;
	if(errors || disbalance || cant_undone || !is_undo_group_possible())
	{
		do
			current = current->prev;
		while(current != &cmds && current->group == current->next->group);
		if(errors)
			return 1;
		else if(disbalance)
			return -4;
		else if(cant_undone)
			return -5;
		else
			return -3;
	}

	current->group->balance--;

	skip = 0;
	do
	{
		if(!skip)
		{
			int err = do_func(current->undo_op.op, current->undo_op.data,
					current->undo_op.src, current->undo_op.dst);
			if(err == SKIP_UNDO_REDO_OPERATION)
			{
				skip = 1;
				current->group->balance++;
			}
			else if(err != 0)
			{
				current->group->error = 1;
				errors = 1;
			}
		}
		current = current->prev;
	}
	while(current != &cmds && current->group == current->next->group);

	if(skip)
		return -6;

	return errors ? -2 : 0;
}

static int
is_undo_group_possible(void)
{
	cmd_t *cmd = current;
	do
	{
		int ret;
		ret = is_op_possible(&cmd->undo_op);
		if(ret == 0)
			return 0;
		else if(ret < 0)
			change_filename_in_trash(cmd, cmd->undo_op.dst);
		cmd = cmd->prev;
	}
	while(cmd != &cmds && cmd->group == cmd->next->group);
	return 1;
}

int
redo_group(void)
{
	int errors, disbalance;
	int skip;
	assert(!group_opened);

	if(current->next == NULL)
		return -1;

	errors = current->next->group->error != 0;
	disbalance = current->next->group->balance == 0;
	if(errors || disbalance || !is_redo_group_possible())
	{
		do
			current = current->next;
		while(current->next != NULL && current->group == current->next->group);
		if(errors)
			return 1;
		else if(disbalance)
			return -4;
		else
			return -3;
	}

	current->next->group->balance++;

	skip = 0;
	do
	{
		current = current->next;
		if(!skip)
		{
			int err = do_func(current->do_op.op, current->do_op.data,
					current->do_op.src, current->do_op.dst);
			if(err == SKIP_UNDO_REDO_OPERATION)
			{
				current->next->group->balance--;
				skip = 1;
			}
			else if(err != 0)
			{
				current->group->error = 1;
				errors = 1;
			}
		}
	}
	while(current->next != NULL && current->group == current->next->group);

	if(skip)
		return -6;

	return errors ? -2 : 0;
}

static int
is_redo_group_possible(void)
{
	cmd_t *cmd = current;
	do
	{
		int ret;
		cmd = cmd->next;
		ret = is_op_possible(&cmd->do_op);
		if(ret == 0)
			return 0;
		else if(ret < 0)
			change_filename_in_trash(cmd, cmd->do_op.dst);
	}
	while(cmd->next != NULL && cmd->group == cmd->next->group);
	return 1;
}

/*
 * Return value:
 *   0 - impossible
 * < 0 - possible with renaming file in trash
 * > 0 - possible
 */
static int
is_op_possible(const op_t *op)
{
	struct stat st;

#ifdef TEST
	if(op->op == OP_MOVE)
		return 1;
#endif
	if(op->exists != NULL && lstat(op->exists, &st) != 0)
		return 0;
	if(op->dont_exist != NULL && lstat(op->dont_exist, &st) == 0)
	{
		if(pathncmp(op->dst, cfg.trash_dir, trash_dir_len) == 0)
			return -1;
		return 0;
	}
	return 1;
}

static void
change_filename_in_trash(cmd_t *cmd, const char *filename)
{
	char *p;
	int i;
	char buf[PATH_MAX];
	char *old;

	p = strchr(filename + trash_dir_len, '_') + 1;

	i = -1;
	do
		snprintf(buf, sizeof(buf), "%s/%03i_%s", cfg.trash_dir, ++i, p);
	while(access(buf, F_OK) == 0);
	rename_in_registers(filename, buf);

	old = cmd->buf2;
	free(cmd->buf2);
	cmd->buf2 = strdup(buf);

	update_entry(&cmd->do_op.src, old, cmd->buf2);
	update_entry(&cmd->do_op.dst, old, cmd->buf2);
	update_entry(&cmd->do_op.exists, old, cmd->buf2);
	update_entry(&cmd->do_op.dont_exist, old, cmd->buf2);
	update_entry(&cmd->undo_op.src, old, cmd->buf2);
	update_entry(&cmd->undo_op.dst, old, cmd->buf2);
	update_entry(&cmd->undo_op.exists, old, cmd->buf2);
	update_entry(&cmd->undo_op.dont_exist, old, cmd->buf2);
}

static void
update_entry(const char **e, const char *old, const char *new)
{
	if(*e == old)
		*e = new;
}

char **
undolist(int detail)
{
	char **list, **p;
	int group_count;
	cmd_t *cmd;

	assert(!group_opened);

	group_count = 1;
	cmd = cmds.prev;
	while(cmd != &cmds)
	{
		if(cmd->group != cmd->prev->group)
			group_count++;
		cmd = cmd->prev;
	}

	if(detail)
		list = malloc(sizeof(char *)*(group_count + command_count*2 + 1));
	else
		list = malloc(sizeof(char *)*group_count);

	if(list == NULL)
		return NULL;

	if(detail)
		p = fill_undolist_detail(list);
	else
		p = fill_undolist_nondetail(list);
	*p = NULL;

	return list;
}

static char **
fill_undolist_detail(char **list)
{
	int left;
	cmd_t *cmd;

	left = *undo_levels;
	cmd = cmds.prev;
	while(cmd != &cmds && left > 0)
	{
		if((*list = strdup(cmd->group->msg)) == NULL)
			break;

		list++;
		do
		{
			const char *p;

			p = get_op_desc(cmd->do_op);
			if((*list = malloc(4 + strlen(p) + 1)) == NULL)
				return list;
			sprintf(*list, "do: %s", p);
			list++;

			p = get_op_desc(cmd->undo_op);
			if((*list = malloc(6 + strlen(p) + 1)) == NULL)
				return list;
			sprintf(*list, "undo: %s", p);
			list++;

			cmd = cmd->prev;
			--left;
		}
		while(cmd != &cmds && cmd->group == cmd->next->group && left > 0);
	}

	return list;
}

static const char *
get_op_desc(op_t op)
{
	static char buf[64 + 2*PATH_MAX] = "";
	switch(op.op)
	{
		case OP_NONE:
			strcpy(buf, "<no operation>");
			break;
		case OP_USR:
			strcpy(buf, (char *)op.data);
			break;
		case OP_REMOVE:
		case OP_REMOVESL:
			snprintf(buf, sizeof(buf), "rm %s", op.src);
			break;
		case OP_COPY:
			snprintf(buf, sizeof(buf), "cp %s to %s", op.src, op.dst);
			break;
		case OP_MOVE:
		case OP_MOVETMP0:
		case OP_MOVETMP1:
		case OP_MOVETMP2:
			snprintf(buf, sizeof(buf), "mv %s to %s", op.src, op.dst);
			break;
		case OP_CHOWN:
			snprintf(buf, sizeof(buf), "chown %ld %s", (long)op.data, op.src);
			break;
		case OP_CHGRP:
			snprintf(buf, sizeof(buf), "chown :%ld %s", (long)op.data, op.src);
			break;
#ifndef _WIN32
		case OP_CHMOD:
		case OP_CHMODR:
			snprintf(buf, sizeof(buf), "chmod %s %s", (char *)op.data, op.src);
			break;
#else
		case OP_ADDATTR:
			snprintf(buf, sizeof(buf), "attrib +%s", attr_str((DWORD)op.data));
			break;
		case OP_SUBATTR:
			snprintf(buf, sizeof(buf), "attrib -%s", attr_str((DWORD)op.data));
			break;
#endif
		case OP_SYMLINK:
		case OP_SYMLINK2:
			snprintf(buf, sizeof(buf), "ln -s %s to %s", op.src, op.dst);
			break;
		case OP_MKDIR:
			snprintf(buf, sizeof(buf), "mkdir %s%s", op.src,
					(op.data == NULL) ? "" : "-p ");
			break;
		case OP_RMDIR:
			snprintf(buf, sizeof(buf), "rmdir %s", op.src);
			break;
		case OP_MKFILE:
			snprintf(buf, sizeof(buf), "touch %s", op.src);
			break;

		default:
			strcpy(buf, "ERROR, update get_op_desc() function");
			break;
	}

	return buf;
}

static char **
fill_undolist_nondetail(char **list)
{
	int left;
	cmd_t *cmd;

	left = *undo_levels;
	cmd = cmds.prev;
	while(cmd != &cmds && left-- > 0)
	{
		if((*list = strdup(cmd->group->msg)) == NULL)
			break;

		do
			cmd = cmd->prev;
		while(cmd != &cmds && cmd->group == cmd->next->group);
		list++;
	}

	return list;
}

int
get_undolist_pos(int detail)
{
	cmd_t *cur = cmds.prev;
	int result_group = 0;
	int result_cmd = 0;

	assert(!group_opened);

	if(cur == &cmds)
		result_group++;
	while(cur != current)
	{
		if(cur->group != cur->prev->group)
			result_group++;
		result_cmd += 2;
		cur = cur->prev;
	}
	return detail ? (result_group + result_cmd) : result_group;
}

void
clean_cmds_with_trash(void)
{
	cmd_t *cur = cmds.prev;

	assert(!group_opened);

	while(cur != &cmds)
	{
		cmd_t *prev = cur->prev;

		if(cur->group->balance < 0)
		{
			if(cur->do_op.exists != NULL &&
					pathncmp(cur->do_op.exists, cfg.trash_dir, trash_dir_len) == 0)
				remove_cmd(cur);
		}
		else
		{
			if(cur->undo_op.exists != NULL &&
					pathncmp(cur->undo_op.exists, cfg.trash_dir, trash_dir_len) == 0)
				remove_cmd(cur);
		}
		cur = prev;
	}
}

/* vim: set tabstop=2 softtabstop=2 shiftwidth=2 noexpandtab cinoptions-=(0 : */
/* vim: set cinoptions+=t0 : */

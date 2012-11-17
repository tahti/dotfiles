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

#include <limits.h>

#include <stdlib.h>
#include <string.h>

#include "utils.h"

#include "tree.h"

typedef struct node_t
{
	char *name;
	size_t name_len;
	unsigned long long data;
	int valid;
	struct node_t *next;
	struct node_t *child;
}node_t;

typedef struct root_t
{
	node_t node;
	int longest;
	int mem;
}root_t;

static void nodes_free(node_t *node);
static node_t * find_node(node_t *root, const char *name, int create,
		node_t **last);

tree_t
tree_create(int longest, int mem)
{
	root_t *tree;

	tree = malloc(sizeof(*tree));
	tree->node.child = NULL;
	tree->node.next = NULL;
	tree->node.name = NULL;
	tree->node.valid = 0;
	tree->longest = longest;
	tree->mem = mem;
	return tree;
}

void
tree_free(tree_t tree)
{
	if(tree == NULL)
		return;
	nodes_free(&tree->node);
}

static void
nodes_free(node_t *node)
{
	if(node->child != NULL)
		nodes_free(node->child);

	if(node->next != NULL)
		nodes_free(node->next);

	free(node->name);
	free(node);
}

int
tree_set_data(tree_t tree, const char *path, unsigned long long data)
{
	node_t *node;
	char real_path[PATH_MAX];

	if(realpath(path, real_path) != real_path)
		return -1;

	node = find_node(&tree->node, real_path, 1, NULL);
	if(node->valid && tree->mem)
	{
		union
		{
			unsigned long long l;
			void *p;
		}u = {
			.l = node->data,
		};

		free(u.p);
	}
	node->data = data;
	node->valid = 1;
	return 0;
}

int
tree_get_data(tree_t tree, const char *path, unsigned long long *data)
{
	node_t *last = NULL;
	node_t *node;
	char real_path[PATH_MAX];

	if(tree->node.child == NULL)
		return -1;

	if(realpath(path, real_path) != real_path)
		return -1;

	node = find_node(&tree->node, real_path, 0, tree->longest ? &last : NULL);
	if((node == NULL || !node->valid) && last == NULL)
		return -1;
	
	if(node != NULL && node->valid)
		*data = node->data;
	else
		*data = last->data;
	return 0;
}

static node_t *
find_node(node_t *root, const char *name, int create, node_t **last)
{
	const char *end;
	size_t name_len;
	node_t *prev = NULL, *curr;
	node_t *new_node;

	if(*name == '/')
		name++;

	if(*name == '\0')
		return root;

	end = strchr(name, '/');
	if(end == NULL)
		end = name + strlen(name);

	name_len = end - name;
	curr = root->child;
	while(curr != NULL)
	{
		int comp = pathncmp(name, curr->name, end - name);
		if(comp == 0 && curr->name_len == name_len)
		{
			if(curr->valid && last != NULL)
				*last = curr;
			return find_node(curr, end, create, last);
		}
		else if(comp < 0)
		{
			break;
		}
		prev = curr;
		curr = curr->next;
	}

	if(!create)
		return NULL;

	new_node = malloc(sizeof(*new_node));
	if(new_node == NULL)
		return NULL;

	if((new_node->name = malloc(name_len + 1)) == NULL)
	{
		free(new_node);
		return NULL;
	}
	strncpy(new_node->name, name, name_len);
	new_node->name[name_len] = '\0';
	new_node->name_len = name_len;
	new_node->valid = 0;
	new_node->child = NULL;
	new_node->next = curr;

	if(root->child == curr)
		root->child = new_node;
	else
		prev->next = new_node;

	return find_node(new_node, end, create, last);
}

/* vim: set tabstop=2 softtabstop=2 shiftwidth=2 noexpandtab cinoptions-=(0 : */
/* vim: set cinoptions+=t0 : */

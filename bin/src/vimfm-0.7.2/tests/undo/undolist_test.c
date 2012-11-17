#include <stdlib.h>

#include "seatest.h"

#include "../../src/undo.h"

static void
test_detail(void)
{
	char **list;
	char **p;

	list = undolist(1);

	assert_false(list == NULL);
	assert_true(list[11] == NULL);

	assert_string_equal("msg3", list[0]);
	assert_string_equal("do: mv do_msg3 to undo_msg3", list[1]);
	assert_string_equal("undo: mv undo_msg3 to do_msg3", list[2]);

	assert_string_equal("msg2", list[3]);
	assert_string_equal("do: mv do_msg2_cmd2 to undo_msg2_cmd2", list[4]);
	assert_string_equal("undo: mv undo_msg2_cmd2 to do_msg2_cmd2", list[5]);
	assert_string_equal("do: mv do_msg2_cmd1 to undo_msg2_cmd1", list[6]);
	assert_string_equal("undo: mv undo_msg2_cmd1 to do_msg2_cmd1", list[7]);

	assert_string_equal("msg1", list[8]);
	assert_string_equal("do: mv do_msg1 to undo_msg1", list[9]);
	assert_string_equal("undo: mv undo_msg1 to do_msg1", list[10]);

	p = list;
	while(*p != NULL)
		free(*p++);
	free(list);
}

static void
test_detail_after_reset(void)
{
	char ** list;

	reset_undo_list();

	list = undolist(1);

	assert_true(list[0] == NULL);

	free(list);
}

static int
exec_func(OPS op, void *data, const char *src, const char *dst)
{
	return 0;
}

static void
test_detail_smaller_limit(void)
{
	static int undo_levels = 2;

	char ** list;
	char ** p;

	init_undo_list(exec_func, &undo_levels);

	list = undolist(1);

	assert_false(list == NULL);
	assert_true(list[6] == NULL);

	assert_string_equal("msg3", list[0]);
	assert_string_equal("do: mv do_msg3 to undo_msg3", list[1]);
	assert_string_equal("undo: mv undo_msg3 to do_msg3", list[2]);

	assert_string_equal("msg2", list[3]);
	assert_string_equal("do: mv do_msg2_cmd2 to undo_msg2_cmd2", list[4]);
	assert_string_equal("undo: mv undo_msg2_cmd2 to do_msg2_cmd2", list[5]);

	p = list;
	while(*p != NULL)
		free(*p++);
	free(list);
}

static void
test_nondetail(void)
{
	char ** list;
	char ** p;

	list = undolist(0);

	assert_false(list == NULL);
	assert_true(list[3] == NULL);

	assert_string_equal("msg3", list[0]);
	assert_string_equal("msg2", list[1]);
	assert_string_equal("msg1", list[2]);

	p = list;
	while(*p != NULL)
		free(*p++);
	free(list);
}

static void
test_nondetail_after_reset(void)
{
	char ** list;

	reset_undo_list();

	list = undolist(0);

	assert_true(list[0] == NULL);

	free(list);
}

static void
test_nondetail_smaller_limit(void)
{
	static int undo_levels = 2;

	char ** list;
	char ** p;

	init_undo_list(exec_func, &undo_levels);

	list = undolist(0);

	assert_false(list == NULL);
	assert_true(list[2] == NULL);

	assert_string_equal("msg3", list[0]);
	assert_string_equal("msg2", list[1]);

	p = list;
	while(*p != NULL)
		free(*p++);
	free(list);
}

static void
test_pos(void)
{
	assert_int_equal(0, get_undolist_pos(0));
	assert_int_equal(0, undo_group());
	assert_int_equal(1, get_undolist_pos(0));
	assert_int_equal(0, undo_group());
	assert_int_equal(2, get_undolist_pos(0));
	assert_int_equal(0, undo_group());
	assert_int_equal(3, get_undolist_pos(0));
	assert_int_equal(-1, undo_group());
	assert_int_equal(3, get_undolist_pos(0));
	assert_int_equal(-1, undo_group());
	assert_int_equal(3, get_undolist_pos(0));
	assert_int_equal(-1, undo_group());
	assert_int_equal(3, get_undolist_pos(0));
	assert_int_equal(-1, undo_group());
}

void
undolist_test(void)
{
	test_fixture_start();

	run_test(test_detail);
	run_test(test_detail_after_reset);
	run_test(test_detail_smaller_limit);
	run_test(test_nondetail);
	run_test(test_nondetail_after_reset);
	run_test(test_nondetail_smaller_limit);

	run_test(test_pos);

	test_fixture_end();
}

/* vim: set tabstop=2 softtabstop=2 shiftwidth=2 noexpandtab cinoptions-=(0 : */
/* vim: set cinoptions+=t0 : */

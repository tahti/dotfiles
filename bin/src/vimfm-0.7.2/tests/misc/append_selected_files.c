#include <stdlib.h>
#include <string.h>

#include "seatest.h"

#include "../../src/commands.h"
#include "../../src/config.h"
#include "../../src/ui.h"

#ifdef _WIN32
#define SL "\\\\"
#else
#define SL "/"
#endif

static void
setup(void)
{
	cfg.shell = strdup("sh");

	/* lwin */
	strcpy(lwin.curr_dir, "/lwin");

	lwin.list_rows = 4;
	lwin.list_pos = 2;
	lwin.dir_entry = calloc(lwin.list_rows, sizeof(*lwin.dir_entry));
	lwin.dir_entry[0].name = strdup("lfile0");
	lwin.dir_entry[1].name = strdup("lfile1");
	lwin.dir_entry[2].name = strdup("lfile2");
	lwin.dir_entry[3].name = strdup("lfile3");

	lwin.dir_entry[0].selected = 1;
	lwin.dir_entry[2].selected = 1;
	lwin.selected_files = 2;

	/* rwin */
	strcpy(rwin.curr_dir, "/rwin");

	rwin.list_rows = 7;
	rwin.list_pos = 5;
	rwin.dir_entry = calloc(rwin.list_rows, sizeof(*rwin.dir_entry));
	rwin.dir_entry[0].name = strdup("rfile0");
	rwin.dir_entry[1].name = strdup("rfile1");
	rwin.dir_entry[2].name = strdup("rfile2");
	rwin.dir_entry[3].name = strdup("rfile3");
	rwin.dir_entry[4].name = strdup("rfile4");
	rwin.dir_entry[5].name = strdup("rfile5");
	rwin.dir_entry[6].name = strdup("rdir6/");

	rwin.dir_entry[1].selected = 1;
	rwin.dir_entry[3].selected = 1;
	rwin.dir_entry[5].selected = 1;
	rwin.dir_entry[6].selected = 1;
	rwin.selected_files = 4;

	curr_view = &lwin;
	other_view = &rwin;
}

static void
teardown(void)
{
	int i;

	free(cfg.shell);

	for(i = 0; i < lwin.list_rows; i++)
		free(lwin.dir_entry[i].name);
	free(lwin.dir_entry);

	for(i = 0; i < rwin.list_rows; i++)
		free(rwin.dir_entry[i].name);
	free(rwin.dir_entry);
}

static void
test_f(void)
{
	char *buf;
	char *expanded;

	expanded = strdup("");
	expanded = append_selected_files(&lwin, expanded, 0, 0, "");
	assert_string_equal("lfile0 lfile2", expanded);
	free(expanded);

	expanded = strdup("/");
	expanded = append_selected_files(&lwin, expanded, 0, 0, "");
	assert_string_equal("/lfile0 lfile2", expanded);
	free(expanded);

	expanded = strdup("");
	expanded = append_selected_files(&rwin, expanded, 0, 0, "");
	assert_string_equal(SL "rwin" SL "rfile1 " SL "rwin" SL "rfile3 " SL "rwin" SL "rfile5 " SL "rwin" SL "rdir6",
			expanded);
	free(expanded);

	expanded = strdup("/");
	expanded = append_selected_files(&rwin, expanded, 0, 0, "");
	assert_string_equal("/" SL "rwin" SL "rfile1 " SL "rwin" SL "rfile3 " SL "rwin" SL "rfile5 " SL "rwin" SL "rdir6",
			expanded);
	free(expanded);
}

static void
test_c(void)
{
	char *expanded;

	expanded = strdup("");
	expanded = append_selected_files(&lwin, expanded, 1, 0, "");
	assert_string_equal("lfile2", expanded);
	free(expanded);

	expanded = strdup("/");
	expanded = append_selected_files(&lwin, expanded, 1, 0, "");
	assert_string_equal("/lfile2", expanded);
	free(expanded);

	expanded = strdup("");
	expanded = append_selected_files(&rwin, expanded, 1, 0, "");
	assert_string_equal("" SL "rwin" SL "rfile5", expanded);
	free(expanded);

	expanded = strdup("/");
	expanded = append_selected_files(&rwin, expanded, 1, 0, "");
	assert_string_equal("/" SL "rwin" SL "rfile5", expanded);
	free(expanded);
}

void
test_append_selected_files(void)
{
	test_fixture_start();

	fixture_setup(setup);
	fixture_teardown(teardown);

	run_test(test_f);
	run_test(test_c);

	test_fixture_end();
}

/* vim: set tabstop=2 softtabstop=2 shiftwidth=2 noexpandtab : */

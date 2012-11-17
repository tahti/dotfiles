/* vifm
 * Copyright (C) 2001 Ken Steen.
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

#ifndef __NORMAL_H__
#define __NORMAL_H__

#include "keys.h"
#include "ui.h"

void init_normal_mode(int *key_mode);
void normal_cmd_zb(key_info_t key_info, keys_info_t *keys_info);
void normal_cmd_zt(key_info_t key_info, keys_info_t *keys_info);
void normal_cmd_zz(key_info_t key_info, keys_info_t *keys_info);
void normal_cmd_ctrl_wequal(key_info_t key_info, keys_info_t *keys_info);
void normal_cmd_ctrl_wless(key_info_t key_info, keys_info_t *keys_info);
void normal_cmd_ctrl_wgreater(key_info_t key_info, keys_info_t *keys_info);
void normal_cmd_ctrl_wplus(key_info_t key_info, keys_info_t *keys_info);
void normal_cmd_ctrl_wminus(key_info_t key_info, keys_info_t *keys_info);
void normal_cmd_ctrl_wpipe(key_info_t key_info, keys_info_t *keys_info);
int ffind(int ch, int backward, int wrap);
int cmd_paren(int lb, int ub, int inc);
int find_npattern(FileView *view, const char *pattern, int backward, int move);

#endif

/* vim: set tabstop=2 softtabstop=2 shiftwidth=2 noexpandtab cinoptions-=(0 : */
/* vim: set cinoptions+=t0 : */

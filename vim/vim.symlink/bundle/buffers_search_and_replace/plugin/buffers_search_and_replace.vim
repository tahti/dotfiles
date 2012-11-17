"""""""""""""""""""""""""""""""
"-----------------------------"
" File: buffers_search_and_replace.vim
" Author: Alexandru Ionut Munteanu (io_fx [ AT ] yahoo.fr)
" Description: The "Buffers Search & Replace" plugin is a simple but powerful buffer(s)
" search & replace tool. Search results can be set in the 'delete' category and replacement
" can be performed on the remaining or deleted search results.
"
" Notice : replacement features are new (from version 0.5) and not yet fully tested.
" Please report any bugs or feature requests.
"
" Version: 0.5.2
" Creation Date: 06.03.2007
" Last Modified: 06.04.2009
" {{{ History:
" History:
"
"         * "06.04.2009" - version 0.5.2 -
"           -added option to set range to the 'Bsc' command :
"                  :<range> Bsc <search_regex>
"              * <range> must be '<,'> for searching in visual selection
"              * if no <range> is specified, the search defaults to the
"                whole current buffer
"           - fixed the 'u' event to update with the current search type
"           - added a top line showing the type of the search
"
"         * "12.03.2009" - version 0.5.1 -
"           -added command to search and exclude some buffers from the search :
"                  :Bse <buffer_name_exclusion_regex> <search_regex>
"            Example: 
"                  :Bse \.c$ ifdef
"             -> this will search for 'ifdef' in all the buffers whose
"                name is not ending with '.c'
"           -added searching on custom buffers (buffer name filtering) command :
"                  :Bsf <buffer_name_filter_regex> <search_regex>
"            Example: 
"                  :Bsf \.h$ ifdef
"             -> this will search for 'ifdef' in all the buffers whose
"                name ends with '.h' (which could be all the '.h' files)
"
"         * "10.03.2009" - version 0.5 -
"           -optimised 'd' action, in order not to refresh the buffer each time
"           -Warning ! 'r' option for refreshing the screen has been
"            replaced with the 'replace' action !
"           -action 'D', 'r' and 'R' added :
"            "D"  : definitely deletes the current range search result (default: current line)
"            "r"  : replace action on the non-deleted searched results
"            "R"  : replace action on the deleted searched results
"           -changed name to buffers_search_and_replace.vim
"
"         * "07.03.2009" - version 0.4 -
"           -option 'd' implemented :
"            "d"  : deletes the current range search result (default: current line)
"                   the deleted search results are showed at the bottom of
"                   the buffer; deleting a deleted search results will
"                   set it again as search result
"           -added the ':Bsc <search_string>' command for searching only in
"            the current buffer
"
"					* "12.03.2007" - version 0.3 -
"					  -changed some internals to follow Jimmy advices (like
"					   using buffer numbers instead of buffer names to support
"					   unnamed buffers); thanks!
"					  -options J,A,Q,O,r,u,i,I,x,? implemented :
"					   "J"  : toggles jumping on the buffer on Enter
"					   "A"  : toggles auto-showing the context when j and k
"					   "Q"  : toggles auto-quitting the results when jumping
"					   "O"  : toggles between showing options or showing results 
"					   "r"  : refreshes the screen
"					   "u"  : update the search
"					   "i"  : toggles search match highlighting on the results buffer
"					   "I"  : toggles search match highlighting the other buffers
"					   "x"  : enables or disables quite-full-screen
"					   "?"  : toggles between showing help or showing results
"
"         * "08.03.2007" - version 0.2 -
"           -fixed an important bug; the search did not returned all
"            the matches
"
"         * "07.03.2007" - version 0.1 -
"           -initial version }}}
"
" Type zR if you use vim and don't understand what this file contains
"
" {{{ License : GNU GPL
" License:
"
" "Buffers Search" plugin searches the buffers for a pattern, and
" prints the results into a new buffer
"
" Copyright (C) 2007-2009  Alexandru Ionut Munteanu
"
" This program is free software; you can redistribute it and/or
" modify it under the terms of the GNU General Public License
" as published by the Free Software Foundation; either version 2
" of the License, or (at your option) any later version.
"
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software
" Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
" 02110-1301, USA.
"
" }}}
"
" {{{ Documentation
" Documentation:
"
" {{{ Installation :
" Installation:
"
" Download the file "buffers_search.vim" and put it into the plugins
" directory; on a unix-like system, the user plugin directory could be
" "~/.vim/plugin"
"
" }}}
" {{{ Optional Configuration
" Optional Configuration:
"
" Variables that you can modify :
"         *the variables take the possible values 0 or 1
"   -g:Bs_stay_on_buffer_results_when_entering_result : if to jump on
"    the result or not when pressing enter (option "J") (default is 1)
"   -g:Bs_auto_jump : if to auto-show the result or not when moving
"    with j and k (option "A") (default is 1)
"   -g:Bs_toggle_quit_enter : if you want to quit the buffer with the
"    search results when jumping (option "Q") (default is 0)
"   -g:Bs_results_buffer_match : if you want to have syntax highlight
"    on the search string in the buffer with the results (option "i")
"    (default is 1)
"   -g:Bs_buffers_match : if you want to have syntax highlight on the
"    search string in other buffers (option "I") (default is 1)
"
" What you may want to modify :
"   -the function "s:Bs_define_user_commands" defines the :Bs command;
"    you may want to change Bs to something else
"   -the functions "s:Bs_keys_mapping", "s:Bs_auto_jump_mapping",
"    "s:Bs_non_auto_jump_mapping" are defining the mappings inside the 
"    buffer that contains the search results (the keys that you use to 
"    navigate easily) 
"   -the function "s:Bs_syntax_highlight" contains the syntax highlight
"    inside the buffer with the search results; you could modify its
"    content as you prefer
"
" }}}
" {{{ Utilisation :
" Utilisation:
" 
"O_____________:
" User Commands:
"O_____________:
"
" Commands available :
"   ":Bs <search_regex>"
"   ":<range> Bsc <search_regex>"
"   ":Bsf <buffer_name_filter_regex> <search_regex>"
"   ":Bse <buffer_name_exclusion_regex> <search_regex>"
"
"   Examples:
"     :Bs function test
"     :Bsc variable
"     :Bsf \.h$ ifdef
"      ->this will search for 'ifdef' in all the buffers whose
"        name ends with '.h' (which could be all the '.h' files)
"     :Bse \.c$ ifdef
"      ->this will search for 'ifdef' in all the buffers whose
"        name is not ending with '.c'
"
" The 'Bs' command is searching in all the buffers and 'Bsc' only in the
" current buffer. The 'Bsc' command accepts an optional <range>, for
" searching only in the specified range. For searching in visual selection,
" <range> must be set to '<,'>
"
" After typing one of those commands followed by Enter (<CR>), two things
" could happend :
"   -if there is no result, there is no much change: nothing appears
"   -if there is at least one result, a new buffer appears at the
"   bottom at the screen, containing the results of the search; the
"   focus is then transferred to the buffer with the search results,
"   on the first result of the first printed buffer
"
"O____________________________:
" The Buffer With The Results:
"O____________________________:
" The buffer with the results contains the results for each buffer.
"
" Syntax of the buffer with the results :
" ---------------------------------------
"   The results for one buffer are between the "-buffer_number:buffer_name" line, and
" the next similar line that starts the next buffer or the end of the
" buffer.
"   Each result is written on one line : on the first part of the line,
" until the first ':' we have the line number of each result, and
" after the ':' we have the line containing the matched string from
" the buffer.
"   The matched string is highlighted like a search result, in all the
" results buffer.
"   Some basic syntax highlight is available, like matching numbers,
" strings, some ponctuation signs and paranthesis.
"
" Usage of the buffer with the results :
" --------------------------------------
"   Like every regular buffer, most of the tasks that you can do
" usually are available inside.
"
"  Default Mappings And Options:
"  -----------------------------
"   -the :number option is off
"   -the buffer is marked as nomodifiable
"   -the buffer is marked as a scratch; everything inside the buffer
"    will be lost, if not saved by the user
"   -some special keymaps are available inside the buffer :
"
"   Special Keymaps In The Buffer With The Results:
"   -----------------------------------------------
"   *  "Space" : moves the cursor on the first result of the next buffer
"              (below the "-buffer_number:buffer_name" line)
"   *  "Enter" or "Control-j" :
"         -see the "J" key to enable or disable jump when pressing Enter,
"          and for some additional infos
"         -if the cursor is on a result line (which starts with a
"           line number) :
"           **if we have jump enabled :
"            -the window from where we called the ":Bs"
"            command will change to the buffer that corresponds to this
"            result (the buffer number is on first line searching backwards,
"            that starts with "-"), and put the cursor on this window,
"            to the position of the first character of the matched search
"           **if we have jump disabled :
"            -the window from where we called the ":Bs" command will
"            change to the buffer that corresponds to this result
"         -if the cursor is not on a result line, then it's quite the
"           same result as before, except that 
"           **if jump is enabled, the cursor position will be at the top 
"             of the file : line 1, column 1
"           **if jump is disabled, the buffer is displayed starting at
"             line 1, column 1 (without moving the cursor)
"   *  "J" : enables or disables jumping on the buffer when pressing
"            Enter or <C-j>; default is disabled
"            *This option can also be changed with the variable 
"            g:Bs_stay_on_buffer_results_when_entering_result
"            -if auto-show option is enabled, this
"             option has no effect, because when auto-show is enabled,
"             we always jump to the location when pressing Enter or <C-j>
"             See the "A" key for auto-show and more infos.
"   *  "A" : enables or disables auto-showing the context of the results;
"            default is enabled
"            *This option can also be changed with the variable g:Bs_auto_jump
"            -if this is enabled, when you press j or k, the last
"             window, from where you called ":Bs" will show you the
"             context of the result line under the cursor
"            -this option puts J to be enabled, to always jump when 
"            pressing Enter or <C-j>
"   *  "d" : deletes the current range search result (default: current line)
"            the deleted search results are showed at the bottom of
"            the buffer; deleting a deleted search results will
"            set it again as search result
"   *  "D" : definitely deletes the current range search result (default: current line)
"   *  "Q" : enables or disables auto-quitting the buffer results
"            when jumping with the cursor on a result; default is
"            disabled
"            *This option can also be changed with the variable
"            g:Bs_toggle_quit_enter
"   *  "O" : toggles between showing the value of the options or
"            showing the results (push O another time to make options
"            disappear)
"   *  "r"  : replace action on the non-deleted searched results
"   *  "R"  : replace action on the deleted searched results
"   *  "u" : updates the search : replaces the current results with
"            the results of another search, with the same pattern
"   *  "i" : enables or disables the search match highlighting on the
"            buffer with the results; default is enabled
"            *This option can also be changed with the variable
"            g:Bs_results_buffer_match
"   *  "I" : enables or disables the search match highlighting on the
"            buffers that are not the buffer with the results;
"            default is enabled
"            *This option can also be changed with the variable
"            g:Bs_buffers_match
"   *  "x" : enables or disables "quite full screen"; default is
"            disabled
"            -resizes the buffer search results window to be quite
"             full screen, by leaving only some lines for other
"             buffers, in order to be able to view the context of the
"             results
"   *  "?" : toggles between showing help or showing results;default
"            is to show results (push ? another time to make help
"            disappear)
"            -when showing the help, a summary of the key maps is
"             printed in the buffer 
"   *  "q" : the q key deletes the buffer with the results
"
" }}}
" {{{ Contributions, Support, Bugs, ...
"
" Contributions Support Bugs:
"
" You can send any support question, bug reports, patches, ...
" to Alexandru Ionut Munteanu <io_fx [ AT ] yahoo.fr>
"
" }}}
" }}}

"""""""""""""""""""""""""""""""
" User settings
"""""""""""""""""""""""""""""""
"{{{ User variables
"if we stay on the buffer with the results when pressing enter
"-default is yes; disable this with J key in the buffer or by changing here at 0
if !exists("g:Bs_stay_on_buffer_results_when_entering_result")
  let g:Bs_stay_on_buffer_results_when_entering_result = 1
endif
"if auto-jump when moving in the buffer search or not
"it shows every time the context of the result;
"you can use enter to go to the result window
if !exists("g:Bs_auto_jump")
  let g:Bs_auto_jump = 1
endif
"set this to 1 if you want to quit the search results buffer when pressing
"enter, or push Q in the buffers search to enable/disable it
if !exists("g:Bs_toggle_quit_enter")
  let g:Bs_toggle_quit_enter = 0
endif
"set this to 0 if you don't want syntax highlight on the buffer with
"the results, or push i
if !exists("g:Bs_results_buffer_match")
  let g:Bs_results_buffer_match = 1
endif
"set this to 0 if you don't want syntax highlight on the other buffers
"(not the one with the results), or push I
if !exists("g:Bs_buffers_match")
  let g:Bs_buffers_match = 1
endif
"}}}
"{{{ User commands
" define user commands
function! s:Bs_define_user_commands()
  if !exists(':Bs')
    command! -nargs=1 Bs call s:Bs_search(<q-args>)
  endif
  if !exists(':Bsc')
    command! -range=% -nargs=1 Bsc call s:Bs_search_current(<line1>, <line2>, <f-args>)
  endif
  if !exists(':Bsf')
    command! -nargs=* Bsf call s:Bs_files_filter_search(<f-args>)
  endif
  if !exists(':Bse')
    command! -nargs=* Bse call s:Bs_files_search_exclude(<f-args>)
  endif
endfunction

"}}}
"{{{ Keys mapping in the buffer with the results
"defines the auto jump option mapping
function! s:Bs_auto_jump_mapping()
  "redefine <CR> and <C-j> to make a real jump,
  "if we have auto jump
  nnoremap <buffer> <silent> <C-j> :BsRealJump<CR>
  nnoremap <buffer> <silent> <CR> :BsRealJump<CR>
  "if we auto-jump define keys up and down to jump
  nnoremap <buffer> <silent> j j:BsJump<CR>
  nnoremap <buffer> <silent> k k:BsJump<CR>
endfunction

"defines the non auto-jump mappings
function s:Bs_non_auto_jump_mapping()
  nnoremap <buffer> <silent> <C-j> :BsJump<CR>
  nnoremap <buffer> <silent> <CR> :BsJump<CR>
endfunction

"defines the main key maps
function! s:Bs_keys_mapping()
	
  "<CR> and <C-j> have the same result
	"jumps to the buffer at the location of the phrase under cursor
  if g:Bs_auto_jump == 1
    call s:Bs_auto_jump_mapping()
  else
    call s:Bs_non_auto_jump_mapping()
  endif

  "space goes to the next buffer result
  nnoremap <buffer> <silent> <Space> :BsSpaceAction<CR>
  "q quits the buffer
  nnoremap <buffer> <silent> q :BsInitVariables<CR>:silent! bdel!<CR>:match<CR>
	"x toggles full screen or not
	nnoremap <buffer> <silent> x :BsToggleFullScreen<CR>
	"J activates the jump or deactivates it
	nnoremap <buffer> <silent> J :BsToggleJump<CR>
	"A auto-show (or jump, depending of J) to the files with the results
	nnoremap <buffer> <silent> A :BsToggleAutoJump<CR>
  "Q toggles buffers search window quit when pressing enter or <C-j>
  nnoremap <buffer> <silent> Q :BsToggleQuitWhenEnter<CR>
  "O shows the options (repush O to show results)
  nnoremap <buffer> <silent> O :BsToggleOptions<CR>
  "s searches again
  nnoremap <buffer> <silent> u :BsUpdateSearch<CR>
  "i toggles the highlight match on the results
  nnoremap <buffer> <silent> i :BsToggleResultsBufferMatch<CR>
  "I toggles the highlight match on the buffers (differets from the
  "results buffer)
  nnoremap <buffer> <silent> I :BsToggleBuffersMatch<CR>
  "? displays help (repush ? to show results)
  nnoremap <buffer> <silent> ? :BsToggleHelp<CR>
	"d deletes the current line(s), by saving it and showing at the bottom
	noremap <buffer> <silent> d :BsDelResults<CR>
	"D Definitely deletes the current line(s) from the results
	noremap <buffer> <silent> D :BsDefinitelyDelResults<CR>
  "r replaces the current search of the non-deleted search results
  nnoremap <buffer> r :call BsReplace(0)<CR>
  "R replaces the current search of the deleted search results
  nnoremap <buffer> R :call BsReplace(1)<CR>
endfunction

"}}}
"{{{ Syntax highlight for the buffer with the results
function! s:Bs_syntax_highlight()
  
  "if we have the option to highlight matches in the results buffer
  if g:Bs_results_buffer_match == 1
    "syntax the search
    exe 'silent! mat Search /'.g:Bs_str_highlight.'/'
  endif
  
  "simple number highlight
  sy match buffers_nums "\<\d\+\>"
  hi def link buffers_nums Number

  "simple keywords
  sy keyword Clike if then else case break void char int long const continue default do double enum extern float for goto register auto return short signed sizeof static struct switch typedef union unsigned volatile while
  hi def link Clike Keyword

  "simple string highlight
  sy match ponctuation "\.\|?\|!\|\,\|;\|<\|>\|\~\|&\|="
  hi def link ponctuation Type
  
  "simple paranthesis highlight
  sy match paranthesis "(\|)\|\[\|\]\|{\|}"
  hi def link paranthesis NonText

  "simple string highlight
  sy region buffers_doubleq start='"' end='"\|$'
  sy region buffers_singleq start="'" end="'\|$"
  sy region buffers_backticks start="`" end="`\|$"
  hi def link buffers_doubleq String
  hi def link buffers_singleq String
  hi def link buffers_backticks String

  "simple '//' comments
  sy match comments "\/\/.*"
  hi def link comments Comment

  "left numbers
  sy match line_numbers "^\d\+"
  hi def link line_numbers LineNr

  "buffer titles
  sy match buffers_title "^\-.*$"
  hi def link buffers_title Title

endfunction

"}}}
"-----------------------------"
"""""""""""""""""""""""""""""""

"{{{ Definition of user commands
"avoid loading it twice
if exists("Bs_buffers_search")
  finish
else
  call s:Bs_define_user_commands()
endif
let Bs_buffers_search = 1
"}}}

"""""""""""""""""""""""""""""""
" Internal variables
"""""""""""""""""""""""""""""""
"{{{ Defines some internal variables
function! s:Bs_init_variables()
  "name of the results buffer
  let g:Bs_results_buffer_name = "Buffers_search_result"
  "the search that we entered
  let g:Bs_search = ""
  let g:Bs_files_filter = ".*"
  let g:Bs_exclude_using_filter = 0
  let g:Bs_str_highlight = ""
  let g:Bs_range_line1 = -1
  let g:Bs_range_line2 = -1
  let g:Bs_current_buffer_number_of_lines = 0
  let g:Bs_only_current_buffer = 0
  let g:Bs_current_buffer = 0
  "string we use to replace the search
  let g:Bs_replace = ""
  "the results of the search
  "results structure :
  "  { 'buffer1_number' : { 'line_number' : 'line_content' , 'line_number2' : 'line_content2' },
  "    'buffer2_number' : { 'line_number' : 'line_content' , 'line_number2' : 'line_content2' } }
  let g:Bs_results = {}
  "the deleted results
  let g:Bs_deleted_results = {}
  "total number of results
  let g:Bs_number_of_results = 0
  "the initial window where we were
  let g:Bs_initial_window_number = 1
  "the initial is not full screen
  let g:Bs_is_full_screen = 0
  "if we are showing help or not
  let g:Bs_on_help = 0
  "if we are showing the options or not
  let g:Bs_on_options = 0
  "if we refresh or update, we keep the old position
  let g:Bs_position = []
  "if we showed the results at least once
  let g:Bs_showed_results = 0
endfunction

call s:Bs_init_variables()

"}}}

"""""""""""""""""""""""""""""""
" Internal functions
"""""""""""""""""""""""""""""""
"Main functions
"{{{
function! s:Bs_remove_result(source_dict, buf_number, line_number)
  "erase the data from the source results
  call remove(a:source_dict["-".a:buf_number],a:line_number)
  if len(keys(a:source_dict["-".a:buf_number])) == 0
    call remove(a:source_dict,"-".a:buf_number)
  endif
endfunction

"}}}
"{{{ s:Bs_move_result(source_dict, dest_dict, buf_number, line_number)
function! s:Bs_move_result(source_dict, dest_dict, buf_number, line_number)
  if has_key(a:source_dict, "-".a:buf_number)
    let l:result_line_content = a:source_dict["-".a:buf_number]

    if has_key(l:result_line_content, a:line_number)
      "get the old (deleted) results
      if has_key(a:dest_dict,"-".a:buf_number)
        let l:old = a:dest_dict["-".a:buf_number]
      else
        let l:old = {}
      endif

      let l:old[a:line_number] = l:result_line_content[a:line_number]

      "put the data in the dest results
      let a:dest_dict["-".a:buf_number] = l:old
      "remove the result from the source
      call s:Bs_remove_result(a:source_dict, a:buf_number, a:line_number)
    endif

  endif
endfunction

"}}}
"{{{ s:Bs_delete_and_paste_line()
function! s:Bs_delete_and_paste_line(delete_result, buf_number, line_number, move_to_results)
  
  let l:source_results = g:Bs_deleted_results
  let l:target_results = g:Bs_results

  "delete current line
  normal! dd

  if a:delete_result == 1
    let l:source_results = g:Bs_results
    let l:target_results = g:Bs_deleted_results
  endif

  "if the buf_number from the source results only contains the current line_number
  ", delete the buffer name line
  let l:source_buffer_results = l:source_results["-".a:buf_number]
  let l:source_buffer_results_length = len(l:source_buffer_results)
  let l:number_of_source_buffers = len(keys(l:source_results))
  if l:source_buffer_results_length <= 1
    let l:test=search("^-".a:buf_number.":","bc")
    normal! "_dd
    if l:number_of_source_buffers > 1
      normal! "_dd
    endif
  endif

  if a:delete_result == 1
    let l:test=search("^\" Deleted from the search results") 
  else
    normal! gg
  endif

  let l:reference_buffer_number = 0
  let l:reference_is_buffer_after = 0
  let l:buffer_exists = 0

  "find out if the buffer exists
  if has_key(l:target_results, "-".a:buf_number)
    let l:buffer_exists = 1
    let l:reference_buffer_number = a:buf_number
  else
    "if the buffer does not exists, find out the existing buffer number
    " "after" or "before" it
    let l:buffer_numbers = sort(keys(l:target_results),"s:Bs_numerical_buffer_sort")

    for l:buffer_number in l:buffer_numbers
      let l:reference_buffer_number = str2nr(strpart(l:buffer_number,1))
      if l:reference_buffer_number > a:buf_number
        let l:reference_is_buffer_after = 1
        break
      endif
    endfor
  endif

  let l:line_after_exists = 0
  let l:reference_buffer_line_number = 0

  "if the buffer exists, find out the line of the buffer "after" or "before" it
  if l:buffer_exists == 1
    let l:buffer_results = l:target_results["-".l:reference_buffer_number]
    let l:buffer_results_keys = sort(keys(l:buffer_results) ,"s:Bs_numerical_line_sort")

    for l:result_line in l:buffer_results_keys
      let l:reference_buffer_line_number = l:result_line
      if l:result_line > a:line_number
        let l:line_after_exists = 1
        break
      endif
    endfor
  endif

  let l:create_new_buffer_result = 1

  "if we already have buffer results
  if l:reference_buffer_number != 0
    "move to the existing buffer or nearest existing buffer
    let l:test=search("^-".l:reference_buffer_number.":") 

    if l:buffer_exists == 1
      let l:create_new_buffer_result = 0
      "move to the existing buffer line number
      let l:test=search("^".l:reference_buffer_line_number."\\s\\+:") 
      if l:line_after_exists == 1
        normal! k
      endif
    endif
  endif

  let l:cursor_line_offset = 0

  if a:move_to_results == 1
    "create a new buffer for results
    if l:create_new_buffer_result == 1
      let l:o_letter = 'o'
      if l:reference_is_buffer_after == 1
        let l:o_letter = 'O'
        normal! O
        let l:cursor_line_offset = l:cursor_line_offset + 1
      else
        let l:test=search("^$")
      endif
  
      if a:move_to_results == 1
       let l:buffer_name = bufname(str2nr(a:buf_number))
       let l:my_command = "normal! ".l:o_letter."-".a:buf_number.":".l:buffer_name
       execute l:my_command
      endif
  
      let l:cursor_line_offset = l:cursor_line_offset + 1
    endif
  
    normal! p
  
    "always have an empty line at the end of the results
    let l:last_line = line('$')
    if a:delete_result != 1
      let l:test=search("^\" Deleted from the search results", 'nc') 
      let l:last_line = l:test - 2
    endif
  
    let l:current_position = getpos('.')
    if l:current_position[1] == l:last_line
      normal! o
      let l:cursor_line_offset = l:cursor_line_offset + 1
    endif
  endif

  return l:cursor_line_offset

endfunction

"}}}
"{{{ s:Bs_delete_action()
"functions mapped by 'd' and 'D', for deleting results(lines) from the buffer
function! s:Bs_delete_action(start_line, end_line, move_to_results)

  let g:Bs_position = getpos('.')

  setlocal modifiable

  "save the initial cursor position
  let l:start_line = a:start_line
  let l:end_line = a:end_line

  "found the number of line results to delete
  let l:number_of_results_to_delete = 0
  let l:first_time = 0
  for l:current_line in range(l:start_line, l:end_line)
    call cursor(l:current_line,0)
    normal! 0
    let l:line_is_to_delete = search("^\\d\\+\\s\\+:","nc", l:current_line)

    if l:line_is_to_delete == l:current_line
      if l:first_time == 0
        let l:first_time = 1
        let g:Bs_position[1] = l:current_line
      endif
      let l:number_of_results_to_delete = l:number_of_results_to_delete + 1
    endif
  endfor

  "delete the lines
  for l:current_line in range(1, l:number_of_results_to_delete)
    call setpos('.', g:Bs_position)

    "get the buf number for the start line
    let l:line_buf_number = s:Bs_get_cur_buf_number()
    "get the line number for the start line
    let l:line_line_number = s:Bs_get_cur_line_number()

    let l:delete_result = 2

    "search if we delete result
    if has_key(g:Bs_results, "-".l:line_buf_number)
      let l:result_line_content = g:Bs_results["-".l:line_buf_number]

      if has_key(l:result_line_content, l:line_line_number)
        let l:delete_result = 1
      endif
    endif

    "search if we delete a deleted result
    if l:delete_result == 2
      if has_key(g:Bs_deleted_results, "-".l:line_buf_number)
        let l:result_line_content = g:Bs_deleted_results["-".l:line_buf_number]

        if has_key(l:result_line_content, l:line_line_number)
          let l:delete_result = 0
        endif
      endif
    endif

    let l:source_results = g:Bs_results
    let l:target_results = g:Bs_deleted_results

    if l:delete_result != 1
      let l:source_results = g:Bs_deleted_results
      let l:target_results = g:Bs_results
    endif

    "find if we delete the latest result
    let l:we_delete_the_latest_result_in_the_buffer = 0
    if l:delete_result == 0 || l:delete_result == 1
      let l:current_buffer_results = l:source_results["-".line_buf_number]
      let l:current_buffer_lines = sort(keys(l:current_buffer_results) ,"s:Bs_numerical_line_sort")
      let l:current_buffer_number_of_results = len(l:current_buffer_lines)
      if l:current_buffer_number_of_results > 1
        let l:last_current_buffer_line = l:current_buffer_lines[l:current_buffer_number_of_results-1]
        if line_line_number == l:last_current_buffer_line
          let l:we_delete_the_latest_result_in_the_buffer = 1
        endif
      endif
    endif

    "delete the current result
    if l:delete_result == 1
      call s:Bs_delete_and_paste_line(1, line_buf_number, line_line_number, a:move_to_results)
    elseif l:delete_result == 0
      let l:cursor_line_offset = s:Bs_delete_and_paste_line(0, line_buf_number, line_line_number, a:move_to_results)
      if a:move_to_results == 1
        let g:Bs_position[1] = g:Bs_position[1] + l:cursor_line_offset
        let g:Bs_position[1] = g:Bs_position[1] + 1
      endif
    endif

    if a:move_to_results == 1
      call s:Bs_move_result(l:source_results, l:target_results, line_buf_number, line_line_number)
    else
      call s:Bs_remove_result(l:source_results, line_buf_number, line_line_number)
    endif

    "if we delete the latest result, move down 2 lines
    if l:we_delete_the_latest_result_in_the_buffer == 1
        let g:Bs_position[1] = g:Bs_position[1] + 2
    endif 

  endfor

  call setpos('.',g:Bs_position)
  normal! zz

  call s:Bs_jump(0)

  setlocal nomodifiable

endfunction

"}}}
"{{{ s:BsReplace(replace_deleted_results)
function! BsReplace(replace_deleted_results)

  let l:string_to_replace = g:Bs_search
  let l:results = g:Bs_results
  let g:Bs_position = getpos('.')
  let l:old_cursor_position = getpos('.')

  if a:replace_deleted_results
    let l:string_to_replace = input("What to replace (deleted results) ? ", g:Bs_search)
    let g:Bs_replace = input("Replace '".l:string_to_replace."' (deleted results) with ? ", g:Bs_search)
    let l:results = g:Bs_deleted_results
  else
    let l:string_to_replace = input("What to replace (results) ? ", g:Bs_search)
    let g:Bs_replace = input("Replace '".l:string_to_replace."' (results) with ? ", g:Bs_search)
  endif

  "change the window - go to the initial window
  exe g:Bs_initial_window_number.' wincmd w'

  "for each buffer
  for l:buffer_number_str in sort(keys(l:results),"s:Bs_numerical_buffer_sort")

    "get the results of this buffer
    let l:buffer_results = l:results[l:buffer_number_str]
    let l:buffer_results_keys = sort(keys(l:buffer_results) ,"s:Bs_numerical_line_sort")
    let l:number_of_buffer_results_keys = len(l:buffer_results_keys)

    if l:number_of_buffer_results_keys > 0
      let l:buffer_number = str2nr(strpart(l:buffer_number_str,1))

      "go to the buffer
      exe 'b! '.l:buffer_number

      "iterate over the results of this buffer
      for l:result_line in l:buffer_results_keys
        execute "normal! ".l:result_line."G"
        execute "silent! .s/".l:string_to_replace."/".g:Bs_replace."/g"
        let l:result_line_content = l:buffer_results[l:result_line]
        let l:new_result_line_content = substitute(l:result_line_content, l:string_to_replace, g:Bs_replace, "g")
        let l:results[l:buffer_number_str][l:result_line] = l:new_result_line_content
      endfor
    endif

  endfor

  let g:Bs_str_highlight = g:Bs_replace

  let g:Bs_showed_results = 0
  call s:Bs_refresh_results()
  let g:Bs_showed_results = 1

  call setpos('.', l:old_cursor_position)
  call s:Bs_jump(0)

endfunction

"}}}
"{{{ s:Bs_show_help()
function! s:Bs_show_help()
  "hm means help message
  let l:hm = "-Default key mappings :\n\n"
  let l:hm = l:hm."\"Space\"\t : moves the cursor on the first result of the next buffer\n"
  let l:hm = l:hm."\"Enter or Control-j\"\t : shows or jumps to the result under the cursor\n"
  let l:hm = l:hm."\"J\"\t : enables or disables jumping on the buffer when pressing Enter or Control-j\n"
  let l:hm = l:hm."\"A\"\t : enables or disables auto-showing the context of the results when pressing j or k\n"
  let l:hm = l:hm."\"d\"\t : deletes the current range search result (default: current line);\n"
  let l:hm = l:hm."          Deleted results are shown at the bottom and can be undeleted with \"d\"\n"
  let l:hm = l:hm."\"D\"\t : Definitely deletes the current range search result (default: current line)\n"
  let l:hm = l:hm."\"r\"\t : replace action on the non-deleted searched results\n"
  let l:hm = l:hm."\"R\"\t : replace action on the deleted searched results\n"
  let l:hm = l:hm."\"Q\"\t : enables or disables auto-quitting the buffer results when jumping\n"
  let l:hm = l:hm."\"O\"\t : toggles between showing options or showing results\n"
  let l:hm = l:hm."\"u\"\t : replaces this search with a newer one (with the same keyword)\n"
  let l:hm = l:hm."\"i\"\t : enables or disables the search match highlighting on the buffer with the results\n"
  let l:hm = l:hm."\"I\"\t : enables or disables the search match highlighting on the other buffers (not the buffer with the results)\n"
  let l:hm = l:hm."\"x\"\t : enables or disables quite-full-screen\n"
  let l:hm = l:hm."\"q\"\t : deletes the buffer with the results\n"
  let l:hm = l:hm."\"?\"\t : toggles between showing help or showing results\n\n"
  let l:hm = l:hm."For more info, read the full docs at the start of the file buffers_search.vim, or contact me.\n"
  let l:hm = l:hm."This program is free software, licensed under the GNU General Public License\n"
  let l:hm = l:hm."Copyright (c) 2007-2009 : Alexandru Ionut Munteanu - io_fx AT yahoo.fr"

  call s:bs_print_in_buffer(l:hm)
  call cursor(1,1)
endfunction

"}}}
"{{{ s:Bs_show_options()
"shows the options
function! s:Bs_show_options()
  "opts means options
  let l:opts = "-Options :\n\n"
  "J option
  let l:opts = l:opts."\"J\" - jumping on the buffer when pressing Enter : "
  if g:Bs_stay_on_buffer_results_when_entering_result == 0
    let l:opts = l:opts."\"enabled\"\n"
  else
    let l:opts = l:opts."\"disabled\"\n"
  endif
  "A option
  let l:opts = l:opts."\"A\" - auto-show result context : "
  if g:Bs_auto_jump == 1
    let l:opts = l:opts."\"enabled\"\n"
  else
    let l:opts = l:opts."\"disabled\"\n"
  endif
  "Q option
  let l:opts = l:opts."\"Q\" - auto-quit the buffer when jumping : "
  if g:Bs_toggle_quit_enter == 1
    let l:opts = l:opts."\"enabled\"\n"
  else
    let l:opts = l:opts."\"disabled\"\n"
  endif
  "i option
  let l:opts = l:opts."\"i\" - show match highlighting on the results buffer : "
  if g:Bs_results_buffer_match == 1
    let l:opts = l:opts."\"enabled\"\n"
  else
    let l:opts = l:opts."\"disabled\"\n"
  endif
  "I option
  let l:opts = l:opts."\"I\" - show match highlighting on the other buffers : "
  if g:Bs_buffers_match == 1
    let l:opts = l:opts."\"enabled\"\n"
  else
    let l:opts = l:opts."\"disabled\"\n"
  endif
  let l:opts = l:opts."\n"

  call s:Bs_print_in_buffer(l:opts)
  call cursor(1,1)
endfunction

"}}}
"{{{ s:Bs_jump()
"	jumps to the buffer under the cursor, at the exact position
"calculates the coords and calls Bs_jump_buffer
" if real_jump == 1, then we jump to the results; otherwise, we only
" show the context
function! s:Bs_jump(real_jump)
  "if we're not on options or help
  if g:Bs_on_options == 0 && g:Bs_on_help == 0
    let l:bs_line = s:Bs_get_cur_line_number()
    let l:bs_buf_number = s:Bs_get_cur_buf_number()
	  let [l:bs_line_number, l:bs_column] = searchpos(g:Bs_search,"n")
    call s:Bs_jump_buffer(l:bs_buf_number,l:bs_line,l:bs_column,a:real_jump)
  endif
endfunction

"}}}
"{{{ s:Bs_files_filter_search(..)
function! s:Bs_files_filter_search(...)
  let g:Bs_files_filter = a:1
  call s:Bs_search_buffers(a:2,0)
endfunction

"}}}
"{{{ s:Bs_files_search_exclude(..)
function! s:Bs_files_search_exclude(...)
  let g:Bs_exclude_using_filter = 1
  call s:Bs_files_filter_search(a:1, a:2)
endfunction

"}}}
"{{{ s:Bs_search(search)
function s:Bs_search(search)
  call s:Bs_search_buffers(a:search,0)
endfunction

"}}}
"{{{ s:Bs_search_current(search)
function s:Bs_search_current(line1, line2, search)
  let g:Bs_range_line1 = a:line1
  let g:Bs_range_line2 = a:line2
  let g:Bs_only_current_buffer = 1
  let g:Bs_current_buffer = bufnr('%')
  call s:Bs_search_buffers(a:search,0)
endfunction

"}}}
"{{{ s:Bs_update_search(search)
function! s:Bs_update_search(search)
  let g:Bs_showed_results = 0
  let g:Bs_position = getpos('.')
  call s:Bs_search_buffers(a:search,1)
endfunction

"}}}
"{{{ s:Bs_refresh_search() : calls Bs_show_results
function! s:Bs_refresh_results()
  "we save cursor position
  let g:Bs_position = getpos('.')
  call s:Bs_show_results()
endfunction

"}}}
"{{{ s:Bs_search_buffers(search, ...)
"search through the buffers and calls show_results
"after that, with the results found in the buffers
"
"results structure :
"  { 'buffer1_number' : { 'line_number' : 'line_content' , 'line_number2' : 'line_content2' },
"    'buffer2_number' : { 'line_number' : 'line_content' , 'line_number2' : 'line_content2' } }
" the second argument, research; if 1, don't change the origin of the
" window; if 0, change the origin
function! s:Bs_search_buffers(search, research)

	"we set the search string
	let g:Bs_search = a:search
  let g:Bs_str_highlight = a:search
	"initialise the empty results
	let g:Bs_results = {}
	"initialise the deleted results
	let g:Bs_deleted_results = {}
  "number of results
  let g:Bs_number_of_results = 0

  "if we don't do a re-search
  if a:research == 0
	  "we save the initial window number
	  let g:Bs_initial_window_number = bufwinnr('%')
  endif

  "we save the initial cursor position
  let l:initial_position = getpos(".")
  
  "we only search in the buffers once
  let l:searched_buffers = []

  let l:buffer_number = 1
  "the number of the buffer at the start
  let l:start_buffer_number = bufnr('%')
  "get the number of the last buffer
  let l:last_buffer_number = bufnr('$')

  "the number of the Buffer search results
  let l:results_buffer_number = bufnr(g:Bs_results_buffer_name)
  
  if g:Bs_only_current_buffer == 1
    let l:buffer_number = g:Bs_current_buffer
    if l:buffer_number <= 0
      let l:buffer_number = 1
    endif
    let l:last_buffer_number = l:buffer_number
  endif 

  "we iterate over the buffers
  while (l:buffer_number <= l:last_buffer_number)

    "if the buffer is listed
    if buflisted(l:buffer_number)

      "filter the buffer names not matching the g:Bs_files_filter
      let l:buffer_name = bufname(l:buffer_number)
      let l:buffer_name_match_start = match(l:buffer_name, g:Bs_files_filter)
      let l:buffer_matches_filter = l:buffer_name_match_start != -1

      let l:keep_buffer = 0
      if l:buffer_matches_filter && ! g:Bs_exclude_using_filter
        let l:keep_buffer = 1
      endif
      if ! l:buffer_matches_filter && g:Bs_exclude_using_filter
        let l:keep_buffer = 1
      endif

      if l:keep_buffer
  
        "go to the buffer
        exe 'b! '.l:buffer_number

        "if we search only in the current buffer, keep the total number of
        "lines
        if g:Bs_only_current_buffer == 1
          let g:Bs_current_buffer_number_of_lines = line('$')
        endif
  
        "skip the buffer search result buffer
        if l:buffer_number == l:results_buffer_number
          call add(l:searched_buffers,l:buffer_number)
          let l:buffer_number = l:buffer_number + 1
          if buflisted(l:buffer_number) 
            exe 'b! '.l:buffer_number
          endif
        endif
    
        "move at the top
        normal! gg
    
        "if we didn't already read this buffer
        if !s:Bs_is_in_list_int(l:searched_buffers,l:buffer_number)
    
          "we put the buffer in the list
          call add(l:searched_buffers,l:buffer_number)
    
          "we search over the current buffer
          let l:buffer_result = {}
          "we stock the searched lines
          let l:stocked_lines = []
          let l:total_lines = line("$$")

          "go at the start of the current buffer range search
          if g:Bs_range_line1 != -1
            call cursor(g:Bs_range_line1,0)
          endif

          while search(g:Bs_search,'',l:total_lines) > 0
            "we get the current line number
            let l:line_number = line('.')

            "get out if more than line2 from the range
            if g:Bs_range_line2 != -1
              if l:line_number > g:Bs_range_line2
                break
              endif
            endif
  
            "if the line it's not stored
            if !s:Bs_is_in_list_int(l:stocked_lines,l:line_number)
              "we store it
              call add(l:stocked_lines,l:line_number)
              let g:Bs_number_of_results = g:Bs_number_of_results + 1
              "we get the line content
              let l:line_content = getline(l:line_number)
              "store the line content
              let l:buffer_result[l:line_number] = l:line_content
            endif
          endwhile "end search on the current buffer
          
          "if we have results,
          if (len(l:buffer_result) > 0)
            "we store the result for this buffer
            let g:Bs_results['-'.l:buffer_number] = l:buffer_result
          endif 
      
        endif "end is_in_list
  
      endif "end l:buffer_name_match_start

    endif "end isbuflisted

    "increment our counter
    let l:buffer_number = l:buffer_number + 1

  endwhile "end iteration over the buffers

  "go to the buffer from start
  exe 'b! '.l:start_buffer_number
  "we restore the initial cursor position
  call setpos('.', l:initial_position)
  
  "if we have results,
  if (len(g:Bs_results) > 0)
    "we show the results
    call s:Bs_show_results()
    call s:Bs_keys_mapping()
  else
    "if we have the Buffer search window, delete this buffer 
    let l:results_buffer_number = s:Bs_get_go_buffer(g:Bs_results_buffer_name)
    "change to the search results buffer
    exe 'b! '.l:results_buffer_number
    "we verify if the current buffer is the right one
    if bufname('%') =~ g:Bs_results_buffer_name
      "we delete it
      bdel!
    endif
    "center the cursor in the old buffer
    normal! z.
  endif

endfunction

"}}}
"{{{ s:Bs_jump_buffer(...)
"jumps to a line into a buffer
function! s:Bs_jump_buffer(...)
  let l:buffer_number = a:1
  let l:buffer_line = str2nr(a:2)
  let l:buffer_column = str2nr(a:3)
  "if we are really jumping or not
  let l:real_jump = str2nr(a:4)

	"we keep the results buffer number
	let l:results_buffer_window_number = bufwinnr('%')
	"we save the position in the results buffer
  let l:results_buffer_position = getpos(".")
  
  if l:buffer_line == 0
    let l:buffer_line = 1
  endif
  
  "don't count the ^\d+\s+: at the end of the line
  let l:buffer_column=l:buffer_column - 7
  if l:buffer_column == 0
    let l:buffer_column = 1
  endif
	
  "change the window - go to the initial window
  exe g:Bs_initial_window_number.' wincmd w'
  "change the buffer
  exe 'b! '.l:buffer_number
  "go to the line
  call cursor(l:buffer_line,l:buffer_column)
	"put in the center of the screen
	normal! zz
	let l:current_line = getline('.')

  "open eventual folding
  exe 'silent! foldopen'
  exe 'silent! foldopen'

  "match the search in the new buffer
  if g:Bs_buffers_match == 1
    exe 'silent! match Search /'.g:Bs_str_highlight.'/'
    redraw
  else
    exe 'match'
  endif

	"if we stay in the buffer with the searches
	if g:Bs_stay_on_buffer_results_when_entering_result == 1
    "if we don't necessary jump
    if l:real_jump == 0
		  exe l:results_buffer_window_number.' wincmd w'
		  "restore the position in the buffer search
  	  call setpos('.', l:results_buffer_position)
    else
      "if we close the buffers search
      if g:Bs_toggle_quit_enter == 1
        exe l:results_buffer_window_number.' wincmd w'
        normal! q
      endif
    endif
  else
    "when jumping on another buffer,
    "if we close the buffers search
    if g:Bs_toggle_quit_enter == 1
		  exe l:results_buffer_window_number.' wincmd w'
      normal! q
    endif
	endif

endfunction

"}}}
"{{{ s:Bs_numerical_line_sort(i1,i2)
"function for sorting the lines
function! s:Bs_numerical_line_sort(i1,i2)
  "ln means line number
  let l:ln1 = str2nr(split(a:i1,':')[0])
  let l:ln2 = str2nr(split(a:i2,':')[0])
  
  return l:ln1 == l:ln2 ? 0 : l:ln1 > l:ln2 ? 1 : -1
endfunction

"}}}
"{{{ s:Bs_numerical_buffer_sort(i1,i2)
"function for sorting the buffers
function! s:Bs_numerical_buffer_sort(i1,i2)
  "bn means buf number
  let l:bn1 = str2nr(strpart(a:i1,1))
  let l:bn2 = str2nr(strpart(a:i2,1))
  
  return l:bn1 == l:bn2 ? 0 : l:bn1 > l:bn2 ? 1 : -1
endfunction

"}}}
"{{{ s:Bs_process_results() : returns string_result
"processes the results and returns a String
"with the representation of the results
function! s:Bs_process_results(results)

  let l:string_result = ""

  "for each buffer, we show the results
  let l:number_of_buffers = len(a:results)
  for l:buffer_number in sort(keys(a:results),"s:Bs_numerical_buffer_sort")

    "we get the results of this buffer
    let l:buffer_results = a:results[l:buffer_number]
    let l:buffer_results_keys = sort(keys(l:buffer_results) ,"s:Bs_numerical_line_sort")
    let l:number_of_buffer_results_keys = len(l:buffer_results_keys)

    if l:number_of_buffer_results_keys > 0

      "we get the buffer name
      let l:buffer_name = bufname(str2nr(strpart(buffer_number,1)))
      "we print the buffer number
      let l:string_result = l:string_result.l:buffer_number.":"
      "we print the buffer name
      let l:string_result = l:string_result.l:buffer_name."\n"

      "we iterate over the results of this buffer
      for l:result_line in l:buffer_results_keys
        let l:result_line_content = l:buffer_results[l:result_line]
        "we print line_number and line_content
        let l:line_number_content = printf('%-4s : %s',l:result_line,l:result_line_content)
        let l:string_result = l:string_result.l:line_number_content."\n"
      endfor

      "put a space
      let l:string_result = l:string_result."\n"

    endif

  endfor
  
  return l:string_result

endfunction

"}}}
"{{{ s:Bs_show_results()
function! s:Bs_show_results()
  
  let l:print_content = "\"--------------------------------------------------------------\"\n"

  "Bse or Bsf
  if g:Bs_files_filter != ".*"
    "Bse
    if g:Bs_exclude_using_filter == 1
      let l:print_content = l:print_content."\" Search excluding files '".g:Bs_files_filter."'. \"\n"
    "Bsf
    else
      let l:print_content = l:print_content."\" Search only in files '".g:Bs_files_filter."'. \"\n"
    endif
  "Bsc command
  else
    if g:Bs_only_current_buffer == 1
      let l:current_buffer_name = bufname(g:Bs_current_buffer)
      let l:current_buf_str = "\" Search in the buffer '".l:current_buffer_name."'"
      "with range
      if g:Bs_range_line1 != 1 || g:Bs_range_line2 != g:Bs_current_buffer_number_of_lines
        let l:print_content = l:print_content.l:current_buf_str." from ".g:Bs_range_line1." to ".g:Bs_range_line2.". \"\n"
      "without range
      else
        let l:print_content = l:print_content.l:current_buf_str.". \"\n"
      endif
    "Bs 
    else
      let l:print_content = l:print_content."\" Search in all buffers. \"\n"
    endif
  endif

  let l:print_content = l:print_content."\" ".g:Bs_number_of_results." search results for '".g:Bs_search."' :  \""
  let l:print_content = l:print_content."\n\"--------------------------------------------------------------\"\n\n"

  "we process the results
  let l:print_content = l:print_content.s:Bs_process_results(g:Bs_results)
  
  let l:print_content = l:print_content."\"------------------------------------\"\n"
  let l:print_content = l:print_content."\" Deleted from the search results :  \""
  let l:print_content = l:print_content."\n\"------------------------------------\"\n\n"

  "the deleted content
  if len(keys(g:Bs_deleted_results)) > 0
    let l:deleted_content = s:Bs_process_results(g:Bs_deleted_results)
    let l:print_content = l:print_content.l:deleted_content."\n"
  endif

  call s:Bs_print_in_buffer(l:print_content)

  "if it's not the first time that we show results
  if g:Bs_showed_results == 1
    "we put back cursor position
    call setpos('.',g:Bs_position)
  else
    "go at the start
    call cursor(7,7)
    normal! zz
  endif

  "if we have auto jump, jump !
  if g:Bs_auto_jump == 1
    call s:Bs_jump(0)
  endif
 
  "define buffer mappings + highlight
  call s:Bs_syntax_highlight()
  
  let g:Bs_showed_results = 1

endfunction

"}}}
"{{{ s:Bs_print_in_buffer(print_content)
"shows the results of the search
function! s:Bs_print_in_buffer(print_content)
  
  "if the search results buffer already exists
  let l:results_buffer_number = s:Bs_get_go_buffer(g:Bs_results_buffer_name)
  "we should be on the results buffer now
  
  "we verify that the current buffer is the correct one
  if bufname('%') =~ g:Bs_results_buffer_name
    
    "set the buffer modifiable
    setlocal modifiable
  
    "we erase the buffer
    silent! 1,$ d
    "we put the results
    silent! put! = a:print_content
    "erase the last line
    $ d d
 
    "no number at left
    setlocal nonumber
		"underline the line under the cursor
		setlocal cursorline
    "don't ask for saving buffer at exit, etc..
    setlocal buftype=nowrite
  
    "set the buffer as non modifiable
    setlocal nomodifiable
    
  endif "end verify if it's the currect buffer

  redraw

endfunction

"}}}

"Utils functions
"{{{ s:Bs_get_cur_buf_number() : returns the current result buffer number
function! s:Bs_get_cur_buf_number()
  let l:bs_num = getline(search("^-.*:","nb"))
  let l:bs_buf_number=strpart(strpart(l:bs_num,1),0,stridx(l:bs_num,":")-1)
  return l:bs_buf_number
endfunction

"}}}
"{{{ s:Bs_get_cur_line_number() : returns the current result line number
function! s:Bs_get_cur_line_number()

  let l:bs_line = 1
  if strpart(getline('.'),0,1) == '-'
    let l:bs_line = 1
  else
    if getline('.') == ""
      let l:bs_line = 1
    else
      let l:str_line = split(getline('.'),':')[0]
	    let l:bs_line=str2nr(l:str_line)
    endif
  endif

  return l:bs_line

endfunction

"}}}
"{{{ s:Bs_toggle_results_buffer_match()
"command to enable/disable results buffer highlight match
function! s:Bs_toggle_results_buffer_match()
  if g:Bs_results_buffer_match == 1
    let g:Bs_results_buffer_match = 0
    exe 'match'
  else
    let g:Bs_results_buffer_match = 1
    exe 'silent! mat Search /'.g:Bs_str_highlight.'/'
  endif
  "if we are on the option screen, refresh
  if g:Bs_on_options == 1
    call s:Bs_show_options()
  endif
endfunction

"}}}
"{{{ s:Bs_toggle_buffers_match()
"command to enable/disable regular buffers highlight match
function! s:Bs_toggle_buffers_match()
  if g:Bs_buffers_match == 1
    let g:Bs_buffers_match = 0
  else
    let g:Bs_buffers_match = 1
  endif
  call s:Bs_jump(0)
  "if we are on the option screen, refresh
  if g:Bs_on_options == 1
    call s:Bs_show_options()
  endif
endfunction

"}}}
"{{{ s:Bs_toggle_help()
function! s:Bs_toggle_help()
  if g:Bs_on_help == 1
    let g:Bs_on_help = 0
    call s:Bs_show_results()
  else
    "we save cursor position
    let g:Bs_position = getpos('.')
    let g:Bs_on_help = 1
    call s:Bs_show_help()
  endif
endfunction

"}}}
"{{{ s:Bs_toggle_options()
function! s:Bs_toggle_options()
  if g:Bs_on_options == 1
    let g:Bs_on_options = 0
    call s:Bs_show_results()
  else
    "we save cursor position
    let g:Bs_position = getpos('.')
    let g:Bs_on_options = 1
    call s:Bs_show_options()
  endif
endfunction

"}}}
"{{{ s:Bs_space_action()
function! s:Bs_space_action()
  let Bsline=search("^-.*$","n")
  call cursor(Bsline+1,7)
  normal! zz
  if g:Bs_auto_jump == 1
    call s:Bs_jump(0)
  endif
endfunction

"}}}
"{{{ s:Bs_toggle_full_screen()
function! s:Bs_toggle_full_screen()
	"if we are in full screen
	if g:Bs_is_full_screen == 1
		let g:Bs_is_full_screen = 0
		resize 15
	else
		"if we are not in full screen
		resize
		resize -6
		let g:Bs_is_full_screen = 1
	endif
endfunction

"}}}
"{{{ s:Bs_toggle_jump()
function! s:Bs_toggle_jump()
	if g:Bs_stay_on_buffer_results_when_entering_result == 0
    let g:Bs_stay_on_buffer_results_when_entering_result = 1
	else
    if g:Bs_auto_jump == 0
		  let g:Bs_stay_on_buffer_results_when_entering_result = 0
    endif
	endif 
  "if we are on the option screen, refresh
  if g:Bs_on_options == 1
    call s:Bs_show_options()
  endif
endfunction

"}}}
"{{{ s:Bs_toggle_auto_jump()
function! s:Bs_toggle_auto_jump()
	if g:Bs_auto_jump == 0
		let g:Bs_auto_jump = 1
    "we disable jump when entering result
    if g:Bs_stay_on_buffer_results_when_entering_result == 0
		  let g:Bs_stay_on_buffer_results_when_entering_result = 1
    endif
    call s:Bs_auto_jump_mapping()
	else
		let g:Bs_auto_jump = 0
		unmap <buffer> <silent> k
		unmap <buffer> <silent> j
    call s:Bs_non_auto_jump_mapping()
	endif 
  "if we are on the option screen, refresh
  if g:Bs_on_options == 1
    call s:Bs_show_options()
  endif
endfunction

"}}}
"{{{ s:Bs_toggle_quit_when_enter()
function! s:Bs_toggle_quit_when_enter()
	if g:Bs_toggle_quit_enter == 0
		let g:Bs_toggle_quit_enter = 1
	else
    let g:Bs_toggle_quit_enter = 0
	endif 
  "if we are on the option screen, refresh
  if g:Bs_on_options == 1
    call s:Bs_show_options()
  endif
endfunction

"}}}
"{{{ s:Bs_get_go_buffer(buffer_name) : returns buffer_number
"checks if the results buffer already exists
"if it does not exists, it creates one
"returns the number of the buffer
"-we also go on that buffer
function! s:Bs_get_go_buffer(buffer_name)

  let l:buffer_number = bufnr(a:buffer_name)
  "if the buffer does not exists
  if l:buffer_number == -1
    "we create it
    let l:buffer_number = bufnr(a:buffer_name,1)
    "we split and go on it
    exe 'bo sp '.a:buffer_name
    exe 'resize 15'
  else
    "if the buffer is not visible, show it
    if bufwinnr(l:buffer_number) == -1
      exe 'bo sp '.a:buffer_name
      exe 'resize 15'
    endif
  endif
	
	"we get the window number
	let l:window_number = bufwinnr(l:buffer_number)
	"and we go on that window
	exe l:window_number.' wincmd w'

  return l:buffer_number

endfunction

"}}}
"{{{ s:Bs_is_in_list_int(list,name) : returns 1 if int is in list
"checks if int is in list
function! s:Bs_is_in_list_int(list,int)
  
  let l:list_length = len(a:list)

  if l:list_length > 0
    for pos in range(0,l:list_length-1)
      if a:list[pos] == a:int
        return 1
      endif
    endfor
  endif

  return 0

endfunction

"}}}

"Internally-used defined commands
"{{{ Internal-used commands

"define BsInitVariables command
if !(exists(":BsInitVariables"))
	command -nargs=0 BsInitVariables :silent call s:Bs_init_variables()
endif
"define BsDelResults command
if !(exists(":BsDelResults"))
	command -range -nargs=0 BsDelResults :silent call s:Bs_delete_action(<line1>,<line2>, 1)
endif
"define BsDefinitelyDelResults command
if !(exists(":BsDefinitelyDelDelResults"))
	command -range -nargs=0 BsDefinitelyDelResults :silent call s:Bs_delete_action(<line1>,<line2>, 0)
endif
"define BsJump command
if !(exists(":BsJump"))
	command -nargs=0 BsJump :silent call s:Bs_jump(0)
endif
"define BsRealJump command - this command ensures that we jump,
"instead of only showing the result, whatever the settings
if !(exists(":BsRealJump"))
	command -nargs=0 BsRealJump :silent call s:Bs_jump(1)
endif
"command to activate deactivate jump
if !(exists(":BsToggleJump"))
	command -nargs=0 BsToggleJump :silent call s:Bs_toggle_jump()
endif
"command to auto jump or not jump
if !(exists(":BsToggleAutoJump"))
	command -nargs=0 BsToggleAutoJump :silent call s:Bs_toggle_auto_jump()
endif
"command to auto jump or not jump
if !(exists(":BsToggleQuitWhenEnter"))
	command -nargs=0 BsToggleQuitWhenEnter :silent call s:Bs_toggle_quit_when_enter()
endif
"command for 'space' action
if !(exists(":BsSpaceAction"))
	command -nargs=0 BsSpaceAction :silent call s:Bs_space_action()
endif
"command to activate deactivate jump
if !(exists(":BsToggleFullScreen"))
	command -nargs=0 BsToggleFullScreen :silent call s:Bs_toggle_full_screen()
endif
"command to display help
if !(exists(":BsToggleHelp"))
	command -nargs=0 BsToggleHelp :silent call s:Bs_toggle_help()
endif
"command to show options
if !(exists(":BsToggleOptions"))
	command -nargs=0 BsToggleOptions :silent call s:Bs_toggle_options()
endif
"command to refresh the results screen
if !(exists(":BsDrawResults"))
	command -nargs=0 BsDrawResults :silent call s:Bs_refresh_results()
endif
"command to research
if !(exists(":BsUpdateSearch"))
	command -nargs=0 BsUpdateSearch :silent call s:Bs_update_search(g:Bs_search)
endif
"command to enable/disable buffer search highlight match
if !(exists(":BsToggleResultsBufferMatch"))
	command -nargs=0 BsToggleResultsBufferMatch :silent call s:Bs_toggle_results_buffer_match()
endif
"command to enable/disable regular buffers highlight match
if !(exists(":BsToggleBuffersMatch"))
	command -nargs=0 BsToggleBuffersMatch :silent call s:Bs_toggle_buffers_match()
endif

"}}}

" vim:ft=vim:fdm=marker:ff=unix:nowrap:tabstop=2:shiftwidth=2

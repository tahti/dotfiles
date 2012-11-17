" Vim color file
" Maintainer:	Adam Blinkinsop <blinks@acm.org>
" Last Change:	2006 Sept 29

" Remove all existing highlighting.
set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "blink"

" Set Default
hi Normal term=NONE cterm=NONE ctermfg=Gray ctermbg=Black gui=NONE guifg=#d2d2d5 guibg=#101010

" Vim colors (reverse for highlighting, red for errors, brown for special)
hi Cursor term=reverse cterm=reverse
hi CursorIME term=reverse cterm=reverse
hi CursorColumn term=reverse cterm=reverse
hi CursorLine term=reverse cterm=reverse
hi ErrorMsg term=reverse ctermfg=Red ctermbg=Black guifg=Red guibg=Black
hi ModeMsg term=reverse cterm=reverse
hi NonText term=bold ctermfg=DarkBlue guifg=#0000c0
hi SpecialKey term=NONE ctermfg=Brown guifg=#c0c000
hi StatusLine term=reverse cterm=reverse
hi Visual term=reverse cterm=reverse gui=NONE guifg=bg guibg=fg
hi WarningMsg term=reverse ctermfg=Brown guifg=#c0c000
hi Folded 		guibg=#303232 guifg=#CACA7A gui=none ctermbg=4 ctermfg=248

" Syntax Colors, General
"hi Comment term=reverse ctermfg=Black ctermbg=DarkCyan guifg=Black guibg=#0090F0
hi Comment term=NONE ctermfg=DarkCyan ctermbg=Black guifg=#5096FF 
"guifg=#808080
hi Underlined term=underline cterm=underline gui=underline
hi Ignore term=NONE cterm=NONE gui=NONE
hi Error term=bold ctermfg=Red ctermbg=Black guifg=Red guibg=Black
hi Todo term=reverse cterm=reverse gui=reverse

" Constants (plain data)
hi Constant term=bold cterm=bold ctermfg=Brown ctermbg=Black gui=NONE guifg=#B06000 
hi def link Boolean Constant
hi def link Character Constant
hi def link String Constant
hi def link Number Constant
hi def link Float Constant

" Identifiers (are things)
hi Identifier term=NONE cterm=NONE ctermfg=DarkCyan ctermbg=Black gui=NONE guifg=#00c0c0 
hi def link Function Identifier

" Statements (do things)
hi Statement term=bold ctermfg=DarkGreen ctermbg=Black gui=NONE guifg=#d0d000 
hi def link Conditional Statement
hi def link Repeat Statement
hi def link Label Statement
hi def link Operator Statement
hi def link Keyword Statement
hi def link Exception Statement

" Preprocessing (meta)
hi PreProc term=bold cterm=NONE ctermfg=DarkGreen ctermbg=Black gui=NONE guifg=#a000a0
hi def link Include PreProc
hi def link Define PreProc
hi def link Macro PreProc
hi def link PreCondit PreProc

" Types (define things)
hi Type term=NONE cterm=bold ctermfg=DarkCyan ctermbg=Black gui=NONE guifg=#00c000 
hi def link StorageClass Type
hi def link Structure Type
hi def link Typedef Type

" Special (undef)
hi Special term=NONE cterm=NONE ctermfg=Brown ctermbg=Black gui=NONE guifg=#c0c000
hi def link Tag Special
hi def link SpecialChar Special
hi def link Delimiter Special
hi def link SpecialComment Special
hi def link Debug Special

" vim: sw=2

function! FileSize()
  let bytes = getfsize(expand("%:p"))
  if bytes <= 0
    return ""
  endif
  if bytes <= 9999
    return bytes . "b"
  else
    return (bytes / 1024) . "k"
  endif
endfunction

" status line
hi User1 gui=bold guibg=#960050 guifg=white ctermfg=white ctermbg=162

set statusline= " completely reset statusline
set statusline+=%f\ " relative path of the file
set statusline+=#%n~%{winnr()}\ " buffer number
set statusline+=%1*%m%r%*\ \ " modified flag and read only flag
set statusline+=[%{strlen(&filetype)?&filetype:'none'}, " filetype
set statusline+=%{strlen(&fenc)?&fenc:'none'}, " file encoding
set statusline+=%{&ff}] " file format
set statusline+=%= " left/right separator
set statusline+=[%{&wrap?'wrap':'nowrap'}, " wrap state
set statusline+=%{&expandtab?'spaces':'tabs'}:%{&tabstop}]\ " expand tab and tab stop info
set statusline+=%{FileSize()}\ " filesize
set statusline+=%l/%L:%-3c " cursor line/total lines:cursor column
set statusline+=\ %P " percent through file

"" helper function for titlestring. Returns the name of the current
" session, if any is loaded, or an empty string when no session is loaded
function! TitleCurrentSession()
    if exists('g:LAST_SESSION')
        return g:LAST_SESSION.': '
    else
        return ''
    endif
endfunction

" title string
set titlestring= " completely reset titlestring
set titlestring+=%{TitleCurrentSession()} " get the name of the current session, if available
set titlestring+=%t " the current filename
set titlestring+=%(\ %M%) " modified flag
set titlestring+=%(\ (%{expand(\"%:~:h\")})%) " relative path to current file
set titlestring+=%(\ %a%) " extra attributes

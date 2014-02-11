" System default for mappings is now the "," character
let mapleader=","
inoremap <MouseMiddle> <Esc>"*pi
noremap <MouseMiddle> <Esc>"*p

" my function to cycle a numeric option
function CycleNum(option,min,inc,max)
  exec ('let tz_value = (((&'.a:option.'-'.a:min.')+'.a:inc.')%(('.a:max.'-'.a:min.')+'.a:inc.'))+'.a:min)
  if (tz_value < a:min) " in case inc<0
    let tz_value = tz_value+a:max
  endif
  exec ('setlocal '.a:option.'='.tz_value)
endfunction

" my function to toggle an option flag
function ToggleFlag(option,flag)
  exec ('let tf_o = &'.a:option)
  exec ('setlocal '.a:option.'-='.a:flag)
  exec ('let tf_t = &'.a:option)
  if (tf_o == tf_t)
    exec ('setlocal '.a:option.'+='.a:flag)
  endif
endfunction

" <leader>dd to delete buffer
noremap <leader>dd <Esc>:bd<CR>
vnoremap <leader>dd <Esc>:bd<CR>
" reverse string mapping
vnoremap <leader>rv c<C-O>:set revins<CR><C-R>"<Esc>:set norevins<CR> 

" Left/Right arrow keys change buffers in all modes
"noremap <Left> <Esc>:bp<CR>
"inoremap <Left> <Esc>:bp<CR>
"nnoremap <Left> <Esc>:bp<CR>
"vnoremap <Left> <Esc>:bp<CR>

"noremap <Right> <Esc>:bn<CR>
"inoremap <Right> <Esc>:bn<CR>
"nnoremap <Right> <Esc>:bn<CR>
"vnoremap <Right> <Esc>:bn<CR>

" Copy/Paste to and from Desktop Environment
noremap Y "+y
noremap p P
noremap P "+gP
"noremap <C-P> p

" Quit window on <leader>q
nnoremap <leader>q :wqall<CR>

" NERDtree on <leader>t
nnoremap <leader>t :NERDTreeToggle<CR>
" settings for ctrlp =====================================================
map <leader>b :CtrlPBuffer<CR>
map <leader>] :CtrlPTag<CR>
let g:ctrlp_prompt_mappings = {
    \ 'PrtBS()': ['<bs>', '<c-]>'],
    \ 'PrtDelete()': ['<del>'],
    \ 'PrtDeleteWord()': ['<c-w>'],
    \ 'PrtClear()': ['<c-u>'],
    \ 'PrtSelectMove("j")': ['<c-t>', '<down>'],
    \ 'PrtSelectMove("k")': ['<c-c>', '<up>'],
    \ 'PrtHistory(-1)': ['<c-j>'],
    \ 'PrtHistory(1)': ['<c-k>'],
    \ 'AcceptSelection("e")': ['<cr>', '<c-m>', '<2-LeftMouse>'],
    \ 'AcceptSelection("h")': ['<c-x>', '<c-cr>', '<c-s>'],
    \ 'AcceptSelection("t")': ['<c-l>', '<MiddleMouse>'],
    \ 'AcceptSelection("v")': ['<c-v>', '<RightMouse>'],
    \ 'ToggleFocus()': ['<s-tab>'],
    \ 'ToggleRegex()': ['<c-r>'],
    \ 'ToggleByFname()': ['<c-d>'],
    \ 'ToggleType(1)': ['<c-f>', '<c-up>'],
    \ 'ToggleType(-1)': ['<c-b>', '<c-down>'],
    \ 'PrtExpandDir()': ['<tab>', '<c-i>'],
    \ 'PrtInsert("w")': ['<F2>'],
    \ 'PrtInsert("s")': ['<F3>'],
    \ 'PrtInsert("v")': ['<F4>'],
    \ 'PrtInsert("+")': ['<F6>'],
    \ 'PrtCurStart()': ['<c-a>'],
    \ 'PrtCurEnd()': ['<c-e>'],
    \ 'PrtCurLeft()': ['<c-h>', '<left>', '<c-^>'],
    \ 'PrtCurRight()': ['<c-n>', '<right>'],
    \ 'PrtClearCache()': ['<F5>'],
    \ 'PrtDeleteMRU()': ['<F8>'],
    \ 'CreateNewFile()': ['<c-y>'],
    \ 'MarkToOpen()': ['<c-z>'],
    \ 'OpenMulti()': ['<c-o>'],
    \ 'PrtExit()': ['<esc>'],
    \ }

" <leader>v selects the just pasted text
nnoremap <leader>v `[v`]
"set timeouts, ttimeoutlen makes it possible to do fast mappings
set timeout timeoutlen=1000 ttimeoutlen=10
" MapFastKeycode: helper for fast keycode mappings
" makes use of unused vim keycodes <[S-]F15> to <[S-]F37>
function! <SID>MapFastKeycode(key, keycode)
    if s:fast_i == 46
        echohl WarningMsg
        echomsg "Unable to map ".a:key.": out of spare keycodes"
        echohl None
        return
    endif
    let vkeycode = '<'.(s:fast_i/23==0 ? '' : 'S-').'F'.(15+s:fast_i%23).'>'
    exec 'set '.vkeycode.'='.a:keycode
    exec 'noremap '.vkeycode.' '.a:key
    let s:fast_i += 1
endfunction
let s:fast_i = 0

"      call <SID>MapFastKeycode('<M-a>', "[a]")
"       call <SID>MapFastKeycode('<M-h>', "[h]")
"       call <SID>MapFastKeycode('<M-t>', "[t]")
"       call <SID>MapFastKeycode('<M-n>', "[n]")
"       call <SID>MapFastKeycode('<M-c>', "[c]")

if !has("gui_running")
    if &term == "rxvt-unicode-256color"
    " map control-backspace to delete the previous word in insert mode
      imap  <C-W>
    " and map control-delete to delete the next word in insert mode
      imap [3^ <C-O>dw
"Fix terminal bindings to allow use of Alt key
      set <F20>=[32~]
      set <F15>=[27~]
      set <F17>=[29~]
      set <F18>=[30~]
      set <F19>=[31~]
      noremap <F15> ggVG
      inoremap <F15> <Esc>ggVG
      noremap <F20> <C-W>h
      inoremap <F20> <Esc><C-W>hi
      noremap <F18> <C-W>l
      inoremap <F18> <Esc><C-W>li
      noremap <F19> <C-W>k
      inoremap <F19> <Esc><C-W>ki
      noremap <F17> <C-W>j
      inoremap <F17> <Esc><C-W>ji
    endif
else
    "GUI running
    "Window movements
    noremap <M-h> <C-W>h
    inoremap <M-h> <C-o><C-W>h
    noremap <M-n> <C-W>l
    inoremap <M-n> <C-o><C-W>l
    noremap <M-c> <C-W>k
    inoremap <M-c> <C-o><C-W>k
    noremap <M-t> <C-W>j
    inoremap <M-t> <C-o><C-W>j
    "Window repositioning
    noremap <C-S-M-h> <C-W>H
    inoremap <C-S-M-h> <C-o><C-W>H
    noremap <C-S-M-n> <C-W>L
    inoremap <C-S-M-n> <C-o><C-W>L
    noremap <C-S-M-c> <C-W>K
    inoremap <C-S-M-c> <C-o><C-W>K
    noremap <C-S-M-t> <C-W>J
    inoremap <C-S-M-t> <C-o><C-W>J
    "switch tabs
    noremap <C-M-h> gT
    inoremap <C-M-h> <C-o>gT
    noremap <C-M-n> gt
    inoremap <C-M-n> <C-o>gt

" Move current tab into the specified direction.
"
" @param direction -1 for left, 1 for right.
function! TabMove(direction)
    " get number of tab pages.
    let ntp=tabpagenr("$")
    " move tab, if necessary.
    if ntp > 1
        " get number of current tab page.
        let ctpn=tabpagenr()
        " move left.
        if a:direction < 0
            let index=((ctpn-1+ntp-1)%ntp)
        else
            let index=(ctpn%ntp)
        endif
        echo "ha".index
        " move tab page.
        execute "tabmove ".index
    endif
endfunction

    "move tabs
    noremap <S-M-h> :call TabMove(-1)<CR>
    inoremap <S-M-h> <C-o>:call TabMove(-1)<CR>
    noremap <S-M-n> :call TabMove(1)<CR>
    inoremap <S-M-n> <C-o>:call TabMove(1)<CR>
    noremap <S-M-c> :tabm0<CR>
    inoremap <S-M-c> <C-o>:tabm0<CR>
    noremap <S-M-t> :tabm<CR>
    inoremap <S-M-t> <C-o>:tabm<CR>

    "toggle menubar anf tabline
    noremap <silent><M-Down> :call ToggleFlag("guioptions","m")<BAR>set guioptions?<CR>
    inoremap <silent><M-Down> <C-o>:call ToggleFlag("guioptions","m")<BAR>set guioptions?<CR>
    noremap <silent><M-Up> :call CycleNum("showtabline",0,1,2)<BAR>set showtabline?<CR>
    inoremap <silent><M-Up> <C-o>:call CycleNum("showtabline",0,1,2)<BAR>set showtabline?<CR>
    noremap <C-Left> :bp<CR>
    nnoremap <C-Left> <Esc>:bp<CR>
    inoremap <C-Left> <C-o>:bp<CR>
    vnoremap <C-Left> <Esc>:bp<CR>
    
    noremap <C-Right> :bn<CR>
    inoremap <C-Right> <C-o>:bn<CR>
    nnoremap <C-Right> <Esc>:bn<CR>
    vnoremap <C-Right> <Esc>:bn<CR>
    "copy - paste
    cnoremap <M-v> <C-r>+
    vnoremap <M-v> "+p
    inoremap <M-v> <C-o>"+p
    cnoremap <M-r> <C-y>
    inoremap <M-r> <C-o><C-y>
    vnoremap <M-r> "+y
    " map control-backspace to delete the previous word in insert mode
    imap <C-BS> <C-W>
    " and map control-delete to delete the next word in insert mode
    imap <C-Del> <C-O>dw
endif

noremap <C-W>c <C-W>k
noremap <C-W>C <C-W>K
noremap <C-W>j <C-W>c
noremap <C-W>J <C-W>C
noremap <C-W>t <C-W>j
noremap <C-W>T <C-W>J
noremap <C-W>k <C-W>n
noremap <C-W>K <C-W>N
noremap <C-W>n <C-W>l
noremap <C-W>N <C-W>L
noremap <C-W>l <C-W>t
noremap <C-W>L <C-W>T
"select all
" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

"kill stupid last search highlight
nnoremap <silent> ,/ :nohlsearch<CR>

" The following beast is something i didn't write... it will return the
" syntax highlighting group that the current "thing" under the cursor
" belongs to -- very useful for figuring out what to change as far as
" syntax highlighting goes.
nnoremap <silent> ,qq :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

"use space to toggle folding
nnoremap <silent> <Space> @=(foldlevel('.')?'za':'l')<CR>
vnoremap <Space> zf

" allow some command line editing like emacs
cnoremap <C-H> <Home>
cnoremap <C-B> <Left>
cnoremap <C-N> <End>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <ESC>h <S-Left>
cnoremap <ESC>n <S-Right>
" "        c
" "remap movement keys to h t n
noremap k n
noremap K N
noremap t gj
noremap T J
noremap n l
noremap N L
noremap c k
noremap C K
noremap j c
noremap J C
noremap l t
noremap L T
" escaping of html characters
"nmap <C-h> <Plug>(operator-html-escape)
nmap <C-h> <C-b>
nmap <C-n> <C-f>
vmap <C-h> <C-b>
vmap <C-n> <C-f>
nmap <C-c> <Plug>(operator-html-unescape)
"vmap <C-h> <Plug>(operator-html-escape)
vmap <C-c> <Plug>(operator-html-unescape)
"noremap ; :
"noremap : ;

"allow to use w!! to write to a file with sudo, in case forgotten
"http://stackoverflow.com/questions/95072/what-are-your-favorite-vim-tricks/96492#96492
cmap w!! %!sudo tee > /dev/null %

" http://stackoverflow.com/questions/164847/what-is-in-your-vimrc/1636961#1636961
inoremap ;; <End>;
"
" enter to the previous line
imap <S-CR> <C-O>O

" Show syntax highlighting groups for word under cursor - useful together with
" :sy list groupName
nmap <Leader>ssg :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
"alt remappings
"set <M-a>=a
"noremap <M-a> ggVG
"set <M-h>=h
"noremap <M-h> <C-w><Left>
"set <M-t>=t
"noremap <M-t> <C-w><Down>
"set <M-n>=n
"noremap <M-n> <C-w><Right>
"set <M-c>=c
"noremap <M-c> <C-w><Up>


nmap <silent> <F6> :call ToggleSpell()<CR>
function! ToggleSpell()
  if !exists( "b:myLang" )
      let b:myLang=0
  endif
  let b:myLang=b:myLang+1
  if b:myLang>=len(g:myLangList) | let b:myLang=0 | endif
  if b:myLang==0
    setlocal nospell
  else
    execute "setlocal spell spelllang=".get(g:myLangList, b:myLang)
  endif
  echo "spell checking language:" g:myLangList[b:myLang]
endfunction


" Really useful!
"  In visual mode when you press * or # to search for the current selection
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>
" From an idea by Michael Naumann
function! VisualSearch(direction) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

"################# Plugin dependant ######################
" Gundo plugin
noremap <F4> :silent GundoToggle<CR>

"Sessions
" open the session list
noremap <Leader>sl :SessionList<CR>
noremap <Leader>ss :SessionSave<CR>
" <F2> for save session in all three modes
noremap <F2> <Esc>:SessionSave<CR>
inoremap <F2> <Esc>:SessionSave<CR>
nnoremap <F2> <Esc>:SessionSave<CR>
vnoremap <F2> <Esc>:SessionSave<CR>

noremap <S-F2> <Esc>:SessionList<CR>
inoremap <S-F2> <Esc>:SessionList<CR>
nnoremap <S-F2> <Esc>:SessionList<CR>
vnoremap <S-F2> <Esc>:SessionList<CR>
" quickly escape a single character
noremap \\ i\<ESC>l

function! SyncTexForward()
       let execstr = "silent !okular --unique %:p:r.pdf\#src:".line(".")."%:p &"
       exec execstr
endfunction
nmap <Leader>f :call SyncTexForward()<CR>



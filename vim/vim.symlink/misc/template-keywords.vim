"Replace keywords
"autocmd User plugin-template-loaded call s:template_keywords()
function! s:template_keywords()
  silent %s/<+FILE NAME+>/\=expand('%:t')/ge
  silent %s/<+DATE+>/\=strftime('%Y-%m-%d')/ge
" add more keywords here...
endfunction
"Evaluate by vim and expand the inside of <%= %>:
"  autocmd User plugin-template-loaded
"  \silent %s/<%=\(.\{-}\)%>/\=eval(submatch(1))/ge
"Move the cursor to (+CURSOR+):
autocmd User plugin-template-loaded
\  if search('(+CURSOR+)')  |   execute 'normal! "_dab' | endif

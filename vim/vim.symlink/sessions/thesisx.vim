verbose function! ThesisSyntax()
   syntax match texGreek '\\TA\>' contained conceal cchar=A
   syntax match texGreek '\\X\>' contained conceal cchar=Ẍ
   syntax match texGreek '\\x\>' contained conceal cchar=x
   syntax match texGreek '\\y\>' contained conceal cchar=y
   syntax match texGreek '\\M\>' contained conceal cchar=M
   syntax match texGreek '\\y\>' contained conceal cchar=y
   syntax match texGreek '\\VA\>' contained conceal cchar=V
   syntax match texGreek '\\va\>' contained conceal cchar=v
   syntax match texGreek '\\val\>' contained conceal cchar=w
   syntax match texGreek '\\g\>' contained conceal cchar=ψ
   syntax match texGreek '\\Q\>' contained conceal cchar=Q
   syntax match texGreek '\\q\>' contained conceal cchar=q
   syntax match texGreek '\\E\>' contained conceal cchar=E
   syntax match texGreek '\\e\>' contained conceal cchar=e
   syntax match texGreek '\\I\>' contained conceal cchar=I
   syntax match texGreek '\\R\>' contained conceal cchar=X
   syntax match texGreek '\\States\>' contained conceal cchar=š
   syntax match texGreek '\\s\>' contained conceal cchar=s
   syntax match texGreek '\\sta\>' contained conceal cchar=p
   syntax match texGreek '\\stat\>' contained conceal cchar=z
   syntax match texGreek '\\Sem\>' contained conceal cchar=Ŝ
   syntax match texGreek '\\step\>' contained conceal cchar=→
   syntax match texGreek '\\RegGraph\>' contained conceal cchar=G
   syntax match texGreek '\\Reg\>' contained conceal cchar=R
   syntax match texGreek '\\reg\>' contained conceal cchar=r
   syntax match texGreek '\\Rstar\>' contained conceal cchar=Ṙ
   syntax match texGreek '\\tr\>' contained conceal cchar=π
   syntax match texGreek '\\regPath\>' contained conceal cchar=p
   syntax match texGreek '\\vertex\>' contained conceal cchar=w
   syntax match texGreek '\\NoReg\>' contained conceal cchar=W
   syntax match texGreek '\\Z\>' contained conceal cchar=Z
   syntax match texGreek '\\Zc\>' contained conceal cchar=Ż
   syntax match texGreek '\\D\>' contained conceal cchar=D
   syntax match texGreek '\\up\>' contained conceal cchar=↑
   syntax match texGreek '\\down\>' contained conceal cchar=↓
   syntax match texGreek '\\upZ\>' contained conceal cchar=ź
   syntax match texGreek '\\ex\>' contained conceal cchar=ξ
   syntax match texGreek '\\Step\>' contained conceal cchar=⇒
   syntax match texGreek '\\Paths\>' contained conceal cchar=Υ
   syntax match texGreek '\\path\>' contained conceal cchar=σ
   syntax match texGreek '\\stableZ\>' contained conceal cchar=ś
   syntax match texGreek '\\True\>' contained conceal cchar=∞
   syntax match texGreek '\\stableM\>' contained conceal cchar=Ś
   syntax match texGreek '\\Delt\>' contained conceal cchar=Δ
   syntax match texGreek '\\stepE\>' contained conceal cchar=≫
   syntax match texGreek '\\stepD\>' contained conceal cchar=▶
   syntax match texGreek '\\limCyc\>' contained conceal cchar=L
   syntax match texGreek '\\eps\>' contained conceal cchar=ϵ
   syntax match texGreek '\\emptyset\>' contained conceal cchar=∅
endfunction
verbose function! SetKeywords()
endfunction
set wrap
let g:tex_conceal= 'adgm'
au FileType tex call SetKeywords()
au FileType tex call ThesisSyntax()

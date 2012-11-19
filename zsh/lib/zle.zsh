# Use vi keybindings
bindkey -v
bindkey -M vicmd "c" up-line-or-history #k
bindkey -M vicmd "j" vi-change		#c
bindkey -M vicmd "n" vi-forward-char	#l
bindkey -M vicmd "k" vi-repeat-search	#n
bindkey -M vicmd "t" down-line-or-history	#j
bindkey -M vicmd "l" vi-find-next-char-skip	#t


#bindkey -M vicmd "C" down-line-or-history
bindkey -M vicmd "J" vi-change-eol
#bindkey -M vicmd "N" down-line-or-history
bindkey -M vicmd "K" vi-rev-repeat-search
bindkey -M vicmd "T" vi-join
bindkey -M vicmd "L" vi-find-prev-char-skip

bindkey -M vicmd '^Xf' insert-files
bindkey -M viins '^Xf' insert-files

#add reverse search
bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '^r' history-incremental-search-backward
insert-last-typed-word() { zle insert-last-word -- 0 -4 };
i-2nd-typed-word() { zle insert-last-word -- 0 -3 };
insert-next-typed-word() { zle insert-last-word -- 0 -2};
i-last-word()       { zle insert-last-word -- -1 0 };
i-next-word()       { zle insert-last-word -- 1 0 };
zle -N insert-last-typed-word;
zle -N i-2nd-typed-word
zle -N insert-next-typed-word;
zle -N i-last-word;
zle -N i-next-word;
bindkey -M viins "^h" insert-last-typed-word
bindkey -M viins '^t' i-2nd-typed-word
bindkey -M viins '^n' insert-next-typed-word
bindkey -M viins '^b' i-last-word
bindkey -M viins '^f' i-next-word
#bindkey -M vicmd '^/' insert-last-word
#bindkey -M vicmd '^@' insert-last-word
#bindkey -M vicmd '^_' insert-last-word
#allow root to use X
#xhost +localhost

# setup key accordingly
[[ -n "${key[Home]}"    ]]  && bindkey -M vicmd "${key[Home]}"    beginning-of-line
bindkey -M vicmd "^a"     beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey -M vicmd "${key[End]}"     end-of-line
bindkey -M vicmd "^e"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey -M vicmd "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey -M vicmd "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey -M vicmd "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey -M vicmd "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey -M vicmd "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey -M vicmd "${key[Right]}"   forward-char

# setup key accordingly
[[ -n "${key[Home]}"    ]]  && bindkey -M viins "${key[Home]}"    beginning-of-line
bindkey -M viins "^a"     beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey -M viins "${key[End]}"     end-of-line
bindkey -M viins "^e"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey -M viins "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey -M viins "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey -M viins "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey -M viins "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey -M viins "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey -M viins "${key[Right]}"   forward-char

zmodload zsh/complist
[[ -n "${key[Backspace]}"   ]]  && bindkey -M menuselect "${key[Backspace]}" undo
bindkey -M menuselect '\e' undo
bindkey -M menuselect '^m' .accept-line
bindkey -M menuselect " " accept-and-menu-complete

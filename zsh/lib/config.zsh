setopt no_beep         #turn off bell
setopt HIST_IGNORE_DUPS
setopt NO_LIST_BEEP

set bell-style none
#setopt / unsetopt
setopt AUTO_CD
typeset -U path        #ignore doules in path
#ZSH_THEME="random"
#set history
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.history
setopt hist_expire_dups_first
setopt hist_ignore_space
setopt share_history # share command history data
setopt hist_verify


if [ "$TERM" = "linux" ]
then
  eval `dircolors ~/.dircolors/dircolors.ansi-dark -b`
else
  eval `dircolors ~/.dircolors/dircolors.256dark -b`
fi


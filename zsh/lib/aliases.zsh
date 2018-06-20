alias reload!='. ~/.zshrc'
alias md='mkdir -p'
alias rm='rm -v'
alias df='df -h'

alias search='find . |grep -i'
alias web='iceweasel'
alias gvim='gvim --servername gvim'
alias v='gvim --remote'
alias sec='iceweasel -P Secure -no-remote'
alias l='ls -ah'
alias la='ls -alh'
alias sub='svn'
alias du='du -h'
alias lp='lp -o media=a4'
alias mykeys='/usr/bin/keychain $HOME/.ssh/id_dsa $HOME/.ssh/id_rsa;source $HOME/.keychain/$HOSTNAME-sh'
alias sshmount='sshfs'
alias sshumount='fusermount -u'
#suffix aliases
alias -s tex=vim
alias -s cpp=vim
alias -s lzz=vim
alias -s htmn=iceweasel
### generic aliases (ie. no special checks needed, using xalias().
alias gpw="makepasswd --chars 12 --string 'abcdefghijklmnopqrrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+*%_-=.,!|/~'"
alias gpwns="makepasswd --chars 12 --string 'abcdefghijklmnopqrrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-'"
alias svn ignore='svn propedit svn:global-ignores'
alias unsvn='find . -name ".svn" -exec rm -rf {} \;'
alias setdvp='setxkbmap -layout "pl,pl" -variant "dvp," -option "caps:swapescape,grp:sclk_toggle"'
alias t='~/bin/todo'
alias irfanview='wine ~/.wine/drive_c/Program\ Files/IrfanView/i_view32.exe'
alias ida='wine ~/progs/IDA\ Pro\ 6.1\ Precracked/idaq.exe'
alias objdump='objdump -C --disassembler-options=intel'

#global aliases
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'
alias -g .......='../../../../../..'
alias -g ........='../../../../../../..'
alias msgview='java -jar /usr/local/MSGViewer-1.9/MSGViewer.jar'
alias updatedb='sudo /usr/libexec/locate.updatedb'

# add a function path
#fpath=($ZSH/zsh/functions $ZSH/zsh/completions $fpath)
export ZSH=$(dirname $(readlink $HOME/.zshrc))
fpath=($ZSH/functions $fpath)
autoload -Uz any
autoload -Uz myip
autoload -Uz givedef
autoload -Uz macopen
autoload -Uz path
autoload -Uz cdup
autoload -Uz nwf
source $ZSH/functions/aun

mcd() { mkdir -p "$1" && cd "$1"; }
pj() { python -mjson.tool } # pretty-print JSON
cj() { curl -sS $@ | pj } # curl JSON
l() { ls -a -h --color=auto "$@" }
o() { macopen "$@" }
apa(){ apack "$@"} 
md5() { echo -n $1 | openssl md5 /dev/stdin }
sha1() { echo -n $1 | openssl sha1 /dev/stdin }
sha256() { echo -n $1 | openssl dgst -sha256 /dev/stdin }
sha512() { echo -n $1 | openssl dgst -sha512 /dev/stdin }
rot13() { echo $1 | tr "A-Za-z" "N-ZA-Mn-za-m" }
rot47() { echo $1 | tr "\!-~" "P-~\!-O" }
latrus() { echo $1 | tr "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM" "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ" }
ruslat() { echo $1 | tr "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ" "qwertyuiop[]asdfghjkl;'zxcvbnm,.QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM" }
up(){
  sudo apt update && 
  sudo apt-file update && 
  sudo DEBIAN_FRONTEND=noninteractive apt -y upgrade 
  # && sudo wget http://winhelp2002.mvps.org/hosts.txt -O /etc/hosts &&
  #sudo cat ~/.etchosts | sudo tee -a /etc/hosts > /dev/null
}
export APTCLEAN_EXCEPTIONS="skypeforlinux"
apt-clean() {
  sudo apt clean &&
  local SED_TMP=""
  for p in ${=APTCLEAN_EXCEPTIONS}; do
    local SED_TMP="${SED_TMP}s/\b$p\b//g;"
  done
  local REMOVE=$(aptitude -F '%p' search '?obsolete'|sed -e "$SED_TMP")
  if [ ! -z $REMOVE ]; then
    echo "Removing the following packages: " $REMOVE
    sudo apt purge $REMOVE
  fi
  echo "******* autoremove ********"
  sudo apt autoremove
}
urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])" $1 }
urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" $1 }
# vim: set filetype=zsh:

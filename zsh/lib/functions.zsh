# add a function path
#fpath=($ZSH/zsh/functions $ZSH/zsh/completions $fpath)
export ZSH=$(dirname $(readlink -f $HOME/.zshrc))
fpath=($ZSH/functions $fpath)
autoload -Uz any
autoload -Uz myip
autoload -Uz givedef
autoload -Uz open
autoload -Uz path
autoload -Uz cdup
autoload -Uz nwf
mcd() { mkdir -p "$1" && cd "$1"; }
pj() { python -mjson.tool } # pretty-print JSON
cj() { curl -sS $@ | pj } # curl JSON
l() { ls -a -h --color=auto "$@" }
o() { open "$@" }
# unpack fileName extension packer other
myunpack() {
   local num=${(l:4::0:)${RANDOM}}
   while [[ -a ./Unpack-$num ]] 
   do
     num=${(l:4::0:)${RANDOM}}
   done
   mkdir Unpack-$num
   local b=$(echo $1 | cut -f 1 -d '.') 
   cp $1 __$b.$2
   cd Unpack-$num
   echo $3 "$@[4, -1]" ../__$b.$2
   $3 "$@[4, -1]" ../__$b.$2
   cd ..
   rm __$b.$2
}
inplaceunpack() {
   local num=${(l:4::0:)${RANDOM}}
   while [[ -a ./Unpack-$num ]] 
   do
     num=${(l:4::0:)${RANDOM}}
   done
   mkdir Unpack-$num
   local b=$(echo $1 | cut -f 1 -d '.') 
   cp $1 Unpack-$num/$b.$2
   cd Unpack-$num
   $3 "$@[4, -1]" $b.$2
   cd ..
}

t () {
  $3 $@[4,-1] $1
}

aun(){ 
  if [[ -f $1 ]]; then
    local t=$(file -b "$1")
    case $t in
      "XZ compressed data")
         inplaceunpack $1 xz xz -d 
        ;;
      "NuFile archive (apple ][) data")
         myunpack $1 nu nulib2 -x -r
        ;;
      "lzip compressed data, version: 1")
         inplaceunpack $1 lzip lzip -d
         ;;
      "ZPAQ stream, level 1")
         myunpack $1 zpq zpaq x
         ;;
       "LZMA compressed data, streamed")
         inplaceunpack $1 xz lzma -d
         ;;
       "Zoo archive data"*)
         myunpack $1 zoo zoo -extract 
         ;;
       "7-zip archive data"*)
         myunpack $1 7z 7z x
         ;;
       "ARJ archive data"*)
         myunpack $1 arj arj x
         ;;
       "Zip archive data"*)
         myunpack $1 zip unzip
         ;;
       "bzip2 compressed data"*)
         inplaceunpack $1 bzip2 bzip2 -d 
         ;;
      *)
        #read \?"I am waiting for you to press [Enter] before I continue."
        aunpack "$@"
        ;;
    esac
  else
    aunpack "$@"
  fi
} 
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
  sudo aptitude update && 
  sudo apt-file update && 
  sudo  DEBIAN_FRONTEND=noninteractive aptitude -y upgrade &&
  #sudo wget http://winhelp2002.mvps.org/hosts.txt -O /etc/hosts &&
  #sudo cat ~/.etchosts | sudo tee -a /etc/hosts > /dev/null

}
urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])" $1 }
urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" $1 }

#!/bin/bash
#By default, only root can change the brightness by this method. To allow users in the video group to change the brightness, a udev rule such as the following can be used:

#/etc/udev/rules.d/backlight.rules

#ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="acpi_video0", RUN+="/bin/chgrp video /sys/class/backlight/%k/brightness"
#ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="acpi_video0", RUN+="/bin/chmod g+w /sys/class/backlight/%k/brightness"
if [ ! -f ~/bin/brightlight ]; then
  TMPDIR=$(mktemp -d -t gitXXXX)
  #Install brightness
  cd $TMPDIR
  git clone https://github.com/multiplexd/brightlight.git 
  cd $TMPDIR/brightlight
  sudo apt install libbsd-dev
  make
  cp brightlight ~/bin
  cd
  rm -Rf $TMPDIR
fi

#polybar
if  ! which polybar; then
  echo "Installing siji fonts" 
  sudo apt-get install x11-utils
  git clone https://github.com/stark/siji && cd siji
  ./install.sh
  sudo rm /etc/fonts/conf.d/70-no-bitmaps.conf && fc-cache
  echo "Installing Polybar" 
  sudo apt install ccache unifont build-essential clang-7 clang-9 git cmake cmake-data pkg-config python3 python3-sphinx libcairo2-dev libxcb1-dev libxcb-util0-dev libxcb-randr0-dev libxcb-composite0-dev python-xcbgen xcb-proto libxcb-image0-dev libxcb-ewmh-dev libxcb-icccm4-dev libxcb-xkb-dev libxcb-xrm-dev libxcb-cursor-dev libasound2-dev i3-wm libjsoncpp-dev libmpdclient-dev libcurl4-openssl-dev libnl-genl-3-dev libpulse-dev fonts-materialdesignicons-webfont
  git clone --recursive https://github.com/polybar/polybar
  cd polybar
  mkdir build
  cd build
  cmake ..
  make -j$(nproc)
  sudo make install
else 
  echo polybar
fi



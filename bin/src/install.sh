#!/bin/bash
#By default, only root can change the brightness by this method. To allow users in the video group to change the brightness, a udev rule such as the following can be used:

#/etc/udev/rules.d/backlight.rules

#ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="acpi_video0", RUN+="/bin/chgrp video /sys/class/backlight/%k/brightness"
#ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="acpi_video0", RUN+="/bin/chmod g+w /sys/class/backlight/%k/brightness"

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

#!/bin/bash
# Call this with file name and title as param
if [ ! -z "$2" ]; then
  ALARMTEXT=$2
else
  ALARMTEXT="Alarm!!!!"
fi
Size=500
Style=" STYLE=\"max-width:$Size; max-height:$Size\""
#function Identical() {
(echo "<h2>$ALARMTEXT</h2><TABLE><TR><TD STYLE=\"width:100%; margin:0px auto;\"><IMG$Style
SRC=\"data:"
mimetype -b "$1"
echo -n ";base64,"
base64 "$1"
echo "\"></TD></TR></TABLE>"
#) | zenity --ok-label=Close --width=$(($Size*2+50)) --height=$(($Size+100)) --title="$alarmtetx" --notification --no-cancel --html --filename=/dev/stdin; 
#) | zenity --width=$(($Size+50)) --height=$(($Size)) --title="$ALARMTEXT" --text-info --html --filename=/dev/stdin; 
) | zenity --width=$(($Size+50)) --height=$(($Size)) --title="$ALARMTEXT" --text-info --html --filename=/dev/stdin; 

#!/bin/bash
if [ $# -eq 0 ]
then
    SIDE="left" 
else
    SIDE="$1"
fi
OUTPUTS=( $(xrandr | grep \ connected | egrep -o "^[a-zA-Z0-9\-]+[0-9]") )

MODES=( $(xrandr | grep -A1 \ connected | egrep -o "[^a-z] [0-9][0-9][0-9][0-9]?x[0-9][0-9][0-9][0-9]?") )
max=${#OUTPUTS[@]}
if test $max -eq 1
then
    echo "Putting ${OUTPUTS[0]} in mode ${MODES[0]}."
    xrandr --output ${OUTPUTS[0]} --auto --mode ${MODES[0]}
elif test $max -eq 2
then
    echo "Putting ${OUTPUTS[1]} $SIDE of ${OUTPUTS[0]}."
    xrandr --output ${OUTPUTS[0]} --auto --mode ${MODES[0]} --output ${OUTPUTS[1]} --${SIDE}-of ${OUTPUTS[0]} --mode ${MODES[1]}
else
    echo "More than 2 output not implemented."
fi
~/.fehbg

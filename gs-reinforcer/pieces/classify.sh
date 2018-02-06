#!/bin/bash

# Displays each image in subdirectories of ./orig and prompts for a class to put
# them in.
for i in `ls orig`
do
    for j in `ls bypage/$i`; do
        origfile=bypage/$i$j
        feh -g 1918x1055 $origfile
        class=$(dialog --title "Pick the class for $origfile" --stdout --dselect ./classes/ 30 50)
        mkdir -p "$class"
        cp -f $origfile $class/
    done
    echo "Continue? "
    read cont
    if [[ "$cont" =~ [nN] ]]; then
        exit 0;
    fi
done

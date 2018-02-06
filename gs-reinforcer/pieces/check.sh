#!/bin/bash

# Displays each image in a subdirectory of ./classes and asks whether the
# classification is correct or not. If not, prompts for a new class. Moves the
# json files (but not text).
for i in `ls classes`
do
    for j in `ls classes/$i/`; do
        origfile=classes/$i/$j
        feh -g 1918x1055 $origfile
        if (! (dialog --yesno "Keep current classification for file $j ($i)?" 8 40)); then
            class=$(dialog --title "Pick the new class" --stdout --dselect ./classes/ 20 50)
            mkdir -p "$class"
            mv -f $origfile $class/
            jsontarget=json/${class##*/}
            mkdir -p "$jsontarget"
            mv json/$i/$j.json $jsontarget
        fi
    done
    echo "Continue? "
    read cont
    if [[ "$cont" =~ [nN] ]]; then
        exit 0;
    fi
done

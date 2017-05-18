#!/bin/sh

E_BADARGS=65

if [ ! -n "$1" ]
then
    echo "Usage: `basename $0` file_name"
    exit $E_BADARGS
fi

tesseract "$1" stdout -psm 0 | grep "Orientation in degrees" | awk -F ": " '{print $2}'

exit 0

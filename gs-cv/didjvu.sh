#!/bin/sh

E_BADARGS=65

if [ ! -n "$1" ]
then
    echo "Usage: `basename $0` directory_name"
    exit $E_BADARGS
fi

dirname=$1
cd $dirname

for image in *.png
do
    imagename=${image%.*}
    for method in abutaleb bernsen brink djvu niblack otsu sauvola shading-subtraction tsai white-rohrer
    do
        djvufile=$imagename-$method.djvu
        tifffile=$imagename-$method.tiff
        didjvu encode -m $method -o $djvufile $imagename.png
        for mode in black color foreground mask
        do
            mkdir -p $mode/$method
            mkdir -p $mode/$imagename
            ddjvu -format=tiff -mode=$mode -page=1 $djvufile $tifffile
            convert $tifffile $mode/$method/$imagename-$method.png
            cp $mode/$method/$imagename-$method.png $mode/$imagename/
        done
        rm $djvufile
        rm $tifffile
    done
done

exit 0

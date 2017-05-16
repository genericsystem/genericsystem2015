#!/bin/sh

E_BADARGS=65

if [ ! -n "$1" ]
then
    echo "Usage: `basename $0` directory_name"
    echo "       or `basename $0` file_name"
    exit $E_BADARGS
fi

convert_image() {
    image="$1"
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
}

if [ -d "$1" ]; then
    cd "$1"
    for image in *.png
    do
        convert_image "$image"
    done
elif [ -f "$1" ]; then
    cd `dirname "$1"`
    convert_image `basename "$1"`
else
    echo "Given argument is not a valid file or directory name: $1."
    exit $E_BADARGS
fi

exit 0

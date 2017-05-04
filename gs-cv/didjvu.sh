
    mkdir all
    for image in *.png
    do
        mkdir all/${image%.*}
        for method in abutaleb bernsen brink djvu global niblack otsu sauvola  shading-subtraction tsai white-rohrer
        do 
            mkdir all/$method
            didjvu encode -m $method -o ${image%.*}-$method.djvu ${image%.*}.png
            ddjvu -format=tiff -page=1 ${image%.*}-$method.djvu ${image%.*}-$method.tiff
            convert ${image%.*}-$method.tiff all/$method/${image%.*}-$method.png
            cp all/$method/${image%.*}-$method.png all/${image%.*}/${image%.*}-$method.png

            rm ${image%.*}-$method.djvu
            rm ${image%.*}-$method.tiff
        done
    done
   
   
   
   
   for mode in black foreground mask background
    do
        mkdir $mode
        for image in *.png
        do
            mkdir $mode/${image%.*}
            for method in abutaleb bernsen brink djvu global niblack otsu sauvola  shading-subtraction tsai white-rohrer
            do 
                mkdir $mode/$method
                didjvu encode -m $method -o ${image%.*}-$method.djvu ${image%.*}.png
                ddjvu -format=tiff -mode=$mode -page=1 ${image%.*}-$method.djvu ${image%.*}-$method.tiff
                convert ${image%.*}-$method.tiff $mode/$method/${image%.*}-$method.png
                cp $mode/$method/${image%.*}-$method.png $mode/${image%.*}/${image%.*}-$method.png

                rm ${image%.*}-$method.djvu
                rm ${image%.*}-$method.tiff
            done
        done
    done

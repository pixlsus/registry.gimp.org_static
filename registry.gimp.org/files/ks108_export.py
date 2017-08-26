#!/usr/bin/env python
# -*- coding: utf-8 -*-

#####################################################################################
## This code is released under the GPL liscence by Raul Aguaviva (aguaviva--gmail.com)
## http://www.gnu.org/licenses/gpl.html
#####################################################################################
## The Gimp exporter 1bit bitmaps to a C language header file Textures
#####################################################################################
## * Installation
##
## Linux:  Copy this file in to your ~/.gimp-2.6/plug-ins
## Windows: Not sure how to install it in windows :-)
##
## * Instructions
##
## 1) Open the image to export in GIMP
## 2) Resize the image to 128x64
## 3) Convert the image to a "indexed image" ( /Image/Transform/Indexed/ )
##    a) Select black adn white (1-bit) palette
##    b) Select the dithering method (Floyd steinberg is a good one )
## 4) Select "Save as"  and look for the KS0108
## 
## * How lo load it
##
## void LoadBitmap( uint8_t *bitmap )
## {
##     for(y=0;y<8;y++)
##     {
##         SelectLeftSide() 
##         LocateXY( 0, y )
##
##         for(x=0;x<64;x++)
##         {
##             ks0108_Write( pgm_read_byte(bitmap++) )
##         }
##
##         SelectRightSide() 
##         LocateXY( 0, y )
##
##         for(x=0;x<64;x++)
##         {
##             ks0108_Write( pgm_read_byte(bitmap++) )
##         }
##     } 
## }
##
## * Greetings
##   
## Thanks to M-A Loyer since this script was heavily inspired by his "Gimp exporter for NDS 3D Textures"
##
#####################################################################################

import struct
import gimp
from gimpfu import *

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)


def python_ks0108export(img, drawable, filename, raw_filename):
    pdb.gimp_layer_resize_to_image_size(drawable)
    width = drawable.width
    height = drawable.height
    
    fileOut = open(filename,"w")

    #remove path from filename and extension
    i = filename.rfind("/")
    p = filename.rfind(".")
    if ( i >= 0 ):
        if ( p == -1 ):
            filename = filename[i+1:len(filename) ]
        else:
            filename = filename[i+1:p ]
        
    gimp.progress_init(_("Saving as KS0108 (1 bit color)"))

    fileOut.write("#ifndef %s\n" % filename.upper() )
    fileOut.write("#define %s\n" % filename.upper() )
    fileOut.write("\n")
    fileOut.write("#include <avr/pgmspace.h>\n")
    fileOut.write("\n")
    fileOut.write("static uint8_t %s[] PROGMEM = {\n" % filename )

    for y in range(0,height, 8):
        for x in range(0,width, 8):
            for xx in range(0,8):
                byte = 0
                for yy in range(0,8):
                    (channels,pixel) = pdb.gimp_drawable_get_pixel(drawable,x + xx, y + yy)
                    byte = byte | ( pixel[0] << yy )
                fileOut.write("0x%02X, " % byte)
            gimp.progress_update(float(x+y*width)/(width*height))
            fileOut.write( "\n" )

    fileOut.write("};\n" )
    fileOut.write("#endif /* %s */\n" % filename.upper() )

    gimp.progress_update(1)
    fileOut.close()

def register_save_ks0108():
    gimp.register_save_handler("file-KS0108exportH-save", "h", "")

register(
        "file-KS0108exportH-save",
        N_("Save in raw KS0108 format as header file"),
        "Export an image to a .h file",
        "Raul Aguaviva",
        "Raul Aguaviva",
        "2009",
        N_("KS0108 1bit header file"),
        "INDEXED",
        [
            (PF_IMAGE, "image", "Input image", None),
            (PF_DRAWABLE, "drawable", "Input drawable", None),
            (PF_STRING, "filename", "The name of the file", None),
            (PF_STRING, "raw-filename", "The name of the file", None),
        ],
        [],
        python_ks0108export, on_query=register_save_ks0108,
        menu="<Save>", domain=("gimp20-python", gimp.locale_directory))
main()

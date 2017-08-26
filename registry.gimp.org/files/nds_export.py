#!/usr/bin/env python
# -*- coding: utf-8 -*-

#####################################################################################
## This code is released under the GPL liscence by M-A Loyer (weirdfox--gmail.com)
## http://www.gnu.org/licenses/gpl.html
#####################################################################################
## The Gimp exporter for NDS 3D Textures
#####################################################################################
##
## File format description :
##  * A3I5 and A5I3
##     char[4]   file type : "a3i5" or "a5i3"
##     uint32    width
##     uint32    height
##     u8[]      image data with encodded alpha and index (8bit by pixel).
##               size: width x height x 1bytes
##     u16[]     palette data. size: nbcolor x 2bytes
##
#####################################################################################

import struct
import gimp
from gimpfu import *

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)

## Convert a rgb color to RGB15 format
def rgb32toint16(rr,gg,bb):
    if rr+4 >255:
        r = 31
    else:
        r = (rr+4) >> 3
    if gg+4 >255:
        g = 31
    else:
        g = (gg+4) >> 3
    if bb+4 >255:
        b = 31
    else:
        b = (bb+4) >> 3
    color = r | (g<<5) | (b<<10)
    return color

## Write the palette from imageIndex to fileOut in RGB15 format
def createPalette(imageIndex,nbColors,fileOut):
    # Write the palette to the file
    (nbindex,colormap) = pdb.gimp_image_get_colormap(imageIndex)
    for i in range(0,nbColors):
        try:
            color = rgb32toint16(colormap[i*3+0],colormap[i*3+1],colormap[i*3+2])
        except IndexError:
            color = 0
        fileOut.write(struct.pack("<H", color))
        gimp.progress_update(float(i*(0.2/nbColors) + 0.8))


def index_copy_of_image(img,index):
    pdb.gimp_edit_copy_visible(img)
    imageIndex = pdb.gimp_edit_paste_as_new()
    pdb.gimp_image_flatten(imageIndex)
    pdb.gimp_image_convert_indexed(imageIndex,0,0,index,False,False,"")
    return (imageIndex,pdb.gimp_image_get_active_layer(imageIndex))


def python_ndsexport_rgbx(img, drawable, filename, raw_filename):
    pdb.gimp_layer_resize_to_image_size(drawable)
    width = drawable.width
    height = drawable.height

    fileOut = open(filename,"wb")

    ## TODO : Use pixelregion instead of gimp_drawable_get_pixel
    #pr = drawable.get_pixel_rgn(0, 0, width, height, False, False)

    gimp.progress_init(_("Saving as Nintendo DS RGB (raw 16 bit color)"))

    fileOut.write("RGBx") # file header
    fileOut.write(struct.pack("<II", width,height))
    bytePartCount = 0
    for y in range(0,height):
        for x in range(0,width):
            (channels,pixel) = pdb.gimp_drawable_get_pixel(drawable,x,y) # TO OPTIMIZE, very slow
            color = rgb32toint16(pixel[0],pixel[1],pixel[2]) | (1<<15)
            fileOut.write(struct.pack("<H", color))
            gimp.progress_update(float(x+y*width)/(width*height))

    gimp.progress_update(1)
    fileOut.close()

def python_ndsexport_rgba(img, drawable, filename, raw_filename):
    pdb.gimp_layer_resize_to_image_size(drawable)
    width = drawable.width
    height = drawable.height

    fileOut = open(filename,"wb")

    ## TODO : Use pixelregion instead of gimp_drawable_get_pixel
    #pr = drawable.get_pixel_rgn(0, 0, width, height, False, False)

    gimp.progress_init(_("Saving as Nintendo DS RGBA (raw 16 bit color with 1 bit alpha)"))

    fileOut.write("RGBA") # file header
    fileOut.write(struct.pack("<II", width,height))
    bytePartCount = 0
    for y in range(0,height):
        for x in range(0,width):
            (channels,pixel) = pdb.gimp_drawable_get_pixel(drawable,x,y) # TO OPTIMIZE, very slow
            color = rgb32toint16(pixel[0],pixel[1],pixel[2])
            if channels > 3 and pixel[3] > 128:
                 color |= (1<<15)
            fileOut.write(struct.pack("<H", color))
            gimp.progress_update(float(x+y*width)/(width*height))

    gimp.progress_update(1)
    fileOut.close()

def python_ndsexport_rgb16(img, drawable, filename, raw_filename):
    pdb.gimp_layer_resize_to_image_size(drawable)
    width = drawable.width
    height = drawable.height

    fileOut = open(filename,"wb")

    ## TODO : Use pixelregion instead of gimp_drawable_get_pixel
    #pr = drawable.get_pixel_rgn(0, 0, width, height, False, False)

    gimp.progress_init(_("Saving as Nintendo DS RGB16 (16 colors palette)"))

    imageIndex,indexedDrawable = index_copy_of_image(img,16)

    fileOut.write("16co") # file header
    fileOut.write(struct.pack("<II", width,height))
    bytePartCount = 0
    for y in range(0,height):
        for x in range(0,width):
            channels, pixels = pdb.gimp_drawable_get_pixel(indexedDrawable,x,y) # TO OPTIMIZE, very slow
            if bytePartCount == 1:
                value += pixels[0] << 4
            else:
                value = pixels[0]
            bytePartCount+=1
            if bytePartCount == 2:
                fileOut.write(struct.pack("<B", value))
                bytePartCount = 0
            gimp.progress_update(float((x+y*width)*0.8)/(width*height))

    createPalette(imageIndex,16,fileOut)
    gimp.progress_update(1)

    fileOut.close()

    gimp.delete(img)

def python_ndsexport_rgb256(img, drawable, filename, raw_filename):
    pdb.gimp_layer_resize_to_image_size(drawable)
    width = drawable.width
    height = drawable.height

    fileOut = open(filename,"wb")

    ## TODO : Use pixelregion instead of gimp_drawable_get_pixel
    #pr = drawable.get_pixel_rgn(0, 0, width, height, False, False)

    gimp.progress_init(_("Saving as Nintendo DS RGB256 (256 colors palette)"))
    imageIndex,indexedDrawable = index_copy_of_image(img,16)

    fileOut.write("256c") # file header
    fileOut.write(struct.pack("<II", width,height))
    for y in range(0,height):
        for x in range(0,width):
            (channels,pixel) = pdb.gimp_drawable_get_pixel(indexedDrawable,x,y) # TO OPTIMIZE, very slow
            value = pixel[0]
            fileOut.write(struct.pack("<B", value))
            gimp.progress_update(float((x+y*width)*0.8)/(width*height))

    createPalette(imageIndex,256,fileOut)
    gimp.progress_update(1)

    fileOut.close()

    gimp.delete(img)

def python_ndsexport_A3I5(img, drawable, filename, raw_filename):
    pdb.gimp_layer_resize_to_image_size(drawable)
    width = drawable.width
    height = drawable.height

    fileOut = open(filename,"wb")

    ## TODO : Use pixelregion instead of gimp_drawable_get_pixel
    #pr = drawable.get_pixel_rgn(0, 0, width, height, False, False)

    gimp.progress_init(_("Saving as Nintendo DS A3I5"))
    imageIndex,indexedDrawable = index_copy_of_image(img,16)

    fileOut.write("a3i5") # file header
    fileOut.write(struct.pack("<II", width,height))
    for y in range(0,height):
        for x in range(0,width):
            (channels,pixel) = pdb.gimp_drawable_get_pixel(drawable,x,y) # TO OPTIMIZE, very slow
            if channels < 4:
                alpha = 7
            else:
                alpha = round((pixel[3] / 255) * 7)
            (channels,pixel) = pdb.gimp_drawable_get_pixel(indexedDrawable,x,y) # TO OPTIMIZE, very slow
            index = pixel[0]
            value = int(alpha) << 5
            value += index
            fileOut.write(struct.pack("<B", value))
            gimp.progress_update(float((x+y*width)*0.8)/(width*height))

    createPalette(imageIndex,32,fileOut)
    gimp.progress_update(1)

    fileOut.close()

    gimp.delete(img)


def python_ndsexport_A5I3(img, drawable, filename, raw_filename):
    pdb.gimp_layer_resize_to_image_size(drawable)
    width = drawable.width
    height = drawable.height

    fileOut = open(filename,"wb")

    ## TODO : Use pixelregion instead of gimp_drawable_get_pixel
    #pr = drawable.get_pixel_rgn(0, 0, width, height, False, False)

    gimp.progress_init(_("Saving as Nintendo DS A5I3"))
    imageIndex,indexedDrawable = index_copy_of_image(img,16)

    fileOut.write("a5i3") # file header
    fileOut.write(struct.pack("<II", width,height))
    for y in range(0,height):
        for x in range(0,width):
            (channels,pixel) = pdb.gimp_drawable_get_pixel(drawable,x,y) # TO OPTIMIZE, very slow
            if channels < 4:
                alpha = 31
            else:
                alpha = round((pixel[3] / 255) * 31)
            (channels,pixel) = pdb.gimp_drawable_get_pixel(indexedDrawable,x,y) # TO OPTIMIZE, very slow
            index = pixel[0]
            value = int(alpha) << 3
            value += index
            fileOut.write(struct.pack("<B", value))
            gimp.progress_update(float((x+y*width)*0.8)/(width*height))

    createPalette(imageIndex,8,fileOut)
    gimp.progress_update(1)

    fileOut.close()
    gimp.delete(img)


def register_save_a3i5():
    gimp.register_save_handler("file-ndsexportA3I5-save", "a3i5", "")
def register_save_a5i3():
    gimp.register_save_handler("file-ndsexportA5I3-save", "a5i3", "")
def register_save_rgb256():
    gimp.register_save_handler("file-ndsexportRGB256-save", "256c", "")
def register_save_rgb16():
    gimp.register_save_handler("file-ndsexportRGB16-save", "16c", "")
def register_save_rgbx():
    gimp.register_save_handler("file-ndsexportRGBx-save", "rgbx", "")
def register_save_rgba():
    gimp.register_save_handler("file-ndsexportRGBA-save", "rgba", "")

register(
        "file-ndsexportA3I5-save",
        N_("Save as NDS A3I5"),
        "Export an image to a native Nintendo DS texture format A3I5 - 8bit: 3bit alpha, 5bit index (32 colors)",
        "M-A Loyer",
        "M-A Loyer",
        "2009",
        N_("NDS 32 colors A3I5"),
        "RGB*, GRAY*",
        [
            (PF_IMAGE, "image", "Input image", None),
            (PF_DRAWABLE, "drawable", "Input drawable", None),
            (PF_STRING, "filename", "The name of the file", None),
            (PF_STRING, "raw-filename", "The name of the file", None),
        ],
        [],
        python_ndsexport_A3I5, on_query=register_save_a3i5,
        menu="<Save>", domain=("gimp20-python", gimp.locale_directory))

register(
        "file-ndsexportA5I3-save",
        N_("Save as NDS A5I3"),
        "Export an image to a native Nintendo DS texture format A5I3 - 8bit: 5bit alpha, 3bit index (8 colors)",
        "M-A Loyer",
        "M-A Loyer",
        "2009",
        N_("NDS 8 colors A5I3"),
        "RGB*, GRAY*",
        [
            (PF_IMAGE, "image", "Input image", None),
            (PF_DRAWABLE, "drawable", "Input drawable", None),
            (PF_STRING, "filename", "The name of the file", None),
            (PF_STRING, "raw-filename", "The name of the file", None),
        ],
        [],
        python_ndsexport_A5I3, on_query=register_save_a5i3,
        menu="<Save>", domain=("gimp20-python", gimp.locale_directory))

register(
        "file-ndsexportRGB256-save",
        N_("Save as NDS RGB256"),
        "Export an image to a native Nintendo DS texture format RGB 256 color - 8bit by pixel",
        "M-A Loyer",
        "M-A Loyer",
        "2009",
        N_("NDS 256 color RGB"),
        "RGB*, GRAY*",
        [
            (PF_IMAGE, "image", "Input image", None),
            (PF_DRAWABLE, "drawable", "Input drawable", None),
            (PF_STRING, "filename", "The name of the file", None),
            (PF_STRING, "raw-filename", "The name of the file", None),
        ],
        [],
        python_ndsexport_rgb256, on_query=register_save_rgb256,
        menu="<Save>", domain=("gimp20-python", gimp.locale_directory))

register(
        "file-ndsexportRGB16-save",
        N_("Save as NDS RGB216"),
        "Export an image to a native Nintendo DS texture format RGB 16 color - 4bit by pixel",
        "M-A Loyer",
        "M-A Loyer",
        "2009",
        N_("NDS 16 colors RGB"),
        "RGB*, GRAY*",
        [
            (PF_IMAGE, "image", "Input image", None),
            (PF_DRAWABLE, "drawable", "Input drawable", None),
            (PF_STRING, "filename", "The name of the file", None),
            (PF_STRING, "raw-filename", "The name of the file", None),
        ],
        [],
        python_ndsexport_rgb16, on_query=register_save_rgb16,
        menu="<Save>", domain=("gimp20-python", gimp.locale_directory))

register(
        "file-ndsexportRGBx-save",
        N_("Save as NDS RGB216"),
        "Export an image to a native Nintendo DS texture format raw RGB 15bit",
        "M-A Loyer",
        "M-A Loyer",
        "2009",
        N_("NDS raw RGB"),
        "RGB",
        [
            (PF_IMAGE, "image", "Input image", None),
            (PF_DRAWABLE, "drawable", "Input drawable", None),
            (PF_STRING, "filename", "The name of the file", None),
            (PF_STRING, "raw-filename", "The name of the file", None),
        ],
        [],
        python_ndsexport_rgbx, on_query=register_save_rgbx,
        menu="<Save>", domain=("gimp20-python", gimp.locale_directory))

register(
        "file-ndsexportRGBA-save",
        N_("Save as NDS RGB216"),
        "Export an image to a native Nintendo DS texture format raw RGBA 16bit - 1bit alpha",
        "M-A Loyer",
        "M-A Loyer",
        "2009",
        N_("NDS raw RGBA"),
        "RGB",
        [
            (PF_IMAGE, "image", "Input image", None),
            (PF_DRAWABLE, "drawable", "Input drawable", None),
            (PF_STRING, "filename", "The name of the file", None),
            (PF_STRING, "raw-filename", "The name of the file", None),
        ],
        [],
        python_ndsexport_rgba, on_query=register_save_rgba,
        menu="<Save>", domain=("gimp20-python", gimp.locale_directory))
main()

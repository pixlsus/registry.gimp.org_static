#!/usr/bin/env python
# Author: Chris Mohler
# Copyright 2009 Chris Mohler
# License: GPL v3
# Version 0.1
# GIMP plugin to create image from palette

from gimpfu import *

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)


def palette_to_image(tpalette, font, size):
	print tpalette, font, size
	palette = pdb.gimp_context_get_palette()
	colors = pdb.gimp_palette_get_colors(palette)[1]
	img = pdb.gimp_image_new(200, len(colors)*size, 0)
	lyr = pdb.gimp_layer_new(img, img.width, img.height, 0, palette, 100, 0)
	pdb.gimp_image_add_layer(img, lyr, -1)
	pdb.gimp_edit_fill(lyr, 2)
	for i in range(len(colors)):
		name = pdb.gimp_palette_entry_get_name(palette, i)
		color = colors[i]
		rect = pdb.gimp_rect_select(img, 0, size*i, size, size, 2, 0, 0)
		pdb.gimp_context_set_foreground(color)
		pdb.gimp_edit_bucket_fill(lyr, 0, 0, 100, 0, False, 0, 0)
		pdb.gimp_context_set_foreground((0,0,0))
		txt = pdb.gimp_text_layer_new(img, name, font, size*0.9, 0)
		pdb.gimp_image_add_layer(img, txt, -1)
		pdb.gimp_layer_translate(txt, size*1.2, (size*i))
		lyr = pdb.gimp_image_merge_down(img, txt, 0)
	pdb.gimp_selection_none(img)
	pdb.gimp_image_resize(img, lyr.width, lyr.height, 0, 0)
	pdb.gimp_context_set_background((1.0,1.0,1.0))
	pdb.gimp_image_flatten(img)
	pdb.gimp_display_new(img)
    
register(
    proc_name=("python-fu-palette-to-image"),
    blurb=("Palette to Image"),
    help=("Create an image from the active palette"),
    author=("Chris Mohler"),
    copyright=("Chris Mohler"),
    date=("2009"),
    label=("Palette to Image"),
    imagetypes=(""),
    params=[
			(PF_PALETTE, "tpalette", "Palette", "Default"),
			(PF_FONT, "font", "Font", "Sans"),
			(PF_INT, "size", "Size", 20),
			],
    results=[],
    function=(palette_to_image), 
    menu=("<Palettes>"), 
    domain=("gimp20-python", gimp.locale_directory)
    )

main()

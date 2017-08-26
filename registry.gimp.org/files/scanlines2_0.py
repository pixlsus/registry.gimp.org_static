#!/usr/bin/env python

# Gimp scanlines plugin for The Gimp 2.x (compiled with --enable-python).

# Written by Werner Hartnagel 02/2004
# www.uniquesource.de

from gimpfu import *

def scanlines2(img, drawable, bar_width, space_width, is_mask):
	# Initial Suff
	img.undo_group_start()
	width = 1
	height = bar_width + space_width
	act_lay = img.active_layer
	
	# Create the Pattern Layer
	pat_lay = gimp.Layer(img, "Scanlines", width, height, RGBA_IMAGE, 100, NORMAL_MODE)
        pat_lay2 = gimp.Layer(img, "Scanlines2", width, height, RGBA_IMAGE, 100, NORMAL_MODE)
	img.add_layer(pat_lay, 0)
	pdb.gimp_edit_clear(pat_lay)
	
	# Draw the Pattern
	r,g,b, a = gimp.get_foreground()
	col = (r,g,b,255)
	for y in xrange(bar_width):
		pdb.gimp_drawable_set_pixel(pat_lay, 0, y, 4, col)
	
	# Scale  width for faster tile
	pat_lay.scale(100, height)
	
	# Tile the Layer
	pdb.plug_in_tile(img, pat_lay, img.width, img.height, FALSE)
	
	if is_mask:
		if not act_lay.has_alpha:
			act_lay.add_alpha()
		pat_mask = pdb.gimp_layer_create_mask(pat_lay, ADD_ALPHA_MASK)
		pdb.gimp_layer_add_mask(act_lay, pat_mask)
		img.remove_layer(pat_lay)
	else:
		img.add_layer(pat_lay2, 0)
	
	# Final Stuff
	img.undo_group_end()

register(
	"python_fu_scanlines2",
	"Create a Scanline Effect",
	"Create a overlayed Layer or Layer Mask with Scanlines using the foreground color",
	"Werner Hartnagel",
	"Werner Hartnagel",
	"2004",
	"<Image>/Python-Fu/Create Layers/Scanlines2",
	"RGB*, GRAY*",
	[
		(PF_SPINNER, "bar_width", "Bar Width", 2, (1, 21, 1)),
		(PF_SPINNER, "space_width", "Space Width", 2, (1, 101, 1)),
		(PF_TOGGLE, "is_mask", "Layer Mask", 1)
	],
	[],
	scanlines2)

main()

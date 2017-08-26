#!/usr/bin/env python
# Author: Chris Mohler
# Copyright 2009 Chris Mohler
# License: GPL v3
# GIMP plugin to count the number of tiles of the FG color

from gimpfu import *

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)


def count_mosaic_tiles(img,  drw, tilesize, min_percent, grid):
	
	# grab FG and BG color
	fg = pdb.gimp_context_get_foreground()
	bg = pdb.gimp_context_get_background()
	
	# figure out how many tiles we're dealing with
	rows = img.height/tilesize
	cols = img.width/tilesize
	tiles = rows * cols
	
	# init some counters
	current = tiles
	tilex = 0
	tiley = 0
	numtiles = 0
	total_pix = 0

	for row in range(rows):  #every row of tiles
		tilex = 0

		for col in range(cols):				#every column of tiles
			
			#let's update the progress bar
			progress = round((1-(float(current)/float(tiles))),1)
			pdb.gimp_progress_update(progress)
			current = current - 1
			
			#print "col " + str(col)
			is_tile = False
			num_pix = 0
			
			for tr in range(tilesize):  # every row of pixels
				for tc in range (tilesize): # every colum of pixels
					pxl = drw.get_pixel((tilex+tc), (tiley +tr)) #get pixel
					color = gimpcolor.RGB(pxl[0], pxl[1], pxl[2]) #change to gimpcolor
					#print fg, color
					if color == fg: 
						num_pix = num_pix +1
						total_pix = total_pix+1
					
			tilex = tilex + tilesize
			percent_tile = (float(num_pix)/(float(tilesize) * float(tilesize))) * 100.0
			#print percent_tile
			if percent_tile > float(min_percent):
				numtiles = numtiles +1
		tiley = tiley + tilesize
		
	if grid:
		lyr = pdb.gimp_layer_new(img, img.width, img.height, img.base_type, "grid", 100, 0)
		lyr.add_alpha()
		lyr.fill(3)
		pdb.gimp_image_add_layer(img, lyr, -1)
		drw = img.active_drawable
		
		pdb.plug_in_grid(img, drw, 1, tilesize, 0, bg, 255, 1, tilesize, 0, bg, 255, 1, 0, 2, bg, 0)
	
	total_percent = (float(total_pix)/(float(img.width)*float(img.height))) * 100.0
	pdb.gimp_message( "Number of tiles: " + str(numtiles) + "\nTotal Percent: " + str(total_percent) + "%")


register(
		"python-fu-count-mosaic-tiles",
		"Count the number of tiles of the FG color.",
		"Count the number of tiles of the FG color.\nOptionally draw a grid in the BG color",
		"Chris Mohler",
		"Chris Mohler",
		"2009",
		"<Image>/Filters/Mosaic/_Count Mosaic Tiles", 
		"RGB*",
		[
		(PF_INT,   "tilesize",    	"Tile Size (pixels):",    "20"),
		(PF_SPINNER, "min_percent", "Minimum Percent:", 1, (0, 100, 1)), 
		(PF_BOOL,   "grid",    	"Draw Grid?",    0)
		],
		[],
		count_mosaic_tiles, 
		domain=("gimp20-python", gimp.locale_directory)
		)

main()

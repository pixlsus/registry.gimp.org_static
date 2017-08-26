#!/usr/bin/env python

#   Gimp yin/yang symbol plugin for The Gimp 2.3.x (Python-Fu Version).

#   Written by Werner Hartnagel based on Perl Plugin from Aaron Sherman
#   Credits: Carol Spears who helped me to find out the trouble with gimp_file_load()

#   Main function. Takes width, height, do_eyes (toggle), eye_images (toggle),
#   white_eye_image (filename) and black_eye_image (filename).
#   Creates a stand-alone image with a yin-yang symbol in black and white.
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#   Modified on 9/23/2009 for GIMP 2.6.7 by Mahvin <mahvin@gmail.com>

from gimpfu import *

def py_yinyang(width, height, do_eyes, eye_images, white_eye_image, black_eye_image, aa):
	# Create new image
	img = gimp.Image(width, height, RGB)
	
	# Disable Undo
	img.undo_group_start()
	oldcolor = gimp.get_foreground()

	layer = gimp.Layer(img, "Yin/Yang", width, height, RGBA_IMAGE, 100, NORMAL_MODE)
	img.add_layer(layer, 0)
	img.active_layer = layer

	draw = pdb.gimp_image_get_active_drawable(img)
	gimp.set_foreground(0,0,0)
	pdb.gimp_selection_all(img)
#	pdb.gimp_edit_bucket_fill(draw,0,0,100,0,0,0,0)
	draw.fill(FOREGROUND_FILL)

	# Create the yin-yang shape
	#gimp_selection_invert($img)
	pdb.gimp_selection_none(img)
	pdb.gimp_rect_select(img, 0, 0, width/2, height, CHANNEL_OP_ADD, False, 0)
	pdb.gimp_ellipse_select(img, width/2-width/4, 0, width/2, int(height/2), CHANNEL_OP_ADD, aa, False, 0)
	pdb.gimp_ellipse_select(img, width/2-width/4, height/2, width/2, height/2, CHANNEL_OP_SUBTRACT, aa, False, 0)
	gimp.set_foreground(255,255,255)
	pdb.gimp_edit_bucket_fill(draw,0,0,100,0,0,0,0)

	# Cut away all but the central circle
	pdb.gimp_ellipse_select(img, 0, 0, width, height, CHANNEL_OP_REPLACE, aa, False, 0)
	pdb.gimp_selection_invert(img)
	pdb.gimp_edit_clear(draw)

	# Create the "eyes"
	if (do_eyes):
		x1 = width/2-width/16
		y1 = height/2-height/4-height/16
		x2 = x1
		y2 = height/2+height/4-height/16
		eyewidth = width/8
		eyeheight = height/8
		insert_eye(img, eye_images, white_eye_image, (0,0,0), x1, y1, eyewidth, eyeheight, draw, aa)
		insert_eye(img, eye_images, black_eye_image, (255,255,255), x2, y2, eyewidth, eyeheight, draw, aa)


	# Finish up
	gimp.set_foreground(oldcolor)
	pdb.gimp_selection_none(img)
	
	# Enable Undo
	img.undo_group_end()

	disp1 = gimp.Display(img)

# This subroutine inserts an "eye" (a dot in the center of the cicular
# part of each of the halves of the yin-yang). The eye is either
# a solid dot of the opposite color from that half of the yin-yang or
# an image, which is loaded and scaled to fit.
def insert_eye(img, do_image, file, color, x, y, width, height, draw, aa):
	pdb.gimp_ellipse_select(img, x, y, width, height, CHANNEL_OP_REPLACE, aa, False, 0)
	pdb.gimp_context_set_foreground(color)
	if (do_image):
		eye = pdb.gimp_file_load(file, file)
		eye.scale(width, height)
		pdb.gimp_selection_all(eye)
		eyedraw = eye.active_drawable
		pdb.gimp_edit_copy(eyedraw)
		float = pdb.gimp_edit_paste(draw, 1)
		pdb.gimp_floating_sel_anchor(float)
		pdb.gimp_image_delete(eye)
	else:
		pdb.gimp_edit_bucket_fill(draw,0,0,100,0,0,0,0)

# Register with The Gimp
register(
	"python_fu_yinyang",
	"Render a stand-alone Yin/Yang image",
	"Renders a black-and-white Yin/Yang symbol optionally with \"eyes\" that may optionally be images.",
	"Werner Hartnagel",
	"(c) 2005, Werner Hartnagel",
	"2005",
	"<Toolbox>/Xtns/Python-Fu/Misc/Yin-Yang...",
	"",
	[
		[PF_INT32, "width", "Width", 256],
		[PF_INT32, "height", "Height", 256],
		[PF_TOGGLE, "insert_eyes", "Insert Eyes", 1],
		[PF_TOGGLE, "eyes_are_images", "Eyes are Images", 0],
		[PF_STRING, "top_eye_filename", "Top Eye Filename", ""],
		[PF_STRING, "bottom_eye_filename", "Bottom Eye Filename", ""],
		[PF_TOGGLE, "anti_aliasing", "Anti Aliasing", 1]
	],
	[],
	py_yinyang)

main()

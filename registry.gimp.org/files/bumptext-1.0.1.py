#!/usr/bin/python
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
#   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

import re
import math
from gimpfu import *

def python_bumptext(image, drawable, text, font, blur, depth, x, y, rotation, invert):
    #split the font size from the name
    font = re.search("(.*) (\d+)", font)
    fontname = font.group(1)
    fontsize = font.group(2)
    #save the back and foreground colors
    background = gimp.get_background()
    foreground = gimp.get_foreground()
    #set the back and foreground colors
    gimp.set_background(255, 255, 255)
    gimp.set_foreground(0, 0, 0)
    #create the text layer and fill it
    textlayer = gimp.Layer(image, "textlayer", drawable.width, drawable.height, RGBA_IMAGE,
    100, NORMAL_MODE)
    image.add_layer(textlayer, 0)
    pdb.gimp_drawable_fill(textlayer, 0)
    #create the text
    gimp.set_background(0, 0, 0)
    gimp.set_foreground(255, 255, 255)
    floattext = pdb.gimp_text_fontname(image, textlayer, x, y, text, 1, 1, fontsize, 1, fontname)
    #rotate the text
    if rotation != 0:
        #set the floating selection rotation center
        centerx = floattext.width /2 + x
        centery = floattext.height /2 + y
        rotation = math.radians(rotation)
        pdb.gimp_drawable_transform_rotate_default(floattext, rotation, 0, centerx, centery, 1, 0)
    #anchor the floating text
    pdb.gimp_floating_sel_anchor(floattext)
    #blur the text
    pdb.plug_in_gauss(image, textlayer, blur, blur, 0)
    #bump the image
    pdb.plug_in_bump_map(image, drawable, textlayer, 135, 45, depth, 0, 0, 0, 0, 1, invert, 0)
    #delete the textlayer and clean up
    image.remove_layer(textlayer)
    
    #reset to original back and foreground
    gimp.set_background(background)
    gimp.set_foreground(foreground)
    
register(
    "python_fu_bumptext",
    "Bump image with specified text",
    "Bumps the input text on the selected layer. Great for watermarking.",
    "Jason T. Powell",
    "Jason T. Powell",
    "2005",
    "<Image>/Python-Fu/Alpha to Logo/Bump Text",
    "*",
    [
    (PF_STRING, "text", "Text to bump", "Bump Me"),
    (PF_FONT, "font", "Choose Font", "Sans 32"),
    (PF_INT, "blur", "Text blur", '5'),
    (PF_INT, "depth", "Depth", '3'),
    (PF_INT, "x", "Text X Placement", '0'), 
    (PF_INT, "y", "Text Y Placement", '0'),
    (PF_SLIDER, "rotation", "Text Rotation", 0, [-180, 180, .5]),
    (PF_TOGGLE, "invert", "Invert", 0)
    ],
    [],
    python_bumptext)
main()

#GPL Last modified 02-12-10


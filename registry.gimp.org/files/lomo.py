#!/usr/bin/env python

#   Lomo effect - allows retouching image lomo style.
#   Copyright (C) 1997  Supreet Sethi <supreet.sethi@gmail.com>
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

from gimpfu import *
import time

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)

def calc_coord(img):
    x1 = img.width/2
    y1 = img.height/2
    x2 = img.width - 1/4*(img.width)
    y2 = img.height - 1/4*(img.height)
    return x1,y1,x2,y2

def lomofy(img, layer, name, c41, flatten):
    img.undo_group_start()
    lomo = gimp.Layer(img, name, layer.width, layer.height, 
                      RGBA_IMAGE, 40, BURN_MODE)
    cross = pdb.gimp_layer_copy(layer, False)
    img.add_layer(cross, 0)
    img.add_layer(lomo, 0)
    lomo_mask = lomo.create_mask(ADD_WHITE_MASK)
    oldbg = gimp.get_background()
    oldfg = gimp.get_foreground()
    gimp.set_foreground((0,0,0))
    gimp.set_background((255,255,255))
    background =  layer
    rc = calc_coord(img)
    pdb.gimp_edit_blend(lomo, FG_BG_RGB_MODE, NORMAL_MODE,
                        GRADIENT_RADIAL,
                        100,
                        1,
                        REPEAT_NONE,
                        True,
                        True,
                        1,
                        1,
                        True,
                        rc[0],
                        rc[1],
                        rc[2],
                        rc[3]
                        )
    gimp.set_foreground(oldfg)
    gimp.set_background(oldbg)

    if c41 == True:
        pdb.gimp_curves_spline(cross, HISTOGRAM_VALUE, 8,
                               [0,0,
                               68,64,
                               190,219,
                               255,255])
        
        pdb.gimp_curves_spline(cross, HISTOGRAM_RED, 8,
                               [0,0,
                               39,93,
                               193,147,
                               255,255])
        pdb.gimp_curves_spline(cross, HISTOGRAM_GREEN, 6,
                               [0,0,
                               68,70,
                               255,207])
        pdb.gimp_curves_spline(cross, HISTOGRAM_BLUE, 6,
                               [0,0,
                               94,94,
                               255,199])
                               

    if flatten == True:
        pdb.gimp_image_flatten(img)

    img.undo_group_end()

register(
    "python-fu-retouch-lomo",
    N_("Creating reproduction of lomo effect"),
    "Creating Lomo camera style image.",
    "Supreet Sethi <supreet.sethi@gmail.com>",
    "Supreet Sethi",
    "2008",
    N_("_Lomo"),
    "RGB*",
    [
        (PF_IMAGE, "image", "Input image", None),
        (PF_DRAWABLE, "drawable", "Input drawable", None),
        (PF_STRING, "name", _("_Layer name"), _("Lomo")),
        (PF_TOGGLE, "c41", _("Apply _C41"), False),
        (PF_TOGGLE, "flatten", _("_Flatten Image"), False)

        ],
    [],
    lomofy,
    menu="<Image>/Filters/Retouch",
    domain=("gimp20-python", gimp.locale_directory),
    )

main()


 

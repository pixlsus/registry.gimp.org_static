#!/usr/bin/env python

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
# layoutlayinlayers.py
# version 0.3 2008-01-17
# lay out lay in layers
#  purpose of this program is, to lay out and lay in a group of consecutive layers of an image
# (a humble kind of collapse and expand groups of layers)
# "collapse":
# a determinable number of layers (including the active layer) will be stored (layed out) in a new image.
# the name of this image is a time stamp ("%Y%m%d-%H%M%S")  plus the string  ".xcf"
#  e.g. 20080117-192642.xcf
# the visible layed out layers will be merged down to a single layer in the source image
# the name of this layer is the above mentioned time stamp
# "expand":
# look for the open image with the name like the active layer's name of the source image
# fetch all the layers of the layed-out-layers-image and place them above the active layer
# this active layer will not be replaced but switched to invisible
#
# laylaylay registers itself under <Image>/Layer/Stack

from gimpfu import *
from time import strftime
import string

def laylaylay(mhl_imgin, mhl_drawable, mhl_num_layers, mhl_collapse):
    mhl_active_layer = mhl_imgin.active_layer
#
# Active layer exists begin    
    if mhl_active_layer <> None:
        mhl_imgin.undo_group_start()
        mhl_active_layer_pos = pdb.gimp_image_get_layer_position(mhl_imgin, mhl_active_layer)
#
# Collapse selected begin
        if mhl_collapse == "C":
            mhl_collapse_layer = pdb.gimp_layer_new_from_drawable(mhl_active_layer, mhl_imgin)
            mhl_dt_stamp = strftime("%Y%m%d-%H%M%S")
            mhl_collapse_layer.name = mhl_dt_stamp
            mhl_imgin.add_layer(mhl_collapse_layer, -1)
            gimp.Image.lower_layer(mhl_imgin, mhl_collapse_layer)
            pdb.gimp_edit_clear(mhl_collapse_layer)
            pdb.gimp_drawable_set_visible(mhl_collapse_layer, True)
            mhl_imgnew = gimp.Image(mhl_imgin.width, mhl_imgin.height, mhl_imgin.base_type)
            pdb.gimp_image_set_filename(mhl_imgnew, mhl_dt_stamp + ".xcf")
            mhl_imgnew.undo_group_start()
            mhl_start = mhl_active_layer_pos
            mhl_stop = mhl_start - mhl_num_layers
            if mhl_stop < -1:
                mhl_stop = -1
            for i in range(mhl_start, mhl_stop, -1):
                mhl_layer_copy = pdb.gimp_layer_new_from_drawable(mhl_imgin.layers[i], mhl_imgnew)
#                                    don't want the "copy"-word as a suffix to the copied layer but just the original name
                mhl_layer_copy.name = mhl_imgin.layers[i].name
                mhl_imgnew.add_layer(mhl_layer_copy, -1)
                visible = pdb.gimp_drawable_get_visible(mhl_imgin.layers[i])
                if visible:
                    pdb.gimp_image_merge_down(mhl_imgin, mhl_imgin.layers[i], 0)
                else:    
                    mhl_imgin.remove_layer(mhl_imgin.layers[i]);
            pdb.gimp_display_new(mhl_imgnew)
            gimp.displays_flush()
            mhl_imgnew.undo_group_end()
# Collapse selected end
#        
#
# Expand selected begin
        else:
            mhl_found = -1
            for j in range(0, len(gimp.image_list())):
                if string.find(gimp.image_list()[j].name, mhl_active_layer.name) == 0:
                    mhl_found = 0
                    for k in range(len(gimp.image_list()[j].layers), 0, -1):
                        mhl_layer_copy = pdb.gimp_layer_new_from_drawable(gimp.image_list()[j].layers[k-1], mhl_imgin)
#                                    don't want the "copy"-word as a suffix to the copied layer but just the original name
                        mhl_layer_copy.name = gimp.image_list()[j].layers[k-1].name
                        mhl_imgin.add_layer(mhl_layer_copy, -1)
                        pdb.gimp_drawable_set_visible(mhl_active_layer, False)
                    break
            if mhl_found == -1:
                message = "no open image corresponds with layer " + mhl_active_layer.name
                gimp.message(message)
# Expand selected end
#        
        mhl_imgin.undo_group_end()
# Active layer exists end
#
#
#No active layer exists begin
    else:
        gimp.message("there's no active layer. maybe a channel is active. please correct and retry")
#No active layer exists end
#
register(
        "python-fu-laylaylay",
        "lay out lay in layers",
        "lay out lay in layers",
        "Michael Hoelzen",
        "Michael Hoelzen",
        "2008",
        "_laylaylay",
        "RGB*, GRAY*",
        [
            (PF_IMAGE, "image", "Input image", None),
            (PF_DRAWABLE, "drawable", "Input drawable", None),
            (PF_SPINNER, "numlayers", "Layers to collapse", 1, (1,100, 1)),
            (PF_RADIO, "mhl-collapse", "Collapse or Expand", "C", (("Collapse", "C"), ("Expand", "E")))
        ],
        [],
        laylaylay, menu="<Image>/Layer/Stack")

main()

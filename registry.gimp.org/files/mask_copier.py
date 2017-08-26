#!/usr/bin/env python
# -*- coding: utf-8 -*-

#Copyright (C) 2008 Joao S. O. Bueno

#This program is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 2 of the License, or
#(at your option) any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

from gimpfu import *

def apply_layer_mask(img, drw, visible_layers):
    """I know it sounds stupid, but as of GIMP 2.5.2 there is no
       PDB call to apply a layer mask.
       The only way to do it seems to be merge_visble_layers
    """
    set_only_visible_layer (img, drw)
    #This hack creates a new laywer, does invalidating the old tatoo
    old_tattoo = drw.tattoo
    new_layer = pdb.gimp_image_merge_visible_layers(img, EXPAND_AS_NECESSARY)
    if new_layer.tattoo != old_tattoo:
        visible_layers[new_layer.tattoo] = visible_layers[old_tattoo]
        del visible_layers[old_tattoo]
    return new_layer


def get_layer_visibility(img):
    visible_layers = {}
    for layer in img.layers:
        visible_layers[layer.tattoo] = layer.visible
    return visible_layers

def set_layer_visibility(img, visible_layers):
    print visible_layers.keys()
    for layer in img.layers:
        print layer.tattoo
        layer.visible = visible_layers[layer.tattoo]

def set_only_visible_layer(img, drw):
    for layer in img.layers:
        layer.visible = True if (layer == drw) else False

def copy_active_mask(img, drw):
    """Copies the active layer mask to all other layers on the image
    """
    #do nothing if current layer has no mask (or if current drawable is no
    #layer
    drw_type = "layer"
    if type(drw) == gimp.Layer and not drw.mask:
        return
    elif type(drw) == gimp.Channel:
        #we could use "pdb.gimp_layuer_from_mask
        # but if drw is another kind of channel and not a  mask
        #that would raise a TypeError __and__ an error dialog
        #(the dialog shows up regardless of a try - except clause)
        drw_type = "channel"
        for layer in img.layers:
            if layer.mask == drw:
                drw = layer
                break
        else:
            return



    mask = drw.mask
    img.undo_group_start()
    visible_layers = get_layer_visibility(img)
    pdb.gimp_edit_copy(mask)
    #img.layers change during the loop due
    #to the hack needed to apply masks:
    layer_list = img.layers[:]
    for layer in layer_list:
        if layer == drw:
            continue
        if layer.mask:
            layer = apply_layer_mask(img, layer, visible_layers)
        new_mask = pdb.gimp_layer_create_mask(layer, ADD_WHITE_MASK)
        pdb.gimp_layer_add_mask(layer, new_mask)
        floating_sel = pdb.gimp_edit_paste(new_mask, True)
        pdb.gimp_floating_sel_anchor(floating_sel)
    set_layer_visibility(img, visible_layers)
    pdb.gimp_image_set_active_layer(img, drw)
    if not drw_type == "layer":
        pdb.gimp_layer_set_edit_mask(drw, True)

    img.undo_group_end()


register(
        "copy_active_mask",
        "Apply other layers masks. Copy mask in active layer to all other layers",
        """
        """,
        "Joao S. O. Bueno",
        "Joao S. O. Bueno",
        "2008. GPL applies.",
        "<Image>/Layer/Transparency/Copy Mask to Other layers",
        "*",
        [],
        [],
        copy_active_mask)

main()

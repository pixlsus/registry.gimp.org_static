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


#made by request of Snake Arsenic in 2008-07-26

from gimpfu import *

def apply_layer_mask(img, drw):
    """I know it sounds stupid, but as of GIMP 2.5.2 there is no
       PDB call to apply a layer mask.
       The only way to do it seems to be merge_visble_layers
    """
    set_only_visible_layer (img, drw)
    #This hack creates a new laywer, does invalidating the old tatoo
    new_layer = pdb.gimp_image_merge_visible_layers(img, EXPAND_AS_NECESSARY)
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

def batch_save_layers(img, drw):
    """Copies the active layer mask to all other layers on the image
    """
    img.undo_group_start()
    visible_layers = get_layer_visibility(img)

    for layer in img.layers:
        layer_copy = layer.copy()
        pdb.gimp_image_add_layer(img, layer_copy, -1)
        print layer, layer_copy

        if layer_copy.mask:
            layer_copy = apply_layer_mask(img, layer_copy)
        else:
            pdb.gimp_layer_flatten(layer_copy)
        print layer_copy, layer_copy.name
        name = img.name.rsplit(".",1)[0] + "-" + layer.name + ".tga"
        pdb.file_tga_save(img, layer_copy, name, name, True, 0)
        pdb.gimp_image_remove_layer(img, layer_copy)
    set_layer_visibility(img, visible_layers)
    pdb.gimp_image_set_active_layer(img, drw)

    img.undo_group_end()


register(
        "batch_save_layers",
        "Save all layers as TGA files",
        """Save all layers as TGA files, discarding alpha channels and
        applying layer masks as alpha if they exist. Name becomes imagename-layername.tga
        """,
        "Joao S. O. Bueno",
        "Joao S. O. Bueno",
        "2008. GPL applies.",
        "<Image>/Layer/Batch Save as TGA",
        "*",
        [],
        [],
        batch_save_layers)

main()

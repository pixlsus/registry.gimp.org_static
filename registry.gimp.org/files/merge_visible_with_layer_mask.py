#!/usr/bin/env python
# -*- coding: utf-8 -*-

#Copyright (C) 2008 Snake Arsenic

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

def merge_visible_with_layer_mask(img, drw):
	for layer in img.layers:
		if layer.mask == drw:
			drw = layer
	# Check if layer has either an alpha channel or a mask.
	if drw.has_alpha != 0:
		pass
	elif drw.layer.mask != "none":
		return
	img.undo_group_start()
	# Transfer mask and alpha channel twice to gather all the transparency to make sure its transferred as a whole
	drw.remove_mask(MASK_APPLY)
	permamask = drw.create_mask(ADD_ALPHA_TRANSFER_MASK)
	drw.add_mask(permamask)
	drw.remove_mask(MASK_APPLY)
	# Now all the transparency is in the layer alpha channel and easy to select
	pdb.gimp_selection_layer_alpha(drw)
	permamask = drw.create_mask(ADD_ALPHA_TRANSFER_MASK)
	drw.add_mask(permamask)
	# Discard mask because the transparency of selected layer should not be affected
	drw.remove_mask(MASK_DISCARD)
	# Merge layers and add a mask from the selection.
	merged_layer = pdb.gimp_image_merge_visible_layers(img, EXPAND_AS_NECESSARY)
	permamask = merged_layer.create_mask(ADD_SELECTION_MASK)
	merged_layer.add_mask(permamask)
	pdb.gimp_selection_none(img)
	img.undo_group_end()

register(
        "merge_visible_with_layer_mask",
        "Merge visible using the active layer transparency to create a mask, visible layers below are discarded",
        "Merge visible using the active layer transparency to create a mask, visible layers below are discarded",
        "Snake-Arsenic",
        "Snake-Arsenic",
        "2008",
        "<Image>/Image/Merge Visible with Layer Mask & Alpha",
        "*",
        [],
        [],
        merge_visible_with_layer_mask)

main()
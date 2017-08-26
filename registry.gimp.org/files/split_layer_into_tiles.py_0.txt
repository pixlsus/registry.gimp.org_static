#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vi:ts=4 sw=4 et

# This script is also available on "GIMP Plugin Registry"
# http://registry.gimp.org/node/25110
#
# Tested on Gimp 2.6.11 with Python 2.6.6

# Thanks to gwidion (Joao S. O. Bueno) from #gimp channel
# for getting me started with Gimp plugins!
#
# See also:
# http://www.gimp.org/docs/python/index.html


from gimpfu import *

def split_layer_into_tiles(img, layer, tile_width, tile_height):
    #print '{0} {1}x{2} @ {3},{4}'.format(
    #    repr(layer),
    #    repr(layer.width), repr(layer.height),
    #    repr(layer.offsets[0]), repr(layer.offsets[1]),
    #)

    pdb.gimp_image_undo_group_start(img)

    x_range = int(layer.width // tile_width)
    y_range = int(layer.height // tile_height)
    x_off, y_off = layer.offsets

    total_tiles = x_range * y_range

    gimp.progress_init('Splitting to {0} layers'.format(total_tiles))
    progress = 0.0
    progress_increment = 1.0 / total_tiles

    for y in range(y_range):
        for x in range(x_range):
            pdb.gimp_rect_select(img,
                x_off + x * tile_width,  # X
                y_off + y * tile_height,  # Y
                tile_width, tile_height,  # Width, Height
                CHANNEL_OP_REPLACE,  # operation
                False, 0  # Feather, Feather radius
            )
            sel = pdb.gimp_selection_float(layer, 0, 0)
            pdb.gimp_floating_sel_to_layer(sel)
            sel.name = '{0},{1} {2}'.format(x, y, layer.name)

            progress += progress_increment
            gimp.progress_update(progress)

    pdb.gimp_image_undo_group_end(img)


# Parameters to register()
#   name
#   blurb
#   help
#   author
#   copyright
#   date
#   menupath
#   imagetypes
#     ""  (if the plugin doesn't need an image)
#     "*" (all image types)
#     "RGB*"
#     "RGB, RGBA"
#     "GRAY, GRAYA"
#     and maybe others
#   params
#     The tuple format is:
#     (type, name, description, default [, extra])
#   results
#   function

register(
    'split_layer_into_tiles',
    'Split a layer into many layers, one for each tile',
    'Split a layer into many layers, one for each tile. This is useful for editing tile-based images for some games and applications.',
    u'Denilson Figueiredo de Sá',
    'Licensed under WTFPL',
    '2011-03-07',
    '<Image>/Layer/Split layer into tiles',
    '*',  # What are the possible image types?
    [
        #(PF_INT, 'tile-width',  'Tile _Width',  16),
        #(PF_INT, 'tile-height', 'Tile _Height', 16),
        (PF_SPINNER, 'tile-width',  'Tile _Width',  16, (2,65536,2)),
        (PF_SPINNER, 'tile-height', 'Tile _Height', 16, (2,65536,2)),
    ],
    [], # Results
    split_layer_into_tiles
)

main()

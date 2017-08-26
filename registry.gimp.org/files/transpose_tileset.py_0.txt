#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vi:ts=4 sw=4 et

# This script is also available on "GIMP Plugin Registry"
# http://registry.gimp.org/node/25112
#
# Tested on Gimp 2.6.11 with Python 2.6.6

# Thanks to people from #gimp channel!
# Specially gwidion and Mikachu


from gimpfu import *

def transpose_tileset(img, layer, tile_width, tile_height, keep_layer):
    #print '{0} {1}x{2} @ {3},{4} keep:{5}'.format(
    #    repr(layer),
    #    repr(layer.width), repr(layer.height),
    #    repr(layer.offsets[0]), repr(layer.offsets[1]),
    #    repr(keep_layer)
    #)

    pdb.gimp_image_undo_group_start(img)

    x_range = int(layer.width // tile_width)
    y_range = int(layer.height // tile_height)
    x_off, y_off = layer.offsets

    total_tiles = x_range * y_range

    # Creating the new layer
    dest = gimp.Layer(
        img,
        layer.name + ' transposed',
        y_range * tile_width,
        x_range * tile_height,
        layer.type,
        layer.opacity,
        NORMAL_MODE
    )
    img.add_layer(dest, -1)  # -1 => insert above current active layer
    dest.set_offsets(x_off, y_off)

    # Making a copy of the old layer
    if keep_layer:
        layer = layer.copy()
        img.add_layer(layer, 0)

    # Initializing the progress bar
    gimp.progress_init('Transposing {0} tiles'.format(total_tiles))
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

            pdb.gimp_edit_named_copy(layer, 'transpose_tileset')
            pasted = pdb.gimp_edit_named_paste(dest, 'transpose_tileset', False)
            pasted.set_offsets(
                x_off + y * tile_width,  # X
                y_off + x * tile_height  # Y
            )
            pdb.gimp_floating_sel_anchor(pasted)
            pdb.gimp_buffer_delete('transpose_tileset')

            progress += progress_increment
            gimp.progress_update(progress)

    # Deleting the old layer
    img.remove_layer(layer)

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
    'transpose_tileset',
    'Transpose all tiles from a tile-based layer',
    'Suppose the current layer has a tile-based image (e.g. the tileset for a game). This script will transpose all tiles, so that the tile at the position (1,0) will end at (0,1).',
    u'Denilson Figueiredo de Sá',
    'Licensed under WTFPL',
    '2011-03-07',
    '<Image>/Layer/Transpose tileset',
    '*',  # What are the possible image types?
    [
        #(PF_INT, 'tile-width',  'Tile _Width',  16),
        #(PF_INT, 'tile-height', 'Tile _Height', 16),
        (PF_SPINNER, 'tile-width',  'Tile _Width',  16, (2,65536,2)),
        (PF_SPINNER, 'tile-height', 'Tile _Height', 16, (2,65536,2)),
        (PF_BOOL, 'keep-layer', '_Keep current layer', 0),
    ],
    [], # Results
    transpose_tileset
)

main()

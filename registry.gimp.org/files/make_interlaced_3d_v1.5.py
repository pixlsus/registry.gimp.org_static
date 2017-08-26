# Make Interlaced 3D v1.5
# (copyLEFT) 2012-2014 Andrey "Efenstor" Pivovarov. No rights reserved
# Copy freely, modify freely, spread the word and remeber me in your prayers

#!/usr/bin/env python

import math
from gimpfu import *

def deinit(img):
    pdb.gimp_undo_push_group_end(img)
    pdb.gimp_progress_end()
    pdb.gimp_displays_flush()

def hipass(img, layer, gbsize):
    copy = layer.copy()
    img.active_layer = layer   # Mind its correct position
    img.add_layer(copy, -1)
    pdb.plug_in_gauss(img, copy, gbsize, gbsize, 0)
    copy.mode = GRAIN_EXTRACT_MODE
    pdb.gimp_image_merge_down(img, copy, 0)

def make_interlaced_3d(img, drawable, source, dwidth, dheight, resize, coff, vertical, antialias, swap, gray, hpsize, eq, flatten):
    # Initialization
    if source==0 and not len(img.layers)==2:
        pdb.gimp_message("Your project must consist of two layers (left and right eye). No more, no less.")
        return
    if source>0 and not len(img.layers)==1:
        pdb.gimp_message("Your project must contain only one layer.")
        return
    gimp.progress_init("Preparing...")
    pdb.gimp_undo_push_group_start(img)

    # Scale
    if resize>0:
        pdb.gimp_context_set_interpolation(2)
        aspect = float(img.width)/float(img.height)
        nwidth = int(dheight*aspect)
        nheight = dheight
        pdb.gimp_image_scale(img, nwidth, nheight)
    else:
        nwidth = img.width
        nheight = img.height

    # De-anaglyph
    if source==1:

        # Make layers
        cyan = img.layers[0]
        cyan.name = "Cyan"
        red = cyan.copy()
        red.name = "Red"
        img.add_layer(red, -1)

        # Filter layers
        pdb.plug_in_colors_channel_mixer(img, cyan, FALSE, 0,0,0, 0,1,0, 0,0,1)
        pdb.plug_in_colors_channel_mixer(img, red, FALSE, 1,0,0, 0,0,0, 0,0,0)

    # Split side-by-side
    if source==2:

        # Make layers
        left = img.layers[0]
        left.name = "Left"
        right = left.copy()
        right.name = "Right"
        img.add_layer(right, -1)

        # Resize and reposition them
        nwidth = img.width/2
        pdb.gimp_layer_resize(left, nwidth, img.height, 0, 0)
        pdb.gimp_layer_resize(right, nwidth, img.height, -nwidth, 0)
        right.translate(-nwidth, 0)

        # Autocrop
        pdb.plug_in_autocrop(img, left)

    # Turn gray
    if gray>0:
        pdb.plug_in_colors_channel_mixer(img, img.layers[0], TRUE, 1,0,0, 0,0,0, 0,0,0)
        pdb.plug_in_colors_channel_mixer(img, img.layers[1], TRUE, 0,.5,.5, 0,0,0, 0,0,0)

    # Hi-pass post-filter
    if gray==2:
        gbsize = (hpsize/100.0)*nheight
        hipass(img, img.layers[0], gbsize)
        hipass(img, img.layers[1], gbsize)

    # Equalize
    if eq==1:
        pdb.plug_in_normalize(img, img.layers[0])
        pdb.plug_in_normalize(img, img.layers[1])

    # Anti-alias
    if antialias>0:
        if vertical==0:
            if antialias==1:
                matrix_up = [0]*25
                matrix_down = [0]*25
                matrix_up[11] = 0.5
                matrix_up[12] = 0.5
                matrix_down[12] = 0.5
                matrix_down[13] = 0.5
            else:
                matrix_up = [0]*25
                matrix_up[11] = 0.333
                matrix_up[12] = 0.333
                matrix_up[13] = 0.333
                matrix_down = matrix_up
        else:
            if antialias==1:
                matrix_up = [0]*25
                matrix_down = [0]*25
                matrix_up[7] = 0.5
                matrix_up[12] = 0.5
                matrix_down[12] = 0.5
                matrix_down[17] = 0.5
            else:
                matrix_up = [0]*25
                matrix_up[7] = 0.333
                matrix_up[12] = 0.333
                matrix_up[17] = 0.333
                matrix_down = matrix_up
        if swap==0: m1 = matrix_down; m2 = matrix_up
        else: m1 = matrix_up; m2 = matrix_down
        pdb.plug_in_convmatrix(img, img.layers[0], 25, m1, 0, 1, 0, 5, (1,1,1,0,0), 0)
        pdb.plug_in_convmatrix(img, img.layers[1], 25, m2, 0, 1, 0, 5, (1,1,1,0,0), 0)

    # Crop/expand
    gimp.set_background(0,0,0)
    if resize<2:
        # "Do not resize" or "Fit to screen height"
        hoff = coff
        nwidth = nwidth+(coff*2)
    else:
        # "Fit to screen height and crop"
        hoff = (dwidth/2)-(nwidth/2)
        nwidth = dwidth
    img.resize(nwidth, nheight, hoff, 0)
    if swap==0:
        img.layers[0].translate(-coff, 0);
        img.layers[1].translate(coff, 0)
    else:
        img.layers[0].translate(coff, 0)
        img.layers[1].translate(-coff, 0)
    pdb.gimp_layer_resize_to_image_size(img.layers[0])
    pdb.gimp_layer_resize_to_image_size(img.layers[1])

    # Erase every other row
    mask = img.layers[0].create_mask(0)
    img.layers[0].add_mask(mask)
    if swap==0: start = 0
    else: start = 1
    if vertical==0:
        for y in range(start, nheight, 2):
            pdb.gimp_rect_select(img, 0, y, nwidth, 1, 2, FALSE, 0)
            pdb.gimp_edit_clear(mask)
    else:
        for x in range(start, nwidth, 2):
            pdb.gimp_rect_select(img, x, 0, 1, nheight, 2, FALSE, 0)
            pdb.gimp_edit_clear(mask)
    pdb.gimp_selection_none(img)

    # Deinitialization
    if flatten: img.flatten()
    deinit(img)

register(
    "make_interlaced_3d",
    "Combines two layers (or a single anaglyph) into one interlaced picture for viewing on 3D-Ready monitors",
    "",
    "Efenstor",
    "(copyLEFT) Andrey \"Efenstor\" Pivovarov",
    "2014",
    "<Image>/Filters/Combine/Make Interlaced 3D...",
    "RGB*, GRAY*",
    [
        (PF_OPTION, "source", "Source type", 0, ("Two Layers", "Anaglyph", "Side-by-side")),
        (PF_INT, "dwidth", "Your screen width", 1920),
        (PF_INT, "dheight", "Your screen height", 1080),
        (PF_OPTION, "resize", "Resizing option", 0, ("Do not resize", "Fit to screen height", "Fit to screen height and crop")),
        (PF_INT, "coff", "Depth correction offset", 0),
        (PF_TOGGLE, "vertical", "Vertical interlace", 0),
        (PF_SPINNER, "antialias" , "Anti-aliasing strength", 1, (0,2,1)),
        (PF_TOGGLE, "swap", "Swap left and right", 0),
        (PF_OPTION, "gray", "Convert to grayscale", 0, ("Do not convert", "Simple", "Better (with hi-pass filter)")),
        (PF_SLIDER, "hpsize", "Hi-pass filter diameter\n(in percent from image height)", 50, (1,100,1)),
        (PF_TOGGLE, "eq", "Post-equalization", 0),
        (PF_TOGGLE, "flatten", "Flatten", 1)
    ],
    [],
    make_interlaced_3d)

main()


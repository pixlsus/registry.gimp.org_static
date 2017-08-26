#!/usr/bin/env python
# -*- coding: utf-8 -*-
# vim: ai ts=4 sts=4 et sw=4
"""
This plugin creates a molten material effect for the current image """

from gimpfu import *
import random


def _apply_lighting_effect(img, layer, bump,
        glowing, bright, shiny, polished):
    """apply the lighting effect for the given layer"""
    pdb.plug_in_lighting(
            img,                    # input image
            layer,                  # input drawable
            bump,                   # bumpmap drawable
            None,                   # environmentmap drawable
            True,                   # enable bumpmapping
            False,                  # enable envmapping
            1,                      # bumpmap type
            0,                      # light type
            (255,255,255),          # lightsource color
            0.5,                    # lightpos x
            0.5,                    # lightpos y
            1,                      # lightpos z
            -1,                     # lightdir x
            -1,                     # lightdir y
            1,                      # lightdir z
            glowing,                # glowing
            bright,                 # bright
            shiny,                  # shiny
            0.10,                   # ????
            polished,               # polished
            True,                   # antialiasing
            False,                  # create new image
            False                   # transparent background
            )
    gimp.displays_flush()


def python_molten_material(timg, drawable, new_image,
        glowing, bright, shiny, polished,
        noise):
    """create an effekt of molten material"""
    width = timg.width
    height = timg.height

    # create a new image?
    if new_image:
        img = timg.duplicate()
        gimp.Display(img)
        gimp.displays_flush()
    else:
        img = timg

    img.undo_group_start()

    # flatten the image first
    img.flatten()

    # new layer: solid noise
    if noise:
        layer_solid_noise = gimp.Layer(img, "solid noise", width, height,
                RGB_IMAGE, 100, BURN_MODE)
        layer_solid_noise.fill(0)
        img.add_layer(layer_solid_noise, 0)
        # set random values for solid noise
        rand_detail = random.randrange(1, 15)
        rand_xy = random.randrange(4,16)
        rand_x = rand_xy + random.randrange(-3, 3)
        rand_y = rand_xy + random.randrange(-3, 3)
        pdb.plug_in_solid_noise(img, layer_solid_noise, 0, 1,
                random.randrange(0, 99999999),
                rand_detail,
                rand_x,
                rand_y)
        pdb.gimp_invert(layer_solid_noise)

        # create a copy, set mode to 'subtract'
        layer_solid_noise_cp = layer_solid_noise.copy()
        layer_solid_noise_cp.mode = SUBTRACT_MODE
        img.add_layer(layer_solid_noise_cp)
        pdb.gimp_drawable_transform_flip_simple(layer_solid_noise_cp,
                0, True, 0 , False)

    # flatten the image
    layer_new_active = img.flatten()
    layer_new_active.name = "flattened image"
    layer_new_active.add_alpha()

    # create 3 copies of the active layer
    layer_mapped_sharp = layer_new_active.copy()
    layer_mapped_blur = layer_new_active.copy()
    layer_sharp = layer_new_active.copy()

    # rename copies
    layer_mapped_sharp.name = "mapped sharp"
    layer_mapped_blur.name = "mapped blur"
    layer_sharp.name = "sharp"
    layer_sharp.visible = 0

    # add copies to image
    img.add_layer(layer_mapped_sharp, 0)
    img.add_layer(layer_mapped_blur, 0)
    img.add_layer(layer_sharp, 0)

    # sharpen layer 'sharp'
    pdb.plug_in_sharpen(img, layer_sharp, 70)

    # duplicate layer 'sharp' and blur it
    layer_blur = layer_sharp.copy()
    layer_blur.name = "blur"
    layer_blur.visible = 0
    img.add_layer(layer_blur)
    pdb.plug_in_gauss(img, layer_blur, 5, 5, 0)

    # apply lighting effects
    _apply_lighting_effect(img, layer_mapped_blur, layer_blur,
            glowing, bright, shiny, polished)
    _apply_lighting_effect(img, layer_mapped_sharp, layer_sharp,
            glowing, bright, shiny, polished)

    # remove temp layers
    img.remove_layer(layer_blur)
    img.remove_layer(layer_sharp)
    img.remove_layer(layer_new_active)

    # set layer 'mapped_blur' active
    img.active_layer = layer_mapped_blur

    img.undo_group_end()


# register the plug-in
register(
        "python_fu_molten_material",
        "Create molten material out of the given image",
        "Create molten material out of the given image",
        "Andi Clemens (TheGrudge)",
        "Andi Clemens (TheGrudge)",
        "2007-2009",
        "<Image>/Filters/Artistic/_Molten Material...",
        "RGB*",
        [
            (PF_BOOL,  "new_image", "Create new image", True),
            (PF_FLOAT, "glowing", "Material ambient intensity (0..1)", 0.13),
            (PF_FLOAT, "bright", "Material diffuse intensity (0..1)", 0.22),
            (PF_FLOAT, "shiny", "Material diffuse reflectivity (0..1)", 0.07),
            (PF_FLOAT, "polished", "Material highlight (0..->)", 10.0),
            (PF_BOOL,  "noise", "Noise", True),
            ],
        [],
        python_molten_material
        )

main()

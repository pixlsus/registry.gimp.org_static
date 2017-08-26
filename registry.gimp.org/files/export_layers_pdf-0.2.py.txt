#!/usr/bin/env python
# -*- coding: <utf-8> -*-
# Author: Ryan Nowakowski <tubaman@fattuba.com>
# Copyright 2011 Ryan NOwakowski
# Most borrowed from Chris's Export as PNG(http://registry.gimp.org/node/18440)
# License: GPL v3+
# Version 0.2
# GIMP plugin to export layers as PDF

import os, re
from subprocess import check_call
from tempfile import mkstemp

from gimpfu import *

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)

def mktmpfile(suffix):
    fd, filename = mkstemp(suffix=suffix)
    return filename
    
def get_layers_to_export(img, only_visible):
    layers = []
    for layer in img.layers:
        if only_visible and layer.visible:
            layers.append(layer)
        if not only_visible:
            layers.append(layer)
    return layers

def combine_imgs_into_pdf(imgfiles, pdfname):
    check_call(['convert'] + imgfiles + [pdfname])

def export_layers(img, drw, pdfname, only_visible=True, flatten=False):
    dupe = img.duplicate()
    savelayers = get_layers_to_export(dupe, only_visible)
    for layer in dupe.layers:
        layer.visible = 0 
    imgfiles = []
    try:
        for layer in dupe.layers:
            if layer in savelayers:
                layer.visible = 1
                fullpath = mktmpfile('.png')
                imgfiles.append(fullpath)
                filename = os.path.basename(fullpath)
                tmp = dupe.duplicate()
                if (flatten):
                    tmp.flatten()
                pdb.file_png_save(tmp, tmp.layers[0], fullpath, filename, 0, 9, 1, 1, 1, 1, 1)
            dupe.remove_layer(layer)
        combine_imgs_into_pdf(imgfiles, pdfname)
    finally:
        for img in imgfiles:
            os.remove(img)
            
register(
    proc_name=("python-fu-export-pdf-layers"),
    blurb=("Export Layers as PDF"),
    help=("Export all layers as a single PDF file."),
    author=("Ryan Nowakowski <tubaman@fattuba.com>"),
    copyright=("Ryan Nowakowski"),
    date=("2011"),
    label=("as _PDF"),
    imagetypes=("*"),
    params=[
        (PF_IMAGE, "img", "Image", None),
        (PF_DRAWABLE, "drw", "Drawable", None),
        (PF_FILENAME, "pdfname", "PDF filename", os.path.join(os.getcwd(), 'output.pdf')),
        (PF_BOOL, "only_visible", "Only Visible Layers?", True),
        (PF_BOOL, "flatten", "Flatten Images?", False),
           ],
    results=[],
    function=(export_layers), 
    menu=("<Image>/File/E_xport Layers"), 
    domain=("gimp20-python", gimp.locale_directory)
    )

main()

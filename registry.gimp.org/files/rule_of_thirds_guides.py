#!/usr/bin/env python
# Author: Chris Mohler
# Copyright 2008 Chris Mohler
# License: GPL v3
# GIMP plugin to add guides for the "rule of thirds"

from gimpfu import *

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)


def add_ROT_guides(img,  drw):
    
    #print img,  drw
    w = pdb.gimp_image_width(img)
    h = pdb.gimp_image_height(img)
    vg1 = pdb.gimp_image_add_vguide(img,  (w/3))
    vg2 = pdb.gimp_image_add_vguide(img,  (2*(w/3)))
    hg1 = pdb.gimp_image_add_hguide(img,  (h/3))
    hg2 = pdb.gimp_image_add_hguide(img,  (2*(h/3)))
    
register(
    "python-fu-add-thirds",
    "Add 'Rule of Thirds' Guides",
    "Add 'Rule of Thirds' Guides.",
    "Chris Mohler",
    "Chris Mohler",
    "2008",
    "<Image>/Image/Guides/Add Rule of _Thirds guides", 
    "",
    [],
    [],
    add_ROT_guides, 
    domain=("gimp20-python", gimp.locale_directory)
    )

main()

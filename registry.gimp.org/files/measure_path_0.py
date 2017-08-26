#!/usr/bin/env python
# Author: Chris Mohler
# Copyright 2009 Chris Mohler
# License: GPL v3
# Version 0.2
# GIMP plugin to measure the length of a path

from gimpfu import *

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)


def measure_path(img, drw):
    try:
	path = pdb.gimp_image_get_active_vectors(img)
	len = pdb.gimp_vectors_stroke_get_length(path, 1, 1)
	len = int(len)
	pdb.gimp_message("Length of " + path.name + ": " + str(len) + " px")
    except:
	pass
    
register(
    proc_name=("python-fu-measure-path"),
    blurb=("Measure Path"),
    help=("Measure Length of the active path.  Output is directed to the Status Bar or Error Console."),
    author=("Chris Mohler"),
    copyright=("Chris Mohler"),
    date=("2009"),
    label=("Active Path"),
    imagetypes=("*"),
    params=[
	(PF_IMAGE, "img", "Image", None),
	(PF_DRAWABLE, "drw", "Drawable", None)
	   ],
    results=[],
    function=(measure_path), 
    menu=("<Image>/Filters/Measure"), 
    domain=("gimp20-python", gimp.locale_directory)
    )

main()

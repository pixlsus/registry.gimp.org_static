#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author: Chris Mohler
# Copyright 2008 Chris Mohler
# License: GPL v3
# GIMP plugin to perform search and replace on all text layers

from gimpfu import *
import re

gettext.install("gimp20-python", gimp.locale_directory, unicode=True)

def find_and_replace(image,  drawable, find,  replace,  all,  case):
    layers = image.layers #get image layers
    if all: count = 0 #replace all
    else: count = 1 #replace only first match
    if not case: find = re.compile(find,  re.I) #case-insentive
    for layer in layers:
        if pdb.gimp_drawable_is_text_layer(layer): #only act on text layer
            haystack = pdb.gimp_text_layer_get_text(layer)  # get texr
            replaced = re.sub(find,  replace,  haystack,  count)  # replace text
            pdb.gimp_text_layer_set_text(layer,  str(replaced)) # set text

register(
    "python-fu-text-find-and-replace",
    "Find and Replace text in all text layers.",
    "Find and Replace text in all text layers.", 
    "Chris Mohler",
    "Chris Mohler",
    "2008",
    "<Image>/Edit/Find & Replace Text...",
    "RGB*, GRAY*",
    [   
        (PF_STRING, "find", "Find:", ""),
        (PF_STRING,  "replace",  "Replace:",  ""), 
        (PF_TOGGLE,  "all", "Replace All?",  True ), 
        (PF_TOGGLE,  "case",  "Case Sensitive?",  True), 
    ],
    [],
    find_and_replace,
    domain=("gimp20-python", gimp.locale_directory))

main()

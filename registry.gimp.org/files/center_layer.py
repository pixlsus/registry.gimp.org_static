#!/usr/bin/env python

# Script that allows to center a layer in the image
# Released under Public Domain by Mariano Simone

# Version 1.0

# Installation : put the template-parameters.py file in your $HOME/.gimp-2.n/plug-ins.
# On Linux and Mac OSX the file must be executable.
# Documentation : http://www.gimp.org/docs/python/index.html

from gimpfu import *
         
def center_layer(image, layer, pf_radio):
  x = layer.offsets[0]
  y = layer.offsets[1]
  if pf_radio == "horizontally" or pf_radio == "both":
   x = (image.width - layer.width) / 2
  if pf_radio == "vertically" or pf_radio == "both":
   y = (image.height - layer.height) / 2
  layer.set_offsets(x, y)

register(
        "center_layer",
        "Layer Centerer for Gimp",
        "Allows to center a layer",
        "Mariano Simone",
        "GPL License",
        "2008",
        "<Image>/Python-Fu/Center Layer",
        "",
        [ 
         # (PF_IMAGE, "image", "Input image", None), #current image
         # (PF_DRAWABLE, "layer", "Input layer", None), #current layer
          (PF_RADIO, "pf_radio", ("Center:"), "both", (("Both","both"), ("Horizontally","horizontally"), ("Verticaly","vertically"))), #both, horizontally or vertically?
        ],
        [],
        center_layer)
main()

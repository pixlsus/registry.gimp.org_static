#!/usr/bin/python

# This Gimp plug-in changes colors in an image based on the HSV difference
# between two colors.  This is particularly useful for applying color themes
# to existing images.

# Version 1.1, no active selection now supported
# Version 1.0, first release 3/6 2008

# It does not currently support INDEXED mode, undo or partial selections

#   Copyright (C) 2008  Amplex A/S (lars@amplex.dk)
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# This plug-in is based on code by Ling Xu and Emanuele Zattin with most
# of the actual code replaced by now. Used with permission.

import math
from gimpfu import *
#from Numeric import *

# Returns the amount of selectedness this pixel has. 0 == not selected,
# 255 = fully selected
def selectedness(drawable, x, y):
    bounds = pdb.gimp_selection_bounds(drawable.image)
    if bounds[0]:
        sel = drawable.image.selection
        if (sel != None):
            tile = sel.get_tile2(False, x, y)
            return ord(tile[x % 64, y % 64])
    return 255

# Get the pixel at x,y anywhere in the drawable.  Slow.
def getpixel(drawable, x, y):
    tile = drawable.get_tile2(False, x, y)
    x_offset = x % 64
    y_offset = y % 64
    pixel = tile[x_offset, y_offset]

    values = []
    for i in range(len(pixel)):
        values.append(ord(pixel[i]))

    if len(values) == 1:
        values.append(255)
    return values

# Put a pixel value into a drawable at x, y. Slow.
def putpixel(drawable, x, y, value):
    tile = drawable.get_tile2(False, x, y)
    x_offset = x % 64
    y_offset = y % 64
    pixel = ""
    for i in range(len(value)):
        pixel += chr(value[i])
    tile[x_offset, y_offset] = pixel

# Convert RGB to HSV
def rgbToHsv(rgb):
    r = rgb[0]
    g = rgb[1]
    b = rgb[2]
    H = 0.0
    minRGB = min(min(r, g), b)
    maxRGB = max(max(r, g), b) * 1.0
    delta = ( maxRGB - minRGB ) * 1.0
    br = maxRGB
    if (maxRGB <> 0.0):
      s = 255.0 * delta / maxRGB
    else:
      s = 0.0
    if (s <> 0.0):
      if r == maxRGB:
        h = (g - b) / delta
      else:
        if g == maxRGB:
          h = 2.0 + (b - r) / delta
        else:
          if b == maxRGB:
            h = 4.0 + (r - g) / delta
    else:
      h = -1.0
    h = h * 60
    if h < 0.0:
      h = h + 360.0
    return [h, s * 100 / 255.0, br * 100 / 255]

# Convert HSV to RGB
def hsvToRgb(hsv):
    h = hsv[0] / 360
    s = hsv[1] / 100
    v = hsv[2] / 100
    if ((s - 0.0) < 0.000):
        return [int(v * 255), int(v * 255), int(v * 255)]
    else:
        var_h = h * 6
        var_i = math.floor(var_h)
        var_1 = v * (1 - s)
        var_2 = v * (1 - s * (var_h - var_i))
        var_3 = v * (1 - s * (1 - (var_h - var_i)))
        
        if (var_i == 0):
            var_r = v
            var_g = var_3
            var_b = var_1
        else:
            if (var_i == 1):
                var_r = var_2
                var_g = v
                var_b = var_1
            else:
                if (var_i == 2):
                    var_r = var_1
                    var_g = v
                    var_b = var_3
                else:
                    if (var_i == 3):
                        var_r = var_1
                        var_g = var_2
                        var_b = v
                    else:
                        if (var_i == 4):
                            var_r = var_3
                            var_g = var_1
                            var_b = v
                        else:
                            var_r = v
                            var_g = var_1
                            var_b = var_2
		
    return [int(var_r * 255), int(var_g * 255), int(var_b * 255)]

def color_remap(image, drawable, source, target):
    image.disable_undo()
    layer = image.active_layer
    height = layer.height
    width = layer.width

    gimp.tile_cache_ntiles(2 * (width + 63) / 64)

    gimp.progress_init("Mapping colors...")
    sourceHsv = rgbToHsv(source)
    targetHsv = rgbToHsv(target)
    hDiff = sourceHsv[0] - targetHsv[0]
    if (sourceHsv[1] > 0):
        sDiff = targetHsv[1] / sourceHsv[1]
        if (targetHsv[1] > sourceHsv[1]):
            sDiff2 = (100 - targetHsv[1]) / (100 - sourceHsv[1])
    else:
        sDiff = -1
    if (sourceHsv[2] > 0):
        vDiff = targetHsv[2] / sourceHsv[2]
        if (targetHsv[2] > sourceHsv[2]):
            vDiff2 = (100 - targetHsv[2]) / (100 - sourceHsv[2])
    else:
        vDiff = -1
    for i in range(width):
        gimp.progress_update( (1.0 * i) / width);
        for j in range(height):
            sel = selectedness(layer, i, j)
            if (sel == 0):
                continue
            pixel = getpixel(layer, i, j)
            hsv = rgbToHsv(pixel)
            hsv[0] = (hsv[0] - hDiff) % 360
            if (sDiff != -1):
                if ((sDiff > 1) & (hsv[1] > sourceHsv[1])):
                    hsv[1] = (100 - (100 - hsv[1]) * sDiff2)
                else:
                    hsv[1] = hsv[1] * sDiff
            if (vDiff != -1):
                if ((vDiff > 1) & (hsv[2] > sourceHsv[2])):
                    hsv[2] = (100 - (100 - hsv[2]) * vDiff2)
                else:
                    hsv[2] = hsv[2] * vDiff
            finalPixel = hsvToRgb(hsv)
            if (len(pixel) > 3):
                finalPixel.append(pixel[3])
            putpixel(layer, i, j, finalPixel)
    layer.update(0, 0, width, height)
    image.enable_undo()

register(
    "RelativeColorRemap",
    "Remap colors in this layer relative to a given color",
    "Given a source and target color, transform all colors proportionally to the difference between the two in HSV space",
    "Lars Clausen",
    "Lars Clausen, based on code from Ling Xu, Emanuele Zattin",
    "2008",
    "<Image>/Colors/Map/Relative",
    "RGB*, GRAY*",
    [
        (PF_COLOUR, "source", "Source colour", (128,128,128)),
        (PF_COLOUR, "target", "Target colour", (255,128,0))
    ],
    [],
    color_remap)

main()

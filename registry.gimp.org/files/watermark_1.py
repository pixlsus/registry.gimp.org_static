#!/usr/bin/env python

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Dave Lichterman
# 2009/11/08
# laviddichterman [AT] gmail [D0T] com
#
# Change History
# 2009/11/08 - Fix menu path. Attempt to handle non-image files
#   with better messaging.
# 2009/11/07 - Tweaked watermark scaling formula.
# 2009/10/10 - Original version

import os  

from gimpfu import *

RATIO = .015

def determine_padding(width, height, padding):
    longerdim = max(width, height)
    return float(padding * longerdim)

def watermark_image(image, layer):
    # add the layer 
    image.add_layer(layer)
    # scale
    # TODO: this could probably use some tweaking still.
    watermark_area = int(RATIO * image.width * image.height)
    layer.scale(watermark_area / layer.height, watermark_area / layer.width)
    # place/translate: determine the padding and new location
    padding = determine_padding(image.width, image.height, .01)
    new_xloc = int(image.width - layer.width - padding)
    new_yloc = int(image.height - layer.height - padding)
    layer.translate(new_xloc, new_yloc)
    # set difference mode on the layer
    layer.mode = DIFFERENCE_MODE
    return image

def save_watermark(image, format):
    filename = image.filename
    (directory, basename) = os.path.split(filename)
    (shortname, extension) = os.path.splitext(basename)
    new_directory = os.path.join(directory, "WM")
    new_filename = os.path.join(new_directory, shortname + "." + format)
    if not os.path.isdir(new_directory):
        os.mkdir(new_directory)
    drawable = image.flatten()
    pdb.gimp_file_save(image, drawable, new_filename, new_filename)


def pywatermark(path, watermark_file, output_format):
    if os.path.isdir(path):
        files=os.listdir(path)
        files = map(lambda x: os.path.join(path, x), files)
    else:
        files=[path]
    # loop through the images
    for img in files:
        if os.path.isdir(img):
            continue
        try:
            # load file
            image = None
            try:
                image =  pdb.gimp_file_load(img, img)
            except:
                print "skipping %s since it doesn't seem to be an image" % img
                continue
            print "processing %s" % img
            layer = pdb.gimp_file_load_layer(image, watermark_file)
            # apply the watermark
            watermark_image(image, layer)
            save_watermark(image, output_format)
            print "...[OK]"
        except RuntimeError, err:
            print "...[FAIL]"

register(
    "python_fu_watermarker",
    "Add a watermark to an image",
    "Help Computer",
    "Dave Lichterman",
    "GPL3",
    "2009.11.08",
    "<Image>/Filters/Batch/_WATERMARK",
    "",
    [
        (PF_FILE, "image_file", "the file to add the watermark to", None),
        (PF_FILE, "watermark_file", "the cropped watermark file", None),
        (PF_STRING, "output_format", "the extension to save the watermarked file", "jpg"),
    ],
    [],
    pywatermark)

main()

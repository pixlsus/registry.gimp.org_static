#!/usr/bin/env python

# *************************************************************************** #
#                                                                             #
#      PyTileLayers - tiles the layers of an image                            #
#      Version 0.4 - 2007-06-16                                               #
#                                                                             #
#      Copyright (C) 2007 Elena Grandi                                        #
#                                                                             #
#      This program is free software; you can redistribute it and/or          #
#      modify it under the terms of the GNU General Public License            #
#      as published by the Free Software Foundation; either version 2         #
#      of the License, or (at your option) any later version.                 #
#                                                                             #
#      This program is distributed in the hope that it will be useful,        #
#      but WITHOUT ANY WARRANTY; without even the implied warranty of         #
#      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          #
#      GNU General Public License for more details.                           #
#                                                                             #
#      You should have received a copy of the GNU General Public License      #
#      along with this program; if not, write to the                          #
#      Free Software Foundation, Inc.,                                        #
#      51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA           #
#                                                                             #
# *************************************************************************** #

from gimpfu import *
import os.path # needed to manipulate the filename

def tile_char(cur_img,cur_layer,x_size,y_size,cols,background,new,flatten):
  
  # input values sanity check
  if ( x_size < 1 ) or ( y_size < 1 ):
    pdb.gimp_message("Invalid image size")
    return False
  
  # work on current image or create a new one?
  if new == True:
    img=pdb.gimp_image_duplicate(cur_img)
    basename, ext = os.path.splitext(cur_img.filename)
    # FIXME: use a sensible text for the new filename
    img.filename=basename+"-tiled.xcf"
    gimp.Display(img)
  else:
    img=cur_img

  # from here to img.undo_group_end() it's all script-intertal stuff 
  # that will be undoed with a single undo
  img.undo_group_start()

  # Calculate target sizes
  tiles=len(img.layers)
  # this is tiles/cols + 1, unless (tiles % cols) == 0, then it is tiles/cols
  rows=(tiles+cols-1)/cols
  width=cols*x_size
  height=rows*y_size

  # resize the canvas
  img.resize(width,height,0,0)

  # move the layers around
  for i in xrange(tiles):
    pos_x=i%cols
    pos_y=i//cols
    img.layers[i].translate(pos_x*x_size,pos_y*y_size)

  # add a background
  bkg_layer=gimp.Layer(img,"Background",width,height,RGB_IMAGE,100,NORMAL_MODE)
  img.add_layer(bkg_layer,tiles)
  oldbck=gimp.get_background()
  gimp.set_background(background)
  pdb.gimp_edit_fill(bkg_layer,BACKGROUND_FILL)
  gimp.set_background(oldbck)

  # if requested, flatten the image
  if flatten == True:
    img.flatten()

  # end of script-internal stuff 
  img.undo_group_end()
  
register(
  "py_tile_layers",
  "Tile the layers of an image",
  "Tile the layers of an image",
  'Elena of Valhalla',
  'Elena of Valhalla',
  "2007-06-16",
  "<Image>/Python-Fu/Pixel Art/Tile Layers...",
  "*",
  [
    (PF_INT, "x_size", "Layer width", 32),
    (PF_INT, "y_size", "Layer height", 32),
    (PF_SPINNER, "cols", "Columns", 3,(1,255,1)),
    (PF_COLOUR, "background", "Background", (255, 255, 255)),
    (PF_TOGGLE, "new", "New Image", False),
    (PF_TOGGLE, "flatten", "Flatten result", False)
  ],
  [],
  tile_char)

main()

#!/usr/bin/env python

# *************************************************************************** #
#                                                                             #
#      PyExtractLayers - Extract every layers of an image                     #
#      Version 0.1 - 2007-07-17                                               #
#      This script adds every single layer to the background.                 #
#      Every layer matched wiht the background is saved by the name of the    #
#      layer in a directory called with the name of the image.                #
#                                                                             #
#      Copyright (C) 2007 Marco Crippa                                        #
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
import os # needed to manipulate the filename

global slash
sist_op=os.name
if sist_op=="nt":
    slash="\\"
else:
    slash="/"

def extract_layer(img, drawable,folder,extension):

    path=os.path.dirname(folder)+slash
    name_img=pdb.gimp_image_get_name(img)
    name_img=name_img[:-4]
    dir_extraction=path+name_img+slash
    
    
    if os.path.exists(dir_extraction)!=1:
        os.mkdir(dir_extraction)
    else:
        pass

    list_layer=img.layers

    H=pdb.gimp_image_height(img)
    W=pdb.gimp_image_width(img)
    
    num_layer=len(list_layer)


    for ind in range (0,num_layer-1):
        
        # disable undo for the image

        new_image=pdb.gimp_image_new(W,H,0)

        img.disable_undo()
        new_image.disable_undo()

        # create the layer base from the first layer in stock
        layer_base = pdb.gimp_layer_new_from_drawable(list_layer[num_layer-1],new_image)
        new_image.add_layer(layer_base,1)

        # create the content layer
        pdb.gimp_drawable_set_visible(list_layer[ind],1)
        layer_new = pdb.gimp_layer_new_from_drawable(list_layer[ind],new_image)
        nome=pdb.gimp_drawable_get_name(list_layer[ind])

        new_image.add_layer(layer_new,0)
        img_total=pdb.gimp_image_merge_visible_layers(new_image,1)
        
        nome_file=dir_extraction+nome+"."+extension

        # save the new file
        pdb.gimp_file_save(new_image,img_total,nome_file,nome_file)    
        
        # enable undo again
        img.enable_undo()
        new_image.enable_undo()


register (
    "py_extract_layers",
    "Extract every layers of an image",
    "Extract every layers of an image",
    "Marco Crippa",
    "Marco Crippa",
    "2007",
    "<Image>/Python-Fu/Layer/Extract Layers...",
    "",
    [
    (PF_FILE, "folder", "Path to store new image\n(filename ignored!)", ""),    
    (PF_STRING, "extension","Extension name\nWithout '.'", "png"),
    ],
    [],
    extract_layer)

main ()



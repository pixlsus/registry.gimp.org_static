#!/usr/bin/env python

###############################

    #Collect layers 
    #python gimp plugin
    #2008, Elmar Hoefner
    #Licensed under GPLv3
    #see www.fsf.org for details
    
    # This plugin collects all layers from open images
    # all images should have the same size!

###############################

# Collect layers 

from gimpfu import *

def collect_layers():
	"""Collect active layers from open files"""
	layerlist=[]
	for image in gimp.image_list():
		for layer in image.layers:
			layerlist.append(layer)	
	width=0
	height=0
	
	for layer in layerlist:
		if width< layer.width:
			width= layer.width
		if height < layer.height:
			height = layer.height
		
	new_image=gimp.Image(width,height,RGB)
	
	for layer in layerlist:	
		pdb.gimp_edit_copy(layer)
		layername="Layer"+str(layerlist.index(layer))
		new_layer=gimp.Layer(new_image,layername,width,height)
		new_image.add_layer(new_layer)
		
		# paste to new_image
		floating_sel=pdb.gimp_edit_paste(new_layer, False)
		pdb.gimp_floating_sel_anchor(floating_sel)
		#pdb.gimp_floating_sel_to_layer(floating_sel)
	disp = gimp.Display(new_image)
	del layerlist, new_image, width, height
	
register(
        "collect_layers",
        "Collects layers from open files and pastes them into new file",
        "Collects layers from open files and pastes them into new file",
        "Elmar W. Hoefner",
	"Licensed und GPLv3",
        "May 2008",
       	"<Toolbox>/Xtns/Python-Fu/Layer/Collect layers...",
        "*",
	[],
        [],
        collect_layers)

main()
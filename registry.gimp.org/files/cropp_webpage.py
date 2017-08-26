#!/usr/bin/python
import os
from gimpfu import *

def cropp_webpage(timg, tdrawable, path, action):
	newsize = 1
	# ------------------ Cropp a webpage automatically --------------------------------
	# ---------------------------------------- by Bastian Schnitzler (in 2009) --------
	# ---------------- used to cropp the parkour aachen Homepage ----------------------
	# --- look for sample files on http://freshflesh.de/files/croppparkour.zip --------
	# ---------------------------------------------------------------------------------
	# --- Usage: ----------------------------------------------------------------------
	# ------------------------ Modify this list to your needs! ------------------------
	# ---	key: name of the png image without extension. (list in tuple: list of   ---
	# --- layers that should be included in order as in the image,(left coordinate, ---
	# --- top coord., width, height of the defined area to cut out of the image ))	---
	# ---------------------------------------------------------------------------------

	images={
		"logintab":(["logintab","background"],(92,60,766,102)),
		"loginfield":(["passfield","passfieldgleam","logintab"],(471,78,5,64)),
		#"headeredge":(["header rounded edge","background"],(1883,162,41,41)),
		"home":(["HOME"],(170,619,192,71)),
		"board":(["BOARD"],(492,618,230,71)),
		"gbook":(["GBOOK"],(1125,618,239,72)),
		"about":(["ABOUT"],(1454,618,231,72)),
		#"menuseperator":(["homeseperator"],(423,618,5,81)),
		"menubackground":(["menu background gradient1"],(154,571,5,153)),


		"arrowleft":(["pfeillinks","subheader"],(1142,917,30,72)),
		"arrowright":(["pfeilrechts","subheader"],(1796,921,30,72)),

		"sessiontop":(["Sessions","sessiontab-Kopie","sessiontabborder","subheader"],(114,736,952,122)),

		"contentheader":(["bodybackground","contenttableft","contenttabborderleft","contentwrapper"],(113,1192,1293,123)),
		"contentfooter":(["bodybackground","contentwrapper"],(114,2733,1292,73)),

		"videos":(["videos","contenttableft"],(210,1223,253,68)),

		"footerbackground":(["footertest","footerbar","footershade","background"],(92,2960,1833,98))
	}
	#print timg.layers 
	
	#loop overall images we want to create
	processedImages=0

	for piece in images.keys():
		layers=images[piece][0]	
		width=images[piece][1][2]
		height=images[piece][1][3]
		top=(-images[piece][1][0])
		left=(-images[piece][1][1])
		name=piece + ".png"
		fullpath=os.path.join(path,name)

		if action==3:
			print "Starting to construct image " + piece

		img=timg.duplicate()
		#loop over all layers we want to add
		layersused=0
		for layer in img.layers:
			#layer is in current list, must be visible
			if layer.name in layers:
				layer.visible = 1
				layersused = layersused + 1
			#layer is not in current list, will be removed
			else:
				layer.visible = 0
				img.remove_layer(layer)
		
		# if no layers are made visible in this picture just continue with the next one

		if layersused == 0:

			if action == 3:
				print "      Warning: No layers used for image " + piece + " , skipping to the next image to construct!"

			continue

		elif action == 3:
			print "      Used " + str(layersused) + " layers for image " + piece

		

		img.resize(width,height,top,left)	
		#merge visible layers KEEPING background transparency
		pdb.gimp_image_merge_visible_layers(img, CLIP_TO_IMAGE)
		merged_layer=img.layers[0]
		newwidth=round(merged_layer.width*newsize)
		newheight=round(merged_layer.height*newsize)
		merged_layer.scale(int(newwidth),int(newheight))
		img.resize(int(round(width*newsize)),int(round(height*newsize)),0,0)
		if action==0:
			gimp.Display(img)
		elif action==1:
			pdb.file_png_save(img, img.layers[0], fullpath, name, 0, 9, 1, 1, 1, 1, 1)
		elif action==2:
			pdb.file_png_save(img, img.layers[0], fullpath, name, 0, 9, 1, 1, 1, 1, 1)
			gimp.Display(pdb.file_png_load(fullpath,name))

		processedImages += 1


	if action!=0 and action!=3:
		gimp.message("%i images saved in %s" % (processedImages,path))
	elif action==3:
		print "Finished by constructing %i images" % processedImages

#	Delete image temp file
	gimp.delete(img)

register(
	proc_name=("python-fu-cropp"),
	blurb=("Cropps a webpage by combining several layers"),
	help=("Enter a python dictionary to define the layers you want saved and enter the directory for the files"),
	author=("Bastian Schnitzler"),
	copyright=("Bastian Schnitzler"),
	date=("2009"),
	menu=("<Image>/Python-Fu"), 
	label=("Cropp"),
	imagetypes=("*"),
	params=[
	(PF_IMAGE, "img", "Image", None),
	(PF_DRAWABLE, "drw", "Drawable", None),
#	(PF_TEXT, "testbox", "Ficken junge", "asdf asdf asdf \n asdfasdfasdf"),
	(PF_DIRNAME, "path", "Save PNGs here", os.getcwd()),
	(PF_OPTION, "action", "What to do with the images", 0, ("Open images in gimp", "Save images in folder set above", "Save images and open them afterwards in gimp","Just test the process with verbose output (if no erros occur everything might be fine)"))
	],
	results=[],
	function=(cropp_webpage), 
)

main()

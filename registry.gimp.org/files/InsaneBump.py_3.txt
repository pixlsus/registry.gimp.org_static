#! /usr/bin/env python

from gimpfu import *
import commands
import glob
import os
import string
import subprocess

def getPrevPath():
	path = '"C:\Program Files\Insane Bump'
	return path

def getImgPath():
	path = ''
	return path

def getFilename(filename, suffix):
	filename = filename.replace(".", suffix + ".")
	return filename

def saveLastOperation(image, filename):
	layer = pdb.gimp_image_get_active_layer(image)
	pdb.gimp_file_save(image, layer, filename, filename)
		
def specularEdge(image, file_name, defin):
	drawable = pdb.gimp_image_get_active_layer(image)
	newlayer = pdb.gimp_layer_copy (drawable, 1)
	pdb.gimp_image_add_layer(image, newlayer, -1)
	pdb.gimp_image_set_active_layer(image, newlayer)
	pdb.gimp_desaturate(newlayer)

	drawable = pdb.gimp_image_get_active_layer(image)
	newlayer = pdb.gimp_layer_copy (drawable, 1)
	pdb.gimp_image_add_layer(image, newlayer, -1)
	pdb.gimp_image_set_active_layer(image, newlayer)
	pdb.gimp_desaturate(newlayer)
	pdb.plug_in_dog(image, newlayer, 1.0, 8.0, 1, 0)
	#pdb.plug_in_vinvert(image,newlayer)
	
	pdb.gimp_layer_set_mode(newlayer, 7)
	pdb.gimp_image_merge_down(image, newlayer, 0)
	
	drawable = pdb.gimp_image_get_active_layer(image)
	pdb.gimp_brightness_contrast(drawable, 0, 64)
	pdb.gimp_levels(drawable, 0, defin, 255, 1, 0, 255)
	pdb.gimp_file_save(image, drawable, getFilename(file_name, "_s"), getFilename(file_name, "_s"))
	pdb.gimp_edit_clear(drawable)
	

def specularSmooth(image, file_name, defin):
	drawable = pdb.gimp_image_get_active_layer(image)
	newlayer = pdb.gimp_layer_copy (drawable, 1)
	pdb.gimp_image_add_layer(image, newlayer, -1)
	pdb.gimp_image_set_active_layer(image, newlayer)
	pdb.gimp_desaturate(newlayer)

	drawable = pdb.gimp_image_get_active_layer(image)
	newlayer = pdb.gimp_layer_copy (drawable, 1)
	pdb.gimp_image_add_layer(image, newlayer, -1)
	pdb.gimp_image_set_active_layer(image, newlayer)
	
	pdb.gimp_desaturate(newlayer)
	pdb.plug_in_dog(image, newlayer, 8.0, 1.0, 1, 0)
	#pdb.plug_in_vinvert(image,newlayer)
	
	pdb.gimp_layer_set_mode(newlayer, 7)
	pdb.gimp_image_merge_down(image, newlayer, 0)
	
	drawable = pdb.gimp_image_get_active_layer(image)
	pdb.gimp_brightness_contrast(drawable, 0, 64)
	pdb.gimp_levels(drawable, 0, defin, 255, 1, 0, 255)
	pdb.gimp_image_raise_layer_to_top(image, drawable)
	pdb.gimp_file_save(image, drawable, getFilename(file_name, "_s"), getFilename(file_name, "_s"))
	pdb.gimp_edit_clear(drawable)

def removeShading(image):
	drawable = pdb.gimp_image_get_active_layer(image)
	newlayer = pdb.gimp_layer_copy (drawable, 1)
	pdb.gimp_image_add_layer(image, newlayer, -1)
	pdb.gimp_image_set_active_layer(image, newlayer)
	pdb.plug_in_gauss(image, newlayer, 20.0, 20.0, 0)
	pdb.plug_in_vinvert(image, newlayer)
	pdb.gimp_layer_set_mode(newlayer, 5)
	pdb.gimp_image_merge_visible_layers(image, 0)

def blur(image, diffuse, width, height, passes, normal):
	desatdiffuse = pdb.gimp_layer_copy (diffuse, 1)
	pdb.gimp_image_add_layer(image, desatdiffuse, -1)
	pdb.gimp_image_set_active_layer(image, desatdiffuse)
	if normal == 0:
		pdb.gimp_desaturate(desatdiffuse)

	for i in range(passes):
		drawable = pdb.gimp_image_get_active_layer(image)
		newlayer = pdb.gimp_layer_copy (drawable, 1)
		pdb.gimp_image_add_layer(image, newlayer, -1)
		pdb.gimp_image_set_active_layer(image, newlayer)
		pdb.plug_in_gauss(image, newlayer, width * 0.05, height * 0.05, 0)
		pdb.gimp_layer_set_mode(newlayer, 5)
		pdb.gimp_image_merge_down(image, newlayer, 0)
		drawable = pdb.gimp_image_get_active_layer(image)
	if normal == 1:
		pdb.plug_in_normalmap(image, drawable, 0, 0.0, 1.0, 0, 0, 0, 8, 0, 0, 0, 0, 0.0, drawable)
		pdb.gimp_layer_set_mode(drawable, 5)
		pdb.gimp_image_merge_down(image, drawable, 0)
				
	

def sharpen(image, diffuse, depth, filterSize, strength):
	sharpnormal = pdb.gimp_layer_copy (diffuse, 1)
	pdb.gimp_image_add_layer(image, sharpnormal, -1)
	pdb.gimp_image_set_active_layer(image, sharpnormal)
	
	pdb.plug_in_normalmap(image, sharpnormal, 0, 0.0, depth, filterSize, 0, 0, 0, 0, 0, 1, 0, 0.0, sharpnormal)
	#pdb.gimp_levels_stretch(sharpnormal)
	pdb.gimp_image_set_active_layer(image, sharpnormal)
	pdb.gimp_layer_set_mode(sharpnormal, 5)
	#pdb.gimp_file_save(image, sharpnormal, getFilename(file_name,"_hn"), getFilename(file_name,"_hn"))
	pdb.gimp_layer_set_opacity(sharpnormal, strength)
	pdb.gimp_image_raise_layer_to_top(image, sharpnormal)

def shapeRecognise(image, normalmap, strength):
	blurnormal = pdb.gimp_layer_copy (normalmap, 1)
	pdb.gimp_image_add_layer(image, blurnormal, -1)
	pdb.gimp_image_set_active_layer(image, blurnormal)
	pdb.plug_in_gauss(image, blurnormal, 20, 20, 0)
	pdb.plug_in_colors_channel_mixer(image, blurnormal, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, -200.0)
	pdb.plug_in_normalmap(image, blurnormal, 0, 0.0, 1.0, 0, 0, 0, 8, 0, 0, 0, 0, 0.0, blurnormal)
	pdb.plug_in_colors_channel_mixer(image, blurnormal, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, -200.0)
	pdb.gimp_layer_set_mode(blurnormal, 5)
	pdb.gimp_layer_set_opacity(blurnormal, strength)
	
def analyze(layer):
	highlights = pdb.gimp_histogram(layer, 0, 179, 256)
	midtones = pdb.gimp_histogram(layer, 0, 77, 178)
	shadows = pdb.gimp_histogram(layer, 0, 0, 76)
	f = open("/tmp/aidata.txt", "w")
	f.write(str(int(shadows[0])) + " " + str(int(shadows[1])) + " " + str(int(shadows[5] * 100)) + " ")
	f.write(str(int(midtones[0])) + " " + str(int(midtones[1])) + " " + str(int(midtones[5] * 100)) + " ")
	f.write(str(int(highlights[0])) + " " + str(int(highlights[1])) + " " + str(int(highlights[5] * 100)) + " ")

def doBaseMap(image, diffuse, Depth, passes):
	for i in range(passes):
		newlayer = pdb.gimp_layer_copy (diffuse, 1)
		pdb.gimp_image_add_layer(image, newlayer, -1)
		pdb.gimp_image_set_active_layer(image, newlayer)
		ok = float(i + 1) * (i + 1)
		pdb.plug_in_gauss(image, newlayer, ok * 3, ok * 3, 0)
		pdb.plug_in_normalmap(image, newlayer, 5, 0.0, Depth * ok, 0, 0, 0, 0, 0, 0, 1, 0, 0.0, newlayer)
		
		if i > 0:
			#pdb.gimp_layer_set_opacity(newlayer,50-(i*10))
			pdb.gimp_layer_set_mode(newlayer, 5)
			pdb.gimp_image_merge_down(image, newlayer, 0)
			newlayer = pdb.gimp_image_get_active_layer(image)
			pdb.plug_in_normalmap(image, newlayer, 0, 0.0, 1.0, 0, 0, 0, 8, 0, 0, 0, 0, 0.0, newlayer)
	drawable = pdb.gimp_image_get_active_layer(image)
	#newlayer=pdb.gimp_layer_copy (drawable, 1)
	#pdb.gimp_image_add_layer(image,newlayer,-1)
	#pdb.gimp_image_set_active_layer(image,newlayer)
	#pdb.gimp_levels_stretch(newlayer)
	#pdb.gimp_layer_set_opacity(newlayer,100)
	#pdb.gimp_layer_set_mode(newlayer,5)
	#pdb.gimp_image_merge_down(image,newlayer,0)		 
		


def batchnr(img, layer, RemoveLighting, Reszie, Tile, newWidth, EdgeSpecular, defSpecular, Depth, LargeDetails, MediumDetails, SmallDetails, ShapeRecog, smoothstep, invh, ao, prev):

	file_name = pdb.gimp_image_get_filename(img)
	image = img
	size = pdb.gimp_image_width(image)
	

	if(Reszie):
		#upScale(getImgPath(),file_name,newWidth,size)
		hiresimg = pdb.gimp_image_resize(image, newWidth * size, newWidth * size, 0, 0)
		#hiresimg = pdb.gimp_file_load( getFilename(file_name,"_hd"), file_name)
		drawable = pdb.gimp_image_get_active_layer(hiresimg)
		noiselayer = pdb.gimp_layer_copy (drawable, 1)
		pdb.gimp_image_add_layer(hiresimg, noiselayer, -1)
		pdb.gimp_image_set_active_layer(hiresimg, noiselayer)
		pdb.plug_in_rgb_noise(hiresimg, noiselayer, 1, 1, 0.20, 0.20, 0.20, 0)
		#pdb.plug_in_blur(hiresimg,noiselayer)
		pdb.gimp_layer_set_mode(noiselayer, 14)
		pdb.gimp_image_merge_down(hiresimg, noiselayer, 0)
		image = hiresimg
		drawable = pdb.gimp_image_get_active_layer(image)
		###########################analyze(drawable)
	if(RemoveLighting):
		removeShading(image)
		
	diffuse = pdb.gimp_image_get_active_layer(image)
	pdb.gimp_levels_stretch(diffuse)
	if(Tile):
		pdb.plug_in_make_seamless(image, diffuse)
	pdb.gimp_file_save(image, diffuse, getFilename(file_name, "_d"), getFilename(file_name, "_d"))
	
	#drawable = pdb.gimp_image_get_active_layer(image)
	#pdb.gimp_levels(drawable,0,64,128,1,0,255)
	wsize = pdb.gimp_image_width(image)
	hsize = pdb.gimp_image_width(image)
	blur(image, diffuse, wsize, hsize, LargeDetails, 0)

	normalmap = pdb.gimp_image_get_active_layer(image)
	if(smoothstep):
		pdb.plug_in_blur(image, normalmap)
	if(invh):
		pdb.plug_in_vinvert(image, normalmap)
		

	pdb.gimp_file_save(image, normalmap, getFilename(file_name, "_h"), getFilename(file_name, "_h"))
	

	#lfnormal=pdb.gimp_layer_copy (diffuse, 1)
	#pdb.gimp_image_add_layer(image,lfnormal,-1)
	#pdb.gimp_image_raise_layer_to_top(image,lfnormal)
	#pdb.plug_in_normalmap(image, lfnormal,8,0.0,Depth,0,0,0,0,0,0,0,0,0.0,normalmap)
	#blur(image,lfnormal,LargeDetails,1)
	#normalmap = pdb.gimp_image_get_active_layer(image)
	doBaseMap(image, diffuse, Depth, LargeDetails)
	normalmap = pdb.gimp_image_get_active_layer(image)
	pdb.gimp_file_save(image, normalmap, getFilename(file_name, "_ln"), getFilename(file_name, "_ln"))


	shapeRecognise(image, normalmap, ShapeRecog)
	if(smoothstep):
		normalmap = pdb.gimp_image_get_active_layer(image)
		pdb.plug_in_blur(image, normalmap)

	saveLastOperation(image, getFilename(file_name, "_sn"))
	
	sharpen(image, diffuse, Depth, 0, SmallDetails)
	normalmap = pdb.gimp_image_get_active_layer(image)
	pdb.plug_in_sharpen(image, normalmap, 20)
	saveLastOperation(image, getFilename(file_name, "_hn"))
					
	sharpen(image, diffuse, Depth, 6, MediumDetails)
	normalmap = pdb.gimp_image_get_active_layer(image)
	pdb.plug_in_blur(image, normalmap)
	saveLastOperation(image, getFilename(file_name, "_mn"))
	
	pdb.gimp_drawable_set_visible(diffuse, 0)
	pdb.gimp_image_merge_visible_layers(image, 0)
	
	drawable = pdb.gimp_image_get_active_layer(image)
	pdb.plug_in_normalmap(image, drawable, 0, 0.0, 1.0, 0, 0, 0, 8, 0, 0, 0, 0, 0.0, drawable)
	pdb.gimp_file_save(image, drawable, getFilename(file_name, "_n"), getFilename(file_name, "_n"))
	pdb.plug_in_colors_channel_mixer(image, drawable, 0.0, -200.0, 0.0, 0.0, 0.0, -200.0, 0.0, 0.0, 0.0, 1.0)
	pdb.gimp_desaturate(drawable)
	pdb.gimp_levels_stretch(drawable)
	pdb.gimp_file_save(image, drawable, getFilename(file_name, "_a"), getFilename(file_name, "_a"))
	pdb.gimp_drawable_set_visible(diffuse, 1)
	pdb.gimp_image_set_active_layer(image, diffuse)
	
	if(EdgeSpecular):		
		specularEdge(image, file_name, defSpecular)

	if EdgeSpecular == False:
		specularSmooth(image, file_name, defSpecular)



register(
        "InsaneBump", "", "", "", "", "",
        "<Image>/Filters/Map/Insane...", "",
        [
        (PF_BOOL, "RemoveLighting", "Remove Lighting", FALSE),
        (PF_BOOL, "Reszie", "Upscale(HD)", FALSE),
        (PF_BOOL, "Tile", "Tile", TRUE),
        (PF_INT32, "newWidth", "New Width(Integer Times larger)", 2),
        (PF_BOOL, "EdgeSpecular", "Edge Enhaning Specular", TRUE),
        (PF_INT32, "defSpecular", "Specular Definition(0-255)", 64),
        (PF_FLOAT, "Depth", "Depth(+/-)", 20),
        (PF_INT32, "LargeDetails", "Large Detail Size", 3),
        (PF_INT32, "MediumDetails", "Medium Detail Intensity(%)", 50),
        (PF_INT32, "SmallDetails", "Small Detail Intensity(%)", 50),
        (PF_INT32, "ShapeRecog", "Shape Recognition(%)", 50),
        (PF_BOOL, "smoothstep", "Smooth Step", TRUE),
        (PF_BOOL, "invh", "Invert Height Map", False),
        (PF_INT32, "ao", "ao", 50),
        (PF_BOOL, "prev", "Preview", TRUE)
        ],
        [],
        batchnr
        )

main()


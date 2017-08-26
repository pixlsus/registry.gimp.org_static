#!/usr/bin/env python
#
# ==============================================================================
#                   StraightenCrop.py V1 - (c) Bert Hinz 2008
# ==============================================================================
#
# IMAGE ROTATION AND CROP FOR THE GIMP
#
# Straightens the image vertically or horizontically and crops with minimal loss
# of image area.
#
# ==============================================================================
#
# INSTRUCTIONS:
#
# Use the path tool of the GIMP to position two points on a line or edge of an
# object you want straighten vertically or horizontally:
#
# Press "B" to activate the path tool. Then put the points by single left-clicks
# without holding the mouse key while moving the mouse. Finally run
# <image>/Image/Transform/Straighten & Crop.
# Press "Ctrl & Z" to undo the crop if required.
#
# Menu location on systems with German local settings:
# <image>/Bild/Transformation/Straighten & Crop
#
# ==============================================================================
#
# This program is free software; you can redistribute it and/or modify it.
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# ==============================================================================


import math
from gimpfu import *

def rotate (img, drw):
	w=img.width
	h=img.height
	try:
		pa=pdb.gimp_path_get_current(img)
	except:
		pdb.gimp_message('No path defined!' + "\n" + 'Execution terminated.')
		return
	try:
		ppts=pdb.gimp_path_get_points(img, pa)[3]
	except:
		pdb.gimp_message('Path reading error!'+ "\n" + 'Execution terminated.')
		return
	pp=len(ppts)
	if pp<15:
		pdb.gimp_message('Second point of path missing!'+ "\n" + 'Execution terminated.')
		return
	if pp>15:
		pdb.gimp_message('More than two points on path!'+ "\n" + 'Only the first two points were regarded.')
	if pp>=15:
	 	x1,y1 = ppts[0:2]
		x2,y2 = ppts[6:8]
	dy=abs(y1-y2)
	dx=abs(x1-x2)
	if dx==0 or dy==0:
		pdb.gimp_message('Line already straightened!' + "\n" + 'Execution terminated.')
		return
	aMode=0 #vertical
	if dy<dx:
		aMode=1 #horizontal
	slope=0.0
	slope=(x2-x1)/(y2-y1)
	angle=math.atan(slope)
	
	##  for debugging purpose:
	##	winkel=1.0
	##	angle=winkel/360*2*math.pi

	if aMode==1:
		if angle >0:
			angle=angle-math.pi
		angle=angle+0.5*math.pi
	pdb.gimp_image_undo_group_start(img)
	layer_copy = drw.copy(True)
	layer_copy.mode = NORMAL_MODE
	img.add_layer(layer_copy, -1)
	drw = layer_copy
	img.layers[0].name="rotated"
	img.layers[1].visible=False
	pdb.gimp_path_delete(img, pa)
	pdb.gimp_drawable_transform_rotate (drw, angle, FALSE, img.width/2, img.height/2,TRANSFORM_FORWARD,INTERPOLATION_LANCZOS, TRUE, 3, 0)
	pdb.gimp_image_undo_group_end(img)
	alpha=angle/(2*math.pi)*360
	alpha_rad=math.pi*alpha/180

	if alpha>0:
		a3=h/2*math.tan(alpha_rad/2)
		b1=w/2-a3
		a4=w/2*math.tan(alpha_rad/2)
		b2=h/2-a4
		a1=b1*math.tan(alpha_rad)
		m1=math.tan(-alpha_rad)
		a2=b2*math.tan(alpha_rad)
		m2=math.tan(-0.5*math.pi+alpha_rad)

	else:
		alpha_rad=abs(alpha_rad)
		a3=h/2*math.tan(alpha_rad/2)
		a2=w/2-a3
		a4=w/2*math.tan(alpha_rad/2)
		a1=h/2-a4
		b1=a1*math.tan(alpha_rad)
		m2=math.tan(-1*alpha_rad)
		b2=a2*math.tan(alpha_rad)
		beta_rad=0.5*math.pi-alpha_rad
		m1=math.tan(-1*beta_rad)

	x=1.0
	x=((b2-a1)/(m1-m2))
	y=(a1+x*m1)
	w1=(w-2*x)+1
	h1=(h-2*y)+1
	if x<0:
		x=0
	if y<0:
		y=0
	if w1>0 and h1>0:
		if w1>w:
			w1=w
		if h1>h:
			h1=h
		if x > w-w1:
			x=w-w1
		if y>h-h1:
			y=h-h1

		## for debugging purpose:
		## pdb.gimp_rect_select(img,x,y,w1,h1,0,0,0)

		pdb.gimp_image_crop(img,w1,h1,x,y)
		s=w*h
		s1=w1*h1*1.0
		l=s-s1*1.0
		loss=round(l/s*100,1)
		rot=round(alpha,2)

		## for debugging purpose:
		## pdb.gimp_message("alpha=" + str(alpha) +"\n"+"a1="+str(a1)+" b1="+str(b1)+"\n" + "a2="+str(a2)+" b2="+str(b2)+"\n" + "x="+str(x)+" y="+str(y)+"\n" + "m1="+str(m1)+" m2="+str(m2)+"\n" + "w="+str(w)+" h="+str(h)+"\n" + "w1="+str(w1)+" h1="+str(h1))

		pdb.gimp_message("Rotation: " + str(rot)+" degrees."+ "\n"+"Loss of image area: " + str(loss) +" percent."+ "\n" + "\n"+ "Press 'Ctrl & Z' to undo the crop if required.")
	else:
		pdb.gimp_message("Angel of rotation too big. Image can't be cropped automatically."+"\n" + "Please crop manually.")
		return
	
register(
	"StraightenCrop",
	"Straightens the image vertically or horizontically and crops with minimal loss of image area",
	"Before running use path tool to put two points on a line or edge of the object you want to straighten vertically or horizontally.",
	"Bert Hinz",
	"public domain",
	"2008",
	"<Image>/Image/Transform/Straighten & Crop",
	"RGB*, GRAY*",
	[],
	[],
	rotate)
main()

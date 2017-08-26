#!/usr/bin/env python
#
#    Copyright 2010   Andy Shelley <andy@andyshelley.co.uk>
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

from gimpfu import *
from math import *
from cmath import *

def draw_curve (img, A1, A2, A3, N1, N2, N3, S1, S2, S3, Steps, Border, ex):
	
	# create new layer
	width = pdb.gimp_image_width (img)
	height = pdb.gimp_image_height (img)
	name = "Farris (" + str (A1) + "," + str (A2) + "," + str (A3) + "," + str (N1) + "," + str (N2) + "," + str (N3) + "," + str (S1) + "," + str (S2) + "," + str (S3) + "," + str (Steps) + "," + str (Border) + ")"	
	if (ex == 1):
		farris = gimp.Layer(img, name, width, height, RGB_IMAGE, 100, NORMAL_MODE)
	else:
		farris = gimp.Layer(img, name, width, height, RGBA_IMAGE, 100, NORMAL_MODE)
	img.add_layer(farris, 0)
	pdb.gimp_edit_clear (farris)		
	
	# trace farris curve
	Points = []
	SOPoints = []
	NP = 0	
	XOffset = width / 2
	YOffset = height / 2
	XMin = 0
	YMin = 0
	XMax = 0
	YMax = 0
	Steps = 1.0 / abs (Steps)	
	Border = int (Border)
	t = 0
	while (t < 1):
		z = A1 * exp(2*pi*(N1*t+S1)*1j) + A2 * exp(2*pi*(N2*t+S2)*1j) + A3 * exp(2*pi*(N3*t+S3)*1j)
		X = z.real
		Y = z.imag
		Points.extend([X, Y])
		NP+= 2
		t+= Steps
		if (X < XMin) :
			XMin = X
		if (X > XMax) :
			XMax = X
		if (Y < YMin) :
			YMin = Y
		if (Y > YMax) :
			YMax = Y
			
	# scale the curve to fit the canvas, and centre it		
	XScale = (width - 2 * Border) / (XMax - XMin)
	YScale = (height - 2 * Border) / (YMax - YMin)
	Scale = min (XScale, YScale)	
	index = 1
	for p in Points:
		if (index % 2) == 1:
			SOPoints.extend ([p * Scale + XOffset])	
		else:
			SOPoints.extend ([p * Scale + YOffset])	
		index+= 1
	
	# draw the curve, using current brush and colour
	pdb.gimp_paintbrush_default (farris, NP, SOPoints)
	
def python_farris (img, drawable, A1, A2, A3, N1, N2, N3, S1, S2, S3, Steps, Border, Show):
	
	if (Show == 0):
		# draw users curve
		draw_curve (img, A1, A2, A3, N1, N2, N3, S1, S2, S3, Steps, Border, 0)
	else:
		# draw examples
		width = pdb.gimp_image_width (img)
		height = pdb.gimp_image_height (img)
		
		# Nephroid
		ex = gimp.Image(width, height, RGB)
		draw_curve (ex, 1, 1, 1, 19, 17, -2, 0, 0, 0, 2001, 20, 1)
		gimp.Display(ex)
		
		# Palm
		ex = gimp.Image(width, height, RGB)
		draw_curve (ex, 1, 1, 1, 7, -5, 2, 0, 0, 0, 2001, 20, 1)
		gimp.Display(ex)
		
		# Farris
		ex = gimp.Image(width, height, RGB)
		draw_curve (ex, 6, 3, 2, 1, 7, -17, 0.3, 0, 0, 2001, 20, 1)
		gimp.Display(ex)

		# Daisy
		ex = gimp.Image(width, height, RGB)
		draw_curve (ex, 1, 1, 1, 19, -13, 3, 0, 0, 0, 2001, 20, 1)
		gimp.Display(ex)
		
		# Star
		ex = gimp.Image(width, height, RGB)
		draw_curve (ex, 1, 4, 2, 29, -11, -3, 0, 0, 0, 2001, 20, 1)
		gimp.Display(ex)
		
		# Maasai Shield
		ex = gimp.Image(width, height, RGB)
		draw_curve (ex, 1, 1, 1, 11, -7, -3, 0, 0, 0, 2001, 20, 1)
		gimp.Display(ex)
		
		# Triangle
		ex = gimp.Image(width, height, RGB)
		draw_curve (ex, 1, 2, 1, 19, -17, -2, 0, 0, 0, 2001, 20, 1)
		gimp.Display(ex)
		
		# Anne's Frog
		ex = gimp.Image(width, height, RGB)
		draw_curve (ex, 1, 2, 3, 10, -5, 3, 0, 0, 0, 2001, 20, 1)
		gimp.Display(ex)
				
register(
        "python_fu_farris",
        "Creates Spirograph type curves.",
        "Creates a Spirograph type curves.",
        "Andrew Shelley",
        "Andrew Shelley",
        "2010",
        "<Image>/Filters/Artistic/_Farris Curves...",
        "RGB*, GRAY*",
        [
                (PF_FLOAT, "A1", "Wheel 1 Radius", 1),
                (PF_FLOAT, "A2", "Wheel 2 Radius", 2),
                (PF_FLOAT, "A3", "Wheel 3 Radius", 1),
                (PF_FLOAT, "N1", "Wheel 1 Speed", 19),
                (PF_FLOAT, "N2", "Wheel 2 Speed", 17),
                (PF_FLOAT, "N3", "Wheel 3 Speed", -2),
                (PF_FLOAT, "S1", "Wheel 1 Phase", 0),
                (PF_FLOAT, "S2", "Wheel 2 Phase", 0),
                (PF_FLOAT, "S3", "Wheel 3 Phase", 0),
                (PF_FLOAT, "Steps", "Steps Per Rev", 2001),
                (PF_FLOAT, "Border", "Border", 20),
		(PF_TOGGLE, "Show", "Show Examples", 0)
        ],
        [],
        python_farris)

main()
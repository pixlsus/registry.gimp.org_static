;	Exact Aligner - (c) 2009 Dr. Volker Tries
;	volker.tries@kfopraxis-oberursel.de
;
;	v. 0.1_alpha 2009/10/01
;
;
;   This is the Exact Aligner TinySCHEME script for GIMP 
;	for overlaying two pictures by clicking on the same 
;	2 points in each picture.
;
;	How to use this tool:
;
;	-1) Copy the script into the script folder of Gimp
;	   In my installation it is in 
;	        D:\Programme\GIMP-2.0\share\gimp\2.0\scripts
;	0) Now start GIMP (you have to start GIMP AFTER you copied the script!
;	1) Open 2 pictures.
;	2) In one picture do "select all" (CTRL-a) and copy (CTRL-c).
;	3) Click on the other picture an paste the copied picture 
;	   as a new layer (CTRL-v) into it.
;	4) Close the first picture. We don't need it any more. Otherwise it only will disturb our script.
;	5) Set transparency of the layer to 50% (slide bar in the upper part
;	   of the layer manager). If you don't see the layer manager, just press CTRL-l.
;	6) Select the path tool in the tool box.
;	7) Now first click 2 points in the first picture (background layer) 
;	   and then the same two points in the second picture (second layer)
;	   You should see both pictures in a transparent way. You have to stroke the 4 point 
;	   consecutively to form one single path! Point 1 and 2 show the alignment marks for
;	   picture 1 and point 3 and 4 are the alignment marks for picture 2.
;	   Point 3 will be schifted to point 1 and point 4 to point 2.
;	   Furthermore a derotation and a rescaleing is done if necessery.
;	8) We are almost done! Now choos the script from the menu bar
;	   under "/Tries Tools/Exact Aligner". That's it.
;
;
;
;	If you encounter error messages, you 
;	   - probably didn't close the first picture or 
;	   - haven't choosen the 4 reference points
;	before calling the script.
;
;	*****  IMPORTANT  *******
;
;	This script is supposed to be very buggy!
;	two days ago I didn't know about this TinyScheme stuff and today the 
;	script works fine for me.
;	If you have any recommendations or even bug reports, please contact me 
;	under my email address shown above.
;	In the meantime enjoy this script!
;
;	* * * * * * * * * * * * * * * * * * * * * * * * 
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;


(define 
	(script-fu-exact-overlay    )
	
	; this is the code...

	(let* (
		; define our local variables. We need the "*" after let to have TinySCHEME 
		; declare the variables exactly this order.

		; here is the first images object in the program...
		(theImageID1 (aref (cadr (gimp-image-list)) 0))
		
		;here is the first vectors object from the first images object...
		(theVectorID1 (car (gimp-image-get-active-vectors theImageID1)))
		
		;here is the first strokes object in the first vectors object...
		(theStrokeID1 (aref (cadr (gimp-vectors-get-strokes theVectorID1)) 0))

		; First lets find out the points of the polygon you just created with the path tool in GIMP.
		; 	The first 2 points (first pair) are the reference points in picture of layer1 , 
		; 	the second pair are the same points, but now in picture of layer2
		
		;define point1...
		(theP1_x (aref (caddr (gimp-vectors-stroke-get-points theVectorID1 theStrokeID1 )) 0))
		(theP1_y (aref (caddr (gimp-vectors-stroke-get-points theVectorID1 theStrokeID1 )) 1))
		;define point2...
		(theP2_x (aref (caddr (gimp-vectors-stroke-get-points theVectorID1 theStrokeID1 )) 6))
		(theP2_y (aref (caddr (gimp-vectors-stroke-get-points theVectorID1 theStrokeID1 )) 7))
		;define point3...
		(theP3_x (aref (caddr (gimp-vectors-stroke-get-points theVectorID1 theStrokeID1 )) 12))
		(theP3_y (aref (caddr (gimp-vectors-stroke-get-points theVectorID1 theStrokeID1 )) 13))
		;define point4...
		(theP4_x (aref (caddr (gimp-vectors-stroke-get-points theVectorID1 theStrokeID1 )) 18))
		(theP4_y (aref (caddr (gimp-vectors-stroke-get-points theVectorID1 theStrokeID1 )) 19))
		
		; Now let's do some simple math to get
		; 	- rotation angle (camera was rotated between two shots)
		;	- displacement (for translation or offset.  e.g., by pointing the camera to a different center of image)
		;	- rescaling (in case the the viewing angle 
		;	  has changed from picture 1 to picture 2., e.g., by choosing another focal leth or chaning the distance to the photographic object)
		
		; Dx1, Dx2, Dy1, Dy2...
		(theDx1 (- theP2_x theP1_x))
		(theDy1 (- theP2_y theP1_y))
		(theDx2 (- theP4_x theP3_x))
		(theDy2 (- theP4_y theP3_y))
		
		; the angle between the two lines. i.e., the angle layer2 must be clockwise rotatet
		; in order to overlap with layer1...
		(theAngle (- (atan (/ theDy1 theDx1) ) (atan (/ theDy2 theDx2))))
		
		;the scale factors layer2 must be mulitplied by in order to fit onto layer1
		; this is simply the ratio of the two line lenths from the path we created with the 4 points
		; For distinct x and y scale factors we need to have another point.
		; But I don't need this so I didn't implement it yet.
		(theScaleX (/ (sqrt (+ (* theDx1 theDx1) (* theDy1 theDy1))) (sqrt (+ (* theDx2 theDx2) (* theDy2 theDy2))) ) ) 
		(theScaleY (/ theDy1 theDy2)) ; we need more then 2 points to calculate seperate scale factors for x and y axis if we also want de-rotation!!!
		(set! theScaleY (theScaleX)) ; this is for convenience and future use!!
		
		; the last info we need is the active drawable of the image
		(theActiveDrawableID (car (gimp-image-get-active-drawable theImageID1)))

		) ; end of variable definition --------------------------------
		
		; now we can make the transformation:
		(gimp-context-push)

		; --------- TRANSFORMATION ----------------------
		; we do the transformation in one function call......
		(gimp-drawable-transform-2d theActiveDrawableID theP3_x theP3_y theScaleX theScaleX theAngle theP1_x theP1_y 0 0 TRUE 1 1)
		
		
		(gimp-displays-flush)
		(gimp-context-pop)
	) ; end of local declaration	

	)

;Now we want to register this stuff...
(script-fu-register "script-fu-exact-overlay"
	_"<Image>/Tries-Tools/Exact Aligner"
	"Exact projection of 2 images in 2 layers"
	"Dr. Volker Tries"
	"(c) 2009, Dr. Volker Tries - Volker.Tries@kfopraxis-oberursel.de"
	"Sep 30, 2009"
	""
)

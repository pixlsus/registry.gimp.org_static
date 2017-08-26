(define (find-dx ax ay az cx cy cz siny sinz cosy cosz)
	(let* (
		(dx 0.0)
		(temp1 0.0)
		(temp2 0.0)
		(temp3 0.0)
		)

; Make the calculations easier to decipher
	(set! temp1 (* sinz (- ay cy)))
	(set! temp2 (* cosz (- ax cx)))
	(set! temp3 (* siny (- az cz)))
	(set! dx (- (* cosy (+ temp1 temp2)) temp3))

	dx
	)
)
; ----------------------------------------------------
(define (find-dy ax ay az cx cy cz sinx siny sinz cosx cosy cosz)
	(let* (
		(dy 0.0)
		(temp1 0.0)
		(temp2 0.0)
		(temp3 0.0)
		(temp4 0.0)
		(temp5 0.0)
		(temp6 0.0)
		)

; Make the calculations easier to decipher
	(set! temp1 (* cosy (- az cz)))
	(set! temp2 (* sinz (- ay cy)))
	(set! temp3 (* cosz (- ax cx)))
	(set! temp4 (* cosz (- ay cy)))
	(set! temp5 (* sinz (- ax cx)))
	(set! temp6 (* siny (+ temp2 temp3)))
	(set! dy (+ (* sinx (+ temp1 temp6)) (* cosx (- temp4 temp5))))

	dy
	)
)
; ----------------------------------------------------
(define (find-dz ax ay az cx cy cz sinx siny sinz cosx cosy cosz)
	(let* (
		(dz 0.0)
		(temp1 0.0)
		(temp2 0.0)
		(temp3 0.0)
		(temp4 0.0)
		(temp5 0.0)
		(temp6 0.0)
		)

; Make the calculations easier to decipher
	(set! temp1 (* cosy (- az cz)))
	(set! temp2 (* sinz (- ay cy)))
	(set! temp3 (* cosz (- ax cx)))
	(set! temp4 (* cosz (- ay cy)))
	(set! temp5 (* sinz (- ax cx)))
	(set! temp6 (* siny (+ temp2 temp3)))
	(set! dz (- (* cosx (+ temp1 temp6)) (* sinx (- temp4 temp5))))

	dz
	)
)
; ----------------------------------------------------
(define (find-b dx ex ez dz)
	(let* (
		(b 0.0)
		)

	(set! b (* (/ ez dz) (- dx ex)))

	b
	)
)
; ----------------------------------------------------
(define (script-fu-JMS-rot3D
	theImage
	baseLayer
	xRot
	yRot
	zRot
	Cz
	magFactor
	)

	(let* (
		(inWidth (car (gimp-drawable-width baseLayer)))
		(inHeight (car (gimp-drawable-height baseLayer)))

; Original Area of the image
		(origArea (* inWidth inHeight))

; Midpoint of the image
		(midX (/ inWidth 2.0))
		(midY (/ inHeight 2.0))

; Get the rotation angles into radians, as well as their trig functions
		(deg2rad (/ *pi* 180.0))
		(xAngle (* xRot deg2rad))
		(cosX (cos xAngle))
		(sinX (sin xAngle))
		(yAngle (* yRot deg2rad))
		(cosY (cos yAngle))
		(sinY (sin yAngle))
		(zAngle (* zRot deg2rad))
		(cosZ (cos zAngle))
		(sinZ (sin zAngle))

; Center point of original image.  Corners will be mapped to (+/-1, +/-1)
		(Ax 0.0)
		(Ay 0.0)
		(Az 0.0)

; X and Y position for the camera.  Cz gives the height above the plane, and is an input variable.
		(Cx 0.0)
		(Cy 0.0)

; Final 3-D positoin of the coordinate on the original plane
		(Dx 0.0)
		(Dy 0.0)
		(Dz 0.0)

; Position of the "Eyeball" that sees the plane.
; The final value of Ez will depend on the magnification factor chosen
		(Ex 0.0)
		(Ey 0.0)
		(Ez 1.0)

; Four corners and center of new image
		(x1 0.0)
		(y1 0.0)
		(x2 0.0)
		(y2 0.0)
		(x3 0.0)
		(y3 0.0)
		(x4 0.0)
		(y4 0.0)
		(xc 0.0)
		(yc 0.0)

; Calculate the new dimensions of the rotated image
		(xMin 0.0)
		(yMin 0.0)
		(xMax 0.0)
		(yMax 0.0)

; New width and height of image after rotations
		(newWidth 0.0)
		(newHeight 0.0)
		(newArea 0.0)
		(changeArea 0.0)
		)

; Start an undo group for the rotation, just in case it turns out ugly.
		(gimp-image-undo-group-start theImage)

; Rotate the Upper Left Corner
		(set! Dx (find-dx -1.0 1.0 Az Cx Cy Cz sinY sinZ cosY cosZ))
		(set! Dy (find-dy -1.0 1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! Dz (find-dz -1.0 1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! x1 (find-b Dx Ex Ez Dz))
		(set! y1 (find-b Dy Ey Ez Dz))

; Rotate the Upper Right Corner
		(set! Dx (find-dx 1.0 1.0 Az Cx Cy Cz sinY sinZ cosY cosZ))
		(set! Dy (find-dy 1.0 1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! Dz (find-dz 1.0 1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! x2 (find-b Dx Ex Ez Dz))
		(set! y2 (find-b Dy Ey Ez Dz))

; Rotate the Left Corner
		(set! Dx (find-dx -1.0 -1.0 Az Cx Cy Cz sinY sinZ cosY cosZ))
		(set! Dy (find-dy -1.0 -1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! Dz (find-dz -1.0 -1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! x3 (find-b Dx Ex Ez Dz))
		(set! y3 (find-b Dy Ey Ez Dz))

; Rotate the Right Corner
		(set! Dx (find-dx 1.0 -1.0 Az Cx Cy Cz sinY sinZ cosY cosZ))
		(set! Dy (find-dy 1.0 -1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! Dz (find-dz 1.0 -1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! x4 (find-b Dx Ex Ez Dz))
		(set! y4 (find-b Dy Ey Ez Dz))

; Get the new dimensions for the rotated image
		(set! xMax (max (max x1 x2) (max x3 x4)))
		(set! xMin (min (min x1 x2) (min x3 x4)))
		(set! newWidth (- xMax xMin))

		(set! yMax (max (max y1 y2) (max y3 y4)))
		(set! yMin (min (min y1 y2) (min y3 y4)))
		(set! newHeight (- yMax yMin))

; Calculate what Ex must be in order to get the requested magnification factor
		(set! newArea
			(* 0.5
				(+
					(- (* x1 y2) (* x2 y1))
					(- (* x2 y4) (* x4 y2))
					(- (* x4 y3) (* x3 y4))
					(- (* x3 y1) (* x1 y3))
				)
			)
		)
		(set! changeArea (/ newArea 4.0))
		(set! Ez (sqrt (/ magFactor (abs changeArea))))

		(if (< newArea 0.0)
			(set! Ez (- 0.0 Ez))
		)

;		(gimp-message (string-append "Ez " (number->string Ez)))

; Redo the calculations to get the correct magnification
; Final Upper Left Corner
		(set! Dx (find-dx -1.0 1.0 Az Cx Cy Cz sinY sinZ cosY cosZ))
		(set! Dy (find-dy -1.0 1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! Dz (find-dz -1.0 1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! x1 (find-b Dx Ex Ez Dz))
		(set! y1 (find-b Dy Ey Ez Dz))

; Final Upper Right Corner
		(set! Dx (find-dx 1.0 1.0 Az Cx Cy Cz sinY sinZ cosY cosZ))
		(set! Dy (find-dy 1.0 1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! Dz (find-dz 1.0 1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! x2 (find-b Dx Ex Ez Dz))
		(set! y2 (find-b Dy Ey Ez Dz))

; Final Lower Left Corner
		(set! Dx (find-dx -1.0 -1.0 Az Cx Cy Cz sinY sinZ cosY cosZ))
		(set! Dy (find-dy -1.0 -1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! Dz (find-dz -1.0 -1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! x3 (find-b Dx Ex Ez Dz))
		(set! y3 (find-b Dy Ey Ez Dz))

; Final Lower Right Corner
		(set! Dx (find-dx 1.0 -1.0 Az Cx Cy Cz sinY sinZ cosY cosZ))
		(set! Dy (find-dy 1.0 -1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! Dz (find-dz 1.0 -1.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! x4 (find-b Dx Ex Ez Dz))
		(set! y4 (find-b Dy Ey Ez Dz))

; Final Center
		(set! Dx (find-dx 0.0 0.0 Az Cx Cy Cz sinY sinZ cosY cosZ))
		(set! Dy (find-dy 0.0 0.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! Dz (find-dz 0.0 0.0 Az Cx Cy Cz sinX sinY sinZ cosX cosY cosZ))
		(set! xc (find-b Dx Ex Ez Dz))
		(set! yc (find-b Dy Ey Ez Dz))

; Now that we have the coordinates, we transform the original plane
		(gimp-drawable-transform-perspective
			baseLayer
				(* midX x1) (* midY y1)
				(* midX x2) (* midY y2)
				(* midX x3) (* midY y3)
				(* midX x4) (* midY y4)
				TRANSFORM-FORWARD
				INTERPOLATION-CUBIC
				1 3 TRANSFORM-RESIZE-ADJUST)

; Resize the image to fit the layer, since it will certainly have changed size
		(gimp-image-resize-to-layers theImage)

; Flip the image around because gimp has the +y direction going down instead of up.
		(gimp-drawable-transform-flip-simple baseLayer ORIENTATION-VERTICAL 1 0 0)

; End the undo image group
		(gimp-image-undo-group-end theImage)

		(gimp-displays-flush)
	)
)

(script-fu-register
	"script-fu-JMS-rot3D"
	_"_3D Image Rotation"
	"Takes an image and rotates around the X, Y and Z-axes, and then magnifies the image by a chosen amount."
	"James Sambrook"
	"4 March 2011"
	""
	""
    SF-IMAGE		"Image"			0
    SF-DRAWABLE		"Drawable"		0
	SF-ADJUSTMENT "X-Axis rotation"	'(40 -89 89 1 10 0 0)
	SF-ADJUSTMENT "Y-Axis rotation"	'(-30 -89 89 1 10 0 0)
	SF-ADJUSTMENT "Z-Axis rotation"	'(10 -89 89 1 10 0 0)
	SF-ADJUSTMENT "Camera position  (Zero will give you an error.  Please do not use that value.)" '(9.9 -50 50 1 10 0 0)
	SF-ADJUSTMENT "Magnification"	'(1.5 0.5 10 0.l 1 1 0)
)

(script-fu-menu-register "script-fu-JMS-rot3D"
	"<Image>/Filters/SambrookJM/")
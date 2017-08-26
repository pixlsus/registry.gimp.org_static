; Perspective Reflection
; Version 0.10
; By David Nickerson
; Copyright 2009 David Nickerson

; Description:
; This script modifies a layer to have a perspective and reflection consistent with that perspective.

; License:
; This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
; The GNU Public License is available at <http://www.gnu.org/licenses/>.


(define (perspective-reflection img layer1 rotate dist c-height)

	; Begin the 'undo' group
	(gimp-image-undo-group-start img)

	; Create a layer and define some values
	(let* (
		(layer2 (car (gimp-layer-copy layer1 1)))
		(old-width (car (gimp-drawable-width layer1)))
		(old-height (car (gimp-drawable-height layer1)))
		(new-width (* (cos (* rotate 0.0174532925)) old-width))
		(new-height (* old-height (- 1 (* (sin (* rotate 0.0174532925)) (- 1 (/ dist 100))))))
		)


	; Add the layer
	(gimp-image-add-layer img layer2 1)

	; Perform the transformations
	(gimp-drawable-transform-perspective layer1
		0 0
		new-width (* (- old-height new-height) (- 1 (/ c-height 100)))
		0 old-height
		new-width (+ (* (- old-height new-height) (- 1 (/ c-height 100))) new-height)
		0 2 0 3 0)

	(gimp-drawable-transform-perspective layer2
		0 (* old-height 2)
		new-width (+ (* (- old-height new-height) (- 1 (/ c-height 100))) (* new-height 2))
		0 old-height
		new-width (+ (* (- old-height new-height) (- 1 (/ c-height 100))) new-height)
		0 2 0 3 0)

	; Resize the canvas
	(gimp-image-resize-to-layers img)

	; End the 'undo' group
	(gimp-image-undo-group-end img)

	; Display the changes
	(gimp-displays-flush)

))

(script-fu-register "perspective-reflection"
					"Perspective Reflection"
					"Modifies a layer to have a perspective and reflection"
					"David Nickerson"
					"David Nickerson"
					"2009"
					"RGB*, GRAY*"
					SF-IMAGE	"Image"						0
					SF-DRAWABLE	"Layer"						0
					SF-VALUE	"Rotation (-90 to 90)"		"45"
					SF-VALUE	"Distance (0 to 100)"		"70"
					SF-VALUE	"Camera height (0=bottom, 100=top)"	"50")

(script-fu-menu-register "perspective-reflection"
						 "<Image>/Filters/Decor")
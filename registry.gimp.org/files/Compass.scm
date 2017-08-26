;;  ***************************************************************************
;;  *   Copyright (C) 2008 by James Sambrook                                  *
;;  *   sambrook@va.metrocast.net                                             *
;;  *                                                                         *
;;  *   This program is free software; you can redistribute it and/or modify  *
;;  *   it under the terms of the GNU General Public License as published by  *
;;  *   the Free Software Foundation; either version 2 of the License, or     *
;;  *   (at your option) any later version.                                   *
;;  *                                                                         *
;;  *   This program is distributed in the hope that it will be useful,       *
;;  *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
;;  *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
;;  *   GNU General Public License for more details.                          *
;;  *                                                                         *
;;  *   You should have received a copy of the GNU General Public License     *
;;  *   along with this program; if not, write to the                         *
;;  *   Free Software Foundation, Inc.,                                       *
;;  *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
;;  ***************************************************************************


(define (script-fu-compass inSize outerThick inThick)

  (let* (
        (theImage (car (gimp-image-new (+ (* inSize 2.0) 1) (+ (* inSize 2.0) 1) RGB)))
        (baseLayer (car (gimp-layer-new theImage (+ (* inSize 2.0) 1) (+ (* inSize 2.0) 1) RGB-IMAGE "Wheel" 100 NORMAL-MODE)))

; Size of full image
		(fullSize (+ (* inSize 2.0) 1))

; Number of points used to make each arrow	  
		(lineArray (make-vector 4 0.0))
		(diamondArray (make-vector 10 0.0))

; Values for trig calculations
		(radius (- inSize outerThick))
		(angleRad (/ (* 45.0 *pi*) 180.0))
		(theta 0.0)
		(theta2 (/ angleRad 2.0))
		(phi 0.0)
		(alpha 0.0)
		(halfRad (* inSize (/ inThick 100.0)))

; Frame Bookkeeping
		(frameName "")
		(frameNum 0)
		(flag 0)
		
		)

	(gimp-context-push)

	(gimp-context-set-foreground '(0 0 0))
	(gimp-context-set-background '(255 255 255))
	
; Add the three layers of the image in reverse order so they appear properly
	(gimp-image-add-layer theImage baseLayer 0)

; Fill the initial image with white
	(gimp-drawable-fill baseLayer 1)

; Outer Wheel
	(gimp-ellipse-select theImage 0 0 fullSize fullSize 0 0 0 0 )
	(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-shrink theImage outerThick)
	(gimp-edit-bucket-fill baseLayer BG-BUCKET-FILL NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Set your brush to Circle 03
	(gimp-context-set-brush "Circle (03)")

	(while (< flag 8)

		(set! theta (* flag angleRad))
		(set! phi (+ theta theta2))
		(set! alpha (- theta theta2))
		
		(vector-set! lineArray 0 inSize)
		(vector-set! lineArray 1 inSize)
		(vector-set! lineArray 2 (+ inSize (* radius (sin theta))))
		(vector-set! lineArray 3 (+ inSize (* radius (cos theta))))

		(gimp-pencil baseLayer 4 lineArray)

		(vector-set! diamondArray 0 inSize)
		(vector-set! diamondArray 1 inSize)
		(vector-set! diamondArray 2 (+ inSize (* (sin phi) halfRad)))
		(vector-set! diamondArray 3 (+ inSize (* (cos phi) halfRad)))
		(vector-set! diamondArray 4 (+ inSize (* radius (sin theta))))
		(vector-set! diamondArray 5 (+ inSize (* radius (cos theta))))
		(vector-set! diamondArray 6 (+ inSize (* (sin alpha) halfRad)))
		(vector-set! diamondArray 7 (+ inSize (* (cos alpha) halfRad)))
		(vector-set! diamondArray 8 inSize)
		(vector-set! diamondArray 9 inSize)

		(gimp-pencil baseLayer 10 diamondArray)

; Increment the flag
		(set! flag (+ flag 1))
	)

; Flatten the Image
;	(gimp-image-flatten theImage)
	
; Show the new image.  TAH-DAH!
	(gimp-display-new theImage)
   )
)

; Register the function with GIMP:

(script-fu-register
  "script-fu-compass"
  _"_Compass..."
  _"Create an eight-point compass."
  "James Sambrook"
  "26 January 2009"
  "James Sambrook.  King George, VA, USA"
  ""
  SF-ADJUSTMENT _"Image half-size"        '(320 150 1500 1 25 0 0)
  SF-ADJUSTMENT _"Outer Circle Thickness" '(3 1 15 1 3 0 0)
  SF-ADJUSTMENT _"Inner Arrow Thickness"  '(50 10 90 1 10 0 0)

)

(script-fu-menu-register "script-fu-compass"
                         "<Image>/Filters/SambrookJM/")
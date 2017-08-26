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


(define (script-fu-gasgiant inSize xTurb numBands rotAngle decSat)

  (let* (
        (theImage (car (gimp-image-new inSize inSize RGB)))
        (baseLayer (car (gimp-layer-new theImage inSize inSize RGBA-IMAGE "Gas" 100 NORMAL-MODE)))

        (flag 0)
	  (flag1 0)
        (bandSize (/ inSize numBands))
	  (blurSize (/ inSize 4.0))
        (angleRad (/ (* rotAngle *pi*) 180))

        )

	(gimp-context-push)

	(gimp-image-add-layer theImage baseLayer 0)

; Set the background and foreground colors to white and black
	(gimp-context-set-background '(255 255 255))
	(gimp-context-set-foreground '(0 0 0))

	(while (< flag numBands)

; (gimp-rect-select image x y width height operation feather feather-radius)
; Make each band, and fill it with the plasma
		(gimp-rect-select theImage 0 (+ (* flag bandSize) 2) inSize bandSize 0 0 0)

; Generate the random plasma for the each band
		(plug-in-plasma RUN-NONINTERACTIVE
			theImage baseLayer (rand 1000000000) xTurb)
;			theImage baseLayer (rand 4294967294) xTurb)

;Blur the plasma horizontally
	(plug-in-mblur RUN-NONINTERACTIVE
		theImage baseLayer 0 blurSize 0 0 0)

; Get rid of the selection
		(gimp-selection-clear theImage)

; Increment the loop counter
		(set! flag (+ flag 1))
	)

; Get rid of all transparency
	(plug-in-threshold-alpha RUN-NONINTERACTIVE
		theImage baseLayer 0)

;Blur the plasma vertically by a small amount
	(plug-in-mblur RUN-NONINTERACTIVE
		theImage baseLayer 0 (/ blurSize 8.0) 90 0 0)

; Make the cloud layers seamless to allow better mapping
	(plug-in-make-seamless RUN-NONINTERACTIVE theImage baseLayer)

; Map to a sphere
    (plug-in-map-object RUN-NONINTERACTIVE theImage baseLayer 
			1			; mapping
			0.5 0.5 2.0		; viewpoint
			0.5 0.5 0.9		; object pos
			1.0 0.0 0.0		; first axis
			0.0 1.0 0.0		; 2nd axis
			0.0 0.0 0.0		; axis rotation
			2 '(255 255 255)	; light (type, color)
			-0.5 -0.5 2		; light position
			-1.0 -1.0 1.0	; light direction
			0.3 1 0.5 0.3 27	; material (amb, diff, refl, spec, high)
			1			; antialias
			0 			; tile
			0	 		; new image
			1 			; transparency
			0.225  		; radius
			1 1 1 1 		; unused parameters
			-1 -1 -1 -1 -1 -1 -1 -1)

; Slight blurring of the planet so the layers aren't quite as sharp
	(plug-in-gauss RUN-NONINTERACTIVE
		theImage baseLayer (/ inSize 50) (/ inSize 50) 0)

; Change the sarutation by decSat amount
	(gimp-hue-saturation baseLayer 0 0 0 decSat)

	(gimp-drawable-transform-rotate-default baseLayer
		angleRad 1 (/ inSize 2) (/ inSize 2) 1 0)

	(gimp-layer-resize-to-image-size baseLayer)

	(plug-in-zealouscrop RUN-NONINTERACTIVE theImage baseLayer)

	(gimp-image-scale theImage inSize inSize)

; Show the new image.  TAH-DAH!
	(gimp-display-new theImage)
   )
)

; Register the function with GIMP:

(script-fu-register
  "script-fu-gasgiant"
  _"_Gas Giant..."
  _"Create a gas giant with colored bands.  Saturation can be changed to make the planet more pale."
  "James Sambrook"
  "19 September 2008"
  "James Sambrook.  King George, VA, USA"
  ""
  SF-ADJUSTMENT _"Image size"             '(640 300 3000 1 50 0 0)
  SF-ADJUSTMENT _"Noise X size"           '(2 1 15 0.1 1 1 0)
  SF-ADJUSTMENT _"Number of Bands"        '(4 1 20 1 5 0 0)
  SF-ADJUSTMENT _"Angle of Rotation"      '(0 -180 180 5 15 0 0)
  SF-ADJUSTMENT _"Change in Saturation"   '(-30 -100 100 1 10 0 0)

)

(script-fu-menu-register "script-fu-gasgiant"
                         "<Image>/Filters/SambrookJM/")
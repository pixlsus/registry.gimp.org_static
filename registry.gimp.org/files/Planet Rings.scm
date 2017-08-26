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


(define (script-fu-planet-rings inWidth inHeight bDetail xSize ySize oRing iRing iterations heightRed ringNoise inColor hueOne saturationOne lightnessOne rotAngle)

  (let* (
        (theImage (car (gimp-image-new inWidth inHeight RGB)))
        (baseLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Rings" 100 NORMAL-MODE)))

        (flag 0)

        (outerRing (/ oRing 100.0))
        (innerRing (/ iRing 100.0))

	  (ringWidth (* inWidth outerRing))
	  (ringHeight (* inHeight outerRing))
	  (innerWidth (* inWidth innerRing))
	  (innerHeight (* inHeight innerRing))

        (xStart (/ inWidth (/ 2 (- 1 outerRing))))
        (yStart (/ inHeight (/ 2 (- 1 outerRing))))

        (xEnd (/ inWidth (/ 2 (- 1 innerRing))))
        (yEnd (/ inHeight (/ 2 (- 1 innerRing))))

        (finalHeight (/  inHeight heightRed))

        )

	(gimp-context-push)

	(gimp-image-add-layer theImage baseLayer 0)

; Set the background and foreground colors to white and black
	(gimp-context-set-background '(255 255 255))
	(gimp-context-set-foreground '(0 0 0))

; Generate the random noise for the rings
	(plug-in-solid-noise RUN-NONINTERACTIVE
		theImage baseLayer 1 0 (rand 4294967294) bDetail xSize ySize)

; Run the whirl and pinch filter for "iterations" times, going a full 360 degrees each time.
      (while (< flag iterations)
		(plug-in-whirl-pinch RUN-NONINTERACTIVE
			theImage baseLayer 720.0 0.0 1.0)
		(set! flag (+ 1 flag))
	)

; Make an elliptical selection and cut out the area surrounding the ellipse
      (gimp-ellipse-select theImage xStart yStart ringWidth ringHeight CHANNEL-OP-ADD
		TRUE TRUE (/ xStart 10.0))
	(gimp-selection-invert theImage)
	(gimp-edit-cut baseLayer)

; Now cut out the inside of the ring layer.
      (gimp-ellipse-select theImage xEnd yEnd innerWidth innerHeight CHANNEL-OP-ADD
		TRUE TRUE (/ xStart 10.0))
	(gimp-edit-cut baseLayer)

; Reduce the height of the final image from inHeight to finalHeight
	(gimp-image-scale theImage inWidth finalHeight)

; Select only the ring portion of the layer
	(gimp-selection-layer-alpha baseLayer)

; Noise in the pixels in the ring to give it the cratered look
      (plug-in-rgb-noise RUN-NONINTERACTIVE
		theImage baseLayer 0 0 ringNoise ringNoise ringNoise 0)

; Deselect everything
	(gimp-selection-clear theImage)

; Rotate the rings if requested
	(if (<> 0 rotAngle)
		(gimp-drawable-transform-rotate-default baseLayer rotAngle 1
		 (/ 2 inWidth) (/ 2 finalHeight) 1 0))

; Resize the layer to the rotated rings if needed
	(if (<> 0 rotAngle)
		(gimp-image-resize-to-layers theImage))

; Crop the final ring layer
	(plug-in-zealouscrop RUN-NONINTERACTIVE
		theImage baseLayer)

; If the image is to be colorized, do so now.
	(if (= inColor TRUE)
		(gimp-colorize baseLayer hueOne saturationOne lightnessOne))

; Show the new image.  TAH-DAH!
	(gimp-display-new theImage)
   )
)

; Register the function with GIMP:

(script-fu-register
  "script-fu-planet-rings"
  _"_Planet Rings..."
  _"Create a planetary ring system."
  "James Sambrook"
  "19 September 2008"
  "James Sambrook.  King George, VA, USA"
  ""
  SF-ADJUSTMENT _"Image width"          '(640 300 3000 1 50 0 0)
  SF-ADJUSTMENT _"Image height"         '(480 300 3000 1 50 0 0)
  SF-ADJUSTMENT _"Detail Level"         '(4 1 15 1 4 0 0)
  SF-ADJUSTMENT _"Noise X size"         '(4 1 15 1 4 0 0)
  SF-ADJUSTMENT _"Noise Y size"         '(3 1 15 1 4 0 0)
  SF-ADJUSTMENT _"Outer Ring Ratio (%)" '(90 61 99 1 5 0 0)
  SF-ADJUSTMENT _"Inner Ring Ratio (%)" '(60 0 60 1 5 0 0)
  SF-ADJUSTMENT _"Whirling Iterations"  '(4 1 10 1 4 0 0)
  SF-ADJUSTMENT _"Height Reduction"     '(2 1 4 0.1 0.5 2 0)
  SF-ADJUSTMENT _"Ring Noise"           '(0.2 0 1 0.05 0.2 2 0)
  SF-TOGGLE     _"Colorize"		    FALSE
  SF-ADJUSTMENT _"Base Hue"             '(180 0 360 1 15 0 0)
  SF-ADJUSTMENT _"Base Saturation"      '(50 0 100 1 10 0 0)
  SF-ADJUSTMENT _"Base Lightness"       '(0 -100 100 1 20 0 0)
  SF-ADJUSTMENT _"Rotation Angle"       '(0 -180 180 1 20 0 0)

)

(script-fu-menu-register "script-fu-planet-rings"
                         "<Image>/Filters/SambrookJM/")
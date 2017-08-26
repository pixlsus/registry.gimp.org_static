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


(define (script-fu-eyeball inSize inColor outColor Pupil IrisL IrisD)

  (let* (
        (theImage (car (gimp-image-new inSize inSize RGB)))
        (baseLayer (car (gimp-layer-new theImage inSize inSize RGB-IMAGE "Eyeball" 100 NORMAL-MODE)))
        (whiteLayer (car (gimp-layer-new theImage inSize inSize RGBA-IMAGE "White" 100 NORMAL-MODE)))
        (borderLayer (car (gimp-layer-new theImage inSize inSize RGBA-IMAGE "Border" 100 NORMAL-MODE)))
        (ellipseLayer (car (gimp-layer-new theImage inSize inSize RGBA-IMAGE "Ellipse" 100 NORMAL-MODE)))

	  (ratio (/ inSize 400.0))

; Three levels of the Eye.
        (Band1 (* inSize Pupil))
        (Band2 (* inSize (+ Pupil IrisL)))
        (Band3 (* inSize (+ IrisD (+ Pupil IrisL))))

; Size of the white blur on the eye
	  (xTop (/ inSize 4.0))
	  (yTop (* 85 ratio))
	  (xBottom (* 3.0 xTop))
	  (yBottom (* 130 ratio))
        )

	(gimp-context-push)

; Add the three layers of the image in reverse order so they appear properly
;	(gimp-image-add-layer theImage whiteLayer 0)
;	(gimp-image-add-layer theImage borderLayer 0)
	(gimp-image-add-layer theImage baseLayer 0)
;	(gimp-image-add-layer theImage ellipseLayer 0)

; Set the foreground color to black
	(gimp-context-set-foreground '(0 0 0))

; Set the background color to white
	(gimp-context-set-background '(255 255 255))

; Fill the initial image with white
	(gimp-drawable-fill baseLayer 2)

;Select the pupil, and fill with black
	(gimp-rect-select theImage 0 0 Band1 inSize 0 0 0)
	(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Select the inner colored portion of the eye, and fill it with the proper color
      (gimp-context-set-foreground inColor)
	(gimp-rect-select theImage (- Band1 1) 0 (- Band2 Band1) inSize 0 0 0)
	(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Select the outer colored portion of the eye, and fill it with the proper color
      (gimp-context-set-foreground outColor)
	(gimp-rect-select theImage (- Band2 1) 0 (- Band3 Band2) inSize 0 0 0)
	(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Select the Wind plugin to move the colors around a bit
; (plug-in-wind run-mode image drawable threshold direction strength algorithm edge)
	(plug-in-wind RUN-NONINTERACTIVE
		theImage baseLayer 4 0 (* 25 ratio) 0 2)

; Blur the colors slightly
	(plug-in-gauss-rle2 RUN-NONINTERACTIVE
		theImage baseLayer (* 1.5 ratio) (* 1.5 ratio) )

; Get rid of most of the color that bled into the white area of the picture
	(gimp-rect-select theImage (- Band3 1) 0 Band3 inSize CHANNEL-OP-ADD 1 (* 30.0 ratio))
	(gimp-edit-clear baseLayer)
	(gimp-selection-clear theImage)

;Rotate the eyeball 90 degrees
	(plug-in-rotate RUN-NONINTERACTIVE
		theImage baseLayer 1 1)

;Make the eyeball into a ball
	(plug-in-polar-coords RUN-NONINTERACTIVE
		theImage baseLayer 100 0 0 1 1)

; Add an alpha layer to the eyeball layer
	(gimp-layer-add-alpha baseLayer)

	(gimp-fuzzy-select baseLayer 1 1 0 CHANNEL-OP-ADD 0 1 0 1)

	(gimp-selection-grow theImage 5)
	(gimp-selection-shrink theImage 5)
	(gimp-edit-clear baseLayer)
	(gimp-selection-clear theImage)

	(gimp-rect-select theImage 10 10 (- inSize 20) (- inSize 20) 0 0 0)
	(gimp-selection-invert theImage)
	(gimp-edit-clear baseLayer)
	(gimp-selection-clear theImage)

	(gimp-selection-layer-alpha baseLayer)
	(gimp-selection-invert theImage)
	(gimp-selection-grow theImage (* ratio 5.0))
	(gimp-edit-clear baseLayer)
	(gimp-selection-clear theImage)

	(gimp-image-add-layer theImage whiteLayer 0)
	(gimp-rect-select theImage 0 0 inSize inSize 0 0 0)
	(gimp-edit-clear whiteLayer)
	(gimp-selection-clear theImage)

	(gimp-selection-layer-alpha baseLayer)
	(gimp-selection-grow theImage (* ratio 50.0))

	(gimp-edit-bucket-fill whiteLayer BG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

	(gimp-image-raise-layer theImage baseLayer)

	(gimp-image-add-layer theImage borderLayer 0)
	(gimp-rect-select theImage 0 0 inSize inSize 0 0 0)
	(gimp-edit-clear borderLayer)
	(gimp-selection-clear theImage)

	(gimp-selection-layer-alpha whiteLayer)
	(gimp-selection-border theImage (* ratio 8.0))
	(gimp-context-set-foreground '(0 0 0))

	(gimp-edit-bucket-fill borderLayer FG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Blur the colors slightly
	(plug-in-gauss-rle2 RUN-NONINTERACTIVE
		theImage borderLayer (* 60 ratio) (* 60 ratio) )

	(gimp-selection-layer-alpha whiteLayer)
	(gimp-selection-invert theImage)
	(gimp-edit-clear borderLayer)
	(gimp-selection-clear theImage)

	(gimp-image-add-layer theImage ellipseLayer 0)
	(gimp-rect-select theImage 0 0 inSize inSize 0 0 0)
	(gimp-edit-clear ellipseLayer)
	(gimp-selection-clear theImage)

	(gimp-context-set-background '(255 255 255))
	(gimp-ellipse-select theImage xTop yTop (- xBottom xTop) (- yBottom yTop) 2 1 0 0 )
	(gimp-edit-bucket-fill ellipseLayer BG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)
	(plug-in-gauss-rle2 RUN-NONINTERACTIVE
		theImage ellipseLayer (* 100 ratio) (* 100 ratio) )

; Show the new image.  TAH-DAH!
	(gimp-display-new theImage)
   )
)

; Register the function with GIMP:

(script-fu-register
  "script-fu-eyeball"
  _"_Eyeball..."
  _"Create an All-Seeing Eye."
  "James Sambrook"
  "23 September 2008"
  "James Sambrook.  King George, VA, USA"
  ""
  SF-ADJUSTMENT _"Image size"           '(400 300 3000 1 50 0 0)
  SF-COLOR      _"Inner Eye Color"      '(0 127 127)
  SF-COLOR      _"Outer Eye Color"      '(0 255 255)
  SF-ADJUSTMENT _"Pupil Thickness"      '(0.12 0.01 0.2 0.01 0.05 2 0)
  SF-ADJUSTMENT _"Dark Iris Thickness"  '(0.14 0.01 0.2 0.01 0.05 2 0)
  SF-ADJUSTMENT _"Light Iris Thickness" '(0.35 0.01 0.4 0.01 0.05 2 0)

)

(script-fu-menu-register "script-fu-eyeball"
                         "<Image>/Filters/SambrookJM/")
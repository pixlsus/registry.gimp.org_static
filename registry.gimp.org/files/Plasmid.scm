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


(define (script-fu-plasmid inWidth inHeight radGrad linGrad imHue imSat imLight chromeColor contrast deform randomColor)

  (let* (
        (theImage (car (gimp-image-new inWidth inHeight RGB)))
        (baseLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Transparent" 100 NORMAL-MODE)))
        (gradLayer (car (gimp-layer-new theImage inWidth inHeight RGB-IMAGE "Gradient Layer" 100 NORMAL-MODE)))
        (colorLayer (car (gimp-layer-new theImage inWidth inHeight RGB-IMAGE "Color Layer" 100 NORMAL-MODE)))
        (grad2Layer (car (gimp-layer-new theImage inWidth inHeight RGB-IMAGE "Gradient 2 Layer" 100 NORMAL-MODE)))
        (graddupeLayer (car (gimp-layer-new theImage inWidth inHeight RGB-IMAGE "Grad Dupe Layer" 100 SCREEN-MODE)))
	  (layer-color (car (gimp-layer-new theImage inWidth inHeight RGB-IMAGE "Color Layer" 100 OVERLAY-MODE)))
	  (point-num (+ 2 (* randomColor 2)))
	  (step (/ 255 (+ (* randomColor 2) 1)))
;	  (control_pts (cons-array (* point-num 2)))
	  (control_pts (cons-array (* point-num 2) 'byte))
	  (linflag 0)
	  (radflag 0)
	  (flag 0)
	  (fgColor '(0 0 0))
	  (bgColor '(0 0 0))
        )

	(gimp-context-push)

	(gimp-image-add-layer theImage baseLayer 0)
	(gimp-image-add-layer theImage gradLayer 0)
;	(gimp-image-add-layer theImage colorLayer 0)

; Show the new image.  TAH-DAH!
;	(gimp-display-new theImage)

		(gimp-context-set-background '(255 255 255))
		(gimp-context-set-foreground '(0 0 0))

		(plug-in-plasma RUN-NONINTERACTIVE
			theImage baseLayer (rand 4000000000) 15)

; Set the background and foreground colors to white and black

	(while (< radflag radGrad)

; Get a random foreground Color
		(set! fgColor (car (gimp-image-pick-color theImage baseLayer (rand inWidth) (rand inHeight) FALSE TRUE 1)))
		(gimp-context-set-foreground fgColor)

; Get a random background Color
		(set! bgColor (car (gimp-image-pick-color theImage baseLayer (rand inWidth) (rand inHeight) FALSE TRUE 1)))
		(gimp-context-set-foreground bgColor)

; (gimp-edit-blend drawable blend-mode paint-mode gradient-type opacity offset repeat reverse supersample max-depth threshold dither x1 y1 x2 y2)
; Do a difference radial gradient using two random colors for the foreground and background
	(gimp-edit-blend gradLayer FG-BG-RGB-MODE DIFFERENCE-MODE GRADIENT-RADIAL
		100 0 REPEAT-NONE FALSE FALSE 0 0 TRUE (rand inWidth) (rand inHeight) (rand inWidth) (rand inHeight))

; Increment the loop counter
		(set! radflag (+ radflag 1))
	)

	(while (< linflag linGrad)

; Get a random foreground Color
		(set! fgColor (car (gimp-image-pick-color theImage baseLayer (rand inWidth) (rand inHeight) FALSE FALSE 0)))
		(gimp-context-set-foreground fgColor)

; Get a random background Color
		(set! bgColor (car (gimp-image-pick-color theImage baseLayer (rand inWidth) (rand inHeight) FALSE FALSE 0)))
		(gimp-context-set-foreground bgColor)

; (gimp-edit-blend drawable blend-mode paint-mode gradient-type opacity offset repeat reverse supersample max-depth threshold dither x1 y1 x2 y2)
; Do a difference linear gradient using two random colors for the foreground and background
	(gimp-edit-blend gradLayer FG-BG-RGB-MODE DIFFERENCE-MODE GRADIENT-LINEAR
		100 0 REPEAT-NONE FALSE FALSE 0 0 TRUE (rand inWidth) (rand inHeight) (rand inWidth) (rand inHeight))

; Increment the loop counter
		(set! linflag (+ linflag 1))
	)

	(set! graddupeLayer (car (gimp-layer-copy gradLayer FALSE)))
	(gimp-image-add-layer theImage graddupeLayer -1)
	(gimp-drawable-set-name graddupeLayer "Grad Dupe Layer")
	(gimp-layer-set-mode graddupeLayer SCREEN-MODE)

	(set! colorLayer (car (gimp-image-merge-visible-layers theImage CLIP-TO-IMAGE)))
;	(gimp-image-add-layer theImage colorLayer -1)
	(gimp-drawable-set-name colorLayer "Color Layer")
	(gimp-layer-set-mode colorLayer NORMAL-MODE)
	(gimp-colorize colorLayer imHue imSat imLight)

	(set! grad2Layer (car (gimp-layer-copy colorLayer FALSE)))
	(gimp-image-add-layer theImage grad2Layer -1)
	(gimp-drawable-set-name grad2Layer "Grad Dupe Layer")

	(plug-in-gauss-rle2 1 theImage grad2Layer deform deform)
	(plug-in-emboss 1 theImage grad2Layer 30 45.0 20 1)

	(aset control_pts 0 0)
	(aset control_pts 1 0)

	(while (< flag randomColor)
		(aset control_pts (+ (* flag 4) 2) (* step (+ (* flag 2) 1)))
		(aset control_pts (+ (* flag 4) 3) (+ 128 contrast))
		(aset control_pts (+ (* flag 4) 4) (* step (+ (* flag 2) 2)))
		(aset control_pts (+ (* flag 4) 5) (- 128 contrast))
		(set! flag (+ flag 1))
	)

	(aset control_pts (- (* point-num 2) 2) 255)
	(aset control_pts (- (* point-num 2) 1) 255)
	(gimp-curves-spline grad2Layer VALUE-LUT (* point-num 2) control_pts)

	(gimp-palette-set-foreground chromeColor)
	(gimp-image-add-layer theImage layer-color -1)
	(gimp-edit-fill layer-color FG-IMAGE-FILL)

; Show the new image.  TAH-DAH!
	(gimp-display-new theImage)

   )
)

; Register the function with GIMP:

(script-fu-register
  "script-fu-plasmid"
  _"_Plasmid..."
  _"Creates something that looks like a plasmid under a microscope."
  "James Sambrook"
  "19 September 2008"
  "James Sambrook.  King George, VA, USA"
  ""
  SF-ADJUSTMENT _"Image Width"                '(640 300 3000 1 50 0 0)
  SF-ADJUSTMENT _"Image Height"               '(480 300 3000 1 50 0 0)
  SF-ADJUSTMENT _"Radial Gradients"           '(25 1 100 1 5 0 0)
  SF-ADJUSTMENT _"Linear Gradients"           '(25 1 100 1 5 0 0)
  SF-ADJUSTMENT _"Hue"                        '(220 0 360 1 20 0 0)
  SF-ADJUSTMENT _"Saturation"                 '(70 -100 100 1 10 0 0)
  SF-ADJUSTMENT _"Lightness"                  '(0 -100 100 1 10 0 0)
  SF-COLOR      _"Chrome Color"               '(30 90 225)
  SF-ADJUSTMENT _"Contrast for color curves"  '(97 0 127 1 10 0 0)
  SF-ADJUSTMENT _"Gaussian Blur Radius"       '(30 1 300 1 10 0 0)
  SF-ADJUSTMENT _"Randomness of colors"       '(4 1 7 1 1 0 1)

)

(script-fu-menu-register "script-fu-plasmid"
                         "<Image>/Filters/SambrookJM/")
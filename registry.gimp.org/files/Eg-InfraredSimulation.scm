;
; Infrared Simulation, V2.8
;
; Martin Egger (martin.egger@gmx.net)
; (C) 2012, Bern, Switzerland
;
; You can find more about simulating IR at
; http://www.gimpguru.org/Tutorials/SimulatedInfrared/
; http://epaperpress.com/psphoto/
; 
; This script was tested with Gimp 2.8
;
; New versions will be distributed from http://registry.gimp.org/ only
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses>.
;
; Define the function
;
(define (script-fu-Eg-Infrared InImage InLayer InOpacity InContrast InBlur InFlatten)
;
; Save history			
;
	(gimp-image-undo-group-start InImage)
;	
	(let*	(
		(CopyLayer (car (gimp-layer-copy InLayer TRUE)))
		(IRLayer (car (gimp-layer-copy InLayer TRUE)))
		(IRLayer2 (car (gimp-layer-copy InLayer TRUE)))
		(IRLayer3 (car (gimp-layer-copy InLayer TRUE)))
		)
		(gimp-image-insert-layer InImage CopyLayer 0 -1)
;
		(gimp-image-insert-layer InImage IRLayer3 0 -1)
		(gimp-layer-set-mode IRLayer3 SCREEN-MODE)
		(gimp-layer-set-opacity IRLayer3 InContrast)
		(gimp-image-insert-layer InImage IRLayer2 0 -1)
		(gimp-layer-set-mode IRLayer2 BURN-MODE)
		(gimp-layer-set-opacity IRLayer2 InContrast)
		(gimp-image-insert-layer InImage IRLayer 0 -1)
		(gimp-layer-set-mode IRLayer SCREEN-MODE)
		(gimp-layer-set-opacity IRLayer InOpacity)
;
; Dreamy IR
;
		(gimp-desaturate CopyLayer)
		(gimp-desaturate IRLayer3)
		(plug-in-colors-channel-mixer TRUE InImage IRLayer2 TRUE 1.0 1.0 -1.0 0 0 0 0 0 0)
		(plug-in-colors-channel-mixer TRUE InImage IRLayer TRUE -.3 2.0 -.7 0 0 0 0 0 0)
		(gimp-brightness-contrast IRLayer -25 70)
;
; Blur the IR layer, if we need to
;
		(if (= InBlur TRUE) (plug-in-gauss TRUE InImage IRLayer 4.0 4.0 TRUE))
;
; Flatten the image, if we need to
;
		(cond
			((= InFlatten TRUE) 
				(begin
					(gimp-image-merge-down InImage CopyLayer CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage IRLayer3 CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage IRLayer2 CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage IRLayer CLIP-TO-IMAGE)
				)
			)
;
			((= InFlatten FALSE) 
				(begin
					(gimp-item-set-name IRLayer "Infrared (Green)")
					(gimp-item-set-name IRLayer2 "Infrared (Sky Contrast)")
					(gimp-item-set-name IRLayer3 "Infrared (Green Glow)")
					(gimp-item-set-name CopyLayer "BW-Copy")
					(gimp-image-set-active-layer InImage InLayer)
				)
			)
		)
	)
;
; Finish work
;
	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
)
;
(script-fu-register
	"script-fu-Eg-Infrared"
	_"_Infrared Simulation"
	"Simulate a B&W Infrared image"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"28.02.2012"
	"RGB*"
	SF-IMAGE	"The Image"	0
	SF-DRAWABLE	"The Layer"	0
	SF-ADJUSTMENT	"IR Layer Opacity" '(80.0 1.0 100.0 1.0 0 2 0)
	SF-ADJUSTMENT	"Contrast/Glow Layer Opacity" '(50.0 1.0 100.0 1.0 0 2 0)
	SF-TOGGLE	"Blur Image"	FALSE
	SF-TOGGLE	"Flatten Image"	FALSE
)
;
(script-fu-menu-register "script-fu-Eg-Infrared"
			 "<Image>/Filters/Eg")
;

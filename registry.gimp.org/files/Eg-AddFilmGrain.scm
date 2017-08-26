;
; Add film grain, 2.8
;
; Martin Egger (martin.egger@gmx.net)
; (C) 2012, Bern, Switzerland
;
; You can find more about adding realistic film grain to BW images at
; http://www.outbackphoto.com/workflow/wf_95/essay.html
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
(define (script-fu-Eg-AddFilmGrain InImage InLayer InFlatten InShadCol InShadRad InMid1Blur InMid2Blur InShad1Blur InShad2Blur)
;
; Save history			
;
	(gimp-image-undo-group-start InImage)
	(if (= (car (gimp-drawable-is-rgb InLayer)) FALSE ) (gimp-image-convert-rgb InImage))
	(gimp-context-set-foreground '(128 128 128))
;
	(let*	(
		(Midtone1Layer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage))  RGBA-IMAGE "Noise 1 - midtones" 80 OVERLAY-MODE)))
		(Midtone2Layer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage))  RGBA-IMAGE "Noise 2 - midtones" 80 OVERLAY-MODE)))
		(ShadHL1Layer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage))  RGBA-IMAGE "Noise 1 - shadows" 80 OVERLAY-MODE)))
		(ShadHL2Layer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage))  RGBA-IMAGE "Noise 2 - shadows" 80 OVERLAY-MODE)))
		)
		(gimp-drawable-fill Midtone1Layer FOREGROUND-FILL)
		(gimp-drawable-fill Midtone2Layer FOREGROUND-FILL)
		(gimp-drawable-fill ShadHL1Layer FOREGROUND-FILL)
		(gimp-drawable-fill ShadHL2Layer FOREGROUND-FILL)
		(gimp-image-insert-layer InImage Midtone1Layer 0 -1)
		(gimp-image-insert-layer InImage Midtone2Layer 0 -1)
		(gimp-image-insert-layer InImage ShadHL1Layer 0 -1)
		(gimp-image-insert-layer InImage ShadHL2Layer 0 -1)
;
		(plug-in-hsv-noise TRUE InImage Midtone1Layer 2 0 0 100)
		(plug-in-hsv-noise TRUE InImage Midtone2Layer 2 0 0 100)
;
		(gimp-context-set-antialias TRUE)
		(gimp-context-set-feather TRUE)
		(gimp-context-set-feather-radius 3 3)
		(gimp-context-set-sample-merged TRUE)
		(gimp-image-select-color InImage CHANNEL-OP-REPLACE InLayer InShadCol)
		(plug-in-hsv-noise TRUE InImage ShadHL1Layer 2 0 0 100)
		(plug-in-hsv-noise TRUE InImage ShadHL2Layer 2 0 0 100)
		(gimp-selection-none InImage)
;
		(plug-in-gauss TRUE InImage Midtone1Layer InMid1Blur InMid1Blur TRUE)
		(plug-in-gauss TRUE InImage Midtone2Layer InMid2Blur InMid2Blur TRUE)
		(plug-in-gauss TRUE InImage ShadHL1Layer InShad1Blur InShad1Blur TRUE)
		(plug-in-gauss TRUE InImage ShadHL2Layer InShad2Blur InShad2Blur TRUE)
;		
; Flatten the image, if we need to
;
		(cond
			((= InFlatten TRUE) 
				(begin
					(gimp-image-merge-down InImage Midtone1Layer CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage Midtone2Layer CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage ShadHL1Layer CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage ShadHL2Layer CLIP-TO-IMAGE)
				)
			)
			((= InFlatten FALSE) 
				(begin
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
;
)
;
(script-fu-register 
	"script-fu-Eg-AddFilmGrain"
	_"Add film grain"
	"Add realistic film grain to BW images"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"29.02.2012"
	"RGB* GRAY*"
	SF-IMAGE	"The Image"		0
	SF-DRAWABLE	"The Layer"		0
	SF-TOGGLE	"Flatten Image"	FALSE
	SF-COLOR	"Shadow Color"	'(35 35 35)
	SF-ADJUSTMENT	"Shadow Selection Range"	'(50.0 1.0 100.0 1.0 0 2 0)
	SF-ADJUSTMENT	"Midtone 1 Blur Radius"	'(3.0 0.5 50.0 0.5 0 2 0)
	SF-ADJUSTMENT	"Midtone 2 Blur Radius"	'(1.5 0.5 10.0 0.5 0 2 0)
	SF-ADJUSTMENT	"Shadow 1 Blur Radius"	'(3.0 0.5 50.0 0.5 0 2 0)
	SF-ADJUSTMENT	"Shadow 2 Blur Radius"	'(1.5 0.5 10.0 0.5 0 2 0)
)
;
(script-fu-menu-register "script-fu-Eg-AddFilmGrain"
			 "<Image>/Filters/Eg")
;

;octave-sharpening.scm
;
; by Andreas Schönfelder
;
; found at http://meetthegimp.org/episode-127-octave-sharpening/
; more information at http://www.varis.com/StepByStep/sharpen/Sharpen.html
;
; Version 1.0 (20091209)
;
; License:
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
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html


(define (script-fu-octave-sharpening img drw)
	(let*
		(
			(layer1 0)
			(layer2 0)
			(layer3 0)
			(layer4 0)
			(layer_os 0)
		)
		
		; start
		(gimp-context-push)
		(gimp-image-undo-group-start img)

		; copy layer 4 times
		(set! layer1 (car (gimp-layer-copy drw FALSE)))
		(set! layer2 (car (gimp-layer-copy drw FALSE)))
		(set! layer3 (car (gimp-layer-copy drw FALSE)))
		(set! layer4 (car (gimp-layer-copy drw FALSE)))
	
		; set opacity of layer
		(gimp-layer-set-opacity layer1 12.5)
		(gimp-layer-set-opacity layer2 25.0)
		(gimp-layer-set-opacity layer3 50.0)
		(gimp-layer-set-opacity layer4 100.0)
	
		; add created layers
		(gimp-image-add-layer img layer1 0)
		(gimp-image-add-layer img layer2 1)
		(gimp-image-add-layer img layer3 2)
		(gimp-image-add-layer img layer4 3)
	
		; unsharp masking layers
		(plug-in-unsharp-mask TRUE img layer1 4.0 5.0 0)
		(plug-in-unsharp-mask TRUE img layer2 2.0 5.0 0)
		(plug-in-unsharp-mask TRUE img layer3 1.0 5.0 0)
		(plug-in-unsharp-mask TRUE img layer4 0.5 5.0 0)
	
		; copy temp layers
		(set! layer_os (car (gimp-layer-new-from-visible img img "Octave Sharpening")))
	
		; delete temp layers
		(gimp-image-remove-layer img layer1)
		(gimp-image-remove-layer img layer2)
		(gimp-image-remove-layer img layer3)
		(gimp-image-remove-layer img layer4)
	
		; add octave layer
		(gimp-image-add-layer img layer_os 0)
		
		; set layer mode
		(gimp-layer-set-mode layer_os SATURATION-MODE)
	
		; done
		(gimp-image-undo-group-end img)
		(gimp-displays-flush)
		(gimp-context-pop)
	)
)
	
(script-fu-register "script-fu-octave-sharpening"
	"<Image>/Filters/Enhance/Octave Sharpening"
	"Octave Sharpening - a special technique for intense sharpening"
	"Andreas Schönfelder <passtschu at freenet dot de>"
	"Andreas Schönfelder <passtschu at freenet dot de>"
	"2009-12-09"
	"*"
	SF-IMAGE      "image"      0
	SF-DRAWABLE   "drawable"   0
)

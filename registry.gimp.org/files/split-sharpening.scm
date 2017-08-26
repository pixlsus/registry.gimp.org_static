;split-sharpening.scm
;
; by Andreas Schönfelder
;
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


(define (script-fu-split-sharpening img drw lightenmode darkenmode)
	(let*
		(
			(layer_lighten 0)
			(layer_darken 0)
		)
		
		; start
		(gimp-context-push)
		(gimp-image-undo-group-start img)

		; copy layer 2 times
		(set! layer_lighten (car (gimp-layer-copy drw FALSE)))
		(set! layer_darken (car (gimp-layer-copy drw FALSE)))
	
		; add created layers
		(gimp-image-add-layer img layer_lighten 0)
		(gimp-image-add-layer img layer_darken 1)
		
		; rename layers
		(gimp-drawable-set-name layer_lighten "Lighten Sharpening")
		(gimp-drawable-set-name layer_darken "Darken Sharpening")
		
		; set opacity of layer
		(gimp-layer-set-opacity layer_lighten 50)
		(gimp-layer-set-opacity layer_darken 50)
		
		; set layer mode
		(gimp-layer-set-mode layer_lighten LIGHTEN-ONLY-MODE)
		(gimp-layer-set-mode layer_darken DARKEN-ONLY-MODE)
	
		; apply modes
		(if (= lightenmode 1)
			(begin
				(plug-in-unsharp-mask TRUE img layer_lighten 5.0 1.0 5)
			)
		)
		(if (= lightenmode 2)
			(begin
				(plug-in-unsharp-mask TRUE img layer_lighten 5.0 4.0 15)
			)
		)
		(if (= darkenmode 1)
			(begin
				(plug-in-unsharp-mask TRUE img layer_darken 5.0 1.0 5)
			)
		)
		(if (= darkenmode 2)
			(begin
				(plug-in-unsharp-mask TRUE img layer_darken 5.0 4.0 15)
			)
		)
	
		; done
		(gimp-image-undo-group-end img)
		(gimp-displays-flush)
		(gimp-context-pop)
	)
)
	
(script-fu-register "script-fu-split-sharpening"
	"<Image>/Filters/Enhance/Split Sharpening"
	"Split Sharpening - making Lighten and Darken sharpening layers"
	"Andreas Schönfelder <passtschu at freenet dot de>"
	"Andreas Schönfelder <passtschu at freenet dot de>"
	"2009-12-09"
	"*"
	SF-IMAGE	"image"      0
	SF-DRAWABLE	"drawable"   0
	SF-OPTION	"Lighten Mode" '("low radius / high amount" "high radius / low amount")
	SF-OPTION	"Darken Mode" '("low radius / high amount" "high radius / low amount")
)

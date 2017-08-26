;highpass-sharpening.scm
;
; by Andreas Schönfelder
;
; more information at http://www.varis.com/StepByStep/sharpen/Sharpen.html
; high pass basing on http://registry.gimp.org/node/7385
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


(define (script-fu-highpass-sharpening img drw)
	(let*
		(
			(layer_blur 0)
			(layer_grey 0)
			(boost (/ (* 128 (+ 100 0)) 200))
			(radius 10)
		)

		; start
		(gimp-context-push)
		(gimp-image-undo-group-start img)

		; copy layer
		(set! layer_grey (car (gimp-layer-copy drw FALSE)))
		
		; add layer
		(gimp-image-add-layer img layer_grey -1)

		; desaturate
		(gimp-desaturate layer_grey)

		; copy grey layer
		(set! layer_blur (car (gimp-layer-copy layer_grey FALSE)))
		
		; add layer
		(gimp-image-add-layer img layer_blur -1)

		; blur
		(plug-in-gauss-rle 1 img layer_blur radius 1 1)

		; invert
		(gimp-invert layer_blur)
		
		; set opacity
		(gimp-layer-set-opacity layer_blur 50)
		
		; merge down
		(set! layer_grey (car (gimp-image-merge-down img layer_blur 0)))

		; boost contrast by pulling down the high and low ends the same amount
		(gimp-levels layer_grey HISTOGRAM-VALUE boost (- 255 boost) 1 0 255)
		
		; apply curves
		(gimp-curves-spline layer_grey HISTOGRAM-VALUE 10 #(95 0 127 128 154 184 222 240 255 255))

		; set value mode
		(gimp-layer-set-mode layer_grey OVERLAY-MODE)

		;done
		(gimp-image-undo-group-end img)
		(gimp-displays-flush)
		(gimp-context-pop)
	)
)
	
(script-fu-register "script-fu-highpass-sharpening"
	"<Image>/Filters/Enhance/High Pass Sharpening"
	"High Pass Sharpening - Overlay mode with High Pass filter"
	"Andreas Schönfelder <passtschu at freenet dot de>"
	"Andreas Schönfelder <passtschu at freenet dot de>"
	"2009-12-09"
	"*"
	SF-IMAGE	"image"      0
	SF-DRAWABLE	"drawable"   0
)

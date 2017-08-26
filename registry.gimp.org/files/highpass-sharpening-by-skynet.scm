;highpass-sharpening.scm
;
; by SkYNeT (this is my first script for The Gimp)
;
; Version 1.0 (20110826)
;
; This technique is described by Rolf Steinort
; in episode 164 of the Meet the Gimp! video podcast.
;
; License:
;
; CC BY-SA 3.0
;
; You are free:
;
;    to Share — to copy, distribute and transmit the work
;    to Remix — to adapt the work
;    to make commercial use of the work
;
; Under the following conditions:
;
;    Attribution — You must attribute the work in the manner specified by the author or licensor
; (but not in any way that suggests that they endorse you or your use of the work).
;
;    Share Alike — If you alter, transform, or build upon this work, you may distribute the resulting
; work only under the same or similar license to this one. 
;
;http://creativecommons.org/licenses/by-sa/3.0/


(define (script-fu-highpass-sharpening img drw radius)
	(let*
		(
			(layer_highpass 0)
			(layer_invert 0)
			(layer_sharpened 0)
		)

		; start
		(gimp-context-push)
		(gimp-image-undo-group-start img)

		; Kopira sliku u novi lejer i daje ime  "Layer Highpass"
		(set! layer_highpass (car (gimp-layer-copy drw FALSE)))
		(gimp-image-add-layer img layer_highpass -1)
		(gimp-layer-set-name layer_highpass "Layer Highpass")
		
		; Kopira sliku u novi lejer i daje ime  "Layer Invert"
		(set! layer_invert (car (gimp-layer-copy drw FALSE)))
		(gimp-image-add-layer img layer_invert -1)
		(gimp-layer-set-name layer_invert "Layer Invert")

		; Negativ
		(gimp-invert layer_invert)

		; Layer grain merge mode
		(gimp-layer-set-mode layer_invert GRAIN-MERGE-MODE)

		; Blur
		(plug-in-gauss-rle 1 img layer_invert radius 1 1)

		; Merge down
		(set! layer_highpass (car (gimp-image-merge-down img layer_invert CLIP-TO-BOTTOM-LAYER)))

		; Layer value mode
		(gimp-layer-set-mode layer_highpass OVERLAY-MODE)

		; New from visible
		(set! layer_sharpened (car (gimp-layer-new-from-visible img img "Highpass Sharpened")))
		(gimp-image-add-layer img layer_sharpened -1)

		; Brise nepotrebni Highpass layer
		(gimp-image-remove-layer img layer_highpass)

		; Done
		(gimp-image-undo-group-end img)
		(gimp-displays-flush)
		(gimp-context-pop)
	)
)
	
(script-fu-register "script-fu-highpass-sharpening"
	"<Image>/Filters/SkYNeT/Enhance/High Pass Sharpening"
	"High Pass Sharpening"
	"SkYNeT"
	"SkYNeT"
	"2011-08-26"
	"*"
	SF-IMAGE	"image"      0
	SF-DRAWABLE	"drawable"   0
	SF-ADJUSTMENT   "radius"     '(10 1 500 1 10 0 1)
)

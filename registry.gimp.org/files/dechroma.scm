; dechroma.scm
; remove chroma noise
; Script-fu for Gimp
; (c) Jean-Pierre Bucciol
; Published under GPL version 2
; Oct 6, 7, 9, 2008

(script-fu-register
	"script-fu-dechroma"
	"<Image>/Filters/jp/dechroma"
	"Remove chroma noise."
	"Jean-Pierre Bucciol <jpsspam(at)free.fr>"
	"(c) Jean-Pierre Bucciol. Published under GPL version 2."
	"Oct 9 2008"
	"RGB*"
	SF-IMAGE "Image" 0
	SF-DRAWABLE "Drawable" 0
	SF-ADJUSTMENT "Green/Red Blur Radius" '(15 0 100 1 5 0 0)
	SF-ADJUSTMENT "Blue/Yellow Blur Radius" '(15 0 100 1 5 0 0)
)

(define (script-fu-dechroma image drawable redBlurRadius blueBlurRadius)

	; begin undo group
	(gimp-undo-push-group-start image)

	; decompose image to LAB
	(define imageLAB (car (plug-in-decompose 1 image drawable "LAB" 1)))
	
	; define layers A & B
	(define layersLAB (gimp-image-get-layers imageLAB))
	(define layerA (aref (cadr layersLAB) 1))
	(define layerB (aref (cadr layersLAB) 2))
	
	; blur layers A & B
	(if (> redBlurRadius 0)
		(begin
			(plug-in-gauss 1 imageLAB layerA redBlurRadius redBlurRadius 0)
		)
	)
	(if (> blueBlurRadius 0)
		(begin
			(plug-in-gauss 1 imageLAB layerB blueBlurRadius blueBlurRadius 0)
		)
	)
	
	; recompose image
	(plug-in-recompose 1 imageLAB layerA)
	
	; remove temporary imageLAB
	(gimp-image-delete imageLAB)
	
	; update image window
	(gimp-displays-flush)
		
	; end undo group
	(gimp-undo-push-group-end image)
		
)

; Adapted from discussions:
; http://www.flickr.com/groups/e510/discuss/72157606554624290/
; http://www.flickr.com/groups/gimpusers/discuss/72157606496961906/

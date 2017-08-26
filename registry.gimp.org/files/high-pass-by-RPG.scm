(define (high-pass img drawable mode)
	(let*
		(
			(high-pass-layer 0)
			(img-w (car (gimp-image-width img)))
			(img-h (car (gimp-image-height img)))
		)
		(gimp-undo-push-group-start img)
		(set! high-pass-layer (car (gimp-layer-new-from-visible img img "High pass filter")))
		(gimp-image-add-layer img high-pass-layer 0)
		(gimp-desaturate-full high-pass-layer DESATURATE-LUMINOSITY)
		(plug-in-gauss 1 img high-pass-layer (/ img-w 5) (/ img-h 5) 0)
		(if (= mode 0)
			(gimp-layer-set-mode high-pass-layer GRAIN-EXTRACT-MODE)
			(begin
				(gimp-layer-set-mode high-pass-layer SOFTLIGHT-MODE)
				(gimp-invert high-pass-layer)
			)
		)
		(gimp-undo-push-group-end img)
		(gimp-displays-flush)
	)
)

(script-fu-register "high-pass"
        		    "<Image>/Filters/Enhance/_High Pass Filter..."
                    "High Pass Filter - normalizes lightness of image"
                    "Pavel aka RPG Roshchin <rpg89@post.ru>"
                    "Pavel aka RPG Roshchin"
                    "2011"
                    "RGB* GRAY*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-OPTION		"Mode"		'("Grain extract" "Soft light")
)
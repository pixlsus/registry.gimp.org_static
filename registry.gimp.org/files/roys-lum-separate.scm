;
; Separate Luminance from Chrominance, a Gimp script by Roy Johnson
;
; Create New From Visible
; Desaturate using Luminance
; Set mode to Grain Extract
; Create New From Visible
; If "Color Spiff" is selected:
;  Auto->Color Enhance
;  Duplicate Layer
;  Desaturate using Luminance
;  Set mode to Grain Extract
;  Merge down
; Set mode to Grain Merge

(define	(script-fu-separate-luminance	theImage
					doEnhance
					excessSeparate
	)
    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    (let ((lum-layer (car (gimp-layer-new-from-visible theImage theImage "Lum"))))
	(gimp-image-add-layer theImage lum-layer -1)
	(gimp-image-raise-layer-to-top theImage lum-layer)
	(gimp-desaturate-full lum-layer DESATURATE-LUMINOSITY)
	(gimp-layer-set-mode lum-layer GRAIN-EXTRACT-MODE)
	;; This is necessary for new-from-visible to work right
	(gimp-edit-copy-visible theImage)
	(let ((color-layer (car (gimp-layer-new-from-visible theImage theImage "Chroma"))))
	    (gimp-image-add-layer theImage color-layer -1)
	    (gimp-layer-set-mode color-layer GRAIN-MERGE-MODE)
	    (gimp-layer-set-mode lum-layer NORMAL-MODE)
	    ;; Check for color clipping
	    (gimp-edit-copy-visible theImage)
	    (let ((check-layer (car (gimp-layer-new-from-visible theImage theImage "Extra Color")))
		 )
		 (gimp-image-add-layer theImage check-layer -1)
		 (gimp-image-raise-layer-to-top theImage check-layer)
		 (gimp-layer-set-mode check-layer GRAIN-EXTRACT-MODE)
		 (gimp-layer-set-visible lum-layer FALSE)
		 (gimp-layer-set-visible color-layer FALSE)
		 (gimp-image-set-active-layer theImage check-layer)
		 (gimp-edit-copy-visible theImage)
		 (gimp-floating-sel-anchor (car (gimp-edit-paste check-layer 0)))
		 (gimp-layer-set-mode check-layer GRAIN-MERGE-MODE)
		 (gimp-layer-set-visible lum-layer TRUE)
		 (if (= FALSE excessSeparate)
		     (set! lum-layer (car (gimp-image-merge-down theImage check-layer 0)))
	         )
		 (gimp-layer-set-visible color-layer TRUE)
	    )
	    (if (< 0 doEnhance)
		(begin
		  (gimp-image-set-active-layer theImage color-layer)
		  (plug-in-color-enhance 0 theImage color-layer)
		  (let ((lum-extract (car (gimp-layer-copy color-layer FALSE))))
		    (gimp-image-add-layer theImage lum-extract -1)
		    (gimp-desaturate-full lum-extract DESATURATE-LUMINOSITY)
		    (gimp-layer-set-mode lum-extract GRAIN-EXTRACT-MODE)
		    (set! color-layer (car (gimp-image-merge-down theImage lum-extract 0)))
		    (gimp-layer-set-mode color-layer GRAIN-MERGE-MODE)
		  )
		)
	     )
	)
    )
    (gimp-displays-flush)

    ;End the undo group
    (gimp-image-undo-group-end theImage)
)

(script-fu-register "script-fu-separate-luminance"
            _"<Image>/FX-Foundry/Color/Separate Luminance..."
            "Separate luminance and color into layers, and optionally enhance the color"
            "Roy Johnson"
            "2009, Roy Johnson"
            "June 2009"
            "RGB*"
            SF-IMAGE		"Image"		0
	    SF-TOGGLE		_"Color Spiff"	TRUE
	    SF-TOGGLE		_"Excess color on separate layer" FALSE
)
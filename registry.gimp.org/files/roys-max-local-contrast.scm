;
; roys-max-local-contrast, a Gimp script by Roy Johnson
;
; Create New From Visible
; Desaturate using Luminance
; Create two new copies, one called Min and one Max
; For each gray value, 0 to 255:
;   color-select from the desaturated image
;   grow selection by Radius parameter and feather it heavily
;   paste selection in Darken-Only mode on Min
;   paste selection in Lighten-Only mode on Max
; Compute (Base-Min)/(Max-Min)
; Grain-Extract Base from that result to get Grain Merge layer

(define	(script-fu-max-loc-contrast	theImage
					theDrawable
					Radius-pct
					myThresh
					feather-factor
					keepMinMaxLayers
	)
    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    ; Copy the visible image to new layer and desaturate it
    (let* (
	   (base-layer (car (gimp-layer-new-from-visible theImage theImage "Base")))
	   (base-copy  ())
           (width (car (gimp-drawable-width theDrawable)))
	   (height (car (gimp-drawable-height theDrawable)))
	   (scaledown-factor (+ 1 (round (/ (min width height) 300)) ))
	   (new-width (round (/ width scaledown-factor)))
	   (new-height (round (/ height scaledown-factor)))
	  )
	(gimp-image-add-layer theImage base-layer -1)
	(gimp-image-raise-layer-to-top theImage base-layer)

	(if (< 0 (car (gimp-drawable-is-rgb base-layer)))
	  (gimp-desaturate-full base-layer DESATURATE-LUMINOSITY)
	)
	(set! base-copy (car (gimp-layer-copy base-layer FALSE)))
	(if (> scaledown-factor 1)
	    (begin
		(gimp-layer-scale base-layer new-width new-height FALSE)
		(gimp-layer-resize base-layer new-width new-height 0 0)
	    )
	)
	(let (
	      (min-layer (car (gimp-layer-copy base-layer 1)))
	      (max-layer (car (gimp-layer-copy base-layer 1)))
	      (theRadius (/ (* Radius-pct (max width height)) (* scaledown-factor 100)))
	     )
	    (gimp-drawable-set-name min-layer "Min")
	    (gimp-image-add-layer theImage min-layer -1)
	    (gimp-drawable-set-name max-layer "Max")
	    (gimp-image-add-layer theImage max-layer -1)

	    ;Loop through the colors, mapping local Max and Min values
	    (letrec ((loop (lambda (i max)
			     (let* (
				    ;; Weird: feathering > theRadius mutes haloes.
				    (feather-pixels (* theRadius feather-factor))
				    (grayval (+ i myThresh))
				    (grayRGB (list grayval grayval grayval))
				   )
			       (gimp-selection-none theImage)
			       (gimp-by-color-select base-layer grayRGB myThresh CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
			       (if (= TRUE (car (gimp-selection-bounds theImage)))
				   (begin
				    (gimp-selection-grow theImage theRadius)
				    (gimp-selection-feather theImage feather-pixels)
				    (gimp-context-set-foreground grayRGB)
				    (gimp-edit-bucket-fill max-layer FG-BUCKET-FILL LIGHTEN-ONLY-MODE 100 0 0 0 0)
				    (gimp-edit-bucket-fill min-layer FG-BUCKET-FILL DARKEN-ONLY-MODE 100 0 0 0 0)
				   )
			       )
			       (gimp-progress-update (/ i 260))
			       (if (< (+ i myThresh myThresh) max) (loop (+ i myThresh myThresh) max))
			     )
			   )
		    ))
		    (loop 0 255)
	    )
	    (gimp-selection-none theImage)

	    ; Now that we have Max and Min, scale them back up
	    (if (> scaledown-factor 1)
		(begin
		  (gimp-layer-resize min-layer new-width new-height 0 0)
		  (gimp-layer-scale min-layer width height FALSE)
		  (gimp-layer-resize max-layer new-width new-height 0 0)
		  (gimp-layer-scale max-layer width height FALSE)
		)
	    )

	    (gimp-image-raise-layer-to-top theImage min-layer)
	    (gimp-layer-set-mode min-layer SUBTRACT-MODE)
	    ;; This is necessary for new-from-visible to work right
	    (gimp-edit-copy-visible theImage)
	    (let (
		  (max-min-layer (car (gimp-layer-new-from-visible theImage theImage "Max-Min")))
		 )
		(gimp-image-add-layer theImage max-min-layer -1)

		(gimp-drawable-set-visible max-min-layer FALSE)
		(gimp-drawable-set-visible max-layer FALSE)
		(gimp-layer-set-mode max-min-layer DIVIDE-MODE)
		;; Put the base-copy (unscaled) on top of base-layer
		(gimp-image-set-active-layer theImage base-layer)
		(gimp-image-add-layer theImage base-copy -1)
		(gimp-image-remove-layer theImage base-layer)
		;; Again, for new-from-visible to work right
		(gimp-edit-copy-visible theImage)
		(let (
		      (base-min-layer (car (gimp-layer-new-from-visible theImage theImage "Base-Min")))
		     )
		    (gimp-displays-flush)
		    (gimp-image-add-layer theImage base-min-layer -1)
		    (gimp-image-raise-layer-to-top theImage base-min-layer)
		)
		(gimp-image-raise-layer-to-top theImage max-min-layer)
		(gimp-drawable-set-visible max-min-layer TRUE)
		(if (= TRUE keepMinMaxLayers)
		  ;; just make them normal/invisible
		  (begin (gimp-layer-set-mode min-layer NORMAL-MODE)
			 (gimp-drawable-set-visible min-layer FALSE)
		  )
		  ;; else delete Min and Max layers
		  (begin (gimp-image-remove-layer theImage min-layer)
			 (gimp-image-remove-layer theImage max-layer)
		  )
		)
		(let ((layer-mask (car (gimp-layer-create-mask base-copy WHITE-MASK))))
		  (gimp-edit-copy max-min-layer)
		  (gimp-drawable-set-name (car (gimp-image-merge-down theImage max-min-layer 2))
			"Contrast Enhanced")
		  (gimp-image-raise-layer-to-top theImage base-copy)
		  (gimp-layer-set-mode base-copy GRAIN-EXTRACT-MODE)
		  (set! base-copy (car (gimp-image-merge-down theImage base-copy 2)))
		  (gimp-layer-set-mode base-copy GRAIN-MERGE-MODE)
		  (gimp-layer-add-mask base-copy layer-mask)
		  (gimp-floating-sel-anchor (car (gimp-edit-paste layer-mask 0)))
		)
	    )
	)
    )
    (gimp-displays-flush)

    ;End the undo group
    (gimp-image-undo-group-end theImage)
)

(script-fu-register "script-fu-max-loc-contrast"
            _"<Image>/FX-Foundry/Photo/Enhancement/Maximize Local Contrast..."
            "Stretch contrast based on local pixel maximum and minimum values"
            "Roy Johnson"
            "2009, Roy Johnson"
            "June 2009"
            "RGB* GREY"
            SF-IMAGE		"Image"		0
            SF-DRAWABLE         "Drawable"      0
            SF-ADJUSTMENT	_"Radius as % of image size"	'(4 1 5  0.1 0.5 1 0)
	    SF-ADJUSTMENT	_"Threshold"	'(4  1 10 1 1 0 0)
	    SF-ADJUSTMENT	_"Feather haloes"	'(8  1  20 1 1 0 0)
	    SF-TOGGLE		_"Keep Min and Max Layers"	FALSE
)
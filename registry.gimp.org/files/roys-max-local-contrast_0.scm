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
					feather-factor
					scaledown-factor
					keepMinMaxLayers
	)
    ;Start an undo group so the process can be undone with one undo
    (gimp-context-push)
    (gimp-image-undo-group-start theImage)

    (gimp-progress-init "Maximize Local Contrast" -1)

    ; Copy the visible image to new layer and desaturate it
    (let* (
	   (base-layer (car (gimp-layer-new-from-visible theImage theImage "Base")))
           (width (car (gimp-drawable-width theDrawable)))
	   (height (car (gimp-drawable-height theDrawable)))
	   (dr-type (car (gimp-drawable-type theDrawable)))
	   (smwidth (/ width scaledown-factor))
	   (smheight (/ height scaledown-factor))
	   (base-copy ())
	  )
	(gimp-image-add-layer theImage base-layer -1)
	(gimp-image-raise-layer-to-top theImage base-layer)

	(if (< 0 (car (gimp-drawable-is-rgb base-layer)))
	  (gimp-desaturate-full base-layer DESATURATE-LUMINOSITY)
	)

        ; Do our selecting on a scaled-down original
	(set! base-copy (car (gimp-layer-copy base-layer FALSE)))
	(gimp-drawable-set-name base-copy "Scaled Original")
	(gimp-image-add-layer theImage base-copy -1)
	(gimp-image-raise-layer-to-top theImage base-copy)
	(gimp-layer-scale base-copy smwidth smheight FALSE)

	(let (
	      (min-layer (car (gimp-layer-new theImage smwidth smheight dr-type "Min" 100 NORMAL-MODE)))
	      (max-layer (car (gimp-layer-new theImage smwidth smheight dr-type "Max" 100 NORMAL-MODE)))
	      (theRadius (* (/ Radius-pct 100) (max smwidth smheight)) )
	      (myChannel (car (gimp-channel-new-from-component theImage GRAY-CHANNEL "Value")))
	     )
	    ; Fill Min with white, Max with black
	    (gimp-image-add-layer theImage min-layer -1)
	    (gimp-edit-fill min-layer WHITE-FILL)
	    (gimp-drawable-set-visible min-layer FALSE)

	    (gimp-image-add-layer theImage max-layer -1)
	    (gimp-context-set-foreground '(0 0 0))
	    (gimp-edit-fill max-layer FOREGROUND-FILL)
	    (gimp-drawable-set-visible max-layer FALSE)


	    ; On base-copy layer, Channel to Selection, then fill min with black
	    (gimp-selection-combine myChannel CHANNEL-OP-REPLACE)
	    (gimp-selection-invert theImage)
	    (gimp-rect-select theImage 0 0 smwidth smheight
		CHANNEL-OP-INTERSECT FALSE 0 )
	    (gimp-selection-grow theImage theRadius)
	    (gimp-selection-feather theImage (* theRadius feather-factor))
	    (gimp-edit-bucket-fill min-layer FG-BUCKET-FILL NORMAL-MODE
		100 0 FALSE 0 0)

	    ; Same series, inverting color and selection for max
	    (gimp-selection-combine myChannel CHANNEL-OP-REPLACE)
	    (gimp-rect-select theImage 0 0 smwidth smheight
		CHANNEL-OP-INTERSECT FALSE 0 )
	    (gimp-selection-grow theImage theRadius)
	    (gimp-selection-feather theImage (* theRadius feather-factor))
	    (gimp-context-set-background '(255 255 255))
	    (gimp-edit-bucket-fill max-layer BG-BUCKET-FILL NORMAL-MODE
		100 0 FALSE 0 0)
	    (gimp-selection-none theImage)

	    ; Scale them back up (if necessary)
	    (gimp-layer-scale min-layer width height FALSE)
	    (gimp-layer-scale max-layer width height FALSE)
	    (gimp-image-remove-layer theImage base-copy)

	    ; Now make them visible as we construct max-min
	    (gimp-drawable-set-visible min-layer TRUE)
	    (gimp-drawable-set-visible max-layer TRUE)

	    (gimp-image-raise-layer-to-top theImage max-layer)
	    (gimp-image-raise-layer-to-top theImage min-layer)
	    (gimp-layer-set-mode min-layer SUBTRACT-MODE)
	    ;; This is necessary for new-from-visible to work right
	    (gimp-edit-copy-visible theImage)
	    (let (
		  (max-min-layer (car (gimp-layer-new-from-visible theImage theImage "Max-Min")))
		 )
		(gimp-image-add-layer theImage max-min-layer -1)
		(gimp-image-raise-layer-to-top theImage max-min-layer)

		(gimp-drawable-set-visible max-min-layer FALSE)
		(gimp-drawable-set-visible max-layer FALSE)
		(gimp-layer-set-mode max-min-layer DIVIDE-MODE)
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
		(let ((layer-mask (car (gimp-layer-create-mask base-layer WHITE-MASK))))
		  (gimp-edit-copy max-min-layer)
		  (gimp-drawable-set-name (car (gimp-image-merge-down theImage max-min-layer 2))
			"Contrast Enhanced")
		  (gimp-image-raise-layer-to-top theImage base-layer)
		  (gimp-layer-set-mode base-layer GRAIN-EXTRACT-MODE)
		  (set! base-layer (car (gimp-image-merge-down theImage base-layer 2)))
		  (gimp-layer-set-mode base-layer GRAIN-MERGE-MODE)
		  (gimp-layer-add-mask base-layer layer-mask)
		  (gimp-floating-sel-anchor (car (gimp-edit-paste layer-mask 0)))
		)
	    )
	)
    )
    (gimp-displays-flush)

    (gimp-progress-end)
    ;End the undo group
    (gimp-image-undo-group-end theImage)
    (gimp-context-pop)
)

(script-fu-register "script-fu-max-loc-contrast"
            _"<Image>/FX-Foundry/Photo/Enhancement/Maximize Local Contrast..."
            "Stretch contrast based on local pixel maximum and minimum values"
            "Roy Johnson"
            "2009, Roy Johnson"
            "June 2009"
            "*"
            SF-IMAGE		"Image"		0
            SF-DRAWABLE         "Drawable"      0
            SF-ADJUSTMENT	_"Radius as % of image size"	'(4 1 5  0.1 0.5 1 0)
	    SF-ADJUSTMENT	_"Feather haloes"	'(8  1  20 1 1 0 0)
	    SF-ADJUSTMENT	_"Scaledown factor"	'(1  1  3 1 1 0 0)
	    SF-TOGGLE		_"Keep Min and Max Layers"	FALSE
)
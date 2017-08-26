;
; Even Low Frequency plugin for the GIMP (an image manipulation program)
; Version: 1.1 (09.IX.2010)
; Author: Miran (Miran4@op.pl)
;
; This is version for GIMP 2.7 (and above)
; All changes for make it working in this version are marked "2.7 Fix"
;
; For older versions or updates check: http://registry.gimp.org
;
; Changelog:
;   Version 1.1
;      - "tmp-layer error" fixed
;      - Added GIMP 2.7 support
;
;   Version 1.0
;      - Initial release
;
; License:
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;

; function that saves alpha channel
(define (script-fu-LF-Even-get-alpha image drawable)
   (let
      ( 
         (selection 0)
	 (alpha 0)
      )

      (if (gimp-drawable-has-alpha drawable)
         (begin
	   (if (= (car(gimp-selection-is-empty image)) FALSE) ; is there selection?
               (set! selection (car (gimp-selection-save image))) ; save old selection
            )

	    (gimp-selection-layer-alpha drawable) ; convert alpha to selection
	    (gimp-selection-invert image) ; invert selection

	    (if (= (car(gimp-selection-is-empty image)) FALSE) ; is there selection (any alpha channel)?
	       (begin
	          (gimp-selection-invert image) ; invert selection
	          (set! alpha (car (gimp-selection-save image))) ; store selection as channel
	       )
	    )
            
	    (if (not(= selection 0)) ; restore old selection
               (begin
                  (gimp-selection-load selection) ; load saved selection
	          (gimp-image-remove-channel image selection) ; delete stored channel
	       )
	       (gimp-selection-none image) ; else clear selection
            )
         )
      )

      alpha ; return
   )
)

; function that sets given alpha channel for drawable (returns new drawable)
(define (script-fu-LF-Even-set-alpha drawable alpha)
   (if (not(= alpha 0)) ; set given alpha
      (let*
	 (
	    (tmp-img (car(gimp-image-new (car(gimp-drawable-width drawable)) (car(gimp-drawable-height drawable)) RGB)))
		(tmp-layer 0) ; Fix
	    (mask 0)
	 )

	 (gimp-image-undo-disable tmp-img) ; disable undo recording

	 (set! tmp-layer (car(gimp-layer-new-from-drawable drawable tmp-img))) ; copy input drawable to tmp-img
         (gimp-image-add-layer tmp-img tmp-layer 0) ; add new layer to image

         (gimp-layer-add-alpha tmp-layer) ; add alpha channel
	 (set! mask (car(gimp-layer-create-mask tmp-layer ADD-ALPHA-TRANSFER-MASK))) ; add mask to drawable
	 (gimp-drawable-fill mask WHITE-FILL) ; remove existing opacity
	 (gimp-channel-combine-masks mask alpha CHANNEL-OP-INTERSECT 0 0) ; replace mask with given alpha channel
	 (gimp-layer-add-mask tmp-layer mask) ; apply
	 (gimp-layer-remove-mask tmp-layer MASK-APPLY) ; collapse

	 (let
            ((result-layer (car(gimp-layer-new-from-drawable tmp-layer (car(gimp-drawable-get-image drawable)))))) ; move result to the input drawable's image space
	 
	    (gimp-image-undo-enable tmp-img)
            (gimp-image-delete tmp-img)
	    ;(gimp-display-new tmp-img) ; debug display
	    result-layer ; return
         )
      )
      drawable ; else return unchanged
   )
)

; main script
(define (script-fu-LF-Even		image 
					in-drawable 
					B-process
					B-radius ; disable for one slider interface
					C-process
					C-radius
					XY-process
					new-layer
					keep-unselected
					VC-separate
					)
   (if (or (= B-process TRUE) (= C-process TRUE))
      (if (= (car (gimp-drawable-is-layer in-drawable)) TRUE)
         (begin
            (let*
	       (
	          (drawable 0)
		  (alpha 0)
		  (alpha-unp 0)
		  (selection 0)
		  (offsetX 0)
		  (offsetY 0)
	          ;(B-radius C-radius) ; value of disabled brightness slider
		  (X-process (or (= XY-process 0) (= XY-process 1)))
		  (Y-process (or (= XY-process 0) (= XY-process 2)))
	          (value-layer 0) ; brightness
	          (color-layer 0)
		  (color-old-fg (car (gimp-context-get-foreground))) ; store current fg color
	       )

	       (gimp-context-push)
	       (gimp-image-undo-group-start image) ; start undo point

               (if (= (car(gimp-selection-is-empty image)) FALSE) ; is there selection?
                  (begin
                     (gimp-selection-invert image) ; invert selection
	             (if (= (car(gimp-selection-is-empty image)) FALSE) ; isn't all selected
		        (begin
			   (gimp-selection-invert image) ; invert selection
			   (set! selection (car (gimp-selection-save image))) ; save selection

			   (if (or (= keep-unselected TRUE) (and (= VC-separate FALSE) (= new-layer FALSE)) )
			      (set! alpha-unp (script-fu-LF-Even-get-alpha image in-drawable)) ; save original alpha channel
			   )

			   (let*
			      (
			         (crop-img (car(gimp-image-new (car(gimp-image-width image)) (car(gimp-image-height image)) RGB)))
			         (bounds (cdr (gimp-selection-bounds image)))
				 (tmp-layer 0)
			      )

			      (gimp-image-undo-disable crop-img) ; disable undo recording

			      (set! tmp-layer (car(gimp-layer-new-from-drawable in-drawable image))) ; copy input layer
			      (gimp-layer-add-alpha tmp-layer) ; add alpha channel (if not already exists)
			      (gimp-image-add-layer image tmp-layer -1) ; add new layer to image
			      (gimp-selection-invert image) ; invert selection
			      (gimp-edit-clear tmp-layer) ; clear user unselected areas (combine alpha with selection)
			      (gimp-selection-none image) ; clear selection
                              
			      (set! drawable (car(gimp-layer-new-from-drawable tmp-layer crop-img))) ; copy tmp layer to crop-img
			      (gimp-image-remove-layer image tmp-layer) ; delete temp layer

			      (gimp-image-add-layer crop-img drawable -1) ; add new layer to crop-img

			      (set! offsetX (car bounds))
			      (set! offsetY (cadr bounds))
                              (gimp-image-crop crop-img (- (caddr bounds) offsetX) (- (car(cdddr bounds)) offsetY) offsetX offsetY) ; crop image to bounds of selection
			      
			      (set! drawable (car(gimp-layer-new-from-drawable (car(gimp-image-get-active-layer crop-img)) image))) ; copy croped back to working image
			      (set! alpha (script-fu-LF-Even-get-alpha image drawable)) ; save alpha channel

			      (gimp-image-undo-enable crop-img)
                              (gimp-image-delete crop-img)
	                      ;(gimp-display-new crop-img) ; debug display

                           ; if selection is bigger than input layer we need to fix the offsets
			      (if (> (car (gimp-drawable-offsets in-drawable)) offsetX)
			         (set! offsetX (car (gimp-drawable-offsets in-drawable)))
			      )

			      (if (> (cadr (gimp-drawable-offsets in-drawable)) offsetY)
			         (set! offsetY (cadr (gimp-drawable-offsets in-drawable)))
			      )
			   )  
		        )
		     )
		  )
               )

	       (gimp-selection-none image) ; clear selection

               (if (= selection 0) ; image no need be croped
	          (begin
	             (set! drawable in-drawable) ; work on unchanged input drawable

                     ; store offset of input layer
		     (set! offsetX (car (gimp-drawable-offsets drawable)))
		     (set! offsetY (cadr (gimp-drawable-offsets drawable)))

		     (set! alpha (script-fu-LF-Even-get-alpha image drawable)) ; save alpha channel
		  )
	       )
		
            ; process image value (brightness)
               ; get value layer
	       (let*
	          ( (decompose-value (car (plug-in-decompose RUN-NONINTERACTIVE image drawable "Value" FALSE))) ) ; get value layer
		  
		  (set! value-layer (car(gimp-layer-new-from-drawable (car(gimp-image-get-active-layer decompose-value)) image))) ; copy to working image
                  (gimp-image-delete decompose-value) ; delete temp decompose image
	       )

	       (if (= B-process TRUE)
	          (begin
		     (set! value-layer (script-fu-LF-Even-set-alpha value-layer alpha)) ; set original alpha
		     (set! value-layer (script-fu-LF-Even-channel value-layer B-radius X-process Y-process)) ; apply LF Even filter
		     (gimp-layer-create-mask value-layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity
	          )
	       )
	       (gimp-layer-set-mode value-layer VALUE-MODE) ; set layer value mode
	       (gimp-image-add-layer image value-layer (car(gimp-image-get-layer-position image in-drawable))) ; add new layer to image
	       (gimp-layer-translate value-layer offsetX offsetY) ; move to original position if croped by selection
	       ;(gimp-layer-set-name value-layer "LF Even - Value") ; change name
		   (gimp-drawable-set-name value-layer "LF Even - Value") ; 2.7 Fix

            ; process image colors
               (if (= C-process TRUE)
	          (begin
                     ; extract and filter RGB channels
	             (let*
	                ( 
		           (decompose-img (car (plug-in-decompose RUN-NONINTERACTIVE image drawable "RGB" TRUE))) ; extract RGB channels
	                   (decompose-layers (cadr (gimp-image-get-layers decompose-img))) ; get RGB channels
		           (channel 0)
		        )

		     ; Red
		        (set! channel (aref decompose-layers 0)) ; get red channel
			(set! channel (script-fu-LF-Even-set-alpha channel alpha)) ; set original alpha
		        (set! channel (script-fu-LF-Even-channel channel C-radius X-process Y-process)) ; apply LF Even filter
			(gimp-layer-create-mask channel ADD-ALPHA-TRANSFER-MASK) ; remove opacity
		        (set! color-layer (car(gimp-layer-new-from-drawable channel image))) ; copy result to working image
		        (gimp-image-add-layer image color-layer -1) ; add new layer to image
	                ;(gimp-layer-set-name color-layer "LF Even - Color RED") ; change name
					(gimp-drawable-set-name color-layer "LF Even - Color RED") ; 2.7 Fix
		        (plug-in-colorify RUN-NONINTERACTIVE image color-layer '(255 0 0)) ; make image red color

	             ; Green
		        (set! channel (aref decompose-layers 1)) ; get green channel
			(set! channel (script-fu-LF-Even-set-alpha channel alpha)) ; set original alpha
		        (set! channel (script-fu-LF-Even-channel channel C-radius X-process Y-process)) ; apply LF Even filter
			(gimp-layer-create-mask channel ADD-ALPHA-TRANSFER-MASK) ; remove opacity
		        (set! color-layer (car(gimp-layer-new-from-drawable channel image))) ; copy result to working image
		        (gimp-image-add-layer image color-layer -1) ; add new layer to image
	                ;(gimp-layer-set-name color-layer "LF Even - Color GREEN") ; change name
					(gimp-drawable-set-name color-layer "LF Even - Color GREEN") ; 2.7 Fix
		        (plug-in-colorify RUN-NONINTERACTIVE image color-layer '(0 255 0)) ; make image green color

		     ; Blue
		        (set! channel (aref decompose-layers 2)) ; get blue channel
			(set! channel (script-fu-LF-Even-set-alpha channel alpha)) ; set original alpha
		        (set! channel (script-fu-LF-Even-channel channel C-radius X-process Y-process)) ; apply LF Even filter
			(gimp-layer-create-mask channel ADD-ALPHA-TRANSFER-MASK) ; remove opacity
		        (set! color-layer (car(gimp-layer-new-from-drawable channel image))) ; copy result to working image
		        (gimp-image-add-layer image color-layer -1) ; add new layer to image
	                ;(gimp-layer-set-name color-layer "LF Even - Color BLUE") ; change name
					(gimp-drawable-set-name color-layer "LF Even - Color BLUE") ; 2.7 Fix
		        (plug-in-colorify RUN-NONINTERACTIVE image color-layer '(0 0 255)) ; make image blue color
                  
		        (gimp-image-delete decompose-img) ; delete temp decompose image
	             )

	             ; merge color layers to compose one rgb image
	             (gimp-layer-set-mode color-layer ADDITION-MODE) ; set layer add mode
	             (set! color-layer (car(gimp-image-merge-down image color-layer CLIP-TO-BOTTOM-LAYER)))
	             (gimp-layer-set-mode color-layer ADDITION-MODE) ; set layer add mode
	             (set! color-layer (car(gimp-image-merge-down image color-layer CLIP-TO-BOTTOM-LAYER)))
	             (gimp-image-lower-layer image color-layer) ; move layer down
	             ;(gimp-layer-set-name color-layer "LF Even - Color base") ; change name
				 (gimp-drawable-set-name color-layer "LF Even - Color base") ; 2.7 Fix
		  )
		  (begin ; else
		     (set! color-layer (car (gimp-layer-copy drawable TRUE))) ; copy input drawable
		     (gimp-layer-create-mask color-layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity
	             (gimp-image-add-layer image color-layer -1) ; add new layer to image
	             (gimp-image-lower-layer image color-layer) ; move layer down
	             (gimp-layer-set-mode color-layer NORMAL-MODE) ; set layer normal mode
	             ;(gimp-layer-set-name color-layer "LF Even - Color base") ; change name
				 (gimp-drawable-set-name color-layer "LF Even - Color base") ; 2.7 Fix
		  )
	       )

               ; keep unselected for color layer
	       (if (and (and (= keep-unselected TRUE) (not(= selection 0))) (= VC-separate TRUE))
	          (let*
		     ((tmp-layer 0))

		     ; apply filter trim to result color layer
	             (if (not(= alpha 0))
	                (begin
			   (gimp-layer-add-alpha color-layer) ; add alpha channel
		           (gimp-selection-load alpha) ; load alpha as selection
		           (gimp-selection-invert image) ; invert selection
		           (gimp-edit-clear color-layer) ; apply alpha to layer
		           (gimp-selection-none image) ; clear selection
		        )
	             )
		     (gimp-layer-translate color-layer offsetX offsetY) ; move color layer to original position if croped by selection

		     (set! tmp-layer (car(gimp-layer-copy in-drawable TRUE))) ; copy input drawable
		     ;(gimp-layer-set-name tmp-layer "LF Even - unselected Color") ; change name
			 (gimp-drawable-set-name tmp-layer "LF Even - unselected Color") ; 2.7 Fix
		     (gimp-layer-create-mask tmp-layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity
		     (gimp-image-add-layer image tmp-layer (+ 1 (car(gimp-image-get-layer-position image color-layer)))) ; add new layer under color-layer

		     (set! color-layer (car(gimp-image-merge-down image color-layer CLIP-TO-BOTTOM-LAYER))) ; merge processed and unprocessed layers
		     ;(gimp-layer-set-name color-layer "LF Even - Color base") ; change name
             (gimp-drawable-set-name color-layer "LF Even - Color base") ; 2.7 Fix
		     (gimp-layer-create-mask color-layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity
		     
		  )
	       )

               ; now remove value (set 0.5 for each pixel) from color layer
	       ; it no changes final result, it's only helpfull for user to colors edit
	       ; so, do it just when color layer will be finally no merged
               (if (= VC-separate TRUE)
	          (begin
	             ; create color extraction layer
                     (set! color-layer (car (gimp-layer-new	image
								(car (gimp-image-width image))
								(car (gimp-image-height image))
								RGBA-IMAGE
								"LF Even - Color extractor"
								100
								VALUE-MODE)))
                     (gimp-context-set-foreground '(128 128 128)) ; gray
                     (gimp-drawable-fill color-layer FOREGROUND-FILL) ; fill layer
                     (gimp-context-set-foreground color-old-fg) ; restore user's fg color
	             (gimp-image-add-layer image color-layer -1) ; add new layer to image
	             (set! color-layer (car(gimp-image-merge-down image color-layer CLIP-TO-BOTTOM-LAYER))) ; merge to get color layer
	             ;(gimp-layer-set-name color-layer "LF Even - Color") ; change name
				 (gimp-drawable-set-name color-layer "LF Even - Color") ; 2.7 Fix
		  )
	       )

               (if (and (and (= keep-unselected TRUE) (not(= selection 0))) (= VC-separate TRUE))
                  (begin
		     (if (not(= alpha-unp 0)) ; apply original aplha
		        (begin
			   (gimp-selection-load alpha-unp) ; load alpha to selection
			   (gimp-selection-invert image) ; invert selection
			   (gimp-edit-clear color-layer) ; clear selected areas
			   (gimp-selection-none image) ; clear selection
			)
	             )
		  )
                  (begin ; else
	             ; apply original alpha and user's selection trim
	             (if (not(= alpha 0))
	                (begin
		           (gimp-selection-load alpha) ; load alpha as selection
		           (gimp-selection-invert image) ; invert selection
		           (gimp-edit-clear color-layer) ; apply alpha to layer
		           (gimp-selection-none image) ; clear selection
		        )
	             )
	             
		     (gimp-layer-translate color-layer offsetX offsetY) ; move color layer to original position if croped by selection
		  )
	       )

	       (if (= VC-separate FALSE)
	          (begin
	             (set! value-layer (car(gimp-image-merge-down image value-layer CLIP-TO-BOTTOM-LAYER)))
		     ;(gimp-layer-set-name value-layer (string-append (car(gimp-drawable-get-name in-drawable)) " - LF Even")) ; change name
			 (gimp-drawable-set-name value-layer (string-append (car(gimp-drawable-get-name in-drawable)) " - LF Even")) ; 2.7 Fix
		  )
		  (begin ; else
		     ;(gimp-layer-set-name value-layer (string-append (car(gimp-drawable-get-name in-drawable)) " - LF Even: Value")) ; change name
			 (gimp-drawable-set-name value-layer (string-append (car(gimp-drawable-get-name in-drawable)) " - LF Even: Value")) ; 2.7 Fix
		     ;(gimp-layer-set-name color-layer (string-append (car(gimp-drawable-get-name in-drawable)) " - LF Even: Color")) ; change name
			 (gimp-drawable-set-name color-layer (string-append (car(gimp-drawable-get-name in-drawable)) " - LF Even: Color")) ; change name
                  )
	       )

               ; keep unselected option
	       ; actually result layers no contains unselected areas (opacity)
	       ; it will copy unprocessed parts of image from original and connect it with filter result
	       (if (and 
	                (or (= keep-unselected TRUE) (and (= VC-separate FALSE) (= new-layer FALSE)))
	                (not(= selection 0)) 
	           )
	          
		  (if (= VC-separate FALSE)
                     (let*
		        ((tmp-layer 0))

		        (set! tmp-layer (car(gimp-layer-copy in-drawable TRUE))) ; copy input drawable
			;(gimp-layer-set-name tmp-layer "LF Even - unselected Value") ; change name
			(gimp-drawable-set-name tmp-layer "LF Even - unselected Value") ; 2.7 Fix
			(gimp-image-add-layer image tmp-layer (+ 1 (car(gimp-image-get-layer-position image value-layer)))) ; add new layer under value-layer
			(gimp-selection-load selection) ; load user's selection
			(gimp-edit-clear tmp-layer) ; clear selected areas
			(gimp-selection-none image) ; clear selection

			(set! value-layer (car(gimp-image-merge-down image value-layer CLIP-TO-BOTTOM-LAYER))) ; merge processed and unprocessed layers
			;(gimp-layer-set-name value-layer (string-append (car(gimp-drawable-get-name in-drawable)) " - LF Even")) ; change name
			(gimp-drawable-set-name value-layer (string-append (car(gimp-drawable-get-name in-drawable)) " - LF Even")) ; 2.7 Fix
			(gimp-layer-create-mask value-layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity
			
			(if (not(= alpha-unp 0)) ; apply original aplha
			   (begin
			      (gimp-selection-load alpha-unp) ; load alpha to selection
			      (gimp-selection-invert image) ; invert selection
			      (gimp-edit-clear value-layer) ; clear selected areas
			      (gimp-selection-none image) ; clear selection
			      (gimp-image-remove-channel image alpha-unp) ; delete stored channel
			   )
			)
		     )
                     (begin ; separated true
		        (let*
	                   ( (decompose-value (car (plug-in-decompose RUN-NONINTERACTIVE image in-drawable "Value" FALSE))) ) ; get value layer

		           (gimp-selection-load selection) ; load user's selection
			   (gimp-selection-invert image) ; invert selection
			   (gimp-edit-clear value-layer) ; clear selected areas
			   (gimp-selection-none image) ; clear selection

		           (gimp-image-add-layer image (car(gimp-layer-new-from-drawable (car(gimp-image-get-active-layer decompose-value)) image)) (+ 1 (car(gimp-image-get-layer-position image value-layer)))) ; add unprocessed value layer under processed one
			   (set! value-layer (car(gimp-image-merge-down image value-layer CLIP-TO-BOTTOM-LAYER))) ; merge layers
			   (gimp-layer-create-mask value-layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity
			   (gimp-layer-set-mode value-layer VALUE-MODE) ; set layer value mode

                           (gimp-image-delete decompose-value) ; delete temp decompose image
			)
		     )
		  )
	       )

               ; apply result to input layer
	       (if (and  (= new-layer FALSE) (= VC-separate FALSE) )
                  (let*
		     ((buffer 0))
		     
		     ;(gimp-selection-none image) ; select all
		     (gimp-selection-all image) ; select all
		     (set! buffer (car(gimp-edit-named-copy value-layer "LF Even result"))) ; copy to gimp's cpilboard (no overwrites user's clipboard data)
		     (gimp-edit-clear in-drawable) ; clear original drawable

		     (gimp-floating-sel-anchor (car (gimp-edit-named-paste in-drawable buffer FALSE))) ; paste processed drawable
	             (gimp-buffer-delete buffer)

		     (gimp-image-remove-layer image value-layer) ; delete new result layer
	          )
	       )

	       (if (not(= selection 0)) ; restore old selection
	          (begin
		     (gimp-selection-load selection) ; load saved selection
		     (gimp-image-remove-channel image selection) ; delete stored channel
		  )
	       )

	       (if (not(= alpha 0))
                  (gimp-image-remove-channel image alpha) ; delete stored channel
	       )

	       (gimp-image-undo-group-end image) ; end undo point
               (gimp-displays-flush) ; update interface
	       (gimp-context-pop)
	    )
         )
         ;(message-box "MISSING ACTIVE LAYER!\n\Please first active layer you want to process.")
		 (gimp-message "MISSING ACTIVE LAYER!\n\Please first active layer you want to process.") ; 2.7 Fix
      )
      ;(message-box "NOTHING TO DO!\n\You have to select least one option to process.")
	  (gimp-message "NOTHING TO DO!\n\You have to select least one option to process.") ; 2.7 Fix
   )
)

(define (script-fu-LF-Even-channel drawable radius X-axis Y-axis) ; function which filters single channel (gryscale image)
   (let*
      (
         (image (car(gimp-image-new (car(gimp-drawable-width drawable)) (car(gimp-drawable-height drawable)) GRAY))) ; create new image window (undisplayed)
	 (layer 0)
	 (filtred-layer 0)
	 (color-current 0)
	 (color-target 0)
	 (alpha 0)
	 (color-old-fg (car (gimp-context-get-foreground))) ; store current fg color
	 (X-transform-size 1)
         (Y-transform-size 1)
      )

      (gimp-image-undo-disable image) ; disable undo recording (disable it when debuging)

   ; create correction layer (local brightness differences)
      (set! layer (car(gimp-layer-new-from-drawable drawable image))) ; copy input image
      (set! alpha (script-fu-LF-Even-get-alpha image layer)) ; save alpha channel
      (gimp-image-add-layer image layer 0) ; add new layer to image
      (gimp-invert layer) ; negative
      
      (if (not X-axis)
	 ;(gimp-scale layer TRUE 0 0 1 (car(gimp-image-height image))) ; scale x-axis to one pixel (no correction in this axis)
	 (gimp-drawable-transform-scale-default layer 0 0 1 (car(gimp-image-height image)) TRUE TRANSFORM-RESIZE-ADJUST) ; 2.7 Fix
         (if (not Y-axis) ; else
	    ;(gimp-scale layer TRUE 0 0 (car(gimp-image-width image)) 1) ; scale y-axis to one pixel (no correction in this axis)
		(gimp-drawable-transform-scale-default layer 0 0 (car(gimp-image-width image)) 1 TRUE TRANSFORM-RESIZE-ADJUST) ; 2.7 Fix
	 )
      )

      (plug-in-gauss RUN-NONINTERACTIVE image layer radius radius 1) ; blur (remove hi-frequency details)
      
      (if (not (and X-axis Y-axis)) ; if any axis disabled
         ;(gimp-scale layer TRUE 0 0 (car(gimp-image-width image)) (car(gimp-image-height image))) ; stretch disabled axis to original size
		 (gimp-drawable-transform-scale-default layer 0 0 (car(gimp-image-width image)) (car(gimp-image-height image)) TRUE TRANSFORM-RESIZE-ADJUST) ; 2.7 Fix
      )
      
      (gimp-layer-create-mask layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity from the layer (if any)

   ; create layer that will be corected 
      (set! layer (car(gimp-layer-new-from-drawable drawable image))) ; copy input image
      (gimp-layer-create-mask layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity from the layer (if any)
      (gimp-image-add-layer image layer 0) ; add new layer to image (above existing layer)
      (gimp-layer-set-mode layer GRAIN-MERGE-MODE) ; set layer grain merge mode

      (set! filtred-layer (car(gimp-image-merge-down image layer CLIP-TO-BOTTOM-LAYER))) ; apply correction (merge layers)
      
   ; now the corrected layer has average brightnes near to 0.5
   ; the average brightness will be set to same as in input image
      (gimp-layer-set-mode filtred-layer HARDLIGHT-MODE) ; set layer hardlight merge mode

      (if (not X-axis)
         (set! X-transform-size (car(gimp-image-width image)))
         (if (not Y-axis) ; else
	    (set! Y-transform-size (car(gimp-image-height image)))
	 )
      )

   ; create layer contains averaged color of input drawable
      (set! layer (car(gimp-layer-new-from-drawable drawable image))) ; copy input image
      (gimp-image-add-layer image layer 1) ; add new layer to image (belov existing layer)
      ;(gimp-scale layer TRUE 0 0 X-transform-size Y-transform-size) ; calculate average color of all pixels (scale layer into 1x1 size, if processing both axis)
	  (gimp-drawable-transform-scale-default layer 0 0 X-transform-size Y-transform-size TRUE TRANSFORM-RESIZE-ADJUST) ; 2.7 Fix
      (gimp-layer-create-mask layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity from the layer (if any)
	  ;(set! color-target (car(gimp-color-picker image layer 0.5 0.5 FALSE TRUE (+ X-transform-size Y-transform-size)))) ; get color of this layer
      (set! color-target (car(gimp-image-pick-color image layer 0.5 0.5 FALSE TRUE (+ X-transform-size Y-transform-size)))) ; 2.7 Fix

   ; create layer contains averaged color of corected drawable
      (set! layer (car(gimp-layer-new-from-drawable filtred-layer image))) ; copy corected layer
      (set! layer (script-fu-LF-Even-set-alpha layer alpha)) ; apply original alpha (we need average of visible pixels only)
      (gimp-image-add-layer image layer 1) ; add new layer to image (betwen existing layers)
      ;(gimp-scale layer TRUE 0 0 X-transform-size Y-transform-size) ; calculate average color of all pixels (scale layer into 1x1 size)
	  (gimp-drawable-transform-scale-default layer 0 0 X-transform-size Y-transform-size TRUE TRANSFORM-RESIZE-ADJUST) ; 2.7 Fix
      (gimp-layer-create-mask layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity from the layer (if any)
	  ;(set! color-current (car(gimp-color-picker image layer 0.5 0.5 FALSE TRUE (+ X-transform-size Y-transform-size)))) ; get color of this layer
      (set! color-current (car(gimp-image-pick-color image layer 0.5 0.5 FALSE TRUE (+ X-transform-size Y-transform-size)))) ; 2.7 Fix
      (gimp-layer-set-mode layer DIFFERENCE-MODE) ; set layer difference mode

      ; create base layer for hardlight correction
      (set! layer (car (gimp-layer-new image	X-transform-size
						Y-transform-size
						GRAYA-IMAGE
						"Final brightness correction"
						100
						NORMAL-MODE)))
      (gimp-context-set-foreground '(128 128 128)) ; hardlight neutral gray
      (gimp-drawable-fill layer FOREGROUND-FILL) ; fill layer
      (gimp-context-set-foreground color-old-fg) ; restore user's fg color
      (gimp-image-add-layer image layer 1) ; add new layer to image (below first layer)
      (if (> (car color-current) (car color-target)) ; compare red value of both colors (both are gray, so rgb are even)
         (gimp-layer-set-mode layer DIFFERENCE-MODE) ; corected layer is brighten than input layer, subtract brightnes difference from hardlight layer
	 (gimp-layer-set-mode layer ADDITION-MODE) ; corected layer is darken than input layer, add brightnes difference to hardlight layer
      )

      (gimp-drawable-set-visible filtred-layer FALSE) ; hide filtred layer
      (set! layer (car(gimp-image-merge-visible-layers image CLIP-TO-BOTTOM-LAYER))) ; merge visible layers to get redy brightnes correction color (one pixel layer)
      ;(gimp-scale layer FALSE 0 0 (car(gimp-image-width image)) (car(gimp-image-height image))) ; rescale to whole image (color fill)
	  (gimp-drawable-transform-scale-default layer 0 0 (car(gimp-image-width image)) (car(gimp-image-height image)) FALSE TRANSFORM-RESIZE-ADJUST) ; 2.7 Fix
      (gimp-layer-create-mask layer ADD-ALPHA-TRANSFER-MASK) ; remove opacity from the layer (if any)

      (gimp-drawable-set-visible filtred-layer TRUE) ; show
      (set! filtred-layer (car(gimp-image-merge-down image filtred-layer CLIP-TO-BOTTOM-LAYER))) ; apply brightness correction to filtred layer
      (gimp-drawable-set-name filtred-layer (string-append (car (gimp-drawable-get-name drawable)) " - LF Even")) ; set name for result layer

      (let
         ((result-layer (car(gimp-layer-new-from-drawable filtred-layer (car(gimp-drawable-get-image drawable)))))) ; move result to the input drawable's image space
	 (set! result-layer (script-fu-LF-Even-set-alpha result-layer alpha)) ; apply original alpha to result image
	 
	 (gimp-image-undo-enable image)
         (gimp-image-delete image)
	 ;(gimp-display-new image) ; debug display
	 result-layer ; return
      )
   )
)

; register script
(script-fu-register	"script-fu-LF-Even"
			"<Image>/Filters/Light and Shadow/Low frequency even" 
	"Removes low frequency brightness and color differences from image. Usefull when creating tilable textures for computer graphic." 
	"Miran <Miran4@op.pl>" 
	"Miran" 
	"26.07.2010" 
	"RGB RGBA"
	SF-IMAGE "Image" 0 
	SF-DRAWABLE "Drawable" 0 
	SF-TOGGLE _"Brightness processing" TRUE
	SF-ADJUSTMENT _"Brightness detail filter pass radius" '( 80 1 600 1 10 0 0 ) ; brightness slider (disable for one slider)
	SF-TOGGLE _"Color processing" TRUE
	SF-ADJUSTMENT _"Colors detail filter pass radius" '( 40 1 600 1 10 0 0 ) ; color slider (disable for one slider)
	;SF-ADJUSTMENT _"Detail filter pass radius" '( 60 1 500 1 10 0 0 ) ; slider for both parameters (enable for one slider)
	SF-OPTION _"Working space" '(_"all directions" _"X axis only" _"Y axis only")
	SF-TOGGLE _"Separate layer" TRUE
	SF-TOGGLE _"Keep unselected" FALSE
	SF-TOGGLE _"Leave separated Value & Color                                                                                                                           " FALSE ; spaces will make dialog and sliders wider (more precision)
)

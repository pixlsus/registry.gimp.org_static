
(define (nklein-burnt-paper image drawable overlay-mode add-shadow add-canvas add-mottling nominal-burn-size)
  (gimp-context-push)
  (gimp-image-undo-group-start image)

  (let ((original-selection 0)
	(xx 0)
	(yy 0)
	(burn-size 0)
	(name (car (gimp-drawable-get-name drawable)))
	(width (car (gimp-drawable-width drawable)))
	(height (car (gimp-drawable-height drawable))))

    (if (= (car (gimp-selection-is-empty image)) TRUE)
	(gimp-selection-layer-alpha drawable)
	(set! original-selection (car (gimp-selection-save image))))

    (let ((bounds (gimp-selection-bounds image)))
      (set! xx (cadr bounds))
      (set! yy (caddr bounds))
      (set! width (- (cadr (cddr bounds)) xx))
      (set! height (- (caddr (cddr bounds)) yy)))

    (set! burn-size (/ (* nominal-burn-size (max width height 1))
		       300.0 30.0))
    (script-fu-distress-selection image
				  drawable
				  127
				  (* 12 burn-size)
				  3
				  2
				  TRUE
				  TRUE)

    (let ((paper-layer (car (gimp-layer-new image
					    width
					    height
					    RGBA-IMAGE
					    "Paper Layer"
					    100
					    NORMAL-MODE)))
	  (edge-layer (car (gimp-layer-new image
					   width
					   height
					   RGBA-IMAGE
					   "Edge Layer"
					   100
					   MULTIPLY-MODE))))

      (gimp-image-add-layer image paper-layer -1)
      (gimp-layer-translate paper-layer xx yy)

      (gimp-context-set-background '(160 153 105))
      (gimp-edit-fill paper-layer BACKGROUND-FILL)

      (if (= add-canvas TRUE)
	  (plug-in-apply-canvas RUN-NONINTERACTIVE
				image
				paper-layer
				0
				1))

      (if (= add-mottling TRUE)
	  (let ((mottle-layer (car (gimp-layer-new image
						   width
						   height
						   RGBA-IMAGE
						   "Mottle Layer"
						   20
						   SOFTLIGHT-MODE))))
	    (gimp-image-add-layer image mottle-layer -1)
	    (gimp-layer-translate mottle-layer xx yy)
	    (plug-in-solid-noise RUN-NONINTERACTIVE
				 image
				 mottle-layer
				 1
				 1
				 (rand 65536)
				 3
				 (* 3.5 burn-size)
				 (* 3.5 burn-size))

	    (gimp-image-merge-down image mottle-layer CLIP-TO-IMAGE)))

      (gimp-drawable-fill edge-layer TRANSPARENT-FILL)
      (gimp-image-add-layer image edge-layer -1)
      (gimp-layer-translate edge-layer xx yy)

      (gimp-layer-add-mask edge-layer
			   (car (gimp-layer-create-mask edge-layer
							ADD-SELECTION-MASK)))
      (gimp-selection-shrink image 3)
      (gimp-selection-invert image)

      (script-fu-distress-selection image
				    edge-layer
				    115
				    (* 22 burn-size)
				    (+ 2 (* 2 burn-size))
				    (+ 1 burn-size)
				    TRUE
				    TRUE)
      (gimp-selection-feather image (+ 12 (* 7 burn-size)))
      (gimp-context-set-background '(124 86 71))
      (gimp-edit-fill edge-layer BACKGROUND-FILL)

      (gimp-image-merge-down image edge-layer CLIP-TO-IMAGE))

    (gimp-layer-set-mode drawable (case overlay-mode
				    ((0) GRAIN-MERGE-MODE)
				    ((1) SOFTLIGHT-MODE)
				    ((2) OVERLAY-MODE)))

    (gimp-image-raise-layer image drawable)
    (set! drawable (car (gimp-image-merge-down image
					       drawable
					       CLIP-TO-IMAGE)))
    (gimp-drawable-set-name drawable name)

    (gimp-selection-none image)
    (if (= add-shadow TRUE)
	(script-fu-drop-shadow image
			       drawable
			       (* 8 burn-size)
			       (* 16 burn-size)
			       (* 21 burn-size)
			       '(0 0 0)
			       80.0
			       TRUE))

    (if (not (zero? original-selection))
	(gimp-selection-load original-selection)))

  ;; when done... display the results and allow undo again
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
  (gimp-context-pop))

(script-fu-register "nklein-burnt-paper"
		    _"Burnt paper"
		    _"Creates a burnt paper effect based on the selection"
		    "Patrick Stein <pat@nklein.com>"
		    "Patrick Stein"
		    "2010-02-16"
		    "RGBA GRAYA"
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-OPTION _"Overlay mode" '(_"Grain merge"
						_"Soft light"
						_"Overlay")
		    SF-TOGGLE _"Add shadow" TRUE
		    SF-TOGGLE _"Add canvas texture" TRUE
		    SF-TOGGLE _"Add mottling of paper" TRUE
		    SF-ADJUSTMENT _"Burn size" '(30 0 100 1 10 0 0))

(script-fu-menu-register "nklein-burnt-paper"
			 "<Image>/Filters/Decor")

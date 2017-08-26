(define (start image drawable colorbg flatbg gaussbg borderwidth borderheight linewidth color radius)
  (gimp-image-undo-group-start image)
  (gimp-context-push)
  (gimp-selection-none image)

  (let*
    (
      (backlayer (car (gimp-image-get-active-layer image)))
      (toplayer (car (gimp-layer-copy backlayer TRUE)))
      (width (car (gimp-drawable-width toplayer)))
      (height (car (gimp-drawable-height toplayer)))
      (shadowlayer (car (gimp-layer-new image width height RGBA-IMAGE "shadow" 100 NORMAL-MODE)))
      (newwidth (- width (* 2 borderwidth)))
      (newheight (- height (* 2 borderheight)))
      (offsetx (/ (- width newwidth) -2))
      (offsety (/ (- height newheight) -2))
      (selection 0)
      (selection2 0)
    )

    (gimp-image-add-layer image shadowlayer 0)
    (gimp-image-add-layer image toplayer 0)

    ;toplayer:
    (gimp-round-rect-select image (* -1 offsetx) (* -1 offsety) newwidth newheight radius radius CHANNEL-OP-REPLACE TRUE FALSE 0 0)
    (set! selection (car (gimp-layer-create-mask toplayer ADD-SELECTION-MASK)))
    (gimp-layer-add-mask toplayer selection)

    ;shadow:
    (gimp-selection-load selection)
    (gimp-selection-grow image (if (= flatbg TRUE) 1 4))
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill shadowlayer FOREGROUND-FILL)
    (gimp-selection-none image)
    (if (= flatbg FALSE)
    	(plug-in-gauss 1 image shadowlayer 20 20 1)
    )

    ;background:
    (if (= colorbg FALSE)
	(gimp-desaturate backlayer)
     )
    (if (> gaussbg 0)
	(plug-in-gauss 1 image backlayer gaussbg gaussbg 1)
    )

    ;line:
    (gimp-selection-load selection)
    (gimp-selection-shrink image linewidth)
    (set! selection2 (car (gimp-selection-save image)))
    (gimp-selection-load selection)
    (gimp-selection-combine selection2 CHANNEL-OP-SUBTRACT)
    (gimp-context-set-foreground color)
    (gimp-edit-fill toplayer FOREGROUND-FILL)
    (gimp-image-remove-channel image selection2)

    (set! shadowlayer (car(gimp-image-merge-down image toplayer EXPAND-AS-NECESSARY)))
    (gimp-image-merge-down image shadowlayer EXPAND-AS-NECESSARY)
  )

  (gimp-selection-none image)
  (gimp-context-pop)
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
)

(script-fu-register "start"
                    "<Image>/Script-Fu/B&W border... v2"
                    "B&W border... v2"
                    "humcio"
                    "GNU GPL"
                    "10 January 2013"
                    "RGB*"
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Drawable" 0
		    SF-TOGGLE "Colour background" FALSE
		    SF-TOGGLE "Flat border" FALSE
		    SF-VALUE "Background blur" "0"
                    SF-VALUE "Border width[px]" "60"
                    SF-VALUE "Border height[px]" "50"
                    SF-VALUE "Line width[px]" "2"
                    SF-COLOR "Line colour" '(255 255 255)
                    SF-VALUE "Rounded corners[px]" "10"
)

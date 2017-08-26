(define (my-make-vista-button btn-text font font-size width height corner-radius button-color-dark button-color-light border-color text-color button-color-hover hover-yorn dis-yorn svg-yorn svg-file svg-color)
	(vista-button-make btn-text font font-size width height corner-radius button-color-dark button-color-light border-color text-color 0.5 svg-yorn svg-file svg-color)
	(if (= hover-yorn TRUE)
		(vista-button-make btn-text font font-size width height corner-radius button-color-dark button-color-hover border-color text-color 0.25 svg-yorn svg-file svg-color)
	)
	(if (= dis-yorn TRUE)
		(vista-button-make btn-text font font-size width height corner-radius '(50 50 50) '(100 100 100) border-color text-color 0.25 svg-yorn svg-file '(180 180 180))
	)
)

(define (vista-button-make btn-text font font-size width height corner-radius button-color-dark button-color-light border-color text-color glow-size svg-yorn svg-file svg-color)
 (let*

     (
      (theImage (car (gimp-image-new width height RGB) ))
      (layerborder (car (gimp-layer-new theImage width height 0 "Border" 100 NORMAL) ) )
      (layerbackground (car (gimp-layer-new theImage width height 0 "Background" 100 NORMAL) ) )
      (layerlowerglow (car (gimp-layer-new theImage width height 0 "Lower Glow" 100 NORMAL) ) )
      (layerhighlight (car (gimp-layer-new theImage width height 0 "Highlight" 100 NORMAL) ) )
      (layercontent (car (gimp-layer-new theImage width height 0 "Content" 100 NORMAL) ) )
      (text-extents (gimp-text-get-extents-fontname btn-text font-size PIXELS font))
	  (svg-path 0)
    )

   (gimp-context-push)
   (gimp-image-undo-disable theImage)

   (gimp-layer-add-alpha layerborder )
   (gimp-layer-add-alpha layerbackground )
   (gimp-layer-add-alpha layerlowerglow )
   (gimp-layer-add-alpha layerhighlight )
   (gimp-layer-add-alpha layercontent )

   (gimp-image-add-layer theImage layerborder 0)
   (gimp-image-add-layer theImage layerbackground 0)
   (gimp-image-add-layer theImage layerlowerglow 0)
   (gimp-image-add-layer theImage layerhighlight 0)
   (gimp-image-add-layer theImage layercontent 0)

   (gimp-selection-all theImage)
   (gimp-edit-clear layerborder)
   (gimp-edit-clear layerbackground)
   (gimp-edit-clear layerlowerglow)
   (gimp-edit-clear layerhighlight)
   (gimp-edit-clear layercontent)
   (gimp-selection-none theImage)

   (gimp-round-rect-select theImage 0 0 width height corner-radius corner-radius CHANNEL-OP-REPLACE TRUE FALSE 0 0)
   (gimp-context-set-foreground border-color)
   (gimp-edit-bucket-fill layerborder 0 0 100 0 FALSE 0 0 )

   (gimp-selection-shrink theImage 2)
   (gimp-context-set-foreground button-color-dark)
   (gimp-edit-bucket-fill layerbackground 0 0 100 0 FALSE 0 0 )

   (gimp-context-set-foreground button-color-light)
   (gimp-context-set-gradient "FG to Transparent")
   (gimp-edit-blend layerlowerglow FG-TRANSPARENT-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 1 0 TRUE 0 height 0 (* height glow-size))

   (gimp-context-set-foreground '(255 255 255))
   (gimp-edit-blend layerhighlight FG-TRANSPARENT-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 1 0 TRUE 0 0 0 (* height 0.75))
   (gimp-round-rect-select theImage 0 (/ height 2) width (/ height 2) (/ width 8) (/ height 8) CHANNEL-OP-REPLACE TRUE TRUE (/ width 24) (/ height 24))
   (gimp-edit-clear layerhighlight)
   (gimp-layer-set-opacity layerhighlight 60)

   (if (= svg-yorn TRUE) (begin 
    (gimp-vectors-import-from-file theImage svg-file TRUE TRUE)
    (set! svg-path (car (gimp-image-get-active-vectors theImage)))
    (gimp-vectors-to-selection svg-path CHANNEL-OP-REPLACE TRUE FALSE 0 0)
    (gimp-context-set-foreground svg-color)
    (gimp-edit-bucket-fill-full layercontent 0 0 50 0 FALSE TRUE SELECT-CRITERION-COMPOSITE 0 0 )
   ))

   (if (> (text-width text-extents) 0) (begin
	(gimp-context-set-foreground text-color)
	(let ((textl (car (gimp-text-fontname theImage -1 0 0 btn-text 0 TRUE font-size PIXELS font))))
	(gimp-layer-set-offsets textl (* (- width (text-width text-extents)) 0.5) (* (- height (text-height text-extents)) 0.5)))
   ))

   (gimp-image-clean-all theImage)

   (gimp-image-undo-enable theImage)
   (gimp-display-new theImage)
   (gimp-context-pop)


))



(script-fu-register "my-make-vista-button"
          "<Image>/Script-Fu/Utils/Vista Button..."
          "Create a vista style button"
          "Lucas Pettett/Patrick Gaunt"
          "Lucas Pettett/Patrick Gaunt"
          "April 2007/September 2008"
          ""
          SF-STRING     "Btn Text" "GO"
          SF-FONT       "Font" "Andalus"
          SF-VALUE      "Font Size" "30"
          SF-VALUE      "Width" "100"
          SF-VALUE      "Height" "100"
          SF-VALUE      "Corner Radius" "25"
          SF-COLOR      "Button Color Dark" '(5 87 5)
          SF-COLOR      "Button Color Light" '(0 153 34)
          SF-COLOR      "Border Color" '(0 0 0)
          SF-COLOR      "Text Color" '(255 255 255)
          SF-COLOR      "Button Color Hover" '(50 200 255)
		  SF-TOGGLE		"Generate Hover Button" TRUE
		  SF-TOGGLE		"Generate Disabled Button" FALSE
		  SF-TOGGLE		"Use SVG image as background" FALSE
          SF-FILENAME   "SVG file as image" ""
          SF-COLOR      "SVG file color" '(180 180 180)
)


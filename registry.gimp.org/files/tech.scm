;Created by Wyatt Arent
;Thanks to Adrian Likins for pieces of the 'Predator' script

(define (script-fu-tech image drawable color edge pixamm brightness)
(let* ((width(car(gimp-image-width image))))
(let* ((height(car(gimp-image-height image))))
(let* ((normcolor(car(gimp-context-get-background))))
(let* ((layer(car(gimp-layer-new image width height RGB-IMAGE "techrender" 100 NORMAL-MODE))))
(gimp-image-add-layer image layer -1)
(gimp-context-set-background color)
(gimp-drawable-fill layer 1)
(gimp-layer-add-alpha layer)
(gimp-context-set-background normcolor)
(plug-in-scatter-hsv 1 image layer 2 0 30 60)
(plug-in-gauss 1 image layer 5 5 0)
(plug-in-sharpen 1 image layer 70)
(plug-in-bump-map 1 image layer layer 150 40.70 40 0 0 0 0 1 0 1)
(plug-in-pixelize 1 image layer pixamm)
(plug-in-max-rgb 1 image layer 0)
(plug-in-edge 1 image layer edge 1 0)
(plug-in-sharpen 1 image layer 65)
(gimp-brightness-contrast layer 50 75)
(plug-in-colorify 1 image layer color)
(plug-in-sharpen 1 image layer 70)
(plug-in-neon 1 image layer 1 brightness)
(gimp-selection-none image)
(gimp-image-set-active-layer image layer)
(gimp-displays-flush)
)))))

(script-fu-register "script-fu-tech"
		    "<Image>/Script-Fu/Render/Tech..."
		    "Create a cool technological-like image"
		    "Wyatt Arent"
		    "Wyatt Arent"
		    "November 2009"
		    ""
		    SF-IMAGE      "SF-IMAGE" 0
		    SF-DRAWABLE   "SF-DRAWABLE" 0
		    SF-COLOR      "Color" '(0 255 20)
		    SF-ADJUSTMENT "Edge" '(20 5 30 1 1 0 0)
		    SF-ADJUSTMENT "Pixel Ammount" '(3 1 10 1 1 0 0)
		    SF-ADJUSTMENT "Brightness" '(.9 .1 1 .01 .01 1 0)
)
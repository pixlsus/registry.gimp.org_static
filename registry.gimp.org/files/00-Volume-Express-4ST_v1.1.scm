; GIMP script-fu-00-volume-express-4st
; version 1.1 2011.04.23 Loic Guyader
; from the script-fu "add-bevel.scm version 1.04" by Andrew Donkin 
; loic.guyader.froggy@gmail.com
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; v1.1  New version of the script
;       Thank you to James Sambrook (sambrookjm) who fixed the issue of bumpmap for certain values of tilewidth, tileheight and thickness.
;       (example: Tile's width: 20, Tile's height: 20, Thickness: 20)
;       I added a debug option because I think this bug is interesting.
;       I fixed the problem of the plug-in-tile that happened, for example, when the tile size was equal to that of the image.

(define (script-fu-00-volume-express-4st
	img
        drw
	tilewidth
	tileheight
	mode
	opac
        debug
	thick	;; change from original script - was thickness
	blur
	blur-radius
	rotation
	)

	(gimp-image-undo-group-start img) ;; debut d'historique d'annulation

	(let* (
		(width (car (gimp-image-width img))) 
		(height (car (gimp-image-height img)))
		(type-d (car (gimp-drawable-type drw)))
		(img-name (car (gimp-image-get-name img)))
		(grey-layer 0)

;; Check to make sure that there is always a selection
;; New to this version of script
		(thickness (if (= debug TRUE) (min thick (/ tilewidth 2) (/ tileheight 2)) thick))
;;
		(mode-opt
			(cond
				((equal? mode 0) GRAIN-MERGE-MODE)
				((equal? mode 1) GRAIN-EXTRACT-MODE)
				((equal? mode 2) OVERLAY-MODE)
				((equal? mode 3) NORMAL-MODE)
			)
		)

		(bump-layer 0)
		(index 1)
		(greyness 0)
		(thickness (abs thickness))
		(rotation-opt
			(cond
				((equal? rotation 0) ROTATE-180)
				((equal? rotation 1) 1)
				((equal? rotation 2) ROTATE-90)
				((equal? rotation 3) ROTATE-270)
			)
		)
		)

;; Save the context
		(gimp-context-push)

;; Add a layer and a selection with a grey background
		(set! grey-layer (car (gimp-layer-new img tilewidth tileheight
			type-d (string-append img-name "_volume") opac mode-opt)))
		(gimp-image-add-layer img grey-layer 0)
		(set! grey-layer (car (gimp-image-get-active-drawable img)))

		(gimp-rect-select img 0 0 tilewidth tileheight CHANNEL-OP-REPLACE FALSE 0)
		(gimp-context-set-background '(127 127 127))
		(gimp-drawable-fill grey-layer BACKGROUND-FILL)

;; Add a layer for bumpmap
		(set! bump-layer (car (gimp-layer-new img width height
			type-d (string-append img-name "_bump") 100 NORMAL-MODE)))
		(gimp-image-add-layer img bump-layer 0)

;; Initialise our bumpmap
		(gimp-context-set-background '(0 0 0))
		(gimp-drawable-fill bump-layer BACKGROUND-FILL)

		(while (< index thickness)
			(set! greyness (/ (* index 255) thickness))
			(gimp-context-set-background (list greyness greyness greyness))
			(gimp-edit-bucket-fill bump-layer BG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
			(gimp-selection-shrink img 1)
			(set! index (+ index 1))
		)

;; Now the white interior
		(gimp-context-set-background '(255 255 255))
		(gimp-edit-bucket-fill bump-layer BG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)

;; BUMPMAP INVOCATION:
		(gimp-selection-none img)
		(plug-in-bump-map RUN-NONINTERACTIVE img grey-layer bump-layer 125 45 3 0 0 0 0 TRUE FALSE 1)
		(gimp-rect-select img 0 0 tilewidth tileheight CHANNEL-OP-REPLACE FALSE 0)

;; Add blur?
		(if (= blur TRUE)
			(plug-in-gauss-rle2 RUN-NONINTERACTIVE img grey-layer blur-radius blur-radius)
		)

;; Clean up
		(gimp-image-remove-layer img bump-layer)
                (if (or (< tileheight height) (< tilewidth width))
                (begin
        		(gimp-selection-invert img)
                        (if (not (or (= tileheight height) (= tilewidth width))) (gimp-edit-cut grey-layer))
;; Plug-in-tile
		        (plug-in-tile RUN-NONINTERACTIVE img grey-layer width height FALSE)
                )
                )
       		(gimp-selection-none img)

;; Rotate?
		(if (not (= rotation 1))
			(gimp-drawable-transform-rotate-simple grey-layer rotation-opt TRUE (/ width 2) (/ height 2) TRUE)
		)

;; Restore the context
		(gimp-context-pop)

	) ;; fin du let*

	(gimp-displays-flush) ;; actualiser l'affichage de l'image 
	(gimp-image-undo-group-end img) ;; fin d'historique d'annulation

) ;; fin de la fonction

(script-fu-register
    "script-fu-00-volume-express-4st"
    "<Image>/Filters/2D Isometric-Fu/00 - Volume Express..for simple tiles"
    "Volume Express 2d by 2d for simple tiles parce que Gimp le vaut bien!"
    "Loic Guyader (froGgy)"
    "From the script-fu add-bevel.scm version 1.04 by Andrew Donkin. v1.1: New version of the script, thank you to sambrookjm"
    "04/2011"
    "RGB* GRAY*" ;; types d'images supportes par le script
        SF-IMAGE        "Image"                 0
        SF-DRAWABLE     "Drawable"              0
        SF-ADJUSTMENT   _"Tile's width"        	'(32 4 256 1 16 0 0)
        SF-ADJUSTMENT   _"Tile's height"       	'(32 4 256 1 16 0 0)
        SF-OPTION       _"Mode"                 '(_"Grain merge" _"Grain extract" _"Overlay" _"Normal")
        SF-ADJUSTMENT   _"Opacity"              '(80 0 100 1 10 1 0)
        SF-TOGGLE       _"Debug"                TRUE
        SF-ADJUSTMENT   _"Thickness"            '(5 0 30 1 2 0 0)
        SF-TOGGLE       _"Blur"                 TRUE
        SF-ADJUSTMENT   _"Blur radius"          '(3 1 100 1 5 0 0)
        SF-OPTION       _"Rotation"             '("180" "0" "90" "270")

 ) ;; fin du register

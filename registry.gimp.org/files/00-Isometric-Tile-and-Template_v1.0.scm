; GIMP script-fu-00-isometric-tile-and-template
; version 1.0 2011.01.31
; Copyright (c) 2011 Loïc Guyader
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


(define (script-fu-00-isometric-tile-and-template
                                                        width
                                                        height
                                                        tileheight
                                                        fg-color
                                                        bg-color
                                                        transp
                                                        name
                                                        filename
                                                        notemplate
        )


  (let* (
	(img (car (gimp-image-new width height RGB)))
	(tilewidth (* tileheight 2))
        (tile-is-odd 0) ;; variable, le tile est impair  0 = False
        (num-of-row 0)
        (vara (- tileheight 2)) ;; variable A
        (varb tileheight) ;; variable B
	(tile (car (gimp-layer-new img tilewidth tileheight
								RGBA-IMAGE "isometric tile" 100 NORMAL-MODE)))
	(template (car (gimp-layer-new img width height
								RGBA-IMAGE "isometric template" 100 NORMAL-MODE)))
	(path 0)
;; Declare variables to draw the pattern
        (draw-in-pencil (cons-array 4 'double))

       )

;; Save the context
        (gimp-context-push)

;; Add layer, fill the background and set the drawing options for the pattern
	(gimp-image-undo-disable img)
	(gimp-image-add-layer img tile 0)
	(gimp-context-set-background bg-color)
	(if (= transp FALSE)
	(gimp-drawable-fill tile BACKGROUND-FILL)
	)
	(gimp-context-set-foreground fg-color)
	(gimp-context-set-brush "Circle (01)")

;; Draw the pattern - 1st part

        (while (<= num-of-row (- (/ tileheight 2) 1))

        	(aset draw-in-pencil 0 vara)
        	(aset draw-in-pencil 1 num-of-row)
        	(aset draw-in-pencil 2 varb)
        	(aset draw-in-pencil 3 num-of-row)
        	(gimp-pencil tile 4 draw-in-pencil)
                        
                (set! num-of-row (+ num-of-row 1))
                (set! vara (- vara 2))
                (set! varb (+ varb 2))
        ) ;; fin du while

;; Test the tile : odd or even? And Set variables

        (if (odd? tileheight) ;; si la hauteur du tile est impaire
                        (set! tile-is-odd 1)  ;; 1 = True
        ) ;; fin du if

        (set! vara (- 1 tile-is-odd))
        (set! varb (- tilewidth (- 3 tile-is-odd)))

;; Draw the pattern - 2nd part

        (while (<= num-of-row tileheight)

        	(aset draw-in-pencil 0 vara)
        	(aset draw-in-pencil 1 num-of-row)
        	(aset draw-in-pencil 2 varb)
        	(aset draw-in-pencil 3 num-of-row)
        	(gimp-pencil tile 4 draw-in-pencil)

                (set! num-of-row (+ num-of-row 1))
                (set! vara (+ vara 2))
                (set! varb (- varb 2))
        ) ;; fin du while

;; Add the created pattern

	(gimp-selection-all img)
	(set! tile (car (gimp-image-get-active-drawable img)))

        (set! path (string-append gimp-directory
                             "/patterns/"
                             filename
                             ".pat"))
        (file-pat-save RUN-NONINTERACTIVE
                       img tile path path
                       name)
	(gimp-patterns-refresh)
        (gimp-context-set-pattern name)

;; Restore the context
        (gimp-context-pop)


  ) ;; fin du 1er let*

 (gimp-displays-flush)


;;  Create a template

    (if (= notemplate FALSE)
        
    
                  (let* (
                	(img (car (gimp-image-new width height RGB)))
                	(template (car (gimp-layer-new img width height
        							RGBA-IMAGE "isometric template" 100 NORMAL-MODE)))
                        )

;; Save the context
                        (gimp-context-push)

;; Fill the layer with the pattern
                        (gimp-context-set-pattern name)
                	(gimp-display-new img)
                	(gimp-image-add-layer img template 0)
                	(set! template (car (gimp-image-get-active-drawable img)))
                	(gimp-selection-all img)
                	(gimp-context-set-pattern name)
                	(gimp-drawable-fill template PATTERN-FILL)
                	(gimp-image-undo-enable img)

;; Restore the context
                        (gimp-context-pop)

                  ) ;; fin du 2eme let*


    ) ;; fin du if "notemplate"


 (gimp-displays-flush)


 ) ;; fin de la fonction

(script-fu-register
  "script-fu-00-isometric-tile-and-template"
  "00 - Isometric Tile and Template"
  "Create an isometric tile in patterns and a template from it"
  "Loïc Guyader (froGgy)"
  "Copyright"
  "01/2011"
  ""

  SF-ADJUSTMENT _"Image width"	        	'(256 8 2048 1 16 0 1)
  SF-ADJUSTMENT _"Image height"	        	'(1024 8 2048 1 16 0 1)
  SF-ADJUSTMENT _"Tile"		        	'(32 8 2048 1 16 0 1)
  SF-COLOR      _"Foreground color"     	'(255 0 0)
  SF-COLOR      _"Background color"     	'(255 255 0)
  SF-TOGGLE     _"Transparent background"       FALSE
  SF-STRING	_"Pattern name"	        	"ISO-tile"
  SF-STRING	_"File name"	        	"ISO-tile"
  SF-TOGGLE     _"No template! Just create the tile in my patterns!"	FALSE

)

(script-fu-menu-register "script-fu-00-isometric-tile-and-template"
                         "<Image>/File/Create/Patterns")
(script-fu-menu-register "script-fu-00-isometric-tile-and-template"
                         "<Image>/Filters/2D Isometric-Fu")

; GIMP script-fu-02-Isometric-Wall
; version 1.0 2011.02.04
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


(define (script-fu-02-Isometric-Wall
                                        img
                                        drw
                                        convert
                                        scale
                                        keep-ratio
                                        width
                                        height
                                        interpol
                                        opac
                                        leftwall
                                        rightwall
        )

(gimp-image-undo-group-start img) ;; debut d'historique d'annulation

   ( let* (
                (type-i (car (gimp-image-base-type img)))
                (type-d (car (gimp-drawable-type drw)))
                (original-width (car (gimp-image-width img))) 
                (original-height (car (gimp-image-height img)))
                (ratio (min (/ width original-width) (/ height original-height)))
                (ratio-width 0)
                (ratio-height 0)
                (img-name (car (gimp-image-get-name img)))
                (layer-interpol-opt 0)
                (interpol-opt
                        (cond
                        ((equal? interpol 0) INTERPOLATION-CUBIC)
                        ((equal? interpol 1) INTERPOLATION-LINEAR)
                        ((equal? interpol 2) INTERPOLATION-LANCZOS)
                        )
                )
           )


;;;;;;;; OPTION: Convertir l'image en RGB:

        (if (= convert TRUE)

                (if (not (= type-i RGB))

                (begin
                        (gimp-image-convert-rgb img)
                        (set! type-d (car (gimp-drawable-type drw)))
                 ) ;; fin du begin
                ) ;; fin du if
        ) ;; fin du if de "convert"


;;;;;;;; OPTION: Reduire ou augmenter la taille de l'image en conservant ou non les proportions:        

        (if (= scale TRUE)

            (if (not (and (= width original-width) (= height original-height))) ;; si la largeur et la hauteur choisies ne sont pas egales aux originales

    ;;;; option: Conserver les proportions

                (begin
                        (if (= keep-ratio TRUE)

                                (begin
                                        (set! width (* ratio original-width))
                                        (set! height (* ratio original-height))
                                ) ;; fin du begin
                        ) ;; fin du if de "keep-ratio"

      ;;  calque sans interpolation
                        (gimp-drawable-set-name drw (string-append img-name "_NO-interpolation"))
                        (gimp-selection-all img)
                        (gimp-edit-copy drw)
                        (gimp-image-scale-full img width height INTERPOLATION-NONE)

      ;;  calque avec interpolation si l'opacite est superieure a .
                        (if (> opac 0)
                        (begin
                        (set! layer-interpol-opt (car (gimp-layer-new img width height
								    type-d (string-append img-name "_interpolation") opac NORMAL-MODE)))
                        (gimp-image-add-layer img layer-interpol-opt 0)
                        (let ((floating-sel (car (gimp-edit-paste layer-interpol-opt FALSE))))
                                (gimp-layer-scale-full floating-sel width height FALSE interpol-opt)
                                (gimp-layer-set-offsets floating-sel 0 0)
                                (gimp-floating-sel-anchor floating-sel)
                        )
                        ) ;; fin du begin
                        ) ;; fin du if de (> opac 0)

      ;;  fusionner les deux calques s'il y a mur gauche ou droite ET si l'opacite est superieure a 0
                        (if (and (or (= leftwall TRUE) (= rightwall TRUE)) (> opac 0))
                                (begin
                                        (gimp-image-merge-down img layer-interpol-opt EXPAND-AS-NECESSARY)
                                        (set! drw (car (gimp-image-get-active-layer img))) 
                                        (gimp-drawable-set-name drw (string-append img-name "_scaled"))
                                )
                        )

                ) ;; fin du begin

            ) ;; fin du if (largeur et hauteur choisies ne sont pas egales aux originales)

         ) ;; fin du if de "scale"


   ) ;; fin du 1er let*

        

   ( let* (
                (type-d (car (gimp-drawable-type drw)))
                (drw (car (gimp-image-get-active-layer img)))
                (width (car (gimp-image-width img))) 
                (height (car (gimp-image-height img)))
                (new-height (+ (round (+ (/ width 2)(/ 1 100))) height)) 
                (img-name (car (gimp-image-get-name img)))
                (layer-iso-wall1 0)
                (layer-iso-wall2 0)
                (num-of-col 0)
            )


      ;; Si besoin, ajouter un canal alpha, definir le type du drawable, le redimensionner et le rendre invisible

        (if (or (= leftwall TRUE) (= rightwall TRUE))
                (begin
                        (gimp-layer-add-alpha drw)
                        (set! type-d (car (gimp-drawable-type drw)))
                        (gimp-image-resize img width new-height 0 0)
                        (gimp-drawable-set-visible drw FALSE)
                )
        ) ;; fin du if


;;;;;;;; OPTION: Mur gauche isometrique:

        (if (= leftwall TRUE)
                (begin
                        (set! layer-iso-wall1 (car (gimp-layer-new img width new-height
								    type-d (string-append img-name "_ISO_L") 100 NORMAL-MODE)))
                        (gimp-image-add-layer img layer-iso-wall1 0)

                        (while (<= num-of-col width)
    
                                (gimp-rect-select img num-of-col 0 2 height CHANNEL-OP-REPLACE FALSE 0) 
                                (gimp-edit-copy drw)
                                (let ((floating-sel (car (gimp-edit-paste layer-iso-wall1 FALSE))))
                                (gimp-layer-set-offsets floating-sel num-of-col (- (- (round (+ (/ width 2) (/ 1 100))) 1) (/ num-of-col 2)))
                                (gimp-floating-sel-anchor floating-sel)
                                )

                                (set! num-of-col (+ num-of-col 2))
                        ) ;; fin du while
	(set! num-of-col 0)
        (gimp-layer-resize-to-image-size layer-iso-wall1)

                 ) ;; fin du begin
        ) ;; fin du if de "leftwall"


;;;;;;;; OPTION: Mur droite isometrique:

        (if (= rightwall TRUE)
                (begin
                        (set! layer-iso-wall2 (car (gimp-layer-new img width new-height
								    type-d (string-append img-name "_ISO_R") 100 NORMAL-MODE)))
                        (gimp-image-add-layer img layer-iso-wall2 0)

                        (while (<= num-of-col width)

                                (gimp-rect-select img num-of-col 0 2 height CHANNEL-OP-REPLACE FALSE 0) 
                                (gimp-edit-copy drw)
                                (let ((floating-sel (car (gimp-edit-paste layer-iso-wall2 FALSE))))
                                (gimp-layer-set-offsets floating-sel num-of-col (/ num-of-col 2))
                                (gimp-floating-sel-anchor floating-sel)
                                )

                                (set! num-of-col (+ num-of-col 2))
                        ) ;; fin du while

        (gimp-image-set-active-layer img layer-iso-wall2)
        (gimp-selection-all img)
        (gimp-layer-set-offsets layer-iso-wall2 0 1)
        (gimp-layer-resize-to-image-size layer-iso-wall2)

                 ) ;; fin du begin
        ) ;; fin du if de "rightwall"

   ) ;; fin du 2eme let*

        (gimp-displays-flush) ;; actualiser l'affichage de l'image
        (gimp-image-undo-group-end img) ;; fin d'historique d'annulation
 ) ;; fin de la fonction

(script-fu-register
    "script-fu-02-Isometric-Wall"
    "<Image>/Filters/2D Isometric-Fu/02 - Isometric Wall"
    "It skews an image in order to help to draw walls for isometric projection. It can also scale image with few options (ratio, a second layer with interpolation and opacity)..."
    "Loïc Guyader (froGgy)"
    "Copyright"
    "02/2011"
    "" ;; types d'images supportes par le script
        SF-IMAGE        "Image"                         0
        SF-DRAWABLE     "Drawable"                      0
        SF-TOGGLE       _"Convert to RGB color mode"    FALSE
        SF-TOGGLE       _"Scale image:"                 FALSE
        SF-TOGGLE       _"Keep aspect ratio! (the minimum ratio is the reference)" FALSE
        SF-ADJUSTMENT   _"Width"                        '(256 4 2048 1 8 0 0)
        SF-ADJUSTMENT   _"Height"                       '(256 4 2048 1 8 0 0)
        SF-OPTION       _"Interpolation"                '(_"Cubic" _"Linear" _"Sinc (Lanczos3)")
        SF-ADJUSTMENT   _"Opacity"                      '(50 0 100 1 10 1 0)
        SF-TOGGLE       _"Left wall"                    TRUE
        SF-TOGGLE       _"Right wall"                   TRUE
 ) ;; fin du register

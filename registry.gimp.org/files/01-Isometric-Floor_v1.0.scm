; GIMP script-fu-01-isometric-floor
; version 1.0 2011.02.03
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


(define (script-fu-01-isometric-floor
                                        img
                                        drw
                                        convert
                                        scale
                                        keep-ratio
                                        width
                                        height
                                        interpol
                                        opac
                                        iso-floor
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
                        (if (and (= iso-floor TRUE) (> opac 0))
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
                (var-is-odd 0) ;; variable est impaire ;; 0 = False
                (new-width (round (+ (+ width height) (/ 1 100)))) 
                (new-height (round (+ (+ width height) (/ 1 100))))
                (img-name (car (gimp-image-get-name img)))
                (layer-iso-floor-tmp 0)
                (layer-iso-floor 0)
                (num-of-row 0) ;; numero de la ligne
                (num-of-col 0) ;; numero de la colonne
            )

        (if (= iso-floor TRUE)

                (begin

    ;;;; Mettre la variable à 1 (vrai) si la somme de la largeur et de la hauteur de l'image est impaire
                        (if (odd? (+ width height))
                                (set! var-is-odd 1)  ;; 1 = True
                        ) ;; fin du if


    ;;;; Ajouter un canal alpha, definir le type du drawable et le rendre invisible
                        (gimp-layer-add-alpha drw)
                        (set! type-d (car (gimp-drawable-type drw)))
                        (gimp-drawable-set-visible drw FALSE)

    ;;;; Redimensionner l'image et ajouter le calque temporaire _ISO_tmp
                        (gimp-image-resize img new-width new-height 0 0)
                        (set! layer-iso-floor-tmp (car (gimp-layer-new img new-width new-height
								    type-d (string-append img-name "_ISO_tmp") 100 NORMAL-MODE)))
                        (gimp-image-add-layer img layer-iso-floor-tmp 0)

    ;;;; Transformer en isometrie (1ere partie)
                        (while (< num-of-row height)

                                (gimp-rect-select img 0 num-of-row width 1 CHANNEL-OP-REPLACE FALSE 0) 
                                (gimp-edit-copy drw)
                                (let ((floating-sel (car (gimp-edit-paste layer-iso-floor-tmp FALSE))))
                                (gimp-layer-set-offsets floating-sel num-of-row num-of-row)
                                (gimp-floating-sel-anchor floating-sel)
                                )

                        (set! num-of-row (+ num-of-row 1))
                        ) ;; fin du while

    ;;;; Supprimer le "drawable" (image d'origine), ajouter le calque _ISO, rendre actif et "drawable" le calque _ISO_tmp et le rendre invisible
                        (gimp-image-remove-layer img drw)
                        (set! layer-iso-floor (car (gimp-layer-new img new-width new-height
								    type-d (string-append img-name "_ISO") 100 NORMAL-MODE)))
                        (gimp-image-add-layer img layer-iso-floor 0)

                        (gimp-image-set-active-layer img layer-iso-floor-tmp)
                	(set! drw (car (gimp-image-get-active-drawable img)))
                        (gimp-drawable-set-visible drw FALSE)

    ;;;; Transformer en isometrie (2eme partie)
                        (while (<= num-of-col new-width)

                                (gimp-rect-select img num-of-col 0 2 height CHANNEL-OP-REPLACE FALSE 0) 
                                (gimp-edit-copy drw)
                                (let ((floating-sel (car (gimp-edit-paste layer-iso-floor FALSE))))
                                (gimp-layer-set-offsets floating-sel num-of-col (- (- (round (+ (/ width 2) (/ 1 100))) 1) (/ num-of-col 2)))
                                (gimp-floating-sel-anchor floating-sel)
                                )

                        (set! num-of-col (+ num-of-col 2))
                        ) ;; fin du while

    ;;;; Supprimer le calque _ISO_tmp, redimensionner l'image et faire calque _ISO aux dimensions de l'image
                        (gimp-image-remove-layer img layer-iso-floor-tmp)
                        (gimp-image-resize img new-width (/ (+ width (+ height var-is-odd)) 2) 0 0)
                        (gimp-layer-resize-to-image-size layer-iso-floor)

                ) ;; fin du begin de "iso-floor"

        ) ;; fin du if de "iso-floor"
 
   ) ;; fin du 2eme let*


(gimp-displays-flush) ;; actualiser l'affichage de l'image 
(gimp-image-undo-group-end img) ;; fin d'historique d'annulation


 ) ;; fin de la fonction

(script-fu-register
    "script-fu-01-isometric-floor"
    "<Image>/Filters/2D Isometric-Fu/01 - Isometric Floor"
    "It turns a flat image into isometric image. This helps to draw floors. It can also scale image with few options (ratio, a second layer with interpolation and opacity)..."
    "Loïc Guyader (froGgy)"
    "Copyright"
    "02/2011"
    "" ;; types d'images supportes par le script
        SF-IMAGE        "Image"                         0
        SF-DRAWABLE     "Drawable"                      0
        SF-TOGGLE       _"Convert to RGB color mode"	FALSE
        SF-TOGGLE       _"Scale image:"                 FALSE
        SF-TOGGLE       _"Keep aspect ratio! (the minimum ratio is the reference)" FALSE
        SF-ADJUSTMENT   _"Width"                        '(256 4 2048 1 8 0 0)
        SF-ADJUSTMENT   _"Height"                       '(256 4 2048 1 8 0 0)
        SF-OPTION       _"Interpolation"                '(_"Cubic" _"Linear" _"Sinc (Lanczos3)")
        SF-ADJUSTMENT   _"Opacity"                      '(50 0 100 1 10 1 0)
        SF-TOGGLE       _"Isometric floor"              TRUE
 ) ;; fin du register

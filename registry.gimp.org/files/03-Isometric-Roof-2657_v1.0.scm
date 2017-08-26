; GIMP script-fu-01-isometric-roof-2657
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


(define (script-fu-03-isometric-roof-2657
                                        img
                                        drw
                                        convert
                                        scale
                                        keep-ratio
                                        width
                                        height
                                        interpol
                                        opac
                                        rotation
                                        transf-dir-rota
                                        interpol-rota
                                        opac-rota
                                        cut-from-alpha
                                        fusion
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

    ;;;; option: Conserver les proportions:

                (begin
                (if (= keep-ratio TRUE)

                        (begin
                                (set! width (* ratio original-width))
                                (set! height (* ratio original-height))
                        ) ;; fin du begin
                ) ;; fin du if de "keep-ratio"

    ;;;; option: Reduire ou augmenter la taille de l'image (deux calques, l'un sans interpolation et l'autre avec):

                        (begin
                                ;; calque (converti ou non ) resultant de l'operation "scale"
                                (gimp-drawable-set-name drw (string-append img-name "_NO-interpolation"))
                                (gimp-selection-all img)
                                (gimp-edit-copy drw)
                                (gimp-image-scale-full img width height INTERPOLATION-NONE)
                                

                                ;; calque avec interpolation si l'opacite est superieure a 0
                                (if (> opac 0)
                                        (begin
                                                (set! layer-interpol-opt (car (gimp-layer-new img width height
                								    type-d (string-append img-name "_interpolation") opac NORMAL-MODE)))
                                                (gimp-image-add-layer img layer-interpol-opt 0)
                                                (let ((floating-sel (car (gimp-edit-paste layer-interpol-opt FALSE))))
                                                        (gimp-layer-scale-full floating-sel width height TRUE interpol-opt)
                                                        (gimp-layer-set-offsets floating-sel 0 0)
                                                        (gimp-floating-sel-anchor floating-sel)
                                                )
                                        )
                                )
                                ;; fusionner les calques visibles s'il y a rotation ou fusion ET si l'opacite est superieure a 0
                                (if (and (or (= rotation TRUE) (= fusion TRUE)) (> opac 0))
                                        (begin
                                                (gimp-image-merge-down img layer-interpol-opt EXPAND-AS-NECESSARY)
                                                (set! drw (car (gimp-image-get-active-layer img))) 
                                                (gimp-drawable-set-name drw (string-append img-name "_scaled"))

                                        )
                                )
                        ) ;; fin du begin (deux calques et fusion)

                ) ;; fin du begin de "scale"
              ) ;; fin du if (largeur et hauteur choisies ne sont pas egales aux originales)
        ) ;; fin du if de "scale"


   ) ;; fin du 1er let*




   ( let* (
                (drw (car (gimp-image-get-active-layer img)))
                (type-d (car (gimp-drawable-type drw)))
                (width (car (gimp-image-width img))) 
                (height (car (gimp-image-height img)))
                (img-name (car (gimp-image-get-name img)))
                (layer-rota-interpol-none 0)
                (layer-rota-interpol-opt 0)
                (transf-opt
                        (cond
                        ((equal? transf-dir-rota 0) TRANSFORM-BACKWARD)
                        ((equal? transf-dir-rota 1) TRANSFORM-FORWARD)
                        )
                )
                (interpol-opt
                        (cond
                        ((equal? interpol-rota 0) INTERPOLATION-CUBIC)
                        ((equal? interpol-rota 1) INTERPOLATION-LINEAR)
                        ((equal? interpol-rota 2) INTERPOLATION-LANCZOS)
                        )
                )
            )


;;;;;;;; OPTION: Effectuer une rotation arriere ou avant d'environ 26.57 degres (deux calques, l'un sans interpolation et l'autre avec):
               ; NB: (180 / pi) * arctan(0.5) = 26.5650512 (calcul Google) soit en Lisp (* (/ 180 *pi*) (atan(/ 1 2))) ce qui fait environ 26.57 degres

        (if (= rotation TRUE)
                        (begin
                                ;; redimensionner l'image en egalisant les cotes du cadre pour eviter que les rectangles sortent a la rotation
                                     ; si la largeur est inferieure ou egale a la hauteur  
                                (if (<= width height)
                                        (begin
                                                (gimp-image-resize img height height (/ (- height width) 2) 0)
                                                (set! width (car (gimp-image-width img))) 
                                                (set! height (car (gimp-image-height img)))
                                        )
                                     ; sinon...
                                        (begin
                                                (gimp-image-resize img width width 0 (/ (- width height) 2))
                                                (set! width (car (gimp-image-width img))) 
                                                (set! height (car (gimp-image-height img)))
                                        )
                                ) ;; fin du if

                                (gimp-drawable-set-visible drw FALSE)

                                ;; redimensionner l'image apres avoir ajoute un canal alpha
                                (gimp-layer-add-alpha drw)
                                (gimp-image-resize img  (+ width (/ width 2)) (+ height (/ height 2)) (/ width 4) (/ height 4))
                                (gimp-layer-resize-to-image-size drw)
                                (gimp-selection-all img)

                                ;; ajouter un calque a partir du resultat de la fusion des deux calques precedents (ou du seul calque si l'opacite etait 0); celui qui sera pivote sans interpolation
                                (set! layer-rota-interpol-none (car (gimp-layer-new-from-drawable drw img)))
                                (gimp-image-add-layer img layer-rota-interpol-none 0)

                                ;; idem, ajouter un calque; celui qui sera pivote en uilisant une interpolation
                                (if (> opac-rota 0) ; si l'opacite est superieure a 0 (car il est inutile d'ajouter un calque que l'on ne veut pas voir)
                                        (begin
                                                (set! layer-rota-interpol-opt (car (gimp-layer-new-from-drawable drw img)))
                                                (gimp-image-add-layer img layer-rota-interpol-opt 0)
                                        )
                                )

                                ;; changer le nom du calque sans interpolation en fonction du type de transformation (arriere ou avant)
                                (if (= transf-opt TRANSFORM-BACKWARD)
                                                (gimp-drawable-set-name layer-rota-interpol-none (string-append img-name "_NO-interpolation_transf-B"))
                                               ;; TRANSFORM-BACKWARD
                                                (gimp-drawable-set-name layer-rota-interpol-none (string-append img-name "_NO-interpolation_transf-F"))
                                )

                                ;; preparer le premier calque
                                (gimp-image-set-active-layer img layer-rota-interpol-none)
                                (set! drw (car (gimp-image-get-active-layer img))) 
                                (gimp-layer-add-alpha drw)
                                (gimp-layer-resize-to-image-size drw)

                                ;; effectuer une rotation du calque sans utiliser l'interpolation et le rendre visible
                                (gimp-drawable-transform-rotate drw     (atan (/ 1 2))
                                                                        TRUE
                                                                        0
                                                                        0
                                                                        transf-opt
                                                                        INTERPOLATION-NONE
                                                                        FALSE
                                                                        3
                                                                        TRANSFORM-RESIZE-ADJUST
                                )

                               (gimp-floating-sel-anchor (car (gimp-image-get-active-layer img)))
                               (gimp-drawable-set-visible drw TRUE)

                                ;; preparer le deuxieme calque si l'opacite est superieure a 0 
                                (if (> opac-rota 0)
                                (begin
                                (gimp-image-set-active-layer img layer-rota-interpol-opt)
                                (set! drw (car (gimp-image-get-active-layer img)))
                                (gimp-layer-add-alpha drw)
                                (gimp-layer-resize-to-image-size drw)

                                ;; effectuer une rotation du calque en utilisant l'interpolation
                                (gimp-drawable-transform-rotate drw     (atan (/ 1 2))
                                                                        TRUE
                                                                        0
                                                                        0
                                                                        transf-opt
                                                                        interpol-opt
                                                                        FALSE
                                                                        3
                                                                        TRANSFORM-RESIZE-ADJUST
                                )

                                ;; changer le nom du calque avec interpolation en fonction du type de transformation (arriere ou avant)
                                (if (= transf-opt TRANSFORM-BACKWARD)
                                        (begin
                                                (gimp-drawable-offset drw FALSE OFFSET-TRANSPARENT 1 0)
                                                (gimp-drawable-set-name layer-rota-interpol-opt (string-append img-name "_interpolation_transf-B"))
                                        )
                                        (begin ;; TRANSFORM-BACKWARD
                                                (gimp-drawable-offset drw FALSE OFFSET-TRANSPARENT 0 1)
                                                (gimp-drawable-set-name layer-rota-interpol-opt (string-append img-name "_interpolation_transf-F"))
                                        )
                                )

                                ;; ajuster le calque a l'image, lui modifier l'opacite et le rendre visible
                                (gimp-layer-resize-to-image-size drw)
                                (gimp-layer-set-opacity drw opac-rota)
                                (gimp-drawable-set-visible drw TRUE)
                                ) ;; fin du begin de "opac-rota"
                                ) ;; fin du if de "opac-rota"


;;;;;;;; OPTION: Couper a partir du canal alpha du calque sans interpolation pour un crenelage regulier:

                                (if (= cut-from-alpha TRUE)
                                        (begin
                                                (gimp-selection-layer-alpha layer-rota-interpol-none)
                                                (gimp-selection-invert img)
                                                (gimp-edit-clear drw)
                                                (gimp-selection-none img)
                                        ) ;; fin du begin de "cut-from-alpha"
                                ) ;; fin du if de "cut-from-alpha"

                        ) ;; fin du begin de "rotation"
         ) ;; fin du if de "rotation"


;;;;;;;; OPTION: Fusionner les calques visibles et changer le nom en fonction du type de transformation:

        (if (= fusion TRUE)
                (begin
                        (set! drw (car (gimp-image-merge-visible-layers img 2)))
                        (if (= rotation TRUE)
                                (if (= transf-opt TRANSFORM-BACKWARD)
                                                (gimp-drawable-set-name drw (string-append img-name "_transf-B"))
                                               ;; TRANSFORM-BACKWARD
                                                (gimp-drawable-set-name drw (string-append img-name "_transf-F"))
                                )                        
                        )
               ) ;; fin du begin de "fusion"
        ) ;; fin du if de "fusion"

   ) ;; fin du 2eme let*

        (gimp-displays-flush) ;; actualiser l'affichage de l'image
        (gimp-image-undo-group-end img) ;; fin d'historique d'annulation
 ) ;; fin de la fonction

(script-fu-register
    "script-fu-03-isometric-roof-2657"
    "<Image>/Filters/2D Isometric-Fu/03 - Isometric Roof (26.57 degrees)"
    "Rotate -/+ 26.57 degrees to help to make an isometric roof"
    "Loïc Guyader (froGgy)"
    "Copyright"
    "01/2011"
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
        SF-TOGGLE       _"Rotate -/+ 26.57 degrees:"    TRUE
        SF-OPTION       _"Transform-direction"          '(_"TRANSFORM-BACKWARD" _"TRANSFORM-FORWARD")
        SF-OPTION       _"Interpolation"                '(_"Cubic" _"Linear" _"Sinc (Lanczos3)")
        SF-ADJUSTMENT   _"Opacity"                      '(50 0 100 1 10 1 0)
        SF-TOGGLE       _"Cut from alpha"               FALSE
        SF-TOGGLE       _"Merge visible layers"         FALSE
 ) ;; fin du register

; GIMP script-fu-00-offset-by-froggy
; version 1.0 2011.02.26 Loïc Guyader
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


(define (script-fu-00-offset-by-froggy
                                        img
                                        drw
                                        new-layer
                                        tilewidth
                                        tileheight
                                        offset-rc
                                        offset-xy
                                        offset-eo

        )

(gimp-image-undo-group-start img) ;; debut d'historique d'annulation



   ( let* (
                (width (car (gimp-image-width img))) 
                (height (car (gimp-image-height img)))
                (type-d (car (gimp-drawable-type drw)))
                (drw-name (car (gimp-drawable-get-name drw)))
                (offset-layer 0)
                (offset-rc-opt
                        (cond
                        ((equal? offset-rc 0) 0)
                        ((equal? offset-rc 1) 1)
                        )
                )
                (i-size (if (= offset-rc-opt 0) height width))
                (i-tile-size (if (= offset-rc-opt 0) tileheight tilewidth))
                (i-tile-center (if (= offset-rc-opt 0) (/ tilewidth 2) (/ tileheight 2))) ;; auto offset
                (offset-xy (if (= offset-xy 0) i-tile-center offset-xy))
                (offset-eo-opt
                        (cond
                        ((equal? offset-eo 0) i-tile-size)
                        ((equal? offset-eo 1) 0)
                        )
                )
          )


                ;; Save the context
                (gimp-context-push)


                ;; Add a new layer?
                (if (= new-layer TRUE)
                        (begin
                                (set! offset-layer (car (gimp-layer-copy drw TRUE)))
                                (gimp-image-add-layer img offset-layer -1)
                                (gimp-drawable-set-name offset-layer (string-append drw-name "__offset"))
                                (gimp-drawable-set-visible drw FALSE)
                                (set! drw offset-layer)
                        )
                )


                ;; OFFSET
                (let loop ((i-num offset-eo-opt)) ;; even/odd
                        (if (< i-num i-size)

                                (begin
                                        (if (= offset-rc-opt 0) ;; rows
                                                (gimp-rect-select img 0 i-num width tileheight CHANNEL-OP-REPLACE FALSE 0)
                                                                ;; columns
                                                (gimp-rect-select img i-num 0 tilewidth height CHANNEL-OP-REPLACE FALSE 0)
                                        )
                                                
                                                (gimp-edit-cut drw)
                                                (let ((floating-sel (car (gimp-edit-paste drw FALSE))))
                                                (set! floating-sel (car (gimp-image-get-active-drawable img)))

                                        (if (= offset-rc-opt 0)  ;; rows
                                                (gimp-drawable-offset floating-sel TRUE OFFSET-TRANSPARENT offset-xy 0)
                                                                ;; columns
                                                (gimp-drawable-offset floating-sel TRUE OFFSET-TRANSPARENT 0 offset-xy)
                                        )

                                                (gimp-floating-sel-anchor floating-sel)
                                                )

                                        (if (= offset-rc-opt 0) ;; rows
                                                (loop (+ i-num (* tileheight 2)))
                                                                ;; columns
                                                (loop (+ i-num (* tilewidth 2)))
                                        )

                                ) ;; fin du begin
                        ) ;; fin du if
                ) ;; fin du let de loop


                ;; Restore the context
                (gimp-context-pop)
    
   ) ;; fin du let*


(gimp-displays-flush) ;; actualiser l'affichage de l'image 
(gimp-image-undo-group-end img) ;; fin d'historique d'annulation


 ) ;; fin de la fonction

(script-fu-register
    "script-fu-00-offset-by-froggy"
    "<Image>/Filters/2D Isometric-Fu/00 - Offset by froGgy"
    "This script-fu allows to shift tiles/bricks/blocks one row or column out of two. It will help to draw brick walls, etc..."
    "Loïc Guyader (froGgy)"
    "Copyright"
    "02/2011"
    "" ;; types d'images supportes par le script
        SF-IMAGE        "Image"                 0
        SF-DRAWABLE     "Drawable"              0
        SF-TOGGLE       _"New layer?"           FALSE
        SF-ADJUSTMENT   _"Tile's width"         '(32 2 512 1 8 0 0)
        SF-ADJUSTMENT   _"Tile's height"        '(16 2 512 1 8 0 0)
        SF-OPTION       _"Rows/cols"            '(_"Rows" _"Columns")
        SF-ADJUSTMENT   _"Offset (0 = auto)"    '(0 -256 256 1 4 0 0)
        SF-OPTION       _"Even/odd"             '(_"Even" _"Odd")
 ) ;; fin du register

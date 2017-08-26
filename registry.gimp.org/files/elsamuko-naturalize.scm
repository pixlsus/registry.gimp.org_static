; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; http://www.gnu.org/licenses/gpl-3.0.html
;
; Copyright (C) 2013 elsamuko <elsamuko@web.de>
;
; Version 0.1 - Renaturalize oversaturated images
;


(define (elsamuko-naturalize aimg adraw)
  (let* ((img (car (gimp-drawable-get-image adraw)))
         (owidth (car (gimp-image-width img)))
         (oheight (car (gimp-image-height img)))
         (desatlayer (car (gimp-layer-copy adraw FALSE)))
         (tmplayer (car (gimp-layer-copy adraw FALSE)))
         (desatlayermask (car (gimp-layer-create-mask desatlayer ADD-WHITE-MASK)))
         (floatingsel 0))
    

    ; init
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
        (gimp-image-convert-rgb img)
        )
    

    ; the desaturated layer
    (gimp-image-add-layer img desatlayer -1)
    (gimp-drawable-set-name desatlayer "B/W")
    (gimp-desaturate-full desatlayer DESATURATE-LIGHTNESS)
    (gimp-layer-set-opacity desatlayer 50)
    

    ; add another temporary copy and run saturation plugin
    (gimp-image-add-layer img tmplayer -1)
    (gimp-drawable-set-name tmplayer "temp")
    (elsamuko-saturation RUN-NONINTERACTIVE img tmplayer 0)


    ; add calculated saturation as layer mask to the desaturated layer
    (gimp-layer-add-mask desatlayer desatlayermask)
    (gimp-selection-all img)
    (gimp-edit-copy tmplayer)
    (set! floatingsel (car (gimp-edit-paste desatlayermask TRUE)))
    (gimp-floating-sel-anchor floatingsel)
    

    ; tidy up
    (gimp-image-remove-layer img tmplayer)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    )
  )

(script-fu-register "elsamuko-naturalize"
                    _"_Naturalize"
                    "This filter reduces the saturation in high saturated areas.
                    Needs the elsamuko-saturation plugin."
                    "elsamuko <elsamuko@web.de>"
                    "elsamuko"
                    "24/06/13"
                    "RGB*"
                    SF-IMAGE       "Input image"          0
                    SF-DRAWABLE    "Input drawable"       0
                    )
(script-fu-menu-register "elsamuko-naturalize" _"<Image>/Colors")

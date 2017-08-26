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
; Copyright (C) 2010 elsamuko <elsamuko@web.de>
;

(define (elsamuko-rainy-landscape aimg adraw
                                  cast preserve
                                  yellow-hues red-hues)
  (let* ((img (car (gimp-drawable-get-image adraw)))
         (owidth (car (gimp-image-width img)))
         (oheight (car (gimp-image-height img)))
         
         (hue-layer     (car (gimp-layer-copy adraw FALSE)))
         (bw-layer      (car (gimp-layer-copy adraw FALSE)))
         (extract-layer (car (gimp-layer-copy adraw FALSE)))
         
         (cast-layer (car (gimp-layer-new img
                                          owidth 
                                          oheight
                                          1
                                          "Blue Cast" 
                                          40 
                                          MULTIPLY-MODE)))
         (cast-layer-mask (car (gimp-layer-create-mask cast-layer ADD-WHITE-MASK)))
         (preserve-layer 0)
         (preserve-layer-mask 0)
         )
    
    ; init
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
        (gimp-image-convert-rgb img)
        )

    ;add hue layer
    (gimp-image-add-layer img hue-layer -1)
    (gimp-drawable-set-name hue-layer "Hue: Y->G, R->Y")
    
    ;create layer mask for the cast layer
    (gimp-image-add-layer img bw-layer -1)
    (gimp-drawable-set-name bw-layer "B/W Filter")
    (plug-in-colors-channel-mixer 1 img bw-layer TRUE
                                  -0.466 0.133 1.333  ;R
                                  0      0     0      ;G
                                  0      0     0 )    ;B)
    (gimp-invert bw-layer)
    (gimp-drawable-set-visible bw-layer FALSE)
    
    ;preserve yellow
    (gimp-image-add-layer img extract-layer -1)
    (gimp-drawable-set-name extract-layer "Extract")
    (gimp-layer-set-mode extract-layer GRAIN-EXTRACT-MODE)
    (plug-in-colortoalpha 1 img extract-layer preserve)
    (gimp-edit-copy-visible img)
    (set! preserve-layer (car (gimp-layer-new-from-visible img img "Preserve Y")))
    (gimp-image-add-layer img preserve-layer 0)
    (gimp-brightness-contrast preserve-layer -90 90)
    
    (set! preserve-layer-mask (car (gimp-layer-create-mask preserve-layer ADD-COPY-MASK)))
    (gimp-layer-add-mask preserve-layer preserve-layer-mask)
    (gimp-drawable-set-visible extract-layer FALSE)
    
    ;add cast layer
    (gimp-image-add-layer img cast-layer -1)
    (gimp-image-raise-layer-to-top img preserve-layer)
    (gimp-context-set-foreground cast)
    (gimp-selection-all img)
    (gimp-edit-bucket-fill cast-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
    
    (gimp-edit-copy bw-layer)
    (gimp-layer-add-mask cast-layer cast-layer-mask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste cast-layer-mask TRUE)))
    (gimp-selection-none img)
    
    ;colorize hue layer
    (gimp-hue-saturation hue-layer YELLOW-HUES yellow-hues 0 0)
    (gimp-hue-saturation hue-layer RED-HUES red-hues 0 0)

    ;remove unneccessary layers
    (gimp-image-remove-layer img extract-layer)
    (gimp-image-remove-layer img bw-layer)
    
    ; tidy up
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    )
  )

(script-fu-register "elsamuko-rainy-landscape"
                    "_Rainy Landscape"
                    "After rain effect."
                    "elsamuko <elsamuko@web.de>"
                    "elsamuko"
                    "12/03/10"
                    "RGB*"
                    SF-IMAGE       "Input image"          0
                    SF-DRAWABLE    "Input drawable"       0
                    SF-COLOR       "Blue Cast"            '( 0   0  255)
                    SF-COLOR      _"Preserve Color"       '(255 255  0 )
                    SF-ADJUSTMENT _"Yellow Hues"          '(20 0 50 1 5 0 0)                    
                    SF-ADJUSTMENT _"Red Hues"             '(30 0 50 1 5 0 0)                    
                    )

(script-fu-menu-register "elsamuko-rainy-landscape" _"<Image>/Filters/Light and Shadow")

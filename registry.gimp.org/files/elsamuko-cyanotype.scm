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


(define (elsamuko-cyanotype aimg adraw
                            color1 color2
                            localcontrast 
                            overlay
                            blackwhite)
  (let* ((img (car (gimp-drawable-get-image adraw)))
         (owidth (car (gimp-image-width img)))
         (oheight (car (gimp-image-height img)))
         (tmplayer1 0)         
         (tmplayer2 0)         
         (contrastlayer 0)
         (bw-layer 0)
         (overlay-layer (car (gimp-layer-copy adraw FALSE)))
         (blue-layer (car (gimp-layer-new img
                                          owidth 
                                          oheight
                                          RGBA-IMAGE
                                          "Prussian Blue" 
                                          100 
                                          NORMAL-MODE)))
         (blue-mask (car (gimp-layer-create-mask blue-layer ADD-WHITE-MASK)))
         (white-layer (car (gimp-layer-new img
                                           owidth 
                                           oheight
                                           RGBA-IMAGE
                                           "Aged White" 
                                           100 
                                           NORMAL-MODE)))
         (white-mask (car (gimp-layer-create-mask white-layer ADD-WHITE-MASK)))
         (blue-overlay-layer (car (gimp-layer-new img
                                                  owidth 
                                                  oheight
                                                  RGBA-IMAGE
                                                  "Blue Overlay" 
                                                  80 
                                                  OVERLAY-MODE)))
         )
    
    ; init
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
        (gimp-image-convert-rgb img)
        )
    
    ;desaturate original
    (if(= blackwhite TRUE)
       (begin
         (gimp-edit-copy-visible img)
         (set! bw-layer (car (gimp-layer-new-from-visible img img "B/W")))
         (gimp-image-add-layer img bw-layer -1)
         (gimp-desaturate-full bw-layer DESATURATE-LIGHTNESS)
         )
       )
    
    ;enhance local contrast
    (if(> localcontrast 0)
       (begin
         (gimp-edit-copy-visible img)
         (set! tmplayer1 (car (gimp-layer-new-from-visible img img "Temp 1")))
         (set! tmplayer2 (car (gimp-layer-new-from-visible img img "Temp 2")))
         (gimp-image-add-layer img tmplayer1 -1)
         (gimp-image-add-layer img tmplayer2 -1)
         (plug-in-unsharp-mask 1 img tmplayer1 60 localcontrast 0)
         (gimp-layer-set-mode tmplayer2 GRAIN-EXTRACT-MODE)
         (gimp-edit-copy-visible img)
         (set! contrastlayer (car (gimp-layer-new-from-visible img img "Local Contrast")))
         (gimp-image-add-layer img contrastlayer -1)
         (gimp-layer-set-mode contrastlayer GRAIN-MERGE-MODE)
         (gimp-image-remove-layer img tmplayer1)
         (gimp-image-remove-layer img tmplayer2)
         )
       )
    
    ;set blue and white layers
    (gimp-image-add-layer img blue-layer -1)
    (gimp-image-add-layer img white-layer -1)
    (gimp-selection-all aimg)
    (gimp-context-set-foreground color1)
    (gimp-edit-bucket-fill blue-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
    (gimp-context-set-foreground color2)
    (gimp-edit-bucket-fill white-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
    
    ;add layer masks
    (gimp-edit-copy adraw)
    (gimp-layer-add-mask blue-layer blue-mask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste blue-mask TRUE)))
    (gimp-invert blue-mask)
    (gimp-layer-add-mask white-layer white-mask)
    (gimp-floating-sel-anchor (car (gimp-edit-paste white-mask TRUE)))
    
    ;set overlay layer
    (if(> overlay 0)
       (begin
         (gimp-image-add-layer img overlay-layer -1)
         (gimp-drawable-set-name overlay-layer "Overlay")
         (gimp-layer-set-mode overlay-layer OVERLAY-MODE)
         (gimp-layer-set-opacity overlay-layer overlay)
         (gimp-desaturate-full overlay-layer DESATURATE-LIGHTNESS)
         )
       )
    
    ;set blue overlay layer
    (gimp-image-add-layer img blue-overlay-layer -1)
    (gimp-selection-all aimg)
    (gimp-context-set-foreground color1)
    (gimp-edit-bucket-fill blue-overlay-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
    (gimp-selection-none img)
    
    ; tidy up
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    )
  )

(script-fu-register "elsamuko-cyanotype"
                    "_Cyanotype"
                    "Cyanotype effect from 1842."
                    "elsamuko <elsamuko@web.de>"
                    "elsamuko"
                    "14/03/10"
                    "RGB*"
                    SF-IMAGE       "Input image"          0
                    SF-DRAWABLE    "Input drawable"       0
                    SF-COLOR      _"Prussian Blue"       '( 61  87 136)
                    SF-COLOR      _"Aged White"          '(251 253 240)
                    SF-ADJUSTMENT _"Local Contrast"      '(0.4 0   2 0.1 0.2 1 0)
                    SF-ADJUSTMENT _"B/W Overlay"         '(50  0  100  1   5 0 0)                    
                    SF-TOGGLE     _"Desaturate Image"     TRUE
                    )

(script-fu-menu-register "elsamuko-cyanotype" _"<Image>/Filters/Artistic")

; This is a script for The GIMP
;
; Description: see below
;
; Version 1.1
; Last changed: 11.06.2009
;
; Copyright (C) 2009 Dr. Martin Rogge <marogge@onlinehome.de>
;
; --------------------------------------------------------------------
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
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
;

(define (script-mr-scale-to-size img draw size)
  (define (round x) (trunc (+ x 0.5)))
  (gimp-image-undo-group-start img)
  (gimp-image-flatten img)
  (let* ( (draw   (car (gimp-image-get-active-drawable img)) ) )
    (if (> (car (gimp-image-width img)) (car (gimp-image-height img)) )
        (gimp-image-scale img (round (/ (* (car (gimp-image-width img)) size ) (car (gimp-image-height img)) ) ) size )
        (gimp-image-scale img size (round (/ (* (car (gimp-image-height img)) size ) (car (gimp-image-width img)) ) ) )
    )
    (gimp-image-undo-group-end img)
    (plug-in-unsharp-mask 1 img draw 0.5 0.1 0)
    (plug-in-unsharp-mask 1 img draw 0.1 0.4 0)
  )
  (gimp-displays-flush)
)

(script-fu-register 
  "script-mr-scale-to-size"
  "<Image>/Tools/Scale to size"
  "Scale image given a target for the smaller side, followed by unsharp mask for the web. Stepwise undo is supported."
  "Dr. Martin Rogge <marogge@onlinehome.de>"
  "Dr. Martin Rogge"
  "11/06/2009"
  "RGB* GRAY*"
  SF-IMAGE    "Image"         0
  SF-DRAWABLE "Drawable"      0
  SF-VALUE    "Target for small side"   "540"
)

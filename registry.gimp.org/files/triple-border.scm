; This is a script for The GIMP
;
; Description: creates up to 3 coloured borders around the image
;
; Version 3.0
; Last changed: 11.06.2009
;
; Copyright (C) 2004 Dr. Martin Rogge <marogge@onlinehome.de>
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

(define (script-mr-border img draw w1 c1 w2 c2 w3 c3)

  (gimp-image-undo-group-start img)
  (gimp-image-flatten img)

  (let* (
       (draw   (car (gimp-image-get-active-drawable img)))
       (save   (car (gimp-context-get-background)))
       (width  (car (gimp-image-width  img)))
       (height (car (gimp-image-height img)))
    )

    (cond ( (> w1 0)
      (gimp-context-set-background c1)
      (set! width  (+ width  w1 w1))
      (set! height (+ height w1 w1))
      (gimp-image-resize img width height w1 w1)
      (gimp-layer-resize-to-image-size draw)
    ) )

    (cond ( (> w2 0)
      (gimp-context-set-background c2)
      (set! width  (+ width  w2 w2))
      (set! height (+ height w2 w2))
      (gimp-image-resize img width height w2 w2)
      (gimp-layer-resize-to-image-size draw)
    ) )

    (cond ( (> w3 0)
      (gimp-context-set-background c3)
      (set! width  (+ width  w3 w3))
      (set! height (+ height w3 w3))
      (gimp-image-resize img width height w3 w3)
      (gimp-layer-resize-to-image-size draw)
    ) )

    (gimp-context-set-background save)
  )
  (gimp-image-undo-group-end img)
  (gimp-displays-flush)
)

(script-fu-register 
  "script-mr-border"
  "<Image>/Tools/Border"
  "Creates up to three coloured borders around the image"
  "Dr. Martin Rogge <marogge@onlinehome.de>"
  "Dr. Martin Rogge"
  "21/12/2004 to 11/06/2009"
  "RGB* GRAY* INDEXED*"
  SF-IMAGE    "Image"          0
  SF-DRAWABLE "Drawable"       0
  SF-VALUE    "Inside Width"   "1"
  SF-COLOR    "Inside Colour"  (car (gimp-context-get-foreground))
  SF-VALUE    "Middle Width"   "46"
  SF-COLOR    "Middle Colour"  (car (gimp-context-get-background))
  SF-VALUE    "Outside Width"  "1"
  SF-COLOR    "Outside Colour" (car (gimp-context-get-background))
)




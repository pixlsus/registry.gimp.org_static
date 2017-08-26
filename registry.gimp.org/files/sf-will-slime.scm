;;  Slime - This is a script for The GIMP to generate a slimy looking bump pattern
;;  Copyright (C) 2010  William Morrison
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.


(define (script-fu-slime img size)
  (let* ((new-layer -1) (wrap-layer -1))

  ; Allows us to lump all the undo states into one big one for the entire script
  (gimp-image-undo-group-start img)
 
  ; Create a new layer
  (set! new-layer (car (gimp-layer-new img 1 1 0 "Slime" 100 0)))
  (gimp-image-add-layer img new-layer -1)
  (gimp-layer-resize-to-image-size new-layer)
  (gimp-drawable-fill new-layer 2)

  ; Process the new layer to add "clouds" of a specific size
  (plug-in-hsv-noise 1 img new-layer 1 0 0 255)
  (plug-in-hsv-noise 1 img new-layer 1 0 0 255)
  (plug-in-pixelize 1 img new-layer (* size 2))
  (gimp-equalize new-layer FALSE)  
  (plug-in-gauss 1 img new-layer (* size 3) (* size 3) 1)

  ; This does all the work for making it look shiny.
  ; This is effectively the same as the plastic wrap script from Photoshop
  (let* (
          (copy1 (car (gimp-layer-copy new-layer 1))) 
          (copy2 (car (gimp-layer-copy new-layer 1)))
          (copy3 -1)
          (copy4 -1)
        )

    ; Magic with the neon filter
    (gimp-image-add-layer img copy1 -1)
    (gimp-image-add-layer img copy2 -1)
    (gimp-desaturate copy1)
    (gimp-desaturate copy2)
    (gimp-invert copy2)
    (plug-in-neon 1 img copy1 size 0)
    (plug-in-neon 1 img copy2 size 0)

    ; Magic with layer blending
    (gimp-layer-set-mode copy2 4)
    (set! copy3 (car (gimp-image-merge-down img copy2 2)))
    (set! copy4 (car (gimp-layer-copy copy3 1)))
    (gimp-image-add-layer img copy4 -1)
    (gimp-layer-set-mode copy4 15)

    ; Playing with levels and more layer blending
    (set! wrap-layer (car (gimp-image-merge-down img copy4 2)))
    (gimp-invert wrap-layer)
    (gimp-layer-set-mode wrap-layer 4)
    (gimp-levels wrap-layer 0 0 (/ 255 size) 1.00 0 255)
    (plug-in-gauss 1 img wrap-layer (/ size 3) (/ size 3) 1)
  )

  ; Merge the remaining layers into one final layer and adjust levels
  (set! new-layer (car (gimp-image-merge-down img wrap-layer 2)))
  (gimp-levels new-layer 0 0 255 0.66 0 255)
  (gimp-image-set-active-layer img new-layer)  

  ; Ends the undo state group 
  (gimp-image-undo-group-end img)

  ; Update the display
  (gimp-displays-flush)

  )
)

(script-fu-register "script-fu-slime"
                    "Slime..."
                    "Adds a new layer and fills it with a slime texture"
                    "Will Morrison"
                    "GNU General Public License"
                    "2010"
                    "RGB*"
                    SF-IMAGE    "Image"         0
		    SF-ADJUSTMENT "Size"	'(5 2 20 1 2 0 0)
)
(script-fu-menu-register "script-fu-slime"
                         "<Image>/Filters/Will's Script Pack")


;;  Shadow Glow - This is a script for The GIMP that inverts the lightness of the layer without affecting colour
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


(define (script-fu-shadow-glow img drawable brightness)
 (let* ((new-layer 0) (array (cons-array 4 'byte)))

 ;Set array values
  (cond
  ((= brightness 0)
    (aset array 0 0)
    (aset array 1 255)
    (aset array 2 255)
    (aset array 3 0)
  )
  ((> brightness 0)
    (aset array 0 brightness)
    (aset array 1 255)
    (aset array 2 255)
    (aset array 3 0)
  )
  ((< brightness 0)
    (aset array 0 0)
    (aset array 1 255)
    (aset array 2 (+ 255 brightness))
    (aset array 3 0)
  )
  )

  (gimp-image-undo-group-start img)

  ; Create a new layer
  (set! new-layer (car (gimp-layer-copy drawable 1)))
  (gimp-layer-set-name new-layer "Glow Layer")
  (gimp-image-add-layer img new-layer -1)

  ; Process the new layer
  ;(gimp-invert new-layer)
  (gimp-curves-spline new-layer 0 4 array)
  (gimp-hue-saturation new-layer 0 100 0 0)
  (gimp-layer-set-mode new-layer 10)

  (gimp-image-undo-group-end img)

  ; Update the display
  (gimp-displays-flush)
 )
)

(script-fu-register "script-fu-shadow-glow"
                    "Shadow Glow..."
                    "Replaces shadows and dark areas with glowing areas. Works best with images that have good light/dark balance"
                    "Will Morrison"
                    "GNU General Public License"
                    "2010"
                    "RGB*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer to invert" 0
		    SF-ADJUSTMENT "Threshold" '(0 -255 255 1 25 0 0)
)
(script-fu-menu-register "script-fu-shadow-glow"
                         "<Image>/Filters/Will's Script Pack")

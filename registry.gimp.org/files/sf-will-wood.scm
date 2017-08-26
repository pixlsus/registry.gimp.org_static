;;  Wood - This is a script for The GIMP to generate a wood grain pattern
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


(define (script-fu-wood-grain img width height)
 (let* ( (new-layer1 0) )

  (gimp-image-undo-group-start img)
 
;; Create the base layer and fill it with the solid noise filter
  (set! new-layer1 (car (gimp-layer-new img 1 1 0 "Wood Grain" 100 0)))
  (gimp-image-add-layer img new-layer1 -1)
  (gimp-layer-resize-to-image-size new-layer1)
  (plug-in-solid-noise 1 img new-layer1 1 0 (random 65535) 2 width height)

;; Playing with hue/saturation and alien map to achieve the end result
  (plug-in-alienmap2 1 img new-layer1 1 0 1 0 15 0 1 0 0 1)
  (plug-in-alienmap2 1 img new-layer1 1 0 1 0 0.1 0 1 0 1 1)
  (gimp-hue-saturation new-layer1 0 0 30 -40)
  
  (gimp-image-undo-group-end img)

  ; Flush the display
  (gimp-displays-flush)

 )
)

(script-fu-register "script-fu-wood-grain"
                    "Wood Grain..."
                    "Adds a new layer and fills it with a tileable woodgrain pattern"
                    "Will Morrison"
                    "GNU General Public License v3+"
                    "2010"
                    "RGB*"
                    SF-IMAGE    "Image"         0
		    SF-ADJUSTMENT "X size" '(9 0.1 16 0.1 1 1 0)
		    SF-ADJUSTMENT "Y size" '(1 0.1 16 0.1 1 1 0)
)

(script-fu-menu-register "script-fu-wood-grain" "<Image>/Filters/Will's Script Pack")




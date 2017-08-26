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


(define (script-fu-wood-grain2 img width height tileable frequency gradient)
 (let* ( (new-layer1 0) )

  (gimp-context-push) ;;save the current context (active gradient, fg and bg colours, etc)
  (gimp-context-set-default-colors) ;; set fg and bg to black and white
  (gimp-context-set-gradient gradient) ;;set active gradient to the chosen gradient

  (gimp-image-undo-group-start img)
 
;; Create the base layer and fill it with solid noise
  (set! new-layer1 (car (gimp-layer-new img 1 1 0 "Wood Grain" 100 0)))
  (gimp-image-add-layer img new-layer1 -1)
  (gimp-layer-resize-to-image-size new-layer1)
  (plug-in-solid-noise 1 img new-layer1 tileable 0 (random 65535) 2 width height)

;; alien map to achieve the banding
  (plug-in-alienmap2 1 img new-layer1 1 0 1 0 frequency 0 1 0 0 1)

;; Gradient map to 
  (plug-in-gradmap 1 img new-layer1)
  
  (gimp-image-undo-group-end img)
  (gimp-context-pop)

  ; Flush the display
  (gimp-displays-flush)

 )
)

(script-fu-register "script-fu-wood-grain2"
                    "Wood Grain 2..."
                    "Adds a new layer and fills it with a tileable woodgrain pattern, with colour defined by a gradient"
                    "Will Morrison"
                    "GNU General Public License v3+"
                    "2010"
                    "RGB*"
                    SF-IMAGE      "Image" 0
		    SF-ADJUSTMENT "X size" '(9 0.1 16 0.1 1 1 0)
		    SF-ADJUSTMENT "Y size" '(1 0.1 16 0.1 1 1 0)
                    SF-TOGGLE     "Make Result Tileable" 1
                    SF-ADJUSTMENT "Frequency" '(15 0 20 0.1 2 2 0)
                    SF-GRADIENT   "Gradient" "Default"
)

(script-fu-menu-register "script-fu-wood-grain2" "<Image>/Filters/Will's Script Pack")




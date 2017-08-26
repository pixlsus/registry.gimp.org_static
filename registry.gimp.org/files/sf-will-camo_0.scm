;;  Camo - This is a script for The GIMP to generate a camouflage pattern
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

;; A helper function to apply one layer of colour
(define (camo-help img clr iterations cutoff size)
  (let* ((newlayer 0) (fgcolor 0))
	(set! fgcolor (car (gimp-context-get-foreground)));preserving the foreground colour

 ;; Create the new layer and the mask
	(set! newlayer (car (gimp-layer-new img 1 1 0 "Camo" 100 0 )))
	(gimp-image-add-layer img newlayer -1)
	(gimp-layer-resize-to-image-size newlayer)
	(gimp-context-set-foreground clr)
	(gimp-drawable-fill newlayer 0)
	(gimp-layer-add-mask newlayer (car (gimp-layer-create-mask newlayer 0)))
	
 ;; Generate the basic smooth camo shapes
	(plug-in-solid-noise 1 img (car (gimp-layer-get-mask newlayer)) 0 0 (random 65535) 1 size size)
	(gimp-equalize (car (gimp-layer-get-mask newlayer)) 0)
	(gimp-threshold (car (gimp-layer-get-mask newlayer)) cutoff 255)
	
 ;; Roughen the edges with the Pick filter
	(while (and (> iterations 0) (not (= 0 cutoff)))
	(plug-in-randomize-pick 1 img (car (gimp-layer-get-mask newlayer)) 100 50 1 0)
	(set! iterations (- iterations 1))
	)
	(plug-in-gauss 1 img (car (gimp-layer-get-mask newlayer)) 5 5 0)
	(gimp-threshold (car (gimp-layer-get-mask newlayer)) 128 255)

 ;; Merge the layer if needed, and restore the foreground colour
	(if (not (= 0 cutoff)) (gimp-image-merge-down img newlayer 1) ())
	(gimp-context-set-foreground fgcolor)
  )
)

(define (script-fu-camo-1 img clr1 use1 clr2 use2 clr3 use3 clr4 use4 clr5 use5 size iterations)
  (let* ((divs 5) (count 0))

  (gimp-image-undo-group-start img)

  (set! divs (+ use1 use2 use3 use4 use5)) ; the number of colours being used
  
  ;; Generate the spots for each colour. All the divs and count stuff is for making sure there's an even distriution of colour
  (if (= 1 use1) (camo-help img clr1 iterations (- 255 (* (- divs count) (/ 255 divs))) size) (set! count (- count 1)))
  (set! count (+ count 1))
  (if (= 1 use2) (camo-help img clr2 iterations (- 255 (* (- divs count) (/ 255 divs))) size) (set! count (- count 1)))
  (set! count (+ count 1))
  (if (= 1 use3) (camo-help img clr3 iterations (- 255 (* (- divs count) (/ 255 divs))) size) (set! count (- count 1)))
  (set! count (+ count 1))
  (if (= 1 use4) (camo-help img clr4 iterations (- 255 (* (- divs count) (/ 255 divs))) size) (set! count (- count 1)))
  (set! count (+ count 1))
  (if (= 1 use5) (camo-help img clr5 iterations (- 255 (* (- divs count) (/ 255 divs))) size) (set! count (- count 1)))
  
  (gimp-image-undo-group-end img)

  ; Update the display
  (gimp-displays-flush)
 )
)

(script-fu-register "script-fu-camo-1"
                    "Camo..."
                    "Generates a layer filled with a camouflage pattern. Be aware that roughening the edges can be very slow."
                    "Will Morrison"
                    "GNU General Public License"
                    "2010"
                    "RGB*"
                    SF-IMAGE    "Image"         0
		    SF-COLOR "Color 1" '(136 125 52)               SF-TOGGLE "Use colour 1" 1
                    SF-COLOR "Color 2" '(62 82 22)                 SF-TOGGLE "Use colour 2" 1
                    SF-COLOR "Color 3" '(82 56 11)                 SF-TOGGLE "Use colour 3" 1
                    SF-COLOR "Color 4" '(50 28 0)                  SF-TOGGLE "Use colour 4" 1
                    SF-COLOR "Color 5" '(0 0 0)                    SF-TOGGLE "Use colour 5" 1 
		    SF-ADJUSTMENT "Size" '(5 0.1 16 1 3 1 0)
		    SF-ADJUSTMENT "Roughness" '(1 0 10 1 3 0 0)
)

(script-fu-menu-register "script-fu-camo-1"
                         "<Image>/Filters/Will's Script Pack")

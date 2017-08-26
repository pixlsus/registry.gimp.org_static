;;  Flames - This is a script for The GIMP to generate a fire animation
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

(define (script-fu-flame xsize ysize numframes flamescale scalex scaley detail gradient background)
 (let* ((img 0) (cloud1 0) (gradient1 0) (newcloud 0) (newgradient 0) ;variables for layers and the image
	(count 1) (imgheight (* ysize 2)) (yshift (/ imgheight numframes)) (flameheight (* ysize flamescale)) );variables for other stuff


  (gimp-context-push) ;;save the current context (active gradient, fg and bg colours, etc)
  (gimp-context-set-default-colors) ;; set fg and bg to black and white
  (gimp-context-set-gradient gradient) ;;set active gradient to the chosen gradient

  (set! img (car (gimp-image-new xsize imgheight 0))) ;; create the new image
  (gimp-image-undo-freeze img) ;; don't save history during the script

  ;; initialize and add the base layers
  (set! cloud1 (car (gimp-layer-new img xsize imgheight 0 "cloud" 100 17))) 
  (set! gradient1 (car (gimp-layer-new img xsize imgheight 0 "Frame" 100 0)))
  (gimp-image-add-layer img gradient1 -1)
  (gimp-image-add-layer img cloud1 -1)

  (plug-in-solid-noise 1 img cloud1 1 0 (rand 2100000000) detail scalex scaley) ;;create the cloud that the fire will be based on
  ;;(plug-in-normalize 1 img cloud1)
  (gimp-edit-blend gradient1 0 0 0 100 0 0 00 00 1 0 0 00 (- imgheight flameheight) 0 imgheight) ;;draw the gradient to define flame height

  (while (< count numframes)

    ;; duplicate the base layers and add the new ones to the image
    (set! newcloud (car (gimp-layer-new-from-drawable cloud1 img)))
    (set! newgradient (car (gimp-layer-new-from-drawable gradient1 img)))
    (gimp-image-add-layer img newgradient -1)
    (gimp-image-add-layer img newcloud -1)

    ;; offset the clouds and merge down
    (gimp-drawable-offset newcloud 1 0 0 (* count (- 0 yshift)))
    (set! newgradient (car (gimp-image-merge-down img newcloud 0)))
    (gimp-drawable-set-name newgradient "Frame")

    ;; gradient map to give the flame its colour
    (plug-in-gradmap 1 img newgradient)
    (if (= background 1) (plug-in-colortoalpha 1 img newgradient '(0 0 0)))

    (set! count (+ 1 count))
  )

  ;; merge and gradient map base layers
  (set! gradient1 (car (gimp-image-merge-down img cloud1 0)))
  (plug-in-gradmap 1 img gradient1)
  (if (= background 1) (plug-in-colortoalpha 1 img gradient1 '(0 0 0)))

  ;; crop image to requested dimensions
  (gimp-image-crop img xsize ysize 0 ysize)

  (gimp-image-undo-thaw img)
  (gimp-context-pop)


  ; Create and update the display
  (gimp-display-new img)
  (gimp-displays-flush)
 )
)

(script-fu-register "script-fu-flame"
                    "Flames..."
                    "Generates an animated fire"
                    "Will Morrison"
                    "Will Morrison"
                    "2010"
                    ""
                    SF-ADJUSTMENT "Image X size" '(800 1 2048 1 50 0 1)
		    SF-ADJUSTMENT "Image Y size" '(600 1 2048 1 50 0 1)
                    SF-ADJUSTMENT "Number of frames" '(50 1 400 1 10 0 1)
		    SF-ADJUSTMENT "Flame height" '(1 0 1.5 0.01 0.1 2 0)
		    SF-ADJUSTMENT "Horizontal scale" '(16 0.1 16 0.1 1 1 0)
                    SF-ADJUSTMENT "Vertical scale" '(7 0.1 16 0.1 1 1 0)
                    SF-ADJUSTMENT "Detail" '(2 0 15 1 1 0 1)
                    SF-GRADIENT "Gradient" "Incandescent" 
                    SF-TOGGLE "Make background transparent" 1
)

(script-fu-menu-register "script-fu-flame"
                         "<Image>/Filters/Will's Script Pack")

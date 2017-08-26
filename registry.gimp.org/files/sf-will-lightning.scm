;;  Flames - This is a script for The GIMP to generate a lightning
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

(define (script-fu-lightning xsize ysize numframes instability frequency detail hue glowstrength background)
 (let* ((img 0) (cloud1 0) (gradient1 0) (newlayer 0) (newgradient 0) (templayer 0);variables for layers and the image
	(count 1) (yshift (/ ysize numframes)) (xshift (/ xsize numframes)) (false 0) (true 1) );variables for other stuff


  (gimp-context-push) ;;save the current context (active gradient, fg and bg colours, etc)
  (gimp-context-set-default-colors) ;; set fg and bg to black and white
  (gimp-context-set-gradient "Default") ;;set active gradient to black->white

  (set! img (car (gimp-image-new xsize ysize 0))) ;; create the new image
  (gimp-image-undo-freeze img) ;; don't save history during the script

  ;; initialize and add the base layers
  (set! cloud1 (car (gimp-layer-new img xsize ysize RGB-IMAGE "cloud" 100 DIFFERENCE-MODE))) 
  (set! gradient1 (car (gimp-layer-new img xsize ysize RGB-IMAGE "Frame" 100 NORMAL-MODE)))
  (gimp-image-add-layer img gradient1 -1)
  (gimp-image-add-layer img cloud1 -1)

  ;;create the cloud that the lightning will be based on
  (plug-in-solid-noise 1 img cloud1 1 0 (rand 2100000000) detail instability frequency)

  ;;draw the gradient to define the direction of the lightning
  (gimp-edit-blend gradient1 FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE false false 1 0 false 0 0 xsize 0)

  (while (<= count numframes)

    ;; duplicate the first two base layers and add them
    (set! newlayer (car (gimp-layer-new-from-drawable cloud1 img)))
    (set! newgradient (car (gimp-layer-new-from-drawable gradient1 img)))
    (gimp-image-add-layer img newgradient -1)
    (gimp-image-add-layer img newlayer -1)

    ;; offset the clouds
    (gimp-drawable-offset newlayer true OFFSET-BACKGROUND (* count xshift) 0) ; (* count yshift))

    ;; add the third layer
    (set! templayer (car (gimp-layer-new-from-drawable gradient1 img)))
    (gimp-image-add-layer img templayer -1)
    (gimp-invert templayer)
    (gimp-layer-set-mode templayer HARDLIGHT-MODE)
    (set! newlayer (car (gimp-image-merge-down img templayer 0)))
    (gimp-layer-set-mode newlayer DIFFERENCE-MODE)
    (set! newgradient (car (gimp-image-merge-down img newlayer 0)))

    ;; add the fourth layer
    (set! newlayer (car (gimp-layer-new-from-drawable gradient1 img)))
    (gimp-image-add-layer img newlayer -1)
    (gimp-layer-set-mode newlayer DIVIDE-MODE)
    (set! newgradient (car (gimp-image-merge-down img newlayer 0)))

    ;; add the fifth layer
    (set! newlayer (car (gimp-layer-new-from-drawable gradient1 img)))
    (gimp-image-add-layer img newlayer -1)
    (gimp-layer-set-mode newlayer DODGE-MODE)
    (set! newgradient (car (gimp-image-merge-down img newlayer 0)))

    ;; Invert the result
    (gimp-invert newgradient)

    ;; controlling for the glow strength around the lightning
    (gimp-levels newgradient HISTOGRAM-VALUE 0 255 glowstrength 0 255)

    ;; colourize, and turn background to alpha if requested
    (gimp-colorize newgradient hue 50 0)
    (if (= background 1) (plug-in-colortoalpha 1 img newgradient '(0 0 0)))

    ;; Set the name. GIMP handles the numbering
    (gimp-drawable-set-name newgradient "Frame")

    (set! count (+ 1 count))
  )

  ;;Cleaning up 
  (gimp-image-remove-layer img cloud1)
  (gimp-image-remove-layer img gradient1)
  (gimp-image-undo-thaw img)
  (gimp-context-pop)

  ; Create and update the display
  (gimp-display-new img)
  (gimp-displays-flush)
 )
)

(script-fu-register "script-fu-lightning"
                    "Lightning Bolt..."
                    "Generates an animated lightning bolt"
                    "Will Morrison"
                    "Will Morrison"
                    "2010"
                    ""
                    SF-ADJUSTMENT "Image X size" '(100 1 2048 1 50 0 1)
		    SF-ADJUSTMENT "Image Y size" '(640 1 2048 1 50 0 1)
                    SF-ADJUSTMENT "Number of frames" '(50 1 400 1 10 0 1)
		    SF-ADJUSTMENT "Instability" '(2 0.1 16 0.1 1 1 0)
		    SF-ADJUSTMENT "Frequency" '(10 0.1 16 0.1 1 1 0)
                    SF-ADJUSTMENT "Detail" '(10 0 15 1 1 0 1)
		    SF-ADJUSTMENT "Hue" '(180 0 360 1 15 0 0)
                    SF-ADJUSTMENT "Glow Strength" '(0.15 0.1 1 0.01 0.1 2 0)
                    SF-TOGGLE "Make background transparent" 0
)

(script-fu-menu-register "script-fu-lightning"
                         "<Image>/Filters/Will's Script Pack")

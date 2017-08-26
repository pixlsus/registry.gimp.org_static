;;  Water pattern - This is a script for The GIMP to generate a highlight pattern such as those found on the bottom of pools
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

(define (script-fu-wavetank srcimg srclayer scalex scaley blend-num numframes displace? highlights? displacestrength)
 (let* ((blend-mode 5)
	(img 0) (layer 0) (templayer 0) (cloud1 0) (cloud2 0) (newlayer1 0) (newlayer2 0) (displacelayer 0) ;variables for layers and the image
	(count 1) (array (cons-array 6 'byte)) (xsize (car (gimp-drawable-width srclayer))) (ysize (car (gimp-drawable-height srclayer)))
	(shiftx (/ xsize numframes)) (shifty (/ ysize numframes)));variables for other stuff


  ;these are the values used for applying the curves filter. 
  (aset array 0 0)
  (aset array 1 255)
  (aset array 2 64)
  (aset array 3 64)
  (aset array 4 255)
  (aset array 5 0)
 
  (set! blend-mode
    (cond
      ((= blend-num 0) 5)
      ((= blend-num 1) 7)
      ((= blend-num 2) 4)
      ((= blend-num 3) 16)
    )
  )

 ;Create the new image based on the dimensions of the source layer
  (set! img (car (gimp-image-new xsize ysize 0)))
  (gimp-image-undo-freeze img)

 ;generates the source layers for the image and the solid noise
  (set! layer (car (gimp-layer-new-from-drawable srclayer img)))
  (set! cloud1 (car (gimp-layer-new img xsize ysize 0 "Frame" 100 0)))
  (set! cloud2 (car (gimp-layer-new img xsize ysize 0 "diff" 100 6)))
  (gimp-image-add-layer img layer -1)
  (gimp-image-add-layer img cloud1 -1)
  (gimp-image-add-layer img cloud2 -1)

 ;Adds the solid noise
  (plug-in-solid-noise 1 img cloud1 1 0 (rand 2100000000) 1 scalex scaley)
  (plug-in-solid-noise 1 img cloud2 1 0 (rand 2100000000) 1 scalex scaley)
  (plug-in-normalize 1 img cloud1)
  (plug-in-normalize 1 img cloud2)

  (while (< count numframes)
  ;Duplicates and adds source layers
    (set! templayer (car (gimp-layer-new-from-drawable layer img)))
    (set! newlayer1 (car (gimp-layer-new-from-drawable cloud1 img)))
    (set! newlayer2 (car (gimp-layer-new-from-drawable cloud2 img)))
    (gimp-image-add-layer img templayer -1)
    (gimp-image-add-layer img newlayer1 -1)
    (gimp-image-add-layer img newlayer2 -1)

  ;Offsets and merges layers, and creates the displace map
    (gimp-drawable-offset newlayer2 1 0 (* count shiftx) (* count shifty))
    (gimp-drawable-offset newlayer1 1 0 (- 0 (* count shiftx)) (- 0 (* count shifty)))
    (set! newlayer1 (car (gimp-image-merge-down img newlayer2 0)))
    (cond ((= displace? 1)
    (set! displacelayer (car (gimp-layer-new-from-drawable newlayer1 img)))
    ))
    (gimp-curves-spline newlayer1 0 6 array)

  ;merges the highlights onto the source image
    (cond 
      ((= highlights? 1) 
        (gimp-layer-set-mode newlayer1 blend-mode)
        (set! templayer (car (gimp-image-merge-down img newlayer1 0)))
      )
      (else
	(gimp-image-remove-layer img newlayer1)
      )
    )

    (gimp-drawable-set-name templayer "Frame")

  ;displaces the final result according to the displacement map
    (cond ((= displace? 1)
    (plug-in-displace 1 img templayer (* xsize displacestrength) (* ysize displacestrength) 1 1 displacelayer displacelayer 1)
    ))
    (set! count (+ 1 count))
  )

 ;The following section is the same as the body of the while loop, but acts on the original source layers
  (set! cloud1 (car (gimp-image-merge-down img cloud2 0)))
  (cond ((= displace? 1)
  (set! displacelayer (car (gimp-layer-new-from-drawable cloud1 img)))
  ))
  (gimp-curves-spline cloud1 0 6 array)

    (cond 
      ((= highlights? 1) 
        (gimp-layer-set-mode cloud1 blend-mode)
        (set! layer (car (gimp-image-merge-down img cloud1 0)))
      )
      (else
        (gimp-image-remove-layer img cloud1)
      )
    )

  (gimp-drawable-set-name layer "Frame#0")

  (cond ((= displace? 1)
  (plug-in-displace 1 img layer (* xsize displacestrength) (* ysize displacestrength) 1 1 displacelayer displacelayer 1)
  ))

  (gimp-image-undo-thaw img)
  ; Create and update the display
  (gimp-display-new img)
  (gimp-displays-flush)
 )
)

(script-fu-register "script-fu-wavetank"
                    "Wave Tank..."
                    "Generates an animation as if the active layer were on the bottom of a ripple tank."
                    "Will Morrison"
                    "Will Morrison"
                    "2010"
                    "RGB*"
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Active Layer" 0
;                    SF-ADJUSTMENT "Image X size" '(800 1 2048 1 50 0 1)
;		     SF-ADJUSTMENT "Image Y size" '(600 1 2048 1 50 0 1)
		    SF-ADJUSTMENT "Horizontal scale" '(2 0.1 16 0.1 1 1 0)
                    SF-ADJUSTMENT "Vertical scale" '(5 0.1 16 0.1 1 1 0)
		    SF-OPTION "Blend Mode" '("Overlay" "Addition" "Screen" "Dodge")
                    SF-ADJUSTMENT "Number of frames" '(50 1 400 1 10 0 1)
		    SF-TOGGLE "Use Displace Map" 0
		    SF-TOGGLE "Use Highlights" 1
		    SF-ADJUSTMENT "Displace strength" '(0.01 0 1 0.01 0.1 3 0)
)

(script-fu-menu-register "script-fu-wavetank"
                         "<Image>/Filters/Will's Script Pack")

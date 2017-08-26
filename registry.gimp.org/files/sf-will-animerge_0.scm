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

(define list-modes
 '("Normal" "Dissolve" "Behind" "Multiply" "Screen" "Overlay" "Difference" "Addition" "Subtract" "Darken Only" "Lighten Only"
   "Hue" "Saturation" "Color" "Value" "Divide" "Dodge" "Burn" "Hard Light" "Soft Light" "Grain Extract" "Grain Merge" 
   "Color Erase" "Erase" "Replace" "Anti-Erase")
)

(define (script-fu-anim-merge active-img base-img top-img merge-type bounds frames offset-x offset-y)
 (let* ((img 0) (layer1 0) (layer2 0) (templayer1 0) (templayer2 0) (width 1) (height 1) 
	(layerlist-top 0) (layerlist-base 0) (counter 0) (framenum 0)
	(top-x 0) (top-y 0) (base-x 0) (base-y 0) (temp-x-base 0) (temp-y-base 0) (temp-x-top 0) (temp-y-top 0))

 ;This determines the required dimensions of the new image and the image offsets in the new image
  (cond
  ((= bounds 0)
    (cond
      ((< offset-x 0)
        (set! width (max (car (gimp-image-width top-img)) (+ (abs offset-x) (car (gimp-image-width base-img)))))
	(set! base-x (abs offset-x))
	(set! top-x 0)
      )
      (else
        (set! width (max (car (gimp-image-width base-img)) (+ offset-x (car (gimp-image-width top-img)))))
	(set! base-x 0)
	(set! top-x offset-x)
      )
    )
    (cond
      ((< offset-y 0)
        (set! height (max (car (gimp-image-height top-img)) (+ (abs offset-y) (car (gimp-image-height base-img)))))
	(set! base-y (abs offset-y))
	(set! top-y 0)
      )
      (else
        (set! height (max (car (gimp-image-height base-img)) (+ offset-y (car (gimp-image-height top-img)))))
	(set! base-y 0)
	(set! top-y offset-y)
      )
    )
  )
  (else
    (set! width (car (gimp-image-width base-img)))
    (set! height (car (gimp-image-height base-img)))
    (set! base-x 0)
    (set! base-y 0)
    (set! top-x offset-x)
    (set! top-y offset-y)
  )
  )

 ;Create a new image of the required dimensions, in RGB mode
  (set! img (car (gimp-image-new width height 0)))
  (gimp-image-undo-freeze img)

 ;Get the layer lists for each source image, and reverse them so that 0 is the bottom layer
  (set! layerlist-base (list->vector (reverse (vector->list (cadr (gimp-image-get-layers base-img))))))
  (set! layerlist-top (list->vector (reverse (vector->list (cadr (gimp-image-get-layers top-img))))))

 ;Determine what the upper bound on frame number should be
  (cond
  ((= frames 0) (set! framenum (lcm (vector-length layerlist-base) (vector-length layerlist-top))))
  (else (set! framenum (min (vector-length layerlist-base) (vector-length layerlist-top))))
  )

  (while (< counter framenum)
   ;Grab the appropriate layer from layer 1 and get the layer offsets in the base image
    (set! layer1 (vector-ref  layerlist-base (modulo counter (vector-length layerlist-base))))
    (set! temp-x-base (car (gimp-drawable-offsets layer1)))
    (set! temp-y-base (cadr (gimp-drawable-offsets layer1)))

   ;Same as above for the second image
    (set! layer2 (vector-ref layerlist-top (modulo counter (vector-length layerlist-top))))
    (set! temp-x-top (car (gimp-drawable-offsets layer2)))
    (set! temp-y-top (cadr (gimp-drawable-offsets layer2)))

   ;Duplicate both source layers and add them to the new image
    (set! templayer1 (car (gimp-layer-new-from-drawable layer1 img)))
    (gimp-image-add-layer img templayer1 -1)
    (set! templayer2 (car (gimp-layer-new-from-drawable layer2 img)))
    (gimp-image-add-layer img templayer2 -1)

   ;Set the offsets for the new layers
    (gimp-layer-set-offsets templayer1 (+ temp-x-base base-x) (+ temp-y-base base-y))
    (gimp-layer-set-offsets templayer2 (+ temp-x-top top-x) (+ temp-y-top top-y))    

   ;Set the requested blend types. The bottom layer is set to normal blend mode, but this could be changed
    (gimp-layer-set-mode templayer1 0)
    (gimp-layer-set-mode templayer2 merge-type)

   ;Set both new layers to be visible
    (gimp-drawable-set-visible templayer1 1)
    (gimp-drawable-set-visible templayer2 1)

   ;Merge the layers and change the new layer's name
    (set! templayer1 (car (gimp-image-merge-down img templayer2 bounds)))
    (gimp-drawable-set-name templayer1 (string-append "Frame " (number->string (+ 1 counter))))

    (set! counter (+ counter 1))
  )

  (gimp-image-undo-thaw img)
  (gimp-image-clean-all img)

  ; Create and update the display
  (gimp-display-new img)
  (gimp-displays-flush)
 )
)

(script-fu-register "script-fu-anim-merge"
                    "Animation Merge..."
                    "Merges one animation on top of another with the specified blend mode."
                    "Will Morrison"
                    "GNU General Public License"
                    "2010"
                    "*"
		    SF-IMAGE		"Active Image (unused)"	'0
                    SF-IMAGE		"Bottom Image"  	'0
                    SF-IMAGE		"Top Image" 		'0
		    SF-OPTION		"Merge Type" 		list-modes
                    SF-OPTION           "Boundary Behavior"     '("Expand as needed" "Crop to Bottom Layer")
		    SF-OPTION		"Number of Frames"	'("LCM of frames" "Min of frames")
		    SF-ADJUSTMENT	"X Offset"		'(0 -20000 20000 10 100 0 1)
                    SF-ADJUSTMENT	"Y Offset"		'(0 -20000 20000 10 100 0 1)
)

(script-fu-menu-register "script-fu-anim-merge"
                         "<Image>/Filters/Will's Script Pack")

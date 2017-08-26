; water-ripple.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.1 (20100715)

; Description
;
; creates a water ripple distortion on an image
; v 1.1 - added option for distortion based on the image edges, plasma or cloud noise
;       - added option to keep the ripple layer seperate
;

; License:
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
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (script-fu-water-ripple img inLayer inType inSize inSpread inFade inNew)
  (let*
    (
       (width (car (gimp-drawable-width inLayer)))
       (height (car (gimp-drawable-height inLayer)))
       (n-width (trunc (/ width inSize)))
       (n-height (trunc (/ height inSize)))
       (LayerNoiseX 0)
       (LayerNoiseY 0)
       (LayerRipple 0)
    )
    ;  it begins here
    (gimp-context-push)
    (gimp-image-undo-group-start img)

    ;Create the ripple Layer
    (set! LayerRipple (car (gimp-layer-copy inLayer FALSE)))
	(gimp-image-add-layer img LayerRipple -1)
    (gimp-layer-set-opacity LayerRipple (round (+ 50 (/ inFade 2))))

    (cond
      ((= inType 0) ; plasma ripple    
        (set! LayerNoiseX (car (gimp-layer-new img n-width n-height (car (gimp-drawable-type inLayer)) "noise" 100 NORMAL-MODE)))
        (gimp-image-add-layer img LayerNoiseX (+ 1 (car (gimp-image-get-layer-position img inLayer))))
        (plug-in-plasma RUN-NONINTERACTIVE img LayerNoiseX (rand 65535) 7)
      )
      ((= inType 1) ; turbulent cloud noise
        (set! LayerNoiseX (car (gimp-layer-new img n-width n-height (car (gimp-drawable-type inLayer)) "noise" 100 NORMAL-MODE)))
        (gimp-image-add-layer img LayerNoiseX (+ 1 (car (gimp-image-get-layer-position img inLayer))))
        (plug-in-solid-noise RUN-NONINTERACTIVE img LayerNoiseX FALSE TRUE (rand 65535) 15
          (if (>= n-height n-width) (/ (* n-height 16) n-width) 16)
          (if (>= n-height n-width) 16 (/ (* n-width 16) n-height))
        )
      )
      ((= inType 2) ; image edge ripple    
        (set! LayerNoiseX (car (gimp-layer-copy LayerRipple FALSE)))
        (gimp-image-add-layer img LayerNoiseX (+ 1 (car (gimp-image-get-layer-position img inLayer))))
        (gimp-desaturate LayerNoiseX)
        (plug-in-edge RUN-NONINTERACTIVE img LayerNoiseX (max 1 inSize) 2 0)
        (plug-in-gauss RUN-NONINTERACTIVE img LayerNoiseX inSize inSize 0)
      )
    )      

    (set! LayerNoiseY (car (gimp-layer-copy LayerNoiseX FALSE)))
    (gimp-image-add-layer img LayerNoiseY (+ 1 (car (gimp-image-get-layer-position img inLayer))))

    ;create x and y bump maps by embossing
    (plug-in-emboss RUN-NONINTERACTIVE img LayerNoiseX 0 30 30 1)
    (plug-in-emboss RUN-NONINTERACTIVE img LayerNoiseY 270 30 30 1)
    
    ;scale them back up
    (when (or (= 0 inType) (= 1 inType))
      (gimp-layer-scale-full LayerNoiseX width height FALSE INTERPOLATION-CUBIC)
      (gimp-layer-scale-full LayerNoiseY width height FALSE INTERPOLATION-CUBIC)
    )    
    
    (plug-in-displace RUN-NONINTERACTIVE img LayerRipple inSpread inSpread TRUE TRUE LayerNoiseX LayerNoiseY 2) ; note this is a "bug" and 2=smear, not black as stated in the pdb documentation.
    
    ;clean-up
    (gimp-image-remove-layer img LayerNoiseX)
    (gimp-image-remove-layer img LayerNoiseY)
    
    (if (equal? inNew FALSE)
	  (gimp-image-set-active-layer img (car (gimp-image-merge-down img LayerRipple CLIP-TO-BOTTOM-LAYER)))
      (gimp-image-set-active-layer img LayerRipple)
	) 
	;done
    (gimp-image-undo-group-end img)
	(gimp-displays-flush)
	(gimp-context-pop)
  )
)

(script-fu-register "script-fu-water-ripple"
        		    "<Image>/Filters/Distorts/Water Ripple..."
                    "Create a water-ripple effect"
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "July 2010"
                    "RGB* GRAY*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-OPTION     "Ripple Type" '("Plasma Based" "Cloud Based" "Edge Based")
                    SF-ADJUSTMENT "Ripple Size" '(4 0.5 10 .1 1 1 0)
                    SF-ADJUSTMENT "Ripple Spread" '(10 -30 30 1 5 0 0)
                    SF-ADJUSTMENT "Ripple Intensity" '(100 1 100 1 5 0 0)
                    SF-TOGGLE     "Create New Layer" FALSE
)
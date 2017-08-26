; "Fake HDR look" is a script for The GIMP
;
; This script produces an HDR-like effect on an image
;
; The script is located in "<Image> / Script-Fu / Enhance / Fake HDR look"
;
; Last changed: 17th March 2009
;
; Copyright (C) 2009 Riccardo Traverso <gr3yfox.fw@gmail.com>
;
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 0.1
;    - Initial version
;
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.  
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;

(define (add-layer-over image
			add
			over)
  (gimp-image-set-active-layer image over)
  (gimp-image-add-layer image add -1)
)


(define (script-fu-fake-hdr theImage
			    theLayer
			    darkLayerLowInput
			    darkLayerOpacity
			    dodgeAndBurn
			    mergeLayers
			    unsharpMask)
  ; Enable undoing this script
  (gimp-image-undo-group-start theImage)

  (let* (
	(softglow1 (car (gimp-layer-copy theLayer FALSE)))
	(softglow2)
	(darken (car (gimp-layer-copy theLayer FALSE)))
  )

  (gimp-drawable-set-name softglow1 "Inverted softglow 1")
  (add-layer-over theImage softglow1 theLayer)
  (gimp-desaturate softglow1)
  (gimp-invert softglow1)
  (plug-in-softglow 1 theImage softglow1 10 0.75 0.85)
  (gimp-layer-set-mode softglow1 SOFTLIGHT-MODE)
  (set! softglow2 (car (gimp-layer-copy softglow1 FALSE)))
  (gimp-drawable-set-name softglow2 "Inverted softglow 2")
  (add-layer-over theImage softglow2 softglow1)
  (gimp-layer-set-opacity softglow1 50)
  (gimp-layer-set-opacity softglow2 75)

  (add-layer-over theImage darken softglow2)
  (gimp-levels darken
	       HISTOGRAM-VALUE    ; channel
	       darkLayerLowInput  ; low-input
	       255                ; high-input
	       1.0                ; gamma
	       0                  ; low-output
	       255                ; high-output
  )
  (gimp-layer-set-opacity darken darkLayerOpacity)
  (if (or (= mergeLayers 1) (= dodgeAndBurn 1))
    (begin
      (set! theLayer (car (gimp-image-merge-down theImage softglow1 2)))
      (set! theLayer (car (gimp-image-merge-down theImage softglow2 2)))
      (set! theLayer (car (gimp-image-merge-down theImage darken 2)))
      (if (= dodgeAndBurn 1)
        (script-fu-dodge-burn theImage theLayer 10 25)
      )
    )
  )

  (if (= unsharpMask 1)
    (plug-in-unsharp-mask 1 theImage theLayer 5.0 0.2 0))

  (gimp-image-set-active-layer theImage theLayer)


  ; End undo group
  (gimp-image-undo-group-end theImage)

  ; Update image
  (gimp-displays-flush)

  )
)


(script-fu-register "script-fu-fake-hdr"
		    "<Image>/Script-Fu/Enhance/Fake HDR look" ;"Fake HDR look"
		    "Enhance your image saturation and contrast"
		    "Riccardo Traverso"
		    "Copyright 2009, Riccardo Traverso"
		    "March 17, 2009"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-ADJUSTMENT "Dark layer low input level" '(100 0 255 1 10 0 0)
		    SF-ADJUSTMENT "Dark layer opacity" '(35 0 255 1 10 0 0)
		    SF-TOGGLE "Dodge and burn (force merge)" FALSE
		    SF-TOGGLE "Merge layers" FALSE
		    SF-TOGGLE "Unsharp mask" TRUE
)


; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; Copyright (C) 2009 Dominic Chomko <dominic.chomko@gmail.com>
;
; Version 1.1 - first release
;
; version 1.2 - updated for consistent effect regardless of image size
;	      - renamed parameter from opacity to softness
; 
; Based on the vignetting method outlined by Sue Chastain 
; <http://graphicssoft.about.com/od/gimp/ss/vignette.htm>
;
; Usage: 
;
; - Creates a vignette based on the selection and active layer.
;
; - The blur radius, opacity, and colour can be set in the dialog box.
;
; - The script merges visible layers so check "Keep Layers" if you have multiple
;   Layers visible or to adjust vignette opacity or other settings.
;
;




(define (script-fu-vignette inImage inLayer inRadius inOpacity inColour inBoolLayers)
  (let*
    	( 
		;variables

		(theImage (car (gimp-drawable-get-image inLayer) ) )
		(theWidth (car (gimp-image-width theImage) ) )
		(theHeight (car (gimp-image-height theImage) ) )
		(theRadius (* inRadius (/ (max theWidth theHeight) 25) ) )
		(vignetteLayer1
		  (car
			(gimp-layer-new
		 	  theImage
		 	  theWidth
		 	  theHeight
		 	  RGB-IMAGE
		 	  "Vignette Layer 1"
		 	  inOpacity
		 	  NORMAL
			)
		  )
		)
		(currentMask (car (gimp-layer-get-mask inLayer) ) )
		(vignetteLayer2
		  (car
			(gimp-layer-copy
			  inLayer
			  FALSE
			)
		  )
		)
		(vignetteMask
		  (car
			(gimp-layer-create-mask
			  vignetteLayer2
			  ADD-BLACK-MASK
			)
		  )
		)		
	) ;end define variables

	(gimp-drawable-set-name
	  vignetteLayer2
	  "Vignette Layer 2"
	)
	(gimp-image-undo-group-start theImage)
	(gimp-context-set-foreground inColour )
	(gimp-context-set-background '(255 255 255) )
	(gimp-drawable-fill
	  vignetteLayer1
	  FOREGROUND-FILL
	)
	(gimp-image-add-layer
	  theImage
	  vignetteLayer1
	  0
	)
	(gimp-image-add-layer
	  theImage
	  vignetteLayer2
	  0
	)

	;if active layer has a mask must add to vignette layer 1 and remove from layer2
	(if (not (= currentMask -1) )
	  ( begin
		(let*
		  (
			 (combinedMask
			  (car
				(gimp-layer-create-mask
				  vignetteLayer1
				  ADD-BLACK-MASK
				)
			  )
			)
		  )
		  (gimp-channel-combine-masks
			combinedMask
			currentMask
			CHANNEL-OP-ADD
			0
			0
		  )
		  (gimp-layer-add-mask
		  	vignetteLayer1
		  	combinedMask
		  )
		)
		(gimp-layer-remove-mask
		  vignetteLayer2
		  MASK-APPLY
		)
	  )
	)

	(gimp-layer-add-mask
	  vignetteLayer2
	  vignetteMask
	)
	(gimp-edit-fill
	  vignetteMask
	  WHITE-FILL
	)
	(gimp-selection-none theImage)
	(if (> inRadius 0)
	  ( begin
		(plug-in-gauss-iir
		  RUN-NONINTERACTIVE
		  theImage
		  vignetteMask
		  theRadius
		  TRUE
		  TRUE
		)
	  )
	)
	(if (= inBoolLayers FALSE)
	  ( begin
		(gimp-image-merge-visible-layers
		  theImage
		  EXPAND-AS-NECESSARY
		)
	  )
	)


	(gimp-image-undo-group-end theImage)
	(gimp-displays-flush)
   
  )
)


(script-fu-register
                  "script-fu-vignette"
                  "Vignette"
                  "Creates a vignette around selection."
                  "Dominic Chomko"
                  "copyright 2009, Dominic Chomko"
                  "August 8, 2009"
                  "*"
                  SF-IMAGE	"Input Image"		0
		  SF-DRAWABLE	"Input Selection"	0
		  SF-ADJUSTMENT	"Softness"		'(8 0 30 .1 1 1 0)
		  SF-ADJUSTMENT	"Darkness"		'(50 0 100 1 5 0 0)
		  SF-COLOR	"Colour"		'(0 0 0)
		  SF-TOGGLE	"Keep Layers"		FALSE
)
        
(script-fu-menu-register "script-fu-vignette" "<Image>/Filters/Light and Shadow")

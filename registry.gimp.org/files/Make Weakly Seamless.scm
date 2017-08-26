; Make Weakly Seamless
; Version 0.04
; By Paul Nickerson (pgn674)
; Copyright 2010 Paul Nickerson

; Description:
; This GIMP script alters an image to be tileable with faded overlapping seams.
; Currently, the corners are messy and need to be manually retouched. I haven't figure out how to make that work better, yet.

; License:
; This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
; The GNU Public License is available at <http://www.gnu.org/licenses/>.

; Section 1

(define (script-fu-make-weakly-seamless inImage inDrawable inOverlap)
	(gimp-undo-push-group-start inImage)	;allows the undo capability to work
	(let*
		(	;define our local variables
			;make copies of the original image, only for left and right first
			(theLayerLeft (car (gimp-layer-copy inDrawable TRUE)))
			(theLayerRight (car (gimp-layer-copy inDrawable TRUE)))
			;find the height and width we're working with
			(theDrawableWidth (car (gimp-drawable-width  inDrawable)))
			(theDrawableHeight (car (gimp-drawable-height inDrawable)))
			;create transparency masks for the new layers to work with, left and right first
			(theMaskLeft (car (gimp-layer-create-mask theLayerLeft ADD-WHITE-MASK)))
			(theMaskRight (car (gimp-layer-create-mask theLayerRight ADD-WHITE-MASK)))
			;other variables
			(flatLayerMidDone)
			(theLayerTop)
			(theLayerBottom)
			(theMaskTop)
			(theMaskBottom)
			(flatLayer)
		)	;end of our local variables
		;set some colors i'll need for the gradiants
		(gimp-context-set-background '(0 0 0))
		(gimp-context-set-foreground '(255 255 255))
		;first, left and right
		;actually add the layers i just made
		(gimp-image-add-layer inImage theLayerLeft -1)
		(gimp-image-add-layer inImage theLayerRight -1)
		;move the new layers to the propper overlapping positions
		(gimp-drawable-offset theLayerLeft FALSE OFFSET-TRANSPARENT 0 (+ (* -1 theDrawableHeight) inOverlap))
		(gimp-drawable-offset theLayerRight FALSE OFFSET-TRANSPARENT 0 (- theDrawableHeight inOverlap))
		;actually apply the masks i had made earlier
		(gimp-layer-add-mask theLayerLeft theMaskLeft)
		(gimp-layer-add-mask theLayerRight theMaskRight)
		;apply gradients to the masks, to make the layers propperly transparent
		(gimp-blend theMaskLeft FG-BG-RGB-MODE MULTIPLY-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 1 0 TRUE 0 0 0 inOverlap)
		(gimp-blend theMaskRight FG-BG-RGB-MODE MULTIPLY-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 1 0 TRUE 0 theDrawableHeight 0 (- theDrawableHeight inOverlap))
		;flatten the left and right
		(set! flatLayerMidDone (car (gimp-image-flatten inImage)))
		;then, top and bottom
		;make copies of the current image, now for top and bottom
		(set! theLayerTop (car (gimp-layer-copy flatLayerMidDone TRUE)))
		(set! theLayerBottom (car (gimp-layer-copy flatLayerMidDone TRUE)))
		;create transparency masks for the new layers to work with, now top and bottom
		(set! theMaskTop (car (gimp-layer-create-mask theLayerTop ADD-WHITE-MASK)))
		(set! theMaskBottom (car (gimp-layer-create-mask theLayerBottom ADD-WHITE-MASK)))
		;actually add the layers i made
		(gimp-image-add-layer inImage theLayerTop -1)
		(gimp-image-add-layer inImage theLayerBottom -1)		
		;move the new layers to the propper overlapping positions
		(gimp-drawable-offset theLayerTop FALSE OFFSET-TRANSPARENT (+ (* -1 theDrawableWidth) inOverlap) 0)
		(gimp-drawable-offset theLayerBottom FALSE OFFSET-TRANSPARENT (- theDrawableWidth inOverlap) 0)
		;actually apply the masks i had made earlier
		(gimp-layer-add-mask theLayerTop theMaskTop)
		(gimp-layer-add-mask theLayerBottom theMaskBottom)
		;apply gradients to the masks, to make the layers propperly transparent
		(gimp-blend theMaskTop FG-BG-RGB-MODE MULTIPLY-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 1 0 TRUE 0 0 inOverlap 0)
		(gimp-blend theMaskBottom FG-BG-RGB-MODE MULTIPLY-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 1 0 TRUE theDrawableWidth 0 (- theDrawableWidth inOverlap) 0)
		;flatten and resize the image and layers to propper measurements
		(set! flatLayer (car (gimp-image-flatten inImage)))
		(gimp-image-resize inImage (- theDrawableWidth inOverlap) (- theDrawableHeight inOverlap) (* -1 (/ inOverlap 2)) (* -1 (/ inOverlap 2)))
		(gimp-layer-resize-to-image-size flatLayer)
	)
	(gimp-undo-push-group-end inImage)	;ending pair to the undo capability
)

; Section 2 - Registrations for GIMP

(script-fu-register
	"script-fu-make-weakly-seamless"			;func name
	"Make Weakly Seamless..."					;menu label
	"Alters image to have gradually\
		fading overlapped borders when tiled."	;description
	"Paul Nickerson"							;author
	"copyright 2010, Paul Nickerson (pgn674);"	;copyright notice
	"February 28, 2010"							;date created
	""											;image type that the script works on
	
	SF-IMAGE		"Image"		-1										;image that the script works on, inImage
	SF-DRAWABLE		"Drawable"	-1										;the active layer, inDrawable
	SF-ADJUSTMENT	"Overlap Amount"	'(32 0 1024 1 16 0 SF-SPINNER)	;a spin-button, inOverlap
)

(script-fu-menu-register "script-fu-make-weakly-seamless" "<Image>/Filters/Map")

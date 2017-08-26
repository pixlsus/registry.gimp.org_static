;
; Shadow Recovery, V2.8
;
; Martin Egger (martin.egger@gmx.net)
; (C) 2012, Bern, Switzerland
;
; This script was tested with Gimp 2.8
;
; New versions will be distributed from http://registry.gimp.org/ only
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
; along with this program; if not, see <http://www.gnu.org/licenses>.
;
; Define the function
;
(define (script-fu-Eg-ShadowRecovery InImage InLayer InOpacity InFlatten)
;	
; Save history			
;
	(gimp-image-undo-group-start InImage)
;
	(let*	(
		(CopyLayer (car (gimp-layer-copy InLayer TRUE)))
		(ShadowLayer (car (gimp-layer-copy InLayer TRUE)))
		(CopyMask (car (gimp-layer-create-mask CopyLayer ADD-COPY-MASK)))
		(ShadowMask (car (gimp-layer-create-mask ShadowLayer ADD-COPY-MASK)))
		)
;
; Create new layer and add it to the image
;
		(gimp-image-insert-layer InImage CopyLayer 0 -1)
		(gimp-layer-set-mode CopyLayer ADDITION-MODE)
		(gimp-layer-set-opacity CopyLayer InOpacity)
		(gimp-image-insert-layer InImage ShadowLayer 0 -1)
		(gimp-layer-set-mode ShadowLayer OVERLAY-MODE)
		(gimp-layer-set-opacity ShadowLayer InOpacity)
;
		(gimp-layer-add-mask CopyLayer CopyMask)
		(gimp-invert CopyMask)
		(gimp-layer-add-mask ShadowLayer ShadowMask)
		(gimp-layer-set-edit-mask CopyLayer FALSE)
		(gimp-layer-set-edit-mask ShadowLayer FALSE)
;
; Flatten the image, if we need to
;
		(cond
			((= InFlatten TRUE)
				(begin
					(gimp-image-merge-down InImage CopyLayer CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage ShadowLayer CLIP-TO-IMAGE)
				)
			)
			((= InFlatten FALSE) 
				(begin
					(gimp-item-set-name CopyLayer "Shadowfree strong")
					(gimp-item-set-name ShadowLayer "Shadowfree normal")
					(gimp-image-set-active-layer InImage InLayer)
				)
			)
		)
	)
;
; Finish work
;
	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
;
)
;
(script-fu-register
	"script-fu-Eg-ShadowRecovery"
	_"Shadow _Recovery"
	"Lighten-up Shadows"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"28.02.2012"
	"RGB* GRAY*"
	SF-IMAGE	"The Image"	0
	SF-DRAWABLE	"The Layer"	0
	SF-ADJUSTMENT	"Layer Opacity"	'(60.0 1.0 100.0 1.0 0 2 0)
	SF-TOGGLE	"Flatten Image"	FALSE
)
;
(script-fu-menu-register "script-fu-Eg-ShadowRecovery"
			 "<Image>/Filters/Eg")
;

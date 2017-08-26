;
; Dynamic Range Increase, V2.8
;
; Martin Egger (martin.egger@gmx.net)
; (C) 2012, Bern, Switzerland
;
; You can find more about Dynamic Range Extension at
; http://www.gimpguru.org/Tutorials/BlendingExposures/
;
; Input image with three layers:
;
;	Upper layer: Darker Image ((under)exposed for highlights)
;	Middle layer: Normal Image (exposed for midtones)
;	Lower layer: Lighter Image ((over)exposed for shadows)
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
(define (script-fu-Eg-DynamicRangeIncrease InImage InLayer InBlur InFlatten)
;
; Save history			
;
	(gimp-image-undo-group-start InImage)
;
	(let*	(
		(OrigLayer (cadr (gimp-image-get-layers InImage)))
		(UpperLayer (aref OrigLayer 0))
		(MiddleLayer (aref OrigLayer 1))
		(LowerLayer (aref OrigLayer 2))
		(LABUpperImage (car (plug-in-decompose TRUE InImage UpperLayer "Value" TRUE)))
		(LABUpperLayer (cadr (gimp-image-get-layers LABUpperImage)))
		(LABMiddleImage (car (plug-in-decompose TRUE InImage MiddleLayer "Value" TRUE)))
		(LABMiddleLayer (cadr (gimp-image-get-layers LABMiddleImage)))
		(UpperLayerMask (car (gimp-layer-create-mask UpperLayer ADD-WHITE-MASK)))
		(MiddleLayerMask (car (gimp-layer-create-mask MiddleLayer ADD-WHITE-MASK)))
		)
		(gimp-layer-add-mask UpperLayer UpperLayerMask)
		(gimp-layer-add-mask MiddleLayer MiddleLayerMask)
		(plug-in-gauss TRUE LABUpperImage (aref LABUpperLayer 0) InBlur InBlur TRUE)
		(plug-in-gauss TRUE LABMiddleImage (aref LABMiddleLayer 0) InBlur InBlur TRUE)
		(gimp-selection-all LABUpperImage)
		(gimp-edit-copy (aref LABUpperLayer 0))
		(gimp-floating-sel-anchor (car (gimp-edit-paste UpperLayerMask FALSE)))
		(gimp-selection-all LABMiddleImage)
		(gimp-edit-copy (aref LABMiddleLayer 0))
		(gimp-floating-sel-anchor (car (gimp-edit-paste MiddleLayerMask FALSE)))
		(gimp-image-delete LABUpperImage)
		(gimp-image-delete LABMiddleImage)
;
		(gimp-layer-set-edit-mask UpperLayer FALSE)
		(gimp-layer-set-edit-mask MiddleLayer FALSE)
		(gimp-layer-set-opacity UpperLayer 80)
		(gimp-layer-set-opacity MiddleLayer 90)
		;
; Flatten the image, if we need to
;
		(cond
			((= InFlatten TRUE) 
				(begin
					(gimp-image-merge-down InImage UpperLayer CLIP-TO-IMAGE)
					(gimp-image-merge-down InImage MiddleLayer CLIP-TO-IMAGE)
				)
			)
;
			((= InFlatten FALSE) 
				(begin
					(gimp-item-set-name UpperLayer "Dark-Overlay")
					(gimp-item-set-name MiddleLayer "Middle-Overlay")
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
(script-fu-register "script-fu-Eg-DynamicRangeIncrease"
	_"D_ynamic Range Increase"
	"Blend three differently exposed images together thus increasing dynamic range."
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"28.02.2012"
	"RGB* GRAY*"
	SF-IMAGE	"The Image"		0
	SF-DRAWABLE	"The Layer"		0
	SF-ADJUSTMENT	"Blur Mask"	'(10.0 1.0 100.0 1.0 0 2 0)
	SF-TOGGLE	"Flatten Image"		FALSE
)
;
(script-fu-menu-register "script-fu-Eg-DynamicRangeIncrease"
			 "<Image>/Filters/Eg")
;

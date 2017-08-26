; GIMP - The GNU Image Manipulation Program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; color noise reduction - reduces color noise in the photographs
; Copyright (C) 2011 Oleg Ivanenko
; oivanenko@gmail.com
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

(define (script-fu-chroma-denoise img drawable intColorModel blnNewLayer blnAddMask intBlurRadius intBlurDelta intMaskThreshold)
	(let* (
		(lyrSource (car (gimp-image-get-active-layer img)))
		(cnlMask 0)
	)
		(gimp-image-undo-group-start img)
		
		;Create new layer if it was demanded
		 (if (= blnNewLayer TRUE)
			(begin
				(set! lyrSource (car (gimp-layer-copy lyrSource TRUE)))
				(gimp-image-add-layer img lyrSource -1)
			)
		 )

		;Rename layer
		(gimp-layer-set-name lyrSource 
			(string-append "Cdenoise: " 
					(number->string intColorModel) "/" 
					(number->string intBlurRadius) "/" 
					(number->string intBlurDelta) "/" 
					(number->string intMaskThreshold))
		)
		
		;Create mask if it was demanded
		(if (= blnAddMask TRUE)
			(begin
				(if (not (equal? -1 (car (gimp-layer-get-mask lyrSource))))
					(gimp-layer-remove-mask lyrSource MASK-APPLY)
				)
				
				(set! cnlMask (car (gimp-layer-create-mask lyrSource ADD-COPY-MASK)))

				(gimp-layer-add-mask lyrSource cnlMask)
				(gimp-threshold cnlMask 0 intMaskThreshold)
				(plug-in-gauss-iir2 1 img cnlMask intBlurRadius intBlurRadius)
			)
		)
		 
		(let*	(		(strColorModel (cond 
								((= intColorModel 0) "LAB")
								((= intColorModel 1) "YCbCr_ITU_R470")
							) 
					)
					
					;Decompose source layer to selected Color Model layers
					(imgLAB (car (plug-in-decompose 1 img lyrSource strColorModel 1)))
				
					(imgLABLayers (cadr (gimp-image-get-layers imgLAB)))

					(lyrL (aref imgLABLayers 0))
					(lyrA (aref imgLABLayers 1))
					(lyrB (aref imgLABLayers 2))
				)
	
			;Apply selective gaussian blur to color parts layers
			(plug-in-sel-gauss 1 imgLAB lyrA intBlurRadius intBlurDelta)
			(plug-in-sel-gauss 1 imgLAB lyrB intBlurRadius intBlurDelta)
			
			;Recompose result to source layer
			(plug-in-recompose 1 imgLAB 0)
			
			;Delete decomposed image
			(gimp-image-delete imgLAB)
			
			(gimp-image-undo-group-end img)
			(gimp-displays-flush)
		)
	)
)

(script-fu-register
	"script-fu-chroma-denoise"		;func name
	"_Chroma Denoise"				;menu label
	"Removes color noise"			;description
	"Oleg Ivanenko"					;author
	"copyright 2011, Oleg Ivanenko"	;copyright notice
	"July 18, 2011"					;date created
	"*"                   			;image type that the script works on
	SF-IMAGE	"Image"					0
	SF-DRAWABLE	"Drawable"				0
	SF-OPTION	"Color Model"			'("LAB" "YCbCr ITU R470")
	SF-TOGGLE	"Create new layer"		TRUE
	SF-TOGGLE	"Add black mask"		TRUE
	SF-ADJUSTMENT	"Blur Radius"		'(5 0 100 0.1 1 2 1)
	SF-ADJUSTMENT	"Blur Delta"		'(50 0 255 1 8 0 0)
	SF-ADJUSTMENT	"Mask Threshold"	'(128 0 255 1 8 0 0)
)

(script-fu-menu-register "script-fu-chroma-denoise" "<Image>/Filters/Enhance")

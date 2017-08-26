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

; Change log

;16-Nov-2011
;Added check layer option for "wavelet decompose super-contrast issue"
;(see http://oivanenko.blogspot.com/2011/07/blog-post_26.html for details)

(define (script-fu-simple-wavelet-decompose img drawable blnKeepOriginal blnOriginalOnTop blnCheck128 intBlurRadius)
	(let* (
		;Remember active layer as source
		(lyrOriginal (car (gimp-image-get-active-layer img)))
		;Copy source layer as future Residue
		(lyrSource 0)
		(lyrResidue 0)
		(lyrResidueCopy 0)
		(lyrSourceC128 0)
		(lyrResidueC128 0)
		(lyrHighPass 0)
		(lyrCheck128 0)
	)
		(gimp-image-undo-group-start img)
		
		;Keep original layer if it was demanded
		(if (= blnKeepOriginal TRUE)
			(begin
				(set! lyrSource (car (gimp-layer-copy lyrOriginal TRUE)))
				(gimp-image-add-layer img lyrSource -1)
			)
			(set! lyrSource lyrOriginal)
		)
		
		;Source must be visible
		(gimp-layer-set-visible lyrSource TRUE)
		
		;Copy Source layer as Residue
		(set! lyrResidue (car (gimp-layer-copy lyrSource TRUE)))
		
		;Add Residue layer at the top of Source layer
		(gimp-image-add-layer img lyrResidue -1)
		
		;Blur Residue layer with necessary Radius
		(plug-in-gauss-iir2 RUN-NONINTERACTIVE img lyrResidue intBlurRadius intBlurRadius)
		
		;Save layers for "check" layer for ">128 issue"
		(if (= blnCheck128 TRUE)
			(begin
				(set! lyrSourceC128 (car (gimp-layer-copy lyrSource TRUE)))
				(set! lyrResidueC128 (car (gimp-layer-copy lyrResidue TRUE)))
			)
		)
		
		;Save copy of Residue
		(set! lyrResidueCopy (car (gimp-layer-copy lyrResidue TRUE)))
		(gimp-image-add-layer img lyrResidueCopy -1)
		
		;Rename Residue copy layer
		(gimp-layer-set-name lyrResidueCopy (string-append "swdResidue: " (number->string intBlurRadius)))
		
		;Set Residue to "Grain extract" mode 
		(gimp-layer-set-mode lyrResidue GRAIN-EXTRACT-MODE)
		
		;Create new HighPass from merging of Residue and Source
		(set! lyrHighPass (car (gimp-image-merge-down img lyrResidue CLIP-TO-BOTTOM-LAYER)))
		(gimp-layer-set-name lyrHighPass (string-append "swdHighPass: " (number->string intBlurRadius)))
		(gimp-image-raise-layer img lyrHighPass)
		
		;Set HighPass to "Grain merge" mode 
		(gimp-layer-set-mode lyrHighPass GRAIN-MERGE-MODE)
		
		(if (and (= blnKeepOriginal TRUE) (= blnOriginalOnTop TRUE))
			(begin
				;Raise up two stairs
				(gimp-image-raise-layer img lyrOriginal)
				(gimp-image-raise-layer img lyrOriginal)
				
				;Switch off Oryginal visibility
				(gimp-layer-set-visible lyrOriginal FALSE)
			)
		)
		
		(if (= blnCheck128 TRUE)
			(begin
				(gimp-image-add-layer img lyrSourceC128 -1)
				(gimp-image-add-layer img lyrResidueC128 -1)
				(gimp-layer-set-mode lyrResidueC128 DIFFERENCE-MODE)
				(set! lyrCheck128 (car (gimp-image-merge-down img lyrResidueC128 CLIP-TO-BOTTOM-LAYER)))
				(gimp-threshold lyrCheck128 128 255)
				(gimp-layer-set-name lyrCheck128 "swdCheck128")
				(gimp-layer-set-visible lyrCheck128 FALSE)
			)
		)
		
		(gimp-image-undo-group-end img)
		(gimp-displays-flush)
	)
)

(script-fu-register
	"script-fu-simple-wavelet-decompose"				;func name
	"_Simple Wavelet Decompose"							;menu label
	"Splices layer by two wavelet components(layers)"	;description
	"Oleg Ivanenko"										;author
	"copyright 2011, Oleg Ivanenko"						;copyright notice
	"July 25, 2011"										;date created
	"*"                   								;image type that the script works on
	SF-IMAGE		"Image"					0
	SF-DRAWABLE		"Drawable"				0
	SF-TOGGLE		"Keep original"			TRUE
	SF-TOGGLE		"Original on top"		TRUE
	SF-TOGGLE		"Check >128 issue"		TRUE
	SF-ADJUSTMENT	"Radius"			'(16 0 1000 0.01 1 2 0)
)

(script-fu-menu-register "script-fu-simple-wavelet-decompose" "<Image>/Layer")

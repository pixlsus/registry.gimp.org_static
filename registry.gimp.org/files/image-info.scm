; GIMP - The GNU Image Manipulation Program
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
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Image Info - a script-fu for GIMP
;
; Image Info creates a new layer (reffered as "info layer") containing
; informations (title, path, size, resolution..) for the current image
;
; Copyright (C) 2008 Radu Feflea <radu.feflea@gmail.com>
;
; v0.1	- 02.12.08
;		- the very first version
; v0.2	- 08.12.08
;		- added a custom title field
;		- some minor improvements
; v0.3	- 16.12.08
;		- bugfix: applying the script on an unsaved image returned an error
; v0.4	- 20.12.08
;		- added progress-bar functionality
;		- changed the way infoScale works
;		- script patched to work with rgb, grayscale and indexed images
;
; TODO
;		- switch to include the text in the picture and not on a new layer (automatic layer merging)
;		- batch-mode for multiple file editing
;		- EXIF information as fields for jpeg images taken with a digital camera
;
;
(define (script-fu-image-info image
							textColor				; color for the text
							backColor				; color for the background
							textFont					; font for the text
							dispCustom				; display custom title
							customText			; custom title
							dispPath					; display image path
							dispSize					; display image size
							dispRes)					; display resolution

	(let*
		(
			(contextFore 0)						; vars for the current colors
			(contextBack 0)						;
			(imageWidth 0)						; image size
			(imageHeight 0)						;
			(imageFilename 0)					; file name	
			(imageResX 0)							; resolution
			(imageResY 0)							;
			(imageType 0)						; image type (rgb, gray ...)
			(infoLayer 0)							; the info layer
			(infoLayerType 0)					; info layer type (converted from imageType)
			(infoLayerHeight 0)					; height of the info layer
			(textLayer 0)							; the text layer used for displaying the strings
			(textLayerWidth 0)					; the width (in pixels) of the text layer (used for right-aligning)
			(textSize 0)							; font size in pixels
			(textRow 0)							; text row count (in this version it can be 1 or 2)
			(infoScale 0.07)						; scaling coefficient for the info layer and textSize
														; infoLayerHeight = imageHeight * infoScale
														; textSize = infoLayerHeight / 3
		)
		
		(gimp-image-undo-group-start image)
		
		; init variables
						
		(gimp-progress-update 0.1)													; progress-bar goes one step (from 10) forward
		
		(set! contextFore (car (gimp-context-get-foreground)))			; getting the current colors
		(set! contextBack (car (gimp-context-get-background)))
		(set! imageWidth (car (gimp-image-width image)))					; getting image size
		(set! imageHeight (car (gimp-image-height image)))
		
		(set! imageFilename (car (gimp-image-get-filename image)))		; getting image file name
		
		(if (equal? imageFilename "")
			(set! imageFilename "Warning: Image not saved yet!")			; setting image file name to "warning: ..." if file wasn't saved
		)
				
		(set! imageResX (car (gimp-image-get-resolution image)))			; getting image resolution
		(set! imageResY (cadr (gimp-image-get-resolution image)))
		
		(set! imageType (car (gimp-image-base-type image)))				; getting image type ...
		
		(case imageType																	; ... and setting the info layer type
			((0) (set! infoLayerType RGB-IMAGE))
			((1) (set! infoLayerType GRAY-IMAGE))
			((2) (set! infoLayerType INDEXED-IMAGE))
		)
		
		(set! infoLayerHeight (* imageHeight infoScale))						; setting the height of the info layer
		
		(set! textSize (/ infoLayerHeight 3))										; setting the font-size for the info layer
		
		; resizing the image
		
		(gimp-progress-update 0.2)
		
		(gimp-image-resize image imageWidth (+ imageHeight infoLayerHeight) 0 0)
		
		; creating the info layer
		
		(gimp-progress-update 0.3)
		
	    (set! infoLayer (car (gimp-layer-new image
											imageWidth
											infoLayerHeight
											infoLayerType
											"image_info"
											100
											NORMAL-MODE)))
												
		(gimp-image-add-layer image infoLayer 200)
		(gimp-layer-set-offsets infoLayer 0 imageHeight)						; moving the layer at the bottom of the image
		
		; setting the new colors for the info Layer and text
		
		(gimp-progress-update 0.4)
		
		(gimp-context-set-foreground textColor)
		(gimp-context-set-background backColor)
		
		; filling the infoLayer with backColor
		
		(gimp-drawable-fill infoLayer BACKGROUND-FILL)		
		
		; writting the cutom title

		(gimp-progress-update 0.5)
		
		(if (= dispCustom 1)
			(begin
				(set! textLayer (car (gimp-text-fontname image
													infoLayer
													3
													(+ imageHeight 2 (* textRow (/ infoLayerHeight 2)))
													customText
													-1
													TRUE
													textSize
													PIXELS
													textFont)))
				
				(gimp-floating-sel-anchor textLayer)
				
				(set! textRow (+ textRow 1))										; incrementing row counter
			)
		)
		
		; writting the filename

		(gimp-progress-update 0.6)
		
		(if (= dispPath 1)
			(begin
				(set! textLayer (car (gimp-text-fontname image
													infoLayer
													3
													(+ imageHeight 2 (* textRow (/ infoLayerHeight 2)))
													imageFilename
													-1
													TRUE
													textSize
													PIXELS
													textFont)))
				
				(gimp-floating-sel-anchor textLayer)
				
				(set! textRow (+ textRow 1))										; incrementing row counter
			)
		)
		
		; going on the second column -> resetting textRow to 0
		
		(set! textRow 0)
		
		; writting the image size
		; right-aligned
		
		(gimp-progress-update 0.7)
		
		(if (= dispSize 1)
			(begin
				(set! textLayer (car (gimp-text-fontname image
												infoLayer
												0
												0
												(string-append (number->string imageWidth) " x " (number->string imageHeight) " px")
												-1
												TRUE
												textSize
												PIXELS
												textFont)))

				

				(set! textLayerWidth (car (gimp-drawable-width textLayer)))		; getting the text's width ...

				(gimp-layer-translate textLayer
												(- imageWidth textLayerWidth 3)
												(+ imageHeight 2))								 ; ... and then aligning it to right
												
				(gimp-floating-sel-anchor textLayer)
				
				(set! textRow (+ textRow 1))												; incrementing row counter
			)
		)

		; writting the resolution
		; on the 2nd row right-aligned

		(gimp-progress-update 0.8)
		
		(if (= dispRes 1)
			(begin
				(set! textLayer (car (gimp-text-fontname image
												infoLayer
												0
												0
												(string-append (number->string imageResX) " x " (number->string imageResY) " dpi")
												-1
												TRUE
												textSize
												PIXELS
												textFont)))
			
				; getting the text's width and then aligning it to right

				(set! textLayerWidth (car (gimp-drawable-width textLayer)))
			
				(gimp-layer-translate textLayer
												(- imageWidth textLayerWidth 3)
												(+ imageHeight 2 (* textRow (/ infoLayerHeight 2))))
			
				(gimp-floating-sel-anchor textLayer)

				(set! textRow (+ textRow 1))
			)
		)
		
		; setting old colors back
		
		(gimp-progress-update 0.9)
		
		(gimp-context-set-foreground contextFore)
		(gimp-context-set-background contextBack)

		; finishing
		
		(gimp-progress-update 1.0)
		
		(gimp-image-undo-group-end image)
		(gimp-displays-flush)
	)
)

(script-fu-register
	"script-fu-image-info"
	"Add Image Info"
	"Adds a new layer at the bottom of the current image containing infos about it"
	"Radu Feflea"
	"2008 Radu Feflea. Public Domain"
	"December 2008"
	""
	SF-IMAGE		"Image"					0
    SF-COLOR		"Text color"				'(255 255 255)
    SF-COLOR		"Background color"	'(0 0 0)
    SF-FONT		"Font"					"sans"
	SF-TOGGLE		"Custom title"			TRUE
	SF-STRING		"Text String"			"my custom title"
	SF-TOGGLE		"Image Path"			TRUE
	SF-TOGGLE		"Image Size"			TRUE
	SF-TOGGLE		"Image Resolution"	TRUE
)

(script-fu-menu-register "script-fu-image-info"
	"<Image>/Image")

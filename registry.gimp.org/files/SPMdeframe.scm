; ------------------------------------------------------------------------------
; Find & remove white framing from Stereophotomaker images
; 
; (c) 2008 Kay Stenschke <info@stenschke.com>
; All rights reserved
;
; This script is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; The GNU General Public License can be found at
; http://www.gnu.org/copyleft/gpl.html.
;
;
; Changelog: (add new entries on top)
; V1.1 (2008-26-08) Changed script registration from Gimp's toolbox window to a new sub menu "Script-Fu" inside the image window
; V1.0 (2008-18-08) Tested and working with Gimp 2.4.5
; ------------------------------------------------------------------------------
(define (script-fu-SPMdeframe img drawable)
	(let*	; @note: Scope of the following variables ends with mathching brace to the one before "let*"
		( (width (car (gimp-drawable-width drawable)))			;image width
		  (height (car (gimp-drawable-height drawable)))		;image height
		  (oldFg (car (gimp-palette-get-foreground) ) )			;foreground color before this script 
		  (oldBg (car (gimp-palette-get-background) ) )
		  (x	0)							;set up test point x
		  (y    0)							;set up test point y
		  (test 255)							;result of colorpicked pixel
		  (test_red 0)							;RGB components and sum
		  (test_blue 0)
		  (test_green 0)
		  (sum_rgb 0)		  
		  (notFound TRUE)						;found border's end?
		  (isBlack TRUE)						;scanned pixel is black?
		) ; ------------------------------------------------------------
		(gimp-context-set-default-colors)				;set to b/w 		
		; -------------------------------------------------------------- find & remove topmost border:
		(set! y 1)	;init y
		(set! x 40)	;init x
		(set! notFound TRUE)
		(while (and(< y 50) notFound)		;scanning will go top to bottom, while not found
			(set! test (gimp-image-pick-color img drawable x y TRUE TRUE 1))
			(set! test_red (car (car test)))			;red
			(set! test_blue (cadr (car test)))			;blue
			(set! test_green (caddr (car test)))			;green
			(set! sum_rgb (+ test_red test_blue test_green) )	;calc sum of r+b+g
			(set! notFound (> sum_rgb 0) )				;is black? => frame end found
			(set! y (+ y 1))					;increment y
		)	; @note: incrementation goes one px too far, thats ok because it'll comprehend the rounded corners
		(gimp-rect-select img 0 0 width y REPLACE FALSE 0)		;select area
		(gimp-edit-fill drawable FOREGROUND-FILL)			;fill rect with black
		;topmost border found & removed.
		; -------------------------------------------------------------- cut equally at the bottom:
		(gimp-rect-select img 0 (- height y) width y REPLACE FALSE 0)	;select area 
		;@note: gimp-rect-select does only select from left->right, top->bottom!
		(gimp-edit-fill drawable FOREGROUND-FILL)			;fill rect with black
		;bottom border removed.
		; -------------------------------------------------------------- find & remove left border:
		(set! y 40)	;init y
		(set! x 1)	;init x
		(set! notFound TRUE)
		(while (and(< x 50) notFound)		;scanning will go top to bottom, while not found
			(set! test (gimp-image-pick-color img drawable x y TRUE TRUE 1))
			(set! test_red (car (car test)))			;red
			(set! test_blue (cadr (car test)))			;blue
			(set! test_green (caddr (car test)))			;green
			(set! sum_rgb (+ test_red test_blue test_green) )	;calc sum of r+b+g
			(set! notFound (> sum_rgb 0) )				;is black? => frame end found
			(set! x (+ x 1))					;increment x
		)	; @note: incrementation goes one px too far, thats ok because it'll comprehend the rounded corners
		(gimp-rect-select img 0 0 x height REPLACE FALSE 0)		;select area
		(gimp-edit-fill drawable FOREGROUND-FILL)			;fill rect with black
		;left border found & removed.
		; -------------------------------------------------------------- cut equally at the right border:
		(gimp-rect-select img (- width x) 0 x height REPLACE FALSE 0)	;select area 
		(gimp-edit-fill drawable FOREGROUND-FILL)			;fill rect with black
		;right border removed.
		; -------------------------------------------------------------- remove vertical middle line:
		(gimp-rect-select img (- (/ width 2) 28) 0 56 height REPLACE FALSE 0)	;select area 
		(gimp-edit-fill drawable FOREGROUND-FILL)			;fill rect with black
		;vertical middle line removed.
		; -------------------------------------------------------------- find & remove white line at images top border:
		(define (
			 removeOnePxFromTopOfImages
			)
			(set! y 1)	;init y
			(set! x 100)	;init x
			(set! isBlack TRUE)
			(while (and(< y 50) isBlack)		;scanning will go top to bottom, while not found
				(set! test (gimp-image-pick-color img drawable x y TRUE TRUE 1))	
				;params: 	image, drawable, x, y, Use the composite image, not the drawable, 
				;		average the color of all the pixels in a specified radius (TRUE or FALSE), radius

				(set! test_red (car (car test)))			;red
				(set! test_blue (cadr (car test)))			;blue
				(set! test_green (caddr (car test)))			;green
				(set! sum_rgb (+ test_red test_blue test_green) )	;calc sum of r+b+g
				(set! isBlack (< sum_rgb 5) )				;is black? => white line not found yet
				(set! y (+ y 1))					;increment y
			)
			(set! y (+ y 1))						;increment y
			(gimp-rect-select img 0 0 width y REPLACE FALSE 0)		;select area
			(gimp-edit-fill drawable FOREGROUND-FILL)			;fill rect with black
			;1 pixel white line at top of images found & removed.
		)
		(removeOnePxFromTopOfImages)
		(removeOnePxFromTopOfImages)
		(removeOnePxFromTopOfImages)
		(removeOnePxFromTopOfImages)
		; -------------------------------------------------------------- find & remove white line at images bottom border:
		(define (
			 removeOnePxFromBottomOfImages
			)
			(set! y (- height 1))	;init y
			(set! x 100)		;init x
			(set! isBlack TRUE)
			(while (and(> y (- height 50)) isBlack)		;scanning will go top to bottom, while not found
				(set! test (gimp-image-pick-color img drawable x y TRUE TRUE 1))	
				;params: 	image, drawable, x, y, Use the composite image, not the drawable, 
				;		average the color of all the pixels in a specified radius (TRUE or FALSE), radius

				(set! test_red (car (car test)))			;red
				(set! test_blue (cadr (car test)))			;blue
				(set! test_green (caddr (car test)))			;green
				(set! sum_rgb (+ test_red test_blue test_green) )	;calc sum of r+b+g
				(set! isBlack (< sum_rgb 5) )				;is black? => white line not found yet
				(set! y (- y 1))					;increment y
			)
			(gimp-rect-select img 0 y width (- height y) REPLACE FALSE 0)	;select area
			(gimp-edit-fill drawable FOREGROUND-FILL)			;fill rect with black
			;1 pixel white line at bottom of images found & removed.
		)
		(removeOnePxFromBottomOfImages)
		(removeOnePxFromBottomOfImages)
		(removeOnePxFromBottomOfImages)
		(removeOnePxFromBottomOfImages)
		; -------------------------------------------------------------- find & remove white line at left border:
		(define (
			 removeOnePxFromLeftOfImage
			)
			(set! x 1)	;init y
			(set! y 100)	;init x
			(set! isBlack TRUE)
			(while (and(< x 50) isBlack)		;scanning will go top to bottom, while not found
				(set! test (gimp-image-pick-color img drawable x y TRUE TRUE 1))	
				;params: 	image, drawable, x, y, Use the composite image, not the drawable, 
				;		average the color of all the pixels in a specified radius (TRUE or FALSE), radius

				(set! test_red (car (car test)))			;red
				(set! test_blue (cadr (car test)))			;blue
				(set! test_green (caddr (car test)))			;green
				(set! sum_rgb (+ test_red test_blue test_green) )	;calc sum of r+b+g
				(set! isBlack (< sum_rgb 5) )				;is black? => white line not found yet
				(set! x (+ x 1))					;increment x
			)
			(set! x (+ x 1))						;increment x
			(gimp-rect-select img 0 0 x height REPLACE FALSE 0)		;select area
			(gimp-edit-fill drawable FOREGROUND-FILL)			;fill rect with black
			;1 pixel white line at left of left image found & removed.
		)
		(removeOnePxFromLeftOfImage)
		(removeOnePxFromLeftOfImage)
		(removeOnePxFromLeftOfImage)
		(removeOnePxFromLeftOfImage)
		; -------------------------------------------------------------- find & remove white line at right border:
		(define (
			 removeOnePxFromRightOfImage
			)
			(set! x (- width 1))	;init y
			(set! y 100)		;init x
			(set! isBlack TRUE)
			(while (and(> x (- width 50)) isBlack)		;scanning will go top to bottom, while not found
				(set! test (gimp-image-pick-color img drawable x y TRUE TRUE 1))	
				;params: 	image, drawable, x, y, Use the composite image, not the drawable, 
				;		average the color of all the pixels in a specified radius (TRUE or FALSE), radius

				(set! test_red (car (car test)))			;red
				(set! test_blue (cadr (car test)))			;blue
				(set! test_green (caddr (car test)))			;green
				(set! sum_rgb (+ test_red test_blue test_green) )	;calc sum of r+b+g
				(set! isBlack (< sum_rgb 5) )				;is black? => white line not found yet
				(set! x (- x 1))					;increment x
			)
			(gimp-rect-select img x 0 (- width x) height REPLACE FALSE 0)		;select area
			(gimp-edit-fill drawable FOREGROUND-FILL)			;fill rect with black
			;1 pixel white line at right of right image found & removed.
		)
		(removeOnePxFromRightOfImage)
		(removeOnePxFromRightOfImage)
		(removeOnePxFromRightOfImage)
		(removeOnePxFromRightOfImage)

		; -------------------------------------------------------------- done. cleanup:
		(gimp-palette-set-background oldBg)				;restore old fg/bg colors
		(gimp-palette-set-foreground oldFg)
	)
	(gimp-displays-flush)	;flush output
 )	; ---------------------------------------------------------------------- register (into image/Script-Fu menu):
(script-fu-register "script-fu-SPMdeframe"
                    _"<Image>/Script-Fu/Stereo imaging/Remove stereophotomaker borders"
		     "Find & remove white framing from Stereophotomaker images (image pairs)"
		     "Kay Stenschke <info@stenschke.com>"
                     "Kay Stenschke"
                     "2008-18-08"
                     "RGB*, GRAY*" 
                     SF-IMAGE    "Image" 0
		     SF-DRAWABLE "Drawable" 0
)	; ----------------------------------------------------------------------



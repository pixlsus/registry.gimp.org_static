; creates a shadow (transparent) version of the image
;
; i use this script for colorizing an img with CSS/HTML
; i.e. I display the shadow img and change the bg color with CSS
;
; if u use the original colors from the picture u should get almost the same picture 
; u can play with the black-point/white-point values for less/more gray...
; though it doesn't change much...
;
; it meant to be use with uni-color img, but works with any img.
;
; tested on png, jpg, GIMP 2.6.6
; it gives an error message: Error: illegal function
; I dont know why (didn't had the energy to debug it) but it works fine for me any way
; so I simply ignore it
;
; see GIMP on-line help for installation instruction of script-fu
;
; usage: open an image and run the script
; Note: i test it only on a flat image,  
; with multiple layer it should operate on the current active layer
;
; after running the script:
; * delete invisible layer (optional)
; * merge visible layer (optional)
; * save with a different name (as png for keeping the transparent charecteristic of the image)
;
; Elia Weiss

 (define (script-fu-create-shadows image
                        drawable
						black-point
						white-point)
 (let* (
		;(black-point 170)
		;(white-point 170)
		;(filename "logo.jpg")
		;(image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
		;(drawable (car (gimp-image-get-active-layer image)))
	    (width (car (gimp-drawable-width drawable)))
        (height (car (gimp-drawable-height drawable)))
        (shadow-layer (car (gimp-layer-new image width height
                                       RGB-IMAGE "shadow" 100 NORMAL-MODE)))
		(shadow-level 0)
		(shadow-mask  0)
		
		(highlights-layer 0)
		(highlights-level 0)
		(highlights-mask  0)

		(anchor-mask  0)
		) 
	(			
	(gimp-drawable-set-name drawable "original layer")
	
	; create shadow layer
	(script-fu-util-image-add-layers image shadow-layer)
	(set! shadow-level (car (gimp-layer-copy drawable TRUE)))
	(gimp-drawable-set-name shadow-level "shadow levels")
	(script-fu-util-image-add-layers image shadow-level)
	(gimp-levels shadow-level 0 0 black-point 1.0 0 255)
	(gimp-invert shadow-level)
	
	(set! shadow-mask (car (gimp-layer-create-mask shadow-layer  ADD-WHITE-MASK)))
	(gimp-layer-add-mask shadow-layer shadow-mask)
	
	(gimp-selection-all image)
	(gimp-edit-copy shadow-level)
	
	(set! anchor-mask (car (gimp-edit-paste shadow-mask FALSE)))
	(gimp-floating-sel-anchor anchor-mask)
	(gimp-layer-remove-mask shadow-layer MASK-APPLY)
	
	; create highlights layer
	(gimp-context-set-foreground '(255 255 255))
	(set! highlights-layer (car (gimp-layer-new image width height
			RGB-IMAGE "highlights" 100 NORMAL-MODE)))
	(script-fu-util-image-add-layers image highlights-layer)
	(gimp-edit-bucket-fill highlights-layer  FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
	(set! highlights-level (car (gimp-layer-copy drawable TRUE)))
	(gimp-drawable-set-name highlights-level "highlights levels")
	(script-fu-util-image-add-layers image highlights-level)
	(gimp-levels highlights-level 0 white-point 255 1.0 0 255)
	
	(set! highlights-mask (car (gimp-layer-create-mask highlights-layer  ADD-WHITE-MASK)))
	(gimp-layer-add-mask highlights-layer highlights-mask)
	
	(gimp-selection-all image)
	(gimp-edit-copy highlights-level)
	
	(set! anchor-mask (car (gimp-edit-paste highlights-mask FALSE)))
	(gimp-floating-sel-anchor anchor-mask)
	(gimp-layer-remove-mask highlights-layer MASK-APPLY)

	(gimp-drawable-set-visible highlights-level FALSE)
	(gimp-drawable-set-visible shadow-level FALSE)
	(gimp-drawable-set-visible  drawable FALSE)
	
	;(gimp-layer-copy drawable TRUE)
	;(gimp-image-clean-all image)
	(gimp-displays-flush image)
	)
)
)



(script-fu-register "script-fu-create-shadows"
  _"shadows"
  _"create a shadow transparent from the image"
  "Elia weiss <elia_weiss@yahoo.com>"
  "Elia weiss"
  "25/9/09"
  "RGB* GRAY*"
  SF-IMAGE       "Image"          0
  SF-DRAWABLE    "Drawable"       0
  SF-ADJUSTMENT _"white-point"    '(170 0 255 1 10 0 1)
  SF-ADJUSTMENT _"black-point"    '(170 0 255 1 10 0 1)
  
)

(script-fu-menu-register "script-fu-create-shadows"
                         "<Image>/Filters/shadows")

; Layers-slices script-fu
; (c)2009 Robert Hendrickx

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3 or higher.

;; v0.3

; Changelog :
; --- v0.3
; Now, keep transparency when saving to .png, by merging layers instead of flatten the image.


; Function to delete "SLICE-" layers from the image
; It also delete invisible layers for optimization
; and scale all the layers to the image size to keep the image size when merging.
(define (delete-layer image)
	(let* (
					(layers (cadr (gimp-image-get-layers image)))		; Layer array
					(i (- (car (gimp-image-get-layers image)) 1 ))	; Layer number
					(name "")
					(layer 0)
					(first TRUE)
				)

		(while (>= i 0)
			(let* (
							(layer (aref layers i))			; Pick the layer to check
							(name (car (gimp-drawable-get-name layer)))		; extract the name
						)
				(set! i (- i 1))

				(if (> (string-length name) 10)		; "slice-" is 6, ".xxx" is 4, any valid name will be at least 11 long
					(if (string-ci=? "slice-" (substring name 0 6))
						(gimp-image-remove-layer image layer)		; delete the layer
					)
					(if  (not (gimp-drawable-get-visible layer)) ; if layer is not visible
						(gimp-image-remove-layer image layer)		; delete the layer
						; then first visible layer must be scaled to the image size, to preserve
						; the slice size when it's bigger than the visible layers
						(if first
							(gimp-layer-resize-to-image-size layer)
							(set! first FALSE)
						)
					)
				)
			)
		)
	)
)

; For each layer, check if the name begins with "SLICE-",
; create a duplicate image, remove the "SLICE-" layers, flatten the image, crop to
; the layer dimensions and save it
(define (manage-images image param-directory param-jpg-quality)
	(let* (
					(layers (cadr (gimp-image-get-layers image)))		; array of layers
					(i (- (car (gimp-image-get-layers image)) 1 ))		; Number of layers
					(layer 0)
					(flatten-layer 0)
					(name "")
				)

		(while (>= i 0)
			(let* (
							(layer (aref layers i))		; pick the layer to work on
							(name (car (gimp-drawable-get-name layer)))		; retreive it's name
						)

				(set! i (- i 1))
				(if (> (string-length name) 10)		; "slice-" is 6, ".xxx" is 4, any valid name will be at least 11 long
					(if (string-ci=? "SLICE-" (substring name 0 6))
						; It's a "slice-" layer
						(let* (
										; Store the Slice layer parameters
										(x (car (gimp-drawable-offsets layer)))
										(y (cadr (gimp-drawable-offsets layer)))
										(height (car (gimp-drawable-height layer)))
										(width (car (gimp-drawable-width layer)))

										; store the filename to use
										(filename (substring name 6 (string-length name)))
										(extension (substring filename (- (string-length filename) 4) (string-length filename)))
								
										;create the duplicated image
										(newimage (car (gimp-image-duplicate image)))
									)
	
							(delete-layer newimage)		; call the function to delete all "SLICE-" layers from the new image
							
							(gimp-image-crop newimage width height x y)		; crop the image to the stored dimensions
	
							(set! flatten-layer (car (gimp-image-merge-visible-layers newimage 1)))		; flatten the resulting image, keeping transparency
							; save the image, depending on the extension
							(if (string-ci=? ".jpg" extension)
								(file-jpeg-save RUN-NONINTERACTIVE											; no dialog
																newimage																; cropped flatten image
																flatten-layer														; resulting layer
																(string-append param-directory "/" filename)		; filename with target directory
																(string-append param-directory "/" filename)		; filename with target directory
																param-jpg-quality												; Requested quality
																0.0																			; smoothing factor
																1																				; Optimize entropy	
																1																				; Progressive loading
																""																			; comment
																0																				; subsampling option number
																1																				; Force baseline JPEG
																0																				; Frequency of restart markers
																1																				; DCT algorithm
								)
								(if (string-ci=? ".png" extension)
									(file-png-save2 RUN-NONINTERACTIVE											; no dialog
																	newimage																; cropped flatten image
																	flatten-layer														; resulting layer
																	(string-append param-directory "/" filename)		; filename with target directory
																	(string-append param-directory "/" filename)		; filename with target directory
																	0																				; Adam7 interlacing
																	9																				; compression factor
																	0																				; Write bkGD chunk
																	0																				; Write gAMA chunk
																	0																				; Write oFFs chunk
																	0																				; Write pHYs chunk
																	0																				; Write tIME chunk
																	0																				; Write comment
																	0																				; Preserve color of transparent pixels
									)
									(gimp-message (string-append "Invalid file extension \"" extension "\"for filename \"" filename "\". Use .jpg or .png only."))
								)
							)
						)
					)
				)
			)
		)
	)
)

; main function
(define (script-fu-layers-slices
					image
					drawable
					param-directory
					param-jpg-quality)
		(set! param-jpg-quality (/ param-jpg-quality 100))		; quality is 0<quality<1 and parameter is 0<param<100

		(gimp-image-undo-group-start image)		; take care of the Undo, even if it should not be needed...
		
		(manage-images image param-directory param-jpg-quality)		; Call the function to scan layers and create the images
		
		(gimp-image-undo-group-end image)
		(gimp-displays-flush)		; update the display (should not be needed...)
)

; Registering of the function in GIMP
(script-fu-register "script-fu-layers-slices"
  "Layers Slices..."
  "Create one image per layer named \"SLICE-xxx\".\
	  Each image will be cropped to the layer size, flatten\
		and saved as xxx.\
		Extension (.png or .jpg) must be specified."
  "Robert Hendrickx"
  "Robert Hendrickx"
  "(c)2009 Robert Hendrickx"
  "RGB* GRAY* INDEXED*"
  SF-IMAGE    	"Image"    0
  SF-DRAWABLE 	"Drawable" 0
	SF-DIRNAME		"Target directory" ""
	SF-ADJUSTMENT	"JPG Quality" '(90 0 100 1 10 0 SF-SLIDER)
)

(script-fu-menu-register "script-fu-layers-slices"
                         "<Image>/Filters/Web")

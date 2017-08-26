(define (write_xc_message ws xc)
	(let 
		((i 0))
		(set! i 0)
		(while (< i 3) 
			(set! ws 
				(string-append 
					ws 
					(number->string (aref xc i)) 
					";"
				)
			) 
			(set! i (+ i 1))
		) 
		(gimp-message ws)
	)
)

(define (script-fu-back-to-white img)

	(gimp-context-push)
	(gimp-image-undo-group-start img)

	(gimp-progress-set-text "Correcting background")
	
	(let
		(
			(xc #(0 0 0)) 
			(px #(0 0 0))
			(dr 0)
			(y 0)
			(i 0)
		)

		(set! xc #(0 0 0))
		(set! dr (car (gimp-image-get-active-drawable img)))

		(set! i 0)
		(while (< i 3)
			(aset xc i 0)
			(set! i (+ i 1))
		)

		;(write_xc_message "before: " xc)
		
		(while (< y 100)
			(set! i 0)
			(set! px (cadr (gimp-drawable-get-pixel dr 
				(rand (- (car (gimp-drawable-width dr)) 1)) 
				(rand (- (car (gimp-drawable-height dr)) 1))
				))
			)
			(while (< i 3)
				(aset xc i (+ (aref xc i) (aref px i) ) )
				(set! i (+ i 1))
			)
			(set! y (+ y 1))
			(gimp-progress-update (/ y 100)) 
		)
		
		;(write_xc_message "after: " xc)
		
		(set! i 0)
		(while (< i 3)
			(set! y (round (/ (aref xc i) 100 ) ))
			(gimp-levels dr (+ i 1) 0 y 1.0 0 255)
			(set! i (+ i 1))
		)
		(gimp-levels dr 0 0 255 0.60 0 255)
	)

	
	(gimp-image-undo-group-end img) 
	(gimp-displays-flush)
	(gimp-progress-update 1.0) 
	(gimp-context-pop)
)

(define (batch-back-to-white pattern)
	(let* 
		(
			(filelist (cadr (file-glob (string-append pattern "\\*.jpg") 1)))
		)
		(while (not (null? filelist))
			(let* 
				(
					(filename (car filelist)
				)
				(image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
				(drawable (car (gimp-image-get-active-layer image))))
				(script-fu-back-to-white image)
				(gimp-file-save RUN-NONINTERACTIVE image drawable filename filename) 
				(gimp-image-delete image)
			)
			(set! filelist (cdr filelist))
		)
	)
)

(script-fu-register "script-fu-back-to-white"
                    "Reset background to white"
                    "Reset background, by changing channels levels upper value to average"
                    "Leonid Koninin"
                    "Leonid Koninin"
                    "2011"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
)

(script-fu-register "batch-back-to-white"
                    "Reset background to white (all *.jpg in directory)"
                    "Reset background, by changing channels levels upper value to average"
                    "Leonid Koninin"
                    "Leonid Koninin"
                    "2011"
                    "RGB*, GRAY*"
                    SF-DIRNAME       "Directory"       ""
)

(script-fu-menu-register "script-fu-back-to-white"
                         "<Image>/Filters/Leon")

(script-fu-menu-register "batch-back-to-white"
                         "<Image>/Filters/Leon")


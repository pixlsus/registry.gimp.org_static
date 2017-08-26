(define (migee-add-watermark 
	watermark_path
	inputfiles
	watermark_size
	watermark_padding
	watermark_layer_mode
	position_num
	output_dir)



	(let* ((filelist (cadr (file-glob inputfiles 1))))
		(while (not (null? filelist))
			(let*	
				(
					(filename (car filelist))
					(image (car (file-jpeg-load RUN-NONINTERACTIVE filename filename)))
					(watermark (car (gimp-file-load-layer RUN-NONINTERACTIVE image watermark_path)))
				) 

				(gimp-display-new image)
				(gimp-image-insert-layer image watermark 0 -1)

				;Scale Watermark        
				(if (>= (car (gimp-drawable-width watermark)) (car (gimp-drawable-height watermark)))
					;Width of watermark is bigger. So set the width to percent, the adjust height to fit into the correct ratio.
					;Height is set to same percent, then adjusted with aspect ratio (watermark) height/width.
					;Scale Watermark [n][          NEW WIDTH             ] [                             NEW HEIGHT  ((image width * .30)/watermark width) watermark height      ] [LOCAL ORIGIN]
					(gimp-layer-scale watermark (* (car (gimp-image-width image)) watermark_size) (* (/ (* (car (gimp-image-width image)) watermark_size) (car (gimp-drawable-width watermark))) (car (gimp-drawable-height watermark))) TRUE)
					;Height is bigger on watermark
					;Scale Watermark [n][            NEW WIDTH  (30% of image height. Then divided by watermark aspect ratio (height/width)    ][ NEW HEIGHT                       ] [LOCAL ORIGIN]
					(gimp-layer-scale watermark (* (/ (* (car (gimp-image-height image)) watermark_size) (car (gimp-drawable-height watermark))) (car (gimp-drawable-width watermark))) (* (car (gimp-image-height image)) watermark_size) TRUE)
				)

				;Top Left positioning
				(if (= position_num 1)
				(gimp-layer-set-offsets watermark (* (car (gimp-image-width image)) watermark_padding) (* (car (gimp-image-height image)) watermark_padding))
				)

				;Top Right positioning
				(if (= position_num 2)
				(gimp-layer-set-offsets watermark (- (- (car (gimp-image-width image)) (car (gimp-drawable-width watermark)))(* (car (gimp-image-width image)) watermark_padding)) (* (car (gimp-image-height image)) watermark_padding))
				)

				;Bottom Left positioning
				(if (= position_num 3)
				(gimp-layer-set-offsets watermark (* (car (gimp-image-width image)) watermark_padding) (- (- (car (gimp-image-height image)) (car (gimp-drawable-height watermark)))(* (car (gimp-image-height image)) watermark_padding)))
				)

				;Bottom Right positioning
				(if (= position_num 4)
				(gimp-layer-set-offsets watermark (- (- (car (gimp-image-width image)) (car (gimp-drawable-width watermark)))(* (car (gimp-image-width image)) watermark_padding)) (- (- (car (gimp-image-height image)) (car (gimp-drawable-height watermark)))(* (car (gimp-image-height image)) watermark_padding)))
				)

				;Center positioning (image dimension / 2) - (watermark same dimension /2) 
				(if (= position_num 5)
					(gimp-layer-set-offsets watermark
						(-
							(/ (car (gimp-image-width image)) 2)
							(/ (car (gimp-drawable-width watermark)) 2)
						)
						(-
							(/ (car (gimp-image-height image)) 2)
							(/ (car (gimp-drawable-height watermark)) 2)
						)
					)
				)

				(gimp-layer-set-mode watermark watermark_layer_mode)

				(gimp-image-merge-visible-layers image 1)

				(gimp-file-save RUN-NONINTERACTIVE image (car (gimp-image-get-active-layer image)) (migee-filename-output filename output_dir) (migee-filename-output filename output_dir))

				(gimp-display-delete 1)
			) 

			(set! filelist (cdr filelist))    
		)
	)
)


(define (migee-filename-output thePathToFile output_dir)
	(let*
		(
			(theFileParts (strbreakup thePathToFile "."))
			(theExtension (car (last theFileParts)))
			
			(thePathParts (strbreakup thePathToFile "\\"))
			(theNoPathFileName (car (last thePathParts)))

			(theNewFileName)
		)
		
		(set! theNewFileName (string-append output_dir "\\wm_" theNoPathFileName))
	)
)
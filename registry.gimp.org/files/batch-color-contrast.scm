(define (batch-color-contrast filepath file-extension keep-original 	apply-highlight highlight-cyan-red highlight-magenta-green highlight-yellow-blue 
																		apply-midtone midtone-cyan-red midtone-magenta-green midtone-yellow-blue
																		apply-shadow shadow-cyan-red shadow-magenta-green shadow-yellow-blue
																		preserve-lum apply-cont brightness contrast)
	(let* 
		(
			(filelist (cadr (file-glob (string-append filepath DIR-SEPARATOR "*." file-extension) 1)))
		)
		(while (not (null? filelist))
			(let* 
				(
					(filename (car filelist)
				)
				(image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
				(drawable (car (gimp-image-get-active-layer image)))
				(Ouiche (car (gimp-drawable-is-indexed drawable))))	; \
				(if (= Ouiche TRUE)										;  Enable indexed images to be edited. Thanks to bakalex92 (http://registry.gimp.org/user/23077)
					(begin (gimp-image-convert-rgb image))				; /
				) 
				;Apply filters    
				(if (< 0 apply-highlight)
					(gimp-color-balance 
								drawable 
								2 
								preserve-lum 
								highlight-cyan-red 
								highlight-magenta-green 
								highlight-yellow-blue)
				)
				(if (< 0 apply-midtone)
					(gimp-color-balance 
								drawable 
								1 
								preserve-lum 
								midtone-cyan-red 
								midtone-magenta-green 
								midtone-yellow-blue)
				)
				(if (< 0 apply-shadow)
					(gimp-color-balance 
								drawable 
								0 
								preserve-lum 
								shadow-cyan-red 
								shadow-magenta-green 
								shadow-yellow-blue)
				)
				(if (< 0 apply-cont)
					(gimp-brightness-contrast 
								drawable brightness contrast)
				)
				(if (= Ouiche TRUE)															; \
						(begin (gimp-image-convert-indexed image 0 0 255 FALSE FALSE ""))   ;  Enable indexed images to be edited. Thanks to bakalex92 (http://registry.gimp.org/user/23077)
				)                                                                           ; /
				;Save to file	
				(if (< 0 keep-original)
					;if true
						(gimp-file-save RUN-NONINTERACTIVE
								 image drawable (string-append filepath DIR-SEPARATOR "_" (substring filename (+ (string-length filepath) 1))) 
												(string-append filepath DIR-SEPARATOR "_" (substring filename (+ (string-length filepath) 1))))
					;if false
						(gimp-file-save RUN-NONINTERACTIVE
								 image drawable filename filename)
				)
				(gimp-image-delete image)
			)
			(set! filelist (cdr filelist))
		)
	)
)
; Register with script-fu.
(script-fu-register "batch-color-contrast"
					"Batch Color Balance and Contrast"
					"Applys Color balance, brightness and contrast adjustments to all files in the selected folder"
					"Kristoffer Myskja <post@kristoffermyskja.com>"
					"Kristoffer Myskja"
					"2010-2-8 (last updated 2010-11-16)"
					""
					SF-DIRNAME "Folder" "C:\\"
					SF-STRING "File type (use * for all types)" "jpg"
					SF-TOGGLE _"Keep original files and save new files with prefix \' _ \'" TRUE
					;Highlights
					SF-TOGGLE _"Make changes to Highlights" FALSE
					SF-ADJUSTMENT "Cyan-Red color balance" '(0 -100 100 1 5 0 SF-SLIDER)
					SF-ADJUSTMENT "Magenta-Green color balance" '(0 -100 100 1 5 0 SF-SLIDER)
					SF-ADJUSTMENT "Yellow-Blue color balance" '(0 -100 100 1 5 0 SF-SLIDER)
					;Midtones
					SF-TOGGLE _"Make changes to Midtones" FALSE
					SF-ADJUSTMENT "Cyan-Red color balance" '(0 -100 100 1 5 0 SF-SLIDER)
					SF-ADJUSTMENT "Magenta-Green color balance" '(0 -100 100 1 5 0 SF-SLIDER)
					SF-ADJUSTMENT "Yellow-Blue color balance" '(0 -100 100 1 5 0 SF-SLIDER)
					;Shadows
					SF-TOGGLE _"Make changes to Shadows" FALSE
					SF-ADJUSTMENT "Cyan-Red color balance" '(0 -100 100 1 5 0 SF-SLIDER)
					SF-ADJUSTMENT "Magenta-Green color balance" '(0 -100 100 1 5 0 SF-SLIDER)
					SF-ADJUSTMENT "Yellow-Blue color balance" '(0 -100 100 1 5 0 SF-SLIDER)
					SF-TOGGLE _"Preserve luminosity values at each pixel" TRUE
					;Brightness & Contrast
					SF-TOGGLE _"Make changes to Brightness and Contrast" FALSE
					SF-ADJUSTMENT "Brightness" '(0 -127 127 1 5 0 SF-SLIDER)
					SF-ADJUSTMENT "Contrast" '(0 -127 127 1 5 0 SF-SLIDER)
					)
(script-fu-menu-register "batch-color-contrast"
						 "<Toolbox>/Xtns/Misc/")

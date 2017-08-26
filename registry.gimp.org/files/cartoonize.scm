(define (batch-cartoon-effect pattern mask-radius pct-black)
(let* ((filelist (cadr (file-glob pattern 1))))
	(while (not (null? filelist))
		(let* ((filename (car filelist))
			(image (car (gimp-file-load RUN-NONINTERACTIVE
												filename filename)))
		(drawable (car (gimp-image-get-active-layer image))))
	(plug-in-cartoon RUN-NONINTERACTIVE
							image drawable mask-radius pct-black)
	(gimp-file-save RUN-NONINTERACTIVE
                     image drawable filename filename)
		(gimp-image-delete image))
	(set! filelist (cdr filelist)))))
	; Register with script-fu.
		(script-fu-register "batch-cartoon-effect"
									"Batch cartoon effect"
									"Applys the cartoon effect to multiple images"
									"Tom Wright <tom.tdw@gmail.com>"
									"Tom Wright"
									"2008-07-3"
									""
									SF-VALUE "pattern" ""
									SF-VALUE "mask-radius" "22"
									SF-VALUE "pct-black" "0.88")
		(script-fu-menu-register "batch-cartoon-effect" "<Toolbox>/Xtns/Misc/")

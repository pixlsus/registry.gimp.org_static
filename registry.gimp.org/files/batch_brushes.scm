(define (script_fu_batch_brushes inDir inLoadType )
	(let*
		(
			(varLoadStr 0)
			(varSaveStr 0)
			(image 0)
			(drawable 0)
			(newfilename 0)
			(filename 0)
			(varFileList 0)
			(imagetype 0)
			(handler (car (gimp-message-get-handler)))
		)
		(set! varLoadStr
			(cond 
				(( equal? inLoadType 0 ) ".jpg" )
				(( equal? inLoadType 1 ) ".bmp" )
				(( equal? inLoadType 2 ) ".png" )
				(( equal? inLoadType 3 ) ".gif" )
				(( equal? inLoadType 4 ) ".xcf" )
			)
		) 
		(set! varFileList (cadr (file-glob (string-append inDir DIR-SEPARATOR "*" varLoadStr)  1)))
 
		(while (not (null? varFileList))
			(set! filename (car varFileList))
			(set! image (car (gimp-file-load 1 filename filename)))
			(set! newfilename (string-append (substring filename 0 (- (string-length filename) 4)) ".gbr"))
			(gimp-image-flatten image)
			(set! imagetype (gimp-image-base-type image))
			;(when (not (= imagetype 1))
			;	(gimp-image-convert-grayscale image)
			;)
			(set! drawable (car (gimp-image-get-active-layer image)))
			(gimp-file-save 1 image drawable newfilename newfilename)
			(gimp-image-delete image)
			(set! varFileList (cdr varFileList))
		)
	)
)

(script-fu-register "script_fu_batch_brushes"
	"<Toolbox>/Xtns/Batch Tools/Batch convert to brushes..."
	"Create Brushes for all files in a directory"
	"Emily Smirle"
	"Emily Smirle"
	"July 2010"
	""
	SF-DIRNAME    "Load from" ""
	SF-OPTION     "Load File Type" (list "jpg" "bmp" "png" "gif" "xcf")
)
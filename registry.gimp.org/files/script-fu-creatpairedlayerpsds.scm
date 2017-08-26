(script-fu-register
	"script-fu-createpairedlayerpsds"
	"combine jpg and tif into psd"
	"Combines section colormaps and\
	background images into\
	multilayered psd files for easy\
	editing."
	"Erich Theiss"
	"Copyright 2009, Erich Theiss"
	"6th August, 2009"
	"RGB"
	SF-DIRNAME "folder" "Desktop"
	SF-VALUE "sectionstart" "1001"
	SF-VALUE "sectionend" "2878"
)
(script-fu-menu-register "script-fu-createpairedlayerpsds" "<Image>/Layers")

(define (script-fu-createpairedlayerpsds folder sectionstart sectionend)
	(let* (
		(section sectionstart)
		)
	
	(while (< section (+ sectionend 1)) ;puts the functions into a loop
	 	(define sectionstring (string-append folder (string-append "/" (number->string section)))) ;creating a string from the folder and the section numbers without the file format
		(define sectionstringjpg (string-append sectionstring ".jpg")) ;appending the file formats
		(define sectionstringtif (string-append sectionstring ".tif"))
		(define sectionstringpsd (string-append sectionstring ".psd"))
		(define jpgid (car (gimp-file-load 1 sectionstringjpg sectionstringjpg))) ;getting the drawable ids of the files to be used
		(define tifid (car (gimp-file-load-layer 1 jpgid sectionstringtif)))
		(gimp-file-load 1 sectionstringjpg sectionstringjpg) ;loads the base image
		(gimp-file-load-layer 1 jpgid sectionstringtif) ;loads the layer to be applied
		(gimp-image-add-layer jpgid tifid -1) ;applies the layer to the base image
		(gimp-file-save 1 jpgid tifid sectionstringpsd sectionstringpsd) ;saves the layered file as a psd
		(set! section (+ section 1)) ;increments the numbered files by one to keep the loop going
	)
	)
)



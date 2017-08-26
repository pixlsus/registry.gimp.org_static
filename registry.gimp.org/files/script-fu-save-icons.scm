; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
; Phoca Save Icons
; Copyright (C) 2008 Jan Pavelka ( http://www.phoca.cz )
; All rights reserved
;
; This script is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; The GNU General Public License can be found at
; http://www.gnu.org/copyleft/gpl.html.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(define (script-fu-save-icons-png image folder name interpolation x512 x256 x128 x64 x48 x32 x22 x16 interlace compression bKGD gAMA oFFs pHYs tIME comment svtrans)

(let* (
	(newImage 0)
	(newDraw 0)
	(newName "")
	(rawName "")
	;(folderNew "")
	(y 0)
	(partName "")
	(formats (cons-array 8 'byte))
	(formatsSelected (cons-array 8 'byte))
	(fS 0)
)


;Values - Formats
(aset formats 0 512)
(aset formats 1 256)
(aset formats 2 128)
(aset formats 3 64)
(aset formats 4 48)
(aset formats 5 32)
(aset formats 6 22)
(aset formats 7 16)

;Values - Selected Formats
(aset formatsSelected 0 x512)
(aset formatsSelected 1 x256)
(aset formatsSelected 2 x128)
(aset formatsSelected 3 x64)
(aset formatsSelected 4 x48)
(aset formatsSelected 5 x32)
(aset formatsSelected 6 x22)
(aset formatsSelected 7 x16)


(while (< y 8)

	(set! fS (aref formatsSelected y))

	(cond
		((= fS TRUE)
			;New Image
			(set! newImage (car (gimp-image-duplicate image)))
			(gimp-image-merge-visible-layers newImage 0)
			(gimp-image-scale-full newImage (aref formats y) (aref formats y) interpolation)
			(set! newDraw (car (gimp-image-get-active-drawable newImage)))
			
			;Name
			(set! partName (number->string (aref formats y)))
			;(set! folderNew (string-append folder "/" partName "/"))
			;(set! newName (string-append folderNew "/" name "-" partName ".png"))
			(set! newName (string-append folder "/" name "-" partName ".png"))
			(set! rawName (string-append name "-" partName "0.png"))
			
			;Save
			(file-png-save2 1 newImage newDraw newName rawName interlace compression bKGD gAMA oFFs pHYs tIME comment svtrans)
			
			;Delete
			(gimp-image-delete newImage)
		)
	)
	
	(set! y (+ y 1))
)


)
)

(script-fu-register	"script-fu-save-icons-png"
					_"<Image>/Script-Fu/Phoca Save Icons/PNG"
					"Save Icons PNG"
					"(c) Jan Pavelka ( http://www.phoca.cz ) 2008"
					"License GPLv3"
					"December 2008"
					"RGB* GRAY* INDEXED*"
					SF-IMAGE 	"Image"				0
					SF-DIRNAME	"Folder"			""
					SF-STRING 	"Name" 				""
					SF-ENUM 	"Interpolation" 	'("InterpolationType" "cubic")
					SF-TOGGLE	"PNG 512 x 512px"	TRUE
					SF-TOGGLE	"PNG 256 x 256px"	TRUE
					SF-TOGGLE	"PNG 128 x 128px"	TRUE
					SF-TOGGLE	"PNG 64 x 64px"		TRUE
					SF-TOGGLE	"PNG 48 x 48px"		TRUE
					SF-TOGGLE	"PNG 32 x 32px"		TRUE
					SF-TOGGLE	"PNG 22 x 22px"		TRUE
					SF-TOGGLE	"PNG 16 x 16px"		TRUE
					
					SF-TOGGLE		"Use Adam7 interlacing?"				FALSE
					SF-ADJUSTMENT	"Deflate Compression factor (0--9)"  	'(9 0 9 1 10 0 0)
					SF-TOGGLE		"Write bKGD chunk?"						TRUE
					SF-TOGGLE		"Write gAMA chunk?"						FALSE
					SF-TOGGLE		"Write oFFs chunk?"						FALSE
					SF-TOGGLE		"Write pHYs chunk?"						TRUE
					SF-TOGGLE		"Write tIME chunk?"						TRUE
					SF-TOGGLE		"Write comment?"						TRUE
					SF-TOGGLE		"Preserve color of transparent pixels?" TRUE
					
)
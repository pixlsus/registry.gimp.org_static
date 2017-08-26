;
; Hot Dot Batch, V2.0
;
; AUTHOR: Darla McKay (Darla@FarcryDesign.com), (C) 2007,2008
;
; This plugin was tested with Gimp 2.4
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License Version 3 as 
; published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
; GNU General Public License at  http://www.gnu.org/licenses for
; more details.
;
; DESCRIPTION:
; To fix areas produced by hot, dead or stuck pixels.
; The script is located in menus:
;     "<Image> / Script-Fu / Darla / Hot Dot" 
;     "<Xtns> / Darla / Hot Dot on File" 
;     "<Xtns> / Darla / Hot Dot on Folder"
;
; USAGE NOTES:
; Can be run on one file either interactively or in the background.
; Can also be run on a folder of files by specifying path and extension.
; Parameters are the location of the center of the bad spot, the coverage 
; required, and the offset (determines where to paste from), and amount
; of feathering to apply.
;     See http://www.FarcryDesign.com/GIMP/ for more information.
; =============================================================================
;
;
; SCRIPT SUMMARY:
; calculate source location and coordinates of top left TO location
; feathered elliptical selection and copy onto a new layer
; 
; Version 1.0 (2007) - Initial version
; Version 2.0 (Jan 2008)
; - updated for GIMP 2.4
; =============================================================================

(define (script-fu-Darla-HotDot InImage InLayer InX InY InCovX InCovY InOffX InOffY InFeather InFlatten)
	(gimp-image-undo-group-start InImage)

	(let*	(
		(NewLayer (car (gimp-layer-new InImage (car (gimp-image-width InImage)) (car (gimp-image-height InImage)) RGBA-IMAGE "Adjustment" 100 NORMAL-MODE)))
		(ToX (- InX (/ InCovX 2)))
		(ToY (- InY (/ InCovY 2)))
		(SrcX (+ InOffX ToX))
		(SrcY (+ InOffY ToY))
		(sel-float 1)
		)

		; add the new layer
		(gimp-image-add-layer InImage NewLayer -1)
		(gimp-selection-all InImage)     
		(gimp-edit-clear NewLayer)  
		(gimp-selection-none InImage)

		; selection
		(gimp-ellipse-select InImage SrcX SrcY InCovX InCovY REPLACE TRUE 1 InFeather)
		(gimp-edit-copy InLayer)

		; Add floating selection, offset and anchor it
		(set! sel-float (car (gimp-edit-paste NewLayer FALSE)))
		(gimp-layer-set-offsets sel-float ToX ToY)
		(gimp-floating-sel-anchor sel-float)

		; flatten image if needed
		(cond
			((= InFlatten TRUE) (gimp-image-merge-down InImage NewLayer CLIP-TO-IMAGE))
			((= InFlatten FALSE) (gimp-drawable-set-name NewLayer "Hot Dot Cover"))
		)
	)

	(gimp-selection-none InImage)
	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
	(set! InLayer (car (gimp-image-get-active-layer InImage)))
)

(define (script-fu-Darla-private-noninteractive-HotDot originalfilename newfilename InX InY InCovX InCovY InOffX InOffY InFeather)
	(let* (
		(InImage (car (gimp-file-load RUN-NONINTERACTIVE originalfilename originalfilename)))
		(InLayer (car (gimp-image-get-active-layer InImage)))
		(quality 1)
		(smoothing 0)
		(optimise 0)
		(progressive 0)
		(comment "Hot Dot Fixed")
		(subsampling 0)
		(baseline 1)
		(restart 0)
		(dct 2)
		)

		(script-fu-Darla-HotDot InImage InLayer InX InY InCovX InCovY InOffX InOffY InFeather TRUE)
		(set! InLayer (car (gimp-image-get-active-layer InImage)))
		(file-jpeg-save RUN-NONINTERACTIVE InImage InLayer newfilename newfilename quality smoothing optimise 
			progressive comment subsampling baseline restart dct )
	)
)

(define (script-fu-Darla-noninteractive-HotDot filename InX InY InCovX InCovY InOffX InOffY InFeather)
	(let* (
		(newfilename ())
		)

		(set! newfilename (string-append (substring filename 0 (- (string-length filename) 4)) "-HotDot.jpg"))
		(script-fu-Darla-private-noninteractive-HotDot filename newfilename InX InY InCovX InCovY InOffX InOffY InFeather)
	)
)

(define (script-fu-Darla-batch-folder-HotDot foldername InExt InSaveType InX InY InCovX InCovY InOffX InOffY InFeather)
	(let* (
		(wincheck ())
		(filenameswithwildcard ()) 
		(filelist ())
		(filename ())
		(newfilename ())
		)

		; set filename according to OS type - if wincheck > 0 then it's windows
		(set! wincheck (string-length (car (strbreakup foldername "/"))))
		(if (> wincheck 0) 
			(set! filenameswithwildcard (string-append foldername (string-append "\\*." InExt))) 
			(set! filenameswithwildcard (string-append foldername "/*.jpg"))
		)
		(set! filelist (cadr (file-glob filenameswithwildcard 1)))

		(while (not (null? filelist))
	            (set! filename (car filelist))
			; InSaveType Options: 0 append-HotDot 1 replace 2 new folder (must exist)
			(cond
				((= InSaveType 0) (set! newfilename (string-append (substring filename 0 (- (string-length filename) 4)) "-HotDot.jpg")))
				((= InSaveType 1) (set! newfilename filename))
				((= InSaveType 2) 
					(if (> wincheck 0) 
						(set! newfilename (string-append foldername "\\hotdot\\" (car (reverse (strbreakup filename "\\"))))) 
						(set! newfilename (string-append foldername "/hotdot/" (car (reverse (strbreakup filename "/")))))
					)
				) 
			) 
			(script-fu-Darla-private-noninteractive-HotDot filename newfilename InX InY InCovX InCovY InOffX InOffY InFeather)
			(gimp-displays-flush)
			(gc)
			(set! filelist (cdr filelist))
		)
	)
)

(script-fu-register "script-fu-Darla-HotDot"
	_"<Image>/Script-F_u/_Darla/_Hot Dot"
	"Hot Dot \n\
To fix areas produced by hot, dead or stuck pixels. \n\
See http://www.FarcryDesign.com/GIMP for more information."
	"Darla McKay (Darla@FarcryDesign.com)"
	"Darla McKay"
	"2007,2008"
      "RGB*"
	SF-IMAGE		"The Image"	0
	SF-DRAWABLE		"The Layer"	0
	SF-ADJUSTMENT	_"Hot Dot X"			'(2298 1 4000 1 0 0 0)
	SF-ADJUSTMENT	_"Hot Dot Y"			'(516 1 3000 1 0 0 0)
	SF-ADJUSTMENT	_"Coverage Pixels X"	'(12 1 200 1 0 0 0)
	SF-ADJUSTMENT	_"Coverage Pixels Y"	'(9 1 200 1 0 0 0)
	SF-ADJUSTMENT	_"Offset X"			'(-5 -20 50 1 0 0 0)
	SF-ADJUSTMENT	_"Offset Y"			'(-6 -20 50 1 0 0 0)
	SF-ADJUSTMENT	_"Feather"		'(3 1 20 1 0 0 0)
	SF-TOGGLE		_"Flatten Image"		FALSE
)

(script-fu-register "script-fu-Darla-noninteractive-HotDot"
	"<Toolbox>/_Xtns/_Darla/_Hot Dot on File"
	"Hot Dot on File\n\
Processes a file in the background - to fix areas \
produced by hot, dead or stuck pixels. \n\
See http://www.FarcryDesign.com/GIMP for more information."
	"Darla McKay (Darla@FarcryDesign.com)"
	"Darla McKay"
	"2007,2008"
	""
	SF-FILENAME 	_"File Name:" ""
	SF-ADJUSTMENT	_"Hot Dot X"			'(2298 1 4000 1 0 0 0)
	SF-ADJUSTMENT	_"Hot Dot Y"			'(516 1 3000 1 0 0 0)
	SF-ADJUSTMENT	_"Coverage Pixels X"	'(12 1 200 1 0 0 0)
	SF-ADJUSTMENT	_"Coverage Pixels Y"	'(9 1 200 1 0 0 0)
	SF-ADJUSTMENT	_"Offset X"			'(-5 -20 50 1 0 0 0)
	SF-ADJUSTMENT	_"Offset Y"			'(-6 -20 50 1 0 0 0)
	SF-ADJUSTMENT	_"Feather"		'(3 1 20 1 0 0 0)
)

(script-fu-register "script-fu-Darla-batch-folder-HotDot"
	"<Toolbox>/_Xtns/_Darla/Hot Dot on _Folder"
	"Hot Dot on Folder\n\
Processes each file in a folder, to fix areas produced \
by hot, dead or stuck pixels. \n\
See http://www.FarcryDesign.com/GIMP for more information."
	"Darla McKay (Darla@FarcryDesign.com)"
	"Darla McKay"
	"2007,2008"
	""
	SF-STRING		_"Folder Path:" ""
	SF-STRING		_"Extension:" "jpg"
	SF-OPTION		_"Type of FileSave:" 	'("Append -HotDot"
									"Replace"
									"hotdot subfolder (must exist)")
	SF-ADJUSTMENT	_"Hot Dot X"			'(2298 1 4000 1 0 0 0)
	SF-ADJUSTMENT	_"Hot Dot Y"			'(516 1 3000 1 0 0 0)
	SF-ADJUSTMENT	_"Coverage Pixels X"	'(12 1 200 1 0 0 0)
	SF-ADJUSTMENT	_"Coverage Pixels Y"	'(9 1 200 1 0 0 0)
	SF-ADJUSTMENT	_"Offset X"			'(-5 -20 50 1 0 0 0)
	SF-ADJUSTMENT	_"Offset Y"			'(-6 -20 50 1 0 0 0)
	SF-ADJUSTMENT	_"Feather"		'(3 1 20 1 0 0 0)
)

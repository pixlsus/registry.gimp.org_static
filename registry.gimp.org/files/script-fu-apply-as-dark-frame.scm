; ==================================---------------------------------------------------------------------------------------------------------------------------
; script-fu-apply-as-dark-frame.scm
; ==================================
; Apply the currently open image to a batch of images as a dark frame,
; suitable for time lapse animations with non-RAW images where dark frame can't be applied
; on opening.
;
; Deducts dark frame from batch of images to remove bright hot pixels from long exposure digital camera data.
;
; Skips all files which do not have same file suffix as the files selected in the range and allows the conversion
; to TIF, JPG or PNG on output if desired.
;
; Known Issues & Limitations:
; 1)	No check done to ensure dark frame is correct one (ie same exposure time, aspect, resolution etc) 
;		for image batch.
; 2)	v1.0 limit of 99999 (+1) images to be written
;
; Workflow: 
; 1) 	Open dark frame image in Gimp
; 2)	Run script.  Specify: 
;			First & final image file to process (all images required to be in same directory)
;			Output directory (or select checkbox to use input directory for output)
;			Select to process all images in range (default) or process all images in directory, or just first file
;			Prefix for files to be output (eg Frame)
;			First in numeric sequence of file suffixes (eg 1 would result in Frame00001.jpg as first)
; 3)	New files will be created as a result of subtracting the dark frame image from each input file
;
; Version: 1.0
; GIMP Script-Fu by Geoff Steele.
;
; Revision history:
;  1.0 (2011-07-13): First version.
;
; -------------------------------------------------------------------------------------------------------------------------------------------------------------
;
(define (script-fu-apply-as-dark-frame
						img
						drawable
						inFileOne
						inFileN
						inActionType
						inOutDir
						inUseInDir
						inFilePre
						inFileCntr
						inFileFormat
)
						
	; local helper functions ----------------------------------------------------------------------------------------------------------------------------------
	
	(define (outputFname p n t)												; generate output filename from prefix (p), counter(n) and type(t) from input file
		(let*
			(
				(tempStr (string-append p "00000"))
				(fIndex (+ inFileCntr n))
			)
			(set! tempStr (substring tempStr 0 (- (string-length tempStr) (string-length (number->string fIndex)))))
			(string-append tempStr (number->string fIndex) t)
		)
	) ; outputFname
	
	(define (directoryNameOf f pd)											; returns directory path for given filename (f) using path delimiter (pd)
		(list->string (reverse (member (car (string->list pd)) (reverse (string->list f))))) 
	)
	
	(define (suffixOf f)	; returns the file suffix (inclusive of the '.') of file f. NB: presumes at least one '.' in filename (reasonable for this surely?)
		(if (boolean? (member #\. (string->list f)))						; recurses until no more '.' to the right
			(string-append "." f)											; prefix with a '.' & return
			(suffixOf (substring (list->string (member #\. (string->list f))) 1 (string-length (list->string (member #\. (string->list f))))))  ; or recurse stripping '.' off front
		)
	)
	
	; end local helper functions ------------------------------------------------------------------------------------------------------------------------------
	
	; script-fu-select-crop-pan main --------------------------------------------------------------------------------------------------------------------------
		
	(let*
		(
			(fileSuffix " ")													; defines the file type of the input files from the file suffix of inFileOne
			(theInputDir " ")													; the input directory
			(theOutputDir " ")													; the output directory
			(filelist (cons '() '()))											; stores list of all files in directory
			(fileCounter 0)														; incrementer for output filename
			(isErrorState FALSE)												; error flag
			(saveFileSuffix " ")												; image output file suffux
			(workingImage 0)													; temporary working image holder
			(pathDelimiter (if (equal? (substring gimp-dir 0 1) "/") "/" "\\"))	; delimiter used in this OS for path creation
		)	; end of variable declarations

		(gimp-context-push)

		(set! saveFileSuffix 													; chosen output file type
			(cond 
				((= inFileFormat 0 ) (suffixOf inFileOne))						; default to input file 1 type
				((= inFileFormat 1 ) ".jpg" )
				((= inFileFormat 2 ) ".tif" )
				((= inFileFormat 3 ) ".png" )
			)
		)

		; ensure inFileOne exists and is valid
		(if (file-exists? inFileOne)
			(begin 
				(set! theInputDir (directoryNameOf inFileOne pathDelimiter))	; define input directory from fully qualified first input file name
				(set! fileSuffix (suffixOf inFileOne))							; define the input file suffix from fully qualified first input file name
			)
			(begin																; inFileOne does not exist
				(set! isErrorState TRUE)										; not a recoverable situation
				(gimp-message "Error: First file in input sequence must be specified and valid.")
			)
		) ; endif file-exists

		;	Test for issues relating to last input file, if used
		(if (and (= isErrorState FALSE)	(= inActionType 0))		; for inActionType 1 or 2 a 2nd file name is not specified (and if specified, is ignored)
			(begin 
				(if(string-ci>? inFileOne inFileN)
					(begin
						(gimp-message "ERROR: File selected as last in sequence not after the first file in this directory.")
						(set! isErrorState TRUE)
					)
					(if (not(string=? fileSuffix (suffixOf inFileN)))
						(begin
							(gimp-message "ERROR: Two files specifying input range are not of same image type.")
							(set! isErrorState TRUE)
						)
					) ; endif comparing file suffixes
				) ; endif name comparison
			) 
		) ; isErrorState & inActionType test

		(if (and (= isErrorState FALSE)	(= inActionType 0))		; for inActionType 1 or 2 a 2nd file name is not specified (and if specified, is ignored)
			(if (file-exists? inFileN)											; does inFileN exist, 
				(if (not (string=? (directoryNameOf inFileN pathDelimiter) theInputDir))			; and is it in same directory as inFileOne?
					(begin
						(gimp-message (string-append "ERROR: First and Last files in sequence are not in same directory.  All input files must be in same directory. (1: "
							(directoryNameOf inFileN pathDelimiter) " & " theInputDir))
						(set! isErrorState TRUE)								; not a recoverable situation
					)
				) ; endif directory compare
				(begin															; file doesn't exist
					(gimp-message "ERROR: Last input file no longer exists or was not specified.")
					(set! isErrorState TRUE)									; huston...we have a problem
				)
			) ; endif file-exists test
		) ; endif isErrorState test

		(if (= isErrorState FALSE)												; if we're all systems go for launch...
			(begin
				; set the output directory (directory selector in UI does not append path delimeter so we need to add it here)
				(if (= inUseInDir 1) (set! theOutputDir theInputDir) (set! theOutputDir (string-append inOutDir pathDelimiter)))
				(if (not (= inActionType 2))									; if more than one file to be processed
					(begin
						; find all files in input directory with the same suffix as inFileOne
						(set! filelist (cadr (file-glob (string-append theInputDir "*" fileSuffix) 1)))		
						(if (= inActionType 0)									; if a range of files specified
							; remove files before first and after nth in list
							(set! filelist (member inFileOne (reverse (member inFileN (reverse filelist)))))
						) ; endif inActionType = 0	
					)
						; else, for when inActionType = 2
						(set! filelist (cons inFileOne '()))					; filelist has just one item
				) ; endif inActionType 2
		
				(gimp-selection-all img)										; select ALL of the currently open dark frame image
				(gimp-edit-copy-visible img)									; copies currently open dark frame image to the edit buffer

				(while (not (null? filelist))									; repeat until filelist is depleted
					(let* 
						(
							(filename (car filelist)) 							; get next filename
							(outputFilename (string-append theOutputDir (outputFname inFilePre fileCounter saveFileSuffix))) ; derived output filename	
							(currentImage (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))	; load this image file
							(currentDrawable (car (gimp-image-get-active-drawable currentImage)))		; active drawable
							(currentLayer 0)
						) ; end local declarations
						
						; do stuff - this is where the magic happens
						(set! currentLayer (car (gimp-edit-paste currentDrawable FALSE)))	; creates a floating selection of dark frame
						(gimp-floating-sel-to-layer currentLayer)				; lock floating selection down
						(gimp-layer-set-mode currentLayer SUBTRACT-MODE)		; change mode of dark frame layer
						(set! currentLayer (car (gimp-image-merge-down currentImage currentLayer CLIP-TO-IMAGE)))
						(gimp-image-set-filename currentImage outputFilename)	; set name of output file
						(set! currentDrawable (car (gimp-image-get-active-drawable currentImage)))		; update variable to new active drawable
						(gimp-file-save RUN-NONINTERACTIVE currentImage currentDrawable outputFilename outputFilename)	; save
						(gimp-image-delete currentImage)						; close this image
						
						(set! fileCounter (+ fileCounter 1))					; file count increments
						(set! filelist (cdr filelist))							; pop first item off list, rinse & repeat
					)	; let* filename, outputFilename, currentImage scopes end
				)		; end while	
				
				(gimp-message (string-append "Apply Dark Frame complete.  " (number->string fileCounter) " files processed."))

				(gimp-context-pop)
			)
		) ; endif for main run loop
	)   ; end of main
) ; end of script-fu-select-crop-pan --------------------------------------------------------------------------------------------------------------------------

(script-fu-register
	"script-fu-apply-as-dark-frame"												;func name
	"Apply open image as dark frame on other images"							;menu label
	"Use the current image to remove hot pixels from a range of images, all images in a directory, or one selected image."	; description
	"Geoff Steele"																; author
	"copyright 2011, Geoff Steele;"												; copyright notice
	"v1.0 July 12, 2011"														; date created
	"*"																			; image type that the script works on (all), enforces script only shown when image open
	SF-IMAGE    	"IF YOU SEE THIS TEXT CANCEL SCRIPT NOW " 0					; active image object - value passed without user control showing if there's an active image
	SF-DRAWABLE 	"!!!!  NO OPEN IMAGE FOUND IN GIMP  !!! " 0					; active image drawable object - value passed without user control
	SF-FILENAME		"First source file in range"	"Select"					; input file 1 - first input file in the range to be processed
	SF-FILENAME		"Last source file in range"		"Select"					; input file n - final input file in the range to be processed
	SF-OPTION		"Action"	'("Process files from First to Last" 
						"Process all files of same file type in directory of First Source File" 
						"Process only First Source File (1 file)")				; action to perform
	SF-DIRNAME		"Output directory"	"Select"								; output location
	SF-TOGGLE		"Use input directory for output"	0						; use directory where input files are located for output directory, default no
	SF-STRING		"Output file prefix" "Frame"								; output filename prefix
	SF-ADJUSTMENT	"Output file suffix start (00000-99999) " '(1 0 99999 1 10 0 1) 
																				; spinner for output filename initial count
	SF-OPTION		"Output file type"	'("Defined by First Source File type" 
						"Save as JPG" "Save as TIFF" "Save as PNG")				; save output format				
)
(script-fu-menu-register "script-fu-apply-as-dark-frame" "<Image>/Filters/Animation/Time Lapse")


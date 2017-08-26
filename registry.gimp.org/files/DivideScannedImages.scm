; DivideScannedImages.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.9 (20101007)

; Description
;
; Locates each separate element and creates a new image from each.
; will call the deskew plugin http://www.cubewano.org/gimp-deskew-plugin/
; if it is installed on each image
;
; Changes:
; v1.1 - Added a size threshold slider, and it will call the deskew plugin if installed
; v1.2 - takes a rectangular selection bounding the path rather than a selection from the path itself, added an abort threshold incase the parameters are wonky
; v1.3 - simplifies the selection via feather/sharpen first to speed up the image analysis.
;        - fixed exporting the whole image as one.
;        - added sliders to pick a background offset.  This is useful if your scanner has a "shadow" around the edge of full scans,
; v1.4 - added the ability to save out dividede images to a directory with an incremental filename and number
; v1.5 - added corner selection to background pick
; v1.6 - changed global buffer use to named buffers
; v1.7 - added batch mode for whole directories, plus bug fix for no deskew plugin and saving files, also a fix to get both cases of file
; v1.8 - should now work on both windows and linux using the pathchar def.
; v1.9 - added sort code to the batch script

; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (script_fu_DivideScannedImages img inLayer inThreshold inSize inLimit inCorner inX inY inSaveFiles inDir inSaveType inFileName inFileNumber)
  (let*
    (
      (width (car (gimp-image-width img)))
      (height (car (gimp-image-height img)))
      (newpath 0)
      (strokes 0)
      (tempVector 0)
      (tempImage 0)
      (tempLayer 0)
      (bounds 0)
      (count 0)
      (numextracted 0)
      (saveString "")
      (newFileName "")
      (tempdisplay 0)
      (buffname "dsibuff")
      (pathchar (if (equal? (substring gimp-dir 0 1) "/") "/" "\\"))
    )
    ;  it begins here
    (gimp-context-push)
    (gimp-image-undo-disable img)
    
    ;logging
    ;(gimp-message-set-handler ERROR-CONSOLE)
    ;(gimp-message-set-handler CONSOLE)
    ;(gimp-message-set-handler MESSAGE-BOX)
    ;or start GIMP wwith "gimp --console-messages" to spawn a console box
    ;then use this:
    ;(gimp-message "foobar") 

    ;testing for functions defined
    ;(if (defined? 'plug-in-shift) (gimp-message "It Exists") (gimp-message "Doesnt Exist"))

    ;set up saving
    (if (= inSaveFiles TRUE)
      (set! saveString
      (cond 
        (( equal? inSaveType 0 ) ".jpg" )
        (( equal? inSaveType 1 ) ".bmp" )
        (( equal? inSaveType 2 ) ".png" )
      )
    ))
    
    ; convert in inverted copy of the background selection to a path using the selected corner
    (cond 
      ( (equal? inCorner 0)
        (gimp-fuzzy-select inLayer inX inY inThreshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE) 
        (gimp-context-set-background (car (gimp-image-pick-color img inLayer inX inY FALSE FALSE 0)))
      )
      ( (equal? inCorner 1)
        (gimp-fuzzy-select inLayer (- width inX) inY inThreshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE) 
        (gimp-context-set-background (car (gimp-image-pick-color img inLayer (- width inX) inY FALSE FALSE 0)))
      )
      ( (equal? inCorner 2)
        (gimp-fuzzy-select inLayer inX (- height inY) inThreshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE) 
        (gimp-context-set-background (car (gimp-image-pick-color img inLayer inX (- height inY) FALSE FALSE 0)))
      )
      ( (equal? inCorner 3)
        (gimp-fuzzy-select inLayer (- width inX) (- height inY) inThreshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE) 
        (gimp-context-set-background (car (gimp-image-pick-color img inLayer (- width inX) (- height inY) FALSE FALSE 0)))
      )
    )
    (gimp-selection-feather img (/ (min width height) 100))
    (gimp-selection-sharpen img)
    (gimp-selection-invert img)
    (plug-in-sel2path RUN-NONINTERACTIVE img inLayer)
    
    ;break up the vectors
    (set! newpath (vector-ref (cadr (gimp-image-get-vectors img)) 0)) 
   
    (set! strokes (gimp-vectors-get-strokes newpath))
    (while (and (< count (car strokes)) (< numextracted inLimit))
    
      (set! tempVector (gimp-vectors-new img "Temp"))
      (gimp-image-add-vectors img (car tempVector) -1)
      (gimp-vectors-stroke-new-from-points (car tempVector)
        (list-ref (gimp-vectors-stroke-get-points newpath (vector-ref (cadr strokes) count)) 0)
        (list-ref (gimp-vectors-stroke-get-points newpath (vector-ref (cadr strokes) count)) 1)
        (list-ref (gimp-vectors-stroke-get-points newpath (vector-ref (cadr strokes) count)) 2)
        (list-ref (gimp-vectors-stroke-get-points newpath (vector-ref (cadr strokes) count)) 3)
      )
      (gimp-vectors-to-selection (car tempVector) CHANNEL-OP-REPLACE TRUE FALSE 0 0)
      
      ;check for minimum size
      (set! bounds (gimp-selection-bounds img))
      (if (and (> (- (list-ref bounds 3) (list-ref bounds 1)) inSize) (> (- (list-ref bounds 4) (list-ref bounds 2)) inSize) ;min size slider
               (< (- (list-ref bounds 3) (list-ref bounds 1)) width) (< (- (list-ref bounds 4) (list-ref bounds 2)) height)) ;max size image
        (begin
          (gimp-rect-select img (list-ref bounds 1) (list-ref bounds 2)
                                (- (list-ref bounds 3) (list-ref bounds 1)) (- (list-ref bounds 4) (list-ref bounds 2))
                                CHANNEL-OP-REPLACE FALSE 0 )
          (set! buffname (car (gimp-edit-named-copy inLayer buffname)))
          (set! tempImage (car (gimp-edit-named-paste-as-new buffname)))
          (set! tempLayer (car (gimp-image-get-active-layer tempImage)))
          (gimp-image-undo-disable tempImage)
          (set! tempdisplay (car (gimp-display-new tempImage)))
      
          ;run deskew if it is installed
          (if (defined? 'gimp-deskew-plugin) 
            (begin
              (gimp-progress-set-text "Deskewing...")
              (gimp-layer-flatten tempLayer)
              (gimp-deskew-plugin 0 tempImage tempLayer 0 0 0 0 0)
              (gimp-image-resize-to-layers tempImage)
              (gimp-layer-flatten tempLayer)
              (gimp-fuzzy-select tempLayer 0 0 inThreshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE) 
              (gimp-selection-invert tempImage)
              (set! bounds (gimp-selection-bounds tempImage))
              (gimp-selection-none tempImage)
              (gimp-image-crop tempImage (- (list-ref bounds 3) (list-ref bounds 1)) (- (list-ref bounds 4) (list-ref bounds 2)) 
                               (list-ref bounds 1) (list-ref bounds 2))
            )
          )
          (gimp-image-undo-enable tempImage)
          
          ;save file
          (if (= inSaveFiles TRUE)
          (begin
            (set! newFileName (string-append inDir pathchar inFileName 
                                       (substring "00000" (string-length (number->string (+ inFileNumber numextracted)))) 
                                       (number->string (+ inFileNumber numextracted)) saveString))
            (gimp-file-save RUN-NONINTERACTIVE tempImage tempLayer newFileName newFileName)
            (gimp-display-delete tempdisplay)
          )
          )
          
          (set! numextracted (+ numextracted 1))
        )
      )     
      (gimp-image-remove-vectors img (car tempVector))
      (set! count (+ count 1))
    )

    ;input drawable name should be set to 1919191919 if in batch
    (if (and (> numextracted 0) (equal? (car (gimp-drawable-get-name inLayer)) "1919191919"))
      (gimp-drawable-set-name inLayer (number->string (+ 1919191919 numextracted))))

    ;delete temp path
    (gimp-image-remove-vectors img newpath)
    (gimp-selection-none img)
    
    ;done
    (gimp-image-undo-enable img)
    (gimp-progress-end)
    (gimp-displays-flush)
    (gimp-context-pop)
  )
)

(script-fu-register "script_fu_DivideScannedImages"
                    "<Image>/Filters/Divide Scanned Images..."
                    "Attempts to isolate each part of the image from the background and creates a new image from it"
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "Dec 2008"
                    "RGB* GRAY*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-ADJUSTMENT "Selection Threshold"                 (list 10 0 255 1 10 1 SF-SLIDER)
                    SF-ADJUSTMENT "Size Threshold"                      (list 100 0 2000 10 100 1 SF-SLIDER)        
                    SF-ADJUSTMENT "Abort Limit"                         (list 5 1 100 1 10 1 SF-SLIDER)                         
                    SF-OPTION     "Background Sample Corner"            (list "Top Left" "Top Right" "Bottom Left" "Bottom Right")
                    SF-ADJUSTMENT "Background Sample X Offset"          (list 5 1 100 1 10 1 SF-SLIDER)                         
                    SF-ADJUSTMENT "Background Sample Y Offset"          (list 5 1 100 1 10 1 SF-SLIDER)
                    SF-TOGGLE     "Save and Close Extracted Images"     FALSE       
                    SF-DIRNAME    "Save Directory"                      ""
                    SF-OPTION     "Save File Type"                      (list "jpg" "bmp" "png")
                    SF-STRING     "Save File Base Name"                 "IMAGE"
                    SF-ADJUSTMENT "Save File Start Number"              (list 0 0 9000 1 100 0 SF-SPINNER)                  
)

(define (script_fu_BatchDivideScannedImages inSourceDir inLoadType inThreshold inSize inLimit inCorner inX inY inDestDir inSaveType inFileName inFileNumber)
(let*
    (
      (varLoadStr "")
      (varFileList 0)
      (varCounter inFileNumber)
      (pathchar (if (equal? (substring gimp-dir 0 1) "/") "/" "\\"))
    )
    
    (define split
      (lambda (ls)
        (letrec ((split-h (lambda (ls ls1 ls2)
                            (cond
                              ((or (null? ls) (null? (cdr ls)))
                               (cons (reverse ls2) ls1))
                              (else (split-h (cddr ls)
                                      (cdr ls1) (cons (car ls1) ls2)))))))
          (split-h ls ls '()))))
          
    (define merge
      (lambda (pred ls1 ls2)
        (cond
          ((null? ls1) ls2)
          ((null? ls2) ls1)
          ((pred (car ls1) (car ls2))
           (cons (car ls1) (merge pred (cdr ls1) ls2)))
          (else (cons (car ls2) (merge pred ls1 (cdr ls2)))))))

    ;pred is the comparison, i.e. <= for an ascending numeric list, or 
    ;string<=? for a case sensitive alphabetical sort, 
    ;string-ci<=? for a case insensitive alphabetical sort, 
    (define merge-sort
      (lambda (pred ls)
        (cond
          ((null? ls) ls)
          ((null? (cdr ls)) ls)
          (else (let ((splits (split ls)))
                  (merge pred
                    (merge-sort pred (car splits))
                    (merge-sort pred (cdr splits))))))))

    ;begin here
    (set! varLoadStr
    (cond 
    (( equal? inLoadType 0 ) ".[jJ][pP][gG]" )
    (( equal? inLoadType 1 ) ".[bB][mM][pP]" )
    (( equal? inLoadType 2 ) ".[pP][nN][gG]" )
    ))  

    (set! varFileList (merge-sort string<=? (cadr (file-glob (string-append inSourceDir pathchar "*" varLoadStr)  1))))
    (while (not (null? varFileList))
      (let* ((filename (car varFileList))
             (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
             (drawable (car (gimp-image-get-active-layer image))))

        ;flag for batch mode
        (gimp-drawable-set-name drawable "1919191919")
        (gimp-progress-set-text (string-append "Working on ->" filename))
      
        (script_fu_DivideScannedImages image drawable inThreshold inSize inLimit inCorner inX inY TRUE inDestDir inSaveType inFileName varCounter)
 
        ;increment by number extracted.
        (set! varCounter (+ varCounter (- (string->number (car (gimp-drawable-get-name drawable))) 1919191919)))
        (gimp-image-delete image)
      )
      (set! varFileList (cdr varFileList))
    )
  )
)

(script-fu-register "script_fu_BatchDivideScannedImages"
                    "<Toolbox>/Xtns/Batch Tools/Batch Divide Scanned Images..."
                    "Batch devide a folder of full page scans images."
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "May 2009"
                    ""
                    SF-DIRNAME    "Load from" ""
                    SF-OPTION     "Load File Type" (list "jpg" "bmp" "png") 
                    SF-ADJUSTMENT "Selection Threshold"                 (list 10 0 255 1 10 1 SF-SLIDER)
                    SF-ADJUSTMENT "Size Threshold"                      (list 100 0 2000 10 100 1 SF-SLIDER)        
                    SF-ADJUSTMENT "Abort Limit"                         (list 5 1 100 1 10 1 SF-SLIDER)                         
                    SF-OPTION     "Background Sample Corner"            (list "Top Left" "Top Right" "Bottom Left" "Bottom Right")
                    SF-ADJUSTMENT "Background Sample X Offset"          (list 5 1 100 1 10 1 SF-SLIDER)                         
                    SF-ADJUSTMENT "Background Sample Y Offset"          (list 5 1 100 1 10 1 SF-SLIDER)
                    SF-DIRNAME    "Save Directory"                      ""
                    SF-OPTION     "Save File Type"                      (list "jpg" "bmp" "png")
                    SF-STRING     "Save File Base Name"                 "IMAGE"
                    SF-ADJUSTMENT "Save File Start Number"              (list 0 0 9000 1 100 0 SF-SPINNER)       
)
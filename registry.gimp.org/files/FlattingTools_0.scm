; flattingtools.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.7 (20130628)

; Changes:
; V1.3 - Change the search logic to speed it up in some cases.
;      - Converted to channel opes as they are faster
; V1.4 - Added option to randomly fill from the palette, or gradient fill as well.
; Description
; will fill each separate area with a different coulour, either randomly, from a selected palette, or with a  directional  gradient.
; v1.5 - got rid of unnecessary listeql? function (thanks saulgoode)
;      - added directory pattern fill option
; v1.6 - made it not destructive to the clipboard
; v1.7 - added endless loop trap for image with very little bg colour.


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

(define (script_fu_Flatting img inLayer)

(let*
    (
    (width (car (gimp-image-width img)))
    (height (car (gimp-image-height img)))
)
    ;  it begins here
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    ;logging
    ;(gimp-message-set-handler ERROR-CONSOLE)
    ;(gimp-message-set-handler CONSOLE)
    ;(gimp-message-set-handler MESSAGE-BOX)
    ;or start GIMP wwith "gimp --console-messages" to spawn a console box
    ;then use this:
    ;(gimp-message "foobar") 

    ;testing for functions defined
    ;(if (defined? 'plug-in-shift) (gimp-message "It Exists") (gimp-message "Doesnt Exist"))

    (gimp-by-color-select inLayer (list 0 0 0) 0 CHANNEL-OP-REPLACE FALSE TRUE 0.5 FALSE)
    
    (while (= (car (gimp-selection-is-empty img)) FALSE)
    (plug-in-dilate RUN-NONINTERACTIVE img inLayer 1 HISTOGRAM-VALUE 1.0 7 0 255)
    (gimp-by-color-select inLayer (list 0 0 0) 0 CHANNEL-OP-REPLACE FALSE TRUE 0.5 FALSE)
    )
    
    ;done
    (gimp-image-undo-group-end img)
    (gimp-progress-end)
    (gimp-displays-flush)
    (gimp-context-pop)
)
)

(script-fu-register "script_fu_Flatting"
                    "<Image>/Filters/Flatting/Flatten"
                    "Flattens an image that has been multicoloured"
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "Jan 2009"
                    "RGB* GRAY*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0                        
)


(define (script_fu_MultiFill img inLayer inColour inThresh inMethod inPalette inDirection inDir inIgnoreSize inFlagSize inFlagColour inHowCheck inMakeCopy inFlatten inHide)

; TinyScheme lacks pi.
(define pi (acos -1))

; TinyScheme lacks a floating-point random number generator.
(define (urandom) (/ (rand 32768) 32768))

(let*
    (
    (img (car (gimp-image-duplicate img)))
    (inLayer (car (gimp-image-get-active-layer img)))
    (width (car (gimp-image-width img)))
    (height (car (gimp-image-height img)))
    (bounds 0)
    (countX 0)
    (selectionValue 0)
    (numcolours (car (gimp-palette-get-colors inPalette)))
    (palettecount 0)
    (nextcolour 0)
    (layercopy 0)
    (temp 0)
    (channel 0)
    (lastY 0)
    (patternImg 0)
    (patternLayers 0)
    (oldclipImg (car (gimp-edit-paste-as-new)))
    (pxcount 0)
    )
    ;  it begins here
    (gimp-context-push)

    ; if inMethod is Directory of patterns, load all these images as layers of a new image
    (when (= inMethod 6)
      (set! patternImg (car (gimp-image-new 1 1 RGB)))
      (gimp-context-set-pattern (list-ref (cadr (gimp-patterns-get-list "")) 0)) ; set patten to clipboard (first in list)
      (gimp-image-undo-group-start patternImg)
      (gimp-image-undo-disable patternImg)
      (let* ((varFileList ()))
        ; get all the files
        (set! varFileList (append varFileList (cadr (file-glob (string-append inDir "\\*.[pP][nN][gG]") 1))))
        (set! varFileList (append varFileList (cadr (file-glob (string-append inDir "\\*.[jJ][pP][gG]") 1))))
        (set! varFileList (append varFileList (cadr (file-glob (string-append inDir "\\*.[jJ][pP][eE][gG]") 1))))
        (set! varFileList (append varFileList (cadr (file-glob (string-append inDir "\\*.[bB][mM][pP]") 1))))
        (set! varFileList (append varFileList (cadr (file-glob (string-append inDir "\\*.[pP][aA][tT]") 1))))
    
         (while (not (null? varFileList))
          (let* 
            (
            (filename (car varFileList))
            (layer (car (gimp-file-load-layer RUN-NONINTERACTIVE patternImg filename)))
            )
            (gimp-image-add-layer patternImg layer -1)
            (gimp-progress-set-text (string-append "Loading " filename))
          )
          (set! varFileList (cdr varFileList))
        )
      )
      (set! patternLayers (gimp-image-get-layers patternImg))
      (gimp-progress-set-text (string-append (number->string (car patternLayers)) "Patterns Loaded."))
    )
    
    (gimp-image-undo-group-start img)
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
    
    (gimp-context-set-gradient (list-ref (cadr (gimp-gradients-get-list "")) 2)) ; FG to BG, RGB mapping
    
    (if (= inHide FALSE) (gimp-display-new img))
    (if (= inMakeCopy TRUE)
        (begin
        (set! layercopy (car (gimp-layer-copy inLayer (car (gimp-drawable-has-alpha inLayer)))))
        (gimp-image-add-layer img layercopy -1)
        (gimp-layer-set-mode layercopy MULTIPLY-MODE)
        (gimp-image-set-active-layer img inLayer)
        )
    )
    
    ;  Change to pure B & W
    (gimp-by-color-select inLayer inColour inThresh CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    (gimp-context-set-foreground (list 255 255 255))
    (gimp-edit-fill inLayer FOREGROUND-FILL)
    ;Save it to a channel
    (set! channel (car (gimp-selection-save img)))

    (gimp-selection-invert img)
    (gimp-context-set-foreground (list 0 0 0))
    (gimp-edit-fill inLayer FOREGROUND-FILL)

    (gimp-context-set-background (list 254 254 254))
    
    ;select white
    (gimp-by-color-select inLayer (list 255 255 255) 0 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
    
    (while (= (car (gimp-selection-is-empty img)) FALSE)
        (set! bounds (gimp-selection-bounds img))
        ;get start the loop looking for this colour)
        
        (if (<> lastY (caddr bounds)) ; there is a change in Y, get the new boundary
        (begin
        (set! countX (cadr bounds)) ; x1
          (set! lastY (caddr bounds))
        )
        )

        (set! lastY (caddr bounds))
        
        (set! selectionValue (car (gimp-selection-value img countX (caddr bounds))))
    
        ;find a bg area
        (while (< selectionValue 255) ; while not white
            (set! countX (+ countX 1))
            (set! selectionValue (car (gimp-selection-value img countX (caddr bounds))))
        )
    
        ;select and fill
        (gimp-fuzzy-select inLayer countX (caddr bounds) 0 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)
        (set! bounds (gimp-selection-bounds img))
        
        (if (or (and (= inHowCheck 0) (<= (- (cadddr bounds) (cadr bounds)) inIgnoreSize) (<= (- (list-ref bounds 4) (caddr bounds)) inIgnoreSize)) ; X and Y size
                (and (= inHowCheck 1) (<= (cadddr (gimp-histogram inLayer HISTOGRAM-VALUE 0 255)) inIgnoreSize))) ; area
            (gimp-context-set-foreground (list 0 0 0)) ; fill with black - will be ignored in flatten step
            (if (or (and (= inHowCheck 0) (> (- (cadddr bounds) (cadr bounds)) inFlagSize) (> (- (list-ref bounds 4) (caddr bounds)) inFlagSize)) ; X and Y size
                (and (= inHowCheck 1) (> (cadddr (gimp-histogram inLayer HISTOGRAM-VALUE 0 255)) inFlagSize))) ; area
                (begin
                
                    (cond ((= inMethod 0) 
                        (set! nextcolour (list (+ (rand 254) 1) (+ (rand 254) 1) (+ (rand 254) 1))) ;Random colour - not B, W or flagcoulor
                        (while (equal? nextcolour inFlagColour)
                            (set! nextcolour (list (+ (rand 254) 1) (+ (rand 254) 1) (+ (rand 254) 1)))
                        )
                      )
                      ((= inMethod 1) 
                        (set! nextcolour (+ (rand 254) 1)) ;Random colour - not B, W or flagcoulor
                        (set! nextcolour (list nextcolour nextcolour nextcolour))
                        (while (equal? nextcolour inFlagColour)
                                (set! nextcolour (+ (rand 254) 1)) ;Random colour - not B, W or flagcoulor
                                (set! nextcolour (list nextcolour nextcolour nextcolour))                        )
                      )
                      ((= inMethod 2)
                        (set! nextcolour (car (gimp-palette-entry-get-color inPalette palettecount))) ; next palette colour, - not B, W or flagcoulor
                        (while (or (= (+ (car nextcolour) (cadr nextcolour) (caddr nextcolour)) 765)
                               (= (+ (car nextcolour) (cadr nextcolour) (caddr nextcolour)) 0)
                               (equal? nextcolour inFlagColour))
                            (set! palettecount (modulo (+ palettecount 1) numcolours)) ; increment palette
                            (set! nextcolour (car (gimp-palette-entry-get-color inPalette palettecount)))
                        )
                        (set! palettecount (modulo (+ palettecount 1) numcolours)) ; increment palette
                      )
                      ((= inMethod 3)
                        (set! nextcolour (car (gimp-palette-entry-get-color inPalette (rand numcolours)))) ; random palette colour, - not B, W or flagcoulor
                        (while (or (< (length nextcolour) 3) ; bug
                               (equal? nextcolour (list 0 0 0))
                               (equal? nextcolour (list 255 255 255))
                               (equal? nextcolour inFlagColour)) 
                            (set! nextcolour (car (gimp-palette-entry-get-color inPalette (rand numcolours)))) ; random palette colour, - not B, W or flagcoulor
                        )
                      )
                      ((or (= inMethod 4)(= inMethod 5)(= inMethod 6)) ;dummy colour for gradioent and pattern
                        (set! nextcolour (list 1 1 1))
                      )
                    )
                    (gimp-context-set-foreground nextcolour)
                )
                (gimp-context-set-foreground inFlagColour)
            )
        )

        (when (< inMethod 4) ;colour fill
          (gimp-edit-fill inLayer FOREGROUND-FILL)
        )
        
        (when (or (= inMethod 4) (= inMethod 5)) ;gradient
          (let*
            ((bounds (cdr (gimp-selection-bounds img)))
             (xc (/ (+ (caddr bounds) (car bounds)) 2))
             (yc (/ (+ (cadddr bounds) (cadr bounds)) 2))
             (radius (sqrt (+ (expt (- (caddr bounds) (car bounds)) 2) (expt (- (cadddr bounds) (cadr bounds)) 2)  )))
             (direction (* 2 pi (if (= inMethod 4) (urandom) (/ inDirection 360.0))))
             (x1 (round (+ xc (* radius (cos direction)))))
             (y1 (round (- yc (* radius (sin direction)))))
             (x2 (- xc (- x1 xc)))
             (y2 (- yc (- y1 yc))))
                
            (gimp-edit-blend inLayer FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE TRUE 3 3 TRUE x1 y1 x2 y2)
          )
        )
        
        (when (= inMethod 6) ;pattern fill with a random pattern
          (gimp-edit-copy (aref (cadr patternLayers) (rand (car patternLayers))))
          (gimp-edit-fill inLayer PATTERN-FILL)
        )
        
        (if (= inHide FALSE) (gimp-displays-flush))
        
        (gimp-channel-combine-masks channel (car (gimp-image-get-selection img)) CHANNEL-OP-SUBTRACT 0 0)
        (gimp-selection-load channel)
    )

    (gimp-selection-none img)
    
    (if (= inFlatten TRUE)
        (begin
        (gimp-by-color-select inLayer (list 0 0 0) 0 CHANNEL-OP-REPLACE FALSE TRUE 0.5 FALSE)
		(set! pxcount (cadddr (gimp-histogram inLayer HISTOGRAM-VALUE 0 255)))   
		(set! temp (+ pxcount 1))   
        (while (and (= (car (gimp-selection-is-empty img)) FALSE) (<> pxcount temp))
			(set! temp pxcount)   
            (plug-in-dilate RUN-NONINTERACTIVE img inLayer 1 HISTOGRAM-VALUE 1.0 7 0 255)
            (gimp-by-color-select inLayer (list 0 0 0) 0 CHANNEL-OP-REPLACE FALSE TRUE 0.5 FALSE)
    		(set! pxcount (cadddr (gimp-histogram inLayer HISTOGRAM-VALUE 0 255)))
    	)
        )
    )

	(when (= (car (gimp-selection-is-empty img)) FALSE)
		(gimp-selection-none img)
		(gimp-message "Endless loop detected - flatting failed.  Please uncheck 'Flatten After' and try again.") 
    )
    
    (gimp-image-remove-channel img channel)
    
    (if (= inHide TRUE) (gimp-display-new img))

    ;delete pattern image
    (when (= inMethod 6)
      (gimp-image-delete patternImg)
    )

    ;reload old clipboard if necessary
    (when (> oldclipImg -1)
      (gimp-edit-copy (car (gimp-image-get-active-layer oldclipImg)))
      (gimp-image-delete oldclipImg)
    )
    
    ;done
    (gimp-image-undo-enable img)
    (gimp-image-undo-group-end img)
    (gimp-progress-end)
    (gimp-displays-flush)
    (gimp-context-pop)
)
)

(script-fu-register "script_fu_MultiFill"
                    "<Image>/Filters/Flatting/MultiFill..."
                    "Fills every separate area with a different colour from the palette."
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "Dec 2008"
                    "RGB* GRAY*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-COLOR      "MultiFill Colour"       "white"
                    SF-ADJUSTMENT "Colour Threshold"       (list 16 0 255 1 10 0 SF-SLIDER)
                    SF-OPTION     "Fill Type"              (list "Random Colours" "Random Greyscale" "Palette" "Palette Random" "Gradient with Random Direction" "Gradient with Specified Direction" "Pattern Directory")
                    SF-PALETTE    "Choose Fill Palette"    "Default"
                    SF-ADJUSTMENT "Gradient Direction"     (list 135 0 360 1 15 1 SF-SLIDER)
                    SF-DIRNAME    "Pattern Directory"      (string-append gimp-directory "/patterns/")
                    SF-ADJUSTMENT "Ignore Areas <="        (list 3 0 255 1 10 1 SF-SLIDER)
                    SF-ADJUSTMENT "Flag Areas <="          (list 10 0 255 1 10 1 SF-SLIDER)
                    SF-COLOR      "Flag Colour"            "magenta"
                    SF-OPTION     "Area Size Check Method" (list "Width & Height" "Area")                    
                    SF-TOGGLE     "Copy Layer First"       TRUE
                    SF-TOGGLE     "Flatten After"           TRUE
                    SF-TOGGLE     "Hide While Processing"  TRUE
)
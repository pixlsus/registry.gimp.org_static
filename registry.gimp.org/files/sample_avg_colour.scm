; sample_avg_colour.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.3 (20100625)
;
; Changes:
; 1.1 - added respect for selection, and layer alpha.
; 1.2 - code clean-up
; 1.3 - fixes "bug" created by bug fix for gimp 2.6.9 in gimp-histogram

; Description
;
; gets the average colour and allows setting of the foreground, the background, 
; or adding to the active palette the average colour
;

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

(define (sample_avg_colour img inLayer inMerged InMode)
  (let*
    (
	  (img (car (gimp-image-duplicate img)))
      (inLayer (car (gimp-image-get-active-layer img)))
      (savedsel 0)
      (handler (car (gimp-message-get-handler)))
      (palette (car (gimp-context-get-palette)))
      (colour 0)
    )
    ;  it begins here
	
	  (unless (= inLayer -1) 
      (gimp-message-set-handler MESSAGE-BOX)

      (if (equal? (car (gimp-selection-is-empty img)) TRUE) (gimp-selection-all img))
      (set! savedsel (car (gimp-selection-save img)))

      (if (equal? (car (gimp-image-base-type img)) INDEXED)
        (gimp-image-convert-rgb img))

      (if (equal? inMerged TRUE)
        (set! inLayer (car (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY))))

      (gimp-selection-layer-alpha inLayer)
      (gimp-channel-combine-masks (car (gimp-image-get-selection img)) savedsel CHANNEL-OP-INTERSECT 0 0)

      (set! colour 
        (if (equal? (car (gimp-image-base-type img)) RGB)
          (list (round (car (gimp-histogram inLayer HISTOGRAM-RED 0 255))) 
                (round (car (gimp-histogram inLayer HISTOGRAM-GREEN 0 255))) 
                (round (car (gimp-histogram inLayer HISTOGRAM-BLUE 0 255))))
          (let ((greyval (round (car (gimp-histogram inLayer HISTOGRAM-VALUE 0 255)))))
            (list greyval greyval greyval))))

      (cond 
        ((and (equal? InMode 0) (equal? (car (gimp-image-base-type img)) RGB))
          (gimp-message (string-append "Average Colour"
                                       " R:" (number->string (car colour))
                                       " G:" (number->string (cadr colour))
                                       " B:" (number->string (caddr colour)))))

        ((and (equal? InMode 0) (equal? (car (gimp-image-base-type img)) GRAY))
          (gimp-message (string-append "Average Value: " (number->string (caddr colour)))))

        ((equal? InMode 1) 
          (gimp-context-set-foreground colour))

        ((equal? InMode 2)
          (gimp-context-set-background colour))

        ((and (equal? InMode 3) (equal? (car (gimp-palette-is-editable palette)) TRUE))
          (gimp-palette-add-entry palette "Untitled" colour)))
	
      (gimp-message-set-handler handler)
    )
		  
    ;done
	(gimp-image-delete img)
  )
)

(script-fu-register "sample_avg_colour"
            		"Sample Average Colour..."
                    "Samples the average colour"
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "June 2009"
                    "*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-TOGGLE     "Sample Merged" TRUE
                    SF-OPTION     "Sample Mode" '("Info Only" "Set Foreground Colour" "Set Background Colour" "Add to Palette")
)

(script-fu-menu-register "sample_avg_colour"
                         "<Image>/Colors")
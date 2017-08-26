; listguides.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.0 (20090826)

; Description
;
; list all the guides in an image
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

(define (list-guides img inLayer)
  (let*
    (
    (guideID (car (gimp-image-find-next-guide img 0)))
    (listH '())
    (listV '())
    (text "")
    )

;selection sort code from http://cs.gmu.edu/~white/CS363/Scheme/SchemeSamples.html
(define (selection L) 
   (cond ( (null? L) '() )
         ( else (cons (smallest L (car L))     ; put the smallest element
                                               ; at the front of the 
                                               ; current list 
                      (selection (remove L (smallest L (car L)))))
                                               ; call selection on the list
                                               ; minus the smallest
                                               ; element
         )
   )
)

(define (remove L A)                ; remove the first occurance of atom A from L
  (cond ( (null? L) '() )           
        ( (= (car L) A) (cdr L))    ; Match found! 
        (else (cons (car L)(remove (cdr L) A)))   ; keep searching
  )
)

(define (smallest L A)             ; looks for the smallest element in the list
                                   ; atom A is the current smallest
  (cond ( (null? L) A)
        ( (< (car L) A) (smallest (cdr L)(car L)))
        (else (smallest (cdr L) A ))
  )
)
;end of selection sort defines.

    (while (not (equal? guideID 0))
      (if (equal? (car (gimp-image-get-guide-orientation img guideID)) ORIENTATION-HORIZONTAL)
        (set! listH (append listH (list (car (gimp-image-get-guide-position img guideID)))))
        (set! listV (append listV (list (car (gimp-image-get-guide-position img guideID))))))
      (set! guideID (car (gimp-image-find-next-guide img guideID))))

    (when (> (length listH) 0)
      (set! listH (selection listH))
      (set! text (string-append text "Horizontal Guides:\n"))
      (for-each
        (lambda (a) (set! text (string-append text (number->string a) "\n")))
        listH))
    (when (> (length listV) 0)
      (set! listV (selection listV))
      (set! text (string-append text "Vertical Guides:\n"))
      (for-each
        (lambda (a) (set! text (string-append text (number->string a) "\n")))
        listV))

    (if (equal? text "")
      (gimp-message "No Guides.")
      (gimp-message text))
  )
)

(script-fu-register "list-guides"
        		        "<Image>/Image/Guides/_List Guides"
                    "List all guides in an image"
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "Aug 2009"
                    "*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0)
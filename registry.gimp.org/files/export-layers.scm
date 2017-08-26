; Copyright (C) 2011 by Stuart P. Bentley

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.

(define (script-fu-export-layers img drw path outnameformat)
; credit to Vijay Mathew on Stack Overflow for the expand keywords function
  (let ((expand-keywords (lambda(format tokens)
    (let loop ((slist (string->list format))
               (in-replace-mode #f)
               (result ""))
      (if (not (null? slist))
          (let ((c (car slist)))
            (cond (in-replace-mode
                   (let ((token (car (cdr (assoc c tokens)))))
                     (loop (cdr slist) #f (string-append result token))))
                  ((char=? c #\~)
                   (loop (cdr slist) #t result))
                  (else
                   (loop (cdr slist) #f (
                      string-append result (make-string 1 c))))))
          result)))))
  (for-each (lambda (layer)
    (let* (
        (name (expand-keywords outnameformat `(
          (#\~ "~")
          (#\i ,(car(gimp-image-get-name img)))
          (#\l ,(car(gimp-drawable-get-name layer))))))
        (outpath (string-append path "/" name)))
      (gimp-file-save RUN-NONINTERACTIVE img layer outpath name)
  )) (vector->list(cadr (gimp-image-get-layers img)))))
)

(script-fu-register
  "script-fu-export-layers"
  "L_ayers"
  "Export all layers as individual files."
  "Stuart P. Bentley <stuart@testtrack4.com>"
  "(C) 2011 by Stuart P. Bentley"
  "June 28, 2011"
  "*"
  SF-IMAGE       "Image"         0
  SF-DRAWABLE    "Drawable"         0
  SF-DIRNAME     "Output directory"  ""
  SF-STRING      "Filename format\
  (~i = image name\
  ~l = layer name\
  ~~ = ~)"
                                                          "~i-~l.png"
)
(script-fu-menu-register "script-fu-export-layers" "<Image>/File/E_xport")

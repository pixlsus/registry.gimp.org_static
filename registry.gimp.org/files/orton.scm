;;; Copyright (c) 2010 Cardinal Peak
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.


(define (script-fu-orton inImage inLayer inSharpFilename)

  (define (orton inImage inBlurLayer inSharpLayer)
    (plug-in-gauss 1 inImage inBlurLayer 40 40 0)
    (plug-in-unsharp-mask 1 inImage inSharpLayer 5 1 0)
    (gimp-layer-set-mode inBlurLayer 3)
    (gimp-layer-set-mode inSharpLayer 3)
    (gimp-image-flatten inImage))

  (let* ((sharpLayer (car (gimp-file-load-layer 1 inImage inSharpFilename))))
    (gimp-image-add-layer inImage sharpLayer -1)
    (orton inImage inLayer sharpLayer))

  (gimp-displays-flush inImage))

(script-fu-register 
 "script-fu-orton"
 "Orton"
 "Ortonize an image.\
  The loaded image should be exposed at +1 EV.\
  You will also need the same image exposed at +2 EV."
 "Ben Mesander"
 "(C) Cardinal Peak, 2010"
 "Thursday"
 ""
 SF-IMAGE "The Image" 0
 SF-DRAWABLE "The Layer" 0
 SF-FILENAME _"Image at +2 EV" ""
)

(script-fu-menu-register "script-fu-orton" "<Image>/Filters/Artistic")
;;; Copyright (c) 2011 Cardinal Peak
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Cross-processing script for the gimp
;
; Description of cross processing:
; http://en.wikipedia.org/wiki/Cross_processing
;
; This script automates the tutorial found at:
; http://www.jesusda.com/blog/index.php?id=375


(define (script-fu-xpro inImage inLayer)

  ; start an undoable group of operations

  (gimp-image-undo-group-start inImage)

  ; remap curves for red/green/blue components

  (gimp-curves-spline inLayer HISTOGRAM-RED 10 #(0 0  88 47  170 188  221 249  255 255))
  (gimp-curves-spline inLayer HISTOGRAM-GREEN 8 #(0 0  65 57  184 208  255 255))
  (gimp-curves-spline inLayer HISTOGRAM-BLUE 4 #(0 29  255 226))

  ; duplicate image layer, mark it as 50% opacity, and overlay it

  (let* ((newLayer (car (gimp-layer-copy inLayer FALSE))))
    (gimp-image-add-layer inImage newLayer -1)
    (gimp-layer-set-opacity newLayer 50)
    (gimp-layer-set-mode newLayer OVERLAY-MODE))

  ; create a new greenish-yellow layer at 10% opacity and overlay it

  (let* ((newLayer (car (gimp-layer-new inImage (car (gimp-image-width inImage))
					(car (gimp-image-height inImage)) RGB-IMAGE 
					"GreenishYellow" 10 OVERLAY-MODE))))
    (gimp-context-set-foreground '(0 255 186))
    (gimp-drawable-fill newLayer FOREGROUND-FILL)
    (gimp-image-add-layer inImage newLayer -1))

  ; display cross processed image, and end the undoable group of operations

  (gimp-displays-flush)
  (gimp-image-undo-group-end inImage))

(script-fu-register
 "script-fu-xpro"
 "Xpro"
 "Cross-process an image."
 "Ben Mesander"
 "(C) Cardinal Peak, 2011"
 "Thursday"
 ""
 SF-IMAGE "The Image" 0
 SF-DRAWABLE "The Layer" 0
)

(script-fu-menu-register "script-fu-xpro" "<Image>/Filters/Xpro")


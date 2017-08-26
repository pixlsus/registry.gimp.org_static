; Copyright (c) 2008 Thomas Baruchel
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
; THE SOFTWARE.


(define (script-fu-bw-labs0 img drawable)
  (gimp-image-undo-group-start img) ; Start an undo group.  
  (let* (
         (new-img (car (plug-in-decompose 1 img drawable "HSV" 1)))
         (new-layers (gimp-image-get-layers new-img))
         (new-img2 (car (plug-in-decompose 1 img drawable "YCbCr_ITU_R470" 1)))
         (new-layers2 (gimp-image-get-layers new-img2))

         (mylayer (car (gimp-layer-new-from-drawable
           (aref (cadr (gimp-image-get-layers new-img)) 2) new-img2)))

         (mymask (car (gimp-layer-create-mask mylayer 0)))
        )

        (gimp-drawable-set-visible (aref (cadr new-layers2) 1) 0)
        (gimp-drawable-set-visible (aref (cadr new-layers2) 2) 0)
        (gimp-image-set-active-layer new-img2 (aref (cadr new-layers2) 0))
        (gimp-image-add-layer new-img2 mylayer -1)
        (gimp-image-set-active-layer new-img (aref (cadr new-layers) 1))
        (gimp-selection-none new-img)
        (gimp-edit-copy (aref (cadr new-layers) 1))

        (gimp-layer-add-mask mylayer mymask)
        (gimp-floating-sel-anchor (car (gimp-edit-paste mymask 1)))

        (let ((result (car (gimp-layer-new-from-drawable
                  (car (gimp-image-merge-visible-layers new-img2 2)) img))))
          (gimp-image-add-layer img result -1)
          (gimp-drawable-set-name result "BW Labs - HSV / HSV")
        )
        (gimp-image-delete new-img)
        (gimp-image-delete new-img2)

      (gimp-displays-flush)
      (gimp-image-undo-group-end img)
   )
)

(script-fu-register "script-fu-bw-labs0"
                    "Black & White HSV / HSV"
                    "Black & White HSV / HSV"
                    "Thomas Baruchel"
                    "Thomas Baruchel"
                    "2008"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer to convert" 0)
(script-fu-menu-register "script-fu-bw-labs0"
                         "<Image>/Script-Fu/Photography/BW Labs")







(define (script-fu-bw-labs1 img drawable)
  (gimp-image-undo-group-start img) ; Start an undo group.  
  (let* (
         (new-img (car (plug-in-decompose 1 img drawable "HSV" 1)))
         (new-layers (gimp-image-get-layers new-img))
         (new-img2 (car (plug-in-decompose 1 img drawable "YCbCr_ITU_R470" 1)))
         (new-layers2 (gimp-image-get-layers new-img2))
         (new-img3 (car (plug-in-decompose 1 img drawable "LAB" 1)))

         (mylayer (car (gimp-layer-new-from-drawable
           (aref (cadr (gimp-image-get-layers new-img3)) 0) new-img2)))

         (mymask (car (gimp-layer-create-mask mylayer 0)))
        )

        (gimp-drawable-set-visible (aref (cadr new-layers2) 1) 0)
        (gimp-drawable-set-visible (aref (cadr new-layers2) 2) 0)
        (gimp-image-set-active-layer new-img2 (aref (cadr new-layers2) 0))
        (gimp-image-add-layer new-img2 mylayer -1)
        (gimp-image-delete new-img3)
        (gimp-image-set-active-layer new-img (aref (cadr new-layers) 1))
        (gimp-selection-none new-img)
        (gimp-edit-copy (aref (cadr new-layers) 1))

        (gimp-layer-add-mask mylayer mymask)
        (gimp-floating-sel-anchor (car (gimp-edit-paste mymask 1)))

        (let ((result (car (gimp-layer-new-from-drawable
                  (car (gimp-image-merge-visible-layers new-img2 2)) img))))
          (gimp-image-add-layer img result -1)
          (gimp-drawable-set-name result "BW Labs - HSV / LAB")
        )
        (gimp-image-delete new-img)
        (gimp-image-delete new-img2)
      (gimp-displays-flush)
      (gimp-image-undo-group-end img)
   )
)

(script-fu-register "script-fu-bw-labs1"
                    "Black & White HSV / LAB"
                    "Black & White HSV / LAB"
                    "Thomas Baruchel"
                    "Thomas Baruchel"
                    "2008"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer to convert" 0)
(script-fu-menu-register "script-fu-bw-labs1"
                         "<Image>/Script-Fu/Photography/BW Labs")




(define (script-fu-bw-labs2 img drawable)
  (gimp-image-undo-group-start img) ; Start an undo group.  
  (let* (
         (new-img (car (plug-in-decompose 1 img drawable "HSV" 1)))
         (new-layers (gimp-image-get-layers new-img))
         (new-img2 (car (plug-in-decompose 1 img drawable "YCbCr_ITU_R470" 1)))
         (new-layers2 (gimp-image-get-layers new-img2))
         (new-img3 (car (plug-in-decompose 1 img drawable "HSL" 1)))

         (mylayer (car (gimp-layer-new-from-drawable
           (aref (cadr (gimp-image-get-layers new-img3)) 2) new-img2)))

         (mymask (car (gimp-layer-create-mask mylayer 0)))
        )

        (gimp-drawable-set-visible (aref (cadr new-layers2) 1) 0)
        (gimp-drawable-set-visible (aref (cadr new-layers2) 2) 0)
        (gimp-image-set-active-layer new-img2 (aref (cadr new-layers2) 0))
        (gimp-image-add-layer new-img2 mylayer -1)
        (gimp-image-delete new-img3)
        (gimp-image-set-active-layer new-img (aref (cadr new-layers) 1))
        (gimp-selection-none new-img)
        (gimp-edit-copy (aref (cadr new-layers) 1))

        (gimp-layer-add-mask mylayer mymask)
        (gimp-floating-sel-anchor (car (gimp-edit-paste mymask 1)))

        (let ((result (car (gimp-layer-new-from-drawable
                  (car (gimp-image-merge-visible-layers new-img2 2)) img))))
          (gimp-image-add-layer img result -1)
          (gimp-drawable-set-name result "BW Labs - HSV / HSL")
        )
        (gimp-image-delete new-img)
        (gimp-image-delete new-img2)
      (gimp-displays-flush)
      (gimp-image-undo-group-end img)
   )
)

(script-fu-register "script-fu-bw-labs2"
                    "Black & White HSV / HSL"
                    "Black & White HSV / HSL"
                    "Thomas Baruchel"
                    "Thomas Baruchel"
                    "2008"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer to convert" 0)
(script-fu-menu-register "script-fu-bw-labs2"
                         "<Image>/Script-Fu/Photography/BW Labs")


























(define (script-fu-bw-labs3 img drawable)
  (gimp-image-undo-group-start img) ; Start an undo group.  
  (let* (
         (new-img (car (plug-in-decompose 1 img drawable "HSL" 1)))
         (new-layers (gimp-image-get-layers new-img))
         (new-img2 (car (plug-in-decompose 1 img drawable "YCbCr_ITU_R470" 1)))
         (new-layers2 (gimp-image-get-layers new-img2))

         (mylayer (car (gimp-layer-new-from-drawable
           (aref (cadr (gimp-image-get-layers new-img)) 2) new-img2)))

         (mymask (car (gimp-layer-create-mask mylayer 0)))
        )

        (gimp-drawable-set-visible (aref (cadr new-layers2) 1) 0)
        (gimp-drawable-set-visible (aref (cadr new-layers2) 2) 0)
        (gimp-image-set-active-layer new-img2 (aref (cadr new-layers2) 0))
        (gimp-image-add-layer new-img2 mylayer -1)
        (gimp-image-set-active-layer new-img (aref (cadr new-layers) 1))
        (gimp-selection-none new-img)
        (gimp-edit-copy (aref (cadr new-layers) 1))

        (gimp-layer-add-mask mylayer mymask)
        (gimp-floating-sel-anchor (car (gimp-edit-paste mymask 1)))

        (let ((result (car (gimp-layer-new-from-drawable
                  (car (gimp-image-merge-visible-layers new-img2 2)) img))))
          (gimp-image-add-layer img result -1)
          (gimp-drawable-set-name result "BW Labs - HSL / HSL")
        )
        (gimp-image-delete new-img)
        (gimp-image-delete new-img2)

      (gimp-displays-flush)
      (gimp-image-undo-group-end img)
   )
)

(script-fu-register "script-fu-bw-labs3"
                    "Black & White HSL / HSL"
                    "Black & White HSL / HSL"
                    "Thomas Baruchel"
                    "Thomas Baruchel"
                    "2008"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer to convert" 0)
(script-fu-menu-register "script-fu-bw-labs3"
                         "<Image>/Script-Fu/Photography/BW Labs")







(define (script-fu-bw-labs4 img drawable)
  (gimp-image-undo-group-start img) ; Start an undo group.  
  (let* (
         (new-img (car (plug-in-decompose 1 img drawable "HSL" 1)))
         (new-layers (gimp-image-get-layers new-img))
         (new-img2 (car (plug-in-decompose 1 img drawable "YCbCr_ITU_R470" 1)))
         (new-layers2 (gimp-image-get-layers new-img2))
         (new-img3 (car (plug-in-decompose 1 img drawable "LAB" 1)))

         (mylayer (car (gimp-layer-new-from-drawable
           (aref (cadr (gimp-image-get-layers new-img3)) 0) new-img2)))

         (mymask (car (gimp-layer-create-mask mylayer 0)))
        )

        (gimp-drawable-set-visible (aref (cadr new-layers2) 1) 0)
        (gimp-drawable-set-visible (aref (cadr new-layers2) 2) 0)
        (gimp-image-set-active-layer new-img2 (aref (cadr new-layers2) 0))
        (gimp-image-add-layer new-img2 mylayer -1)
        (gimp-image-delete new-img3)
        (gimp-image-set-active-layer new-img (aref (cadr new-layers) 1))
        (gimp-selection-none new-img)
        (gimp-edit-copy (aref (cadr new-layers) 1))

        (gimp-layer-add-mask mylayer mymask)
        (gimp-floating-sel-anchor (car (gimp-edit-paste mymask 1)))

        (let ((result (car (gimp-layer-new-from-drawable
                  (car (gimp-image-merge-visible-layers new-img2 2)) img))))
          (gimp-image-add-layer img result -1)
          (gimp-drawable-set-name result "BW Labs - HSL / LAB")
        )
        (gimp-image-delete new-img)
        (gimp-image-delete new-img2)
      (gimp-displays-flush)
      (gimp-image-undo-group-end img)
   )
)

(script-fu-register "script-fu-bw-labs4"
                    "Black & White HSL / LAB"
                    "Black & White HSL / LAB"
                    "Thomas Baruchel"
                    "Thomas Baruchel"
                    "2008"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer to convert" 0)
(script-fu-menu-register "script-fu-bw-labs4"
                         "<Image>/Script-Fu/Photography/BW Labs")




(define (script-fu-bw-labs5 img drawable)
  (gimp-image-undo-group-start img) ; Start an undo group.  
  (let* (
         (new-img (car (plug-in-decompose 1 img drawable "HSL" 1)))
         (new-layers (gimp-image-get-layers new-img))
         (new-img2 (car (plug-in-decompose 1 img drawable "YCbCr_ITU_R470" 1)))
         (new-layers2 (gimp-image-get-layers new-img2))
         (new-img3 (car (plug-in-decompose 1 img drawable "HSV" 1)))

         (mylayer (car (gimp-layer-new-from-drawable
           (aref (cadr (gimp-image-get-layers new-img3)) 2) new-img2)))

         (mymask (car (gimp-layer-create-mask mylayer 0)))
        )

        (gimp-drawable-set-visible (aref (cadr new-layers2) 1) 0)
        (gimp-drawable-set-visible (aref (cadr new-layers2) 2) 0)
        (gimp-image-set-active-layer new-img2 (aref (cadr new-layers2) 0))
        (gimp-image-add-layer new-img2 mylayer -1)
        (gimp-image-delete new-img3)
        (gimp-image-set-active-layer new-img (aref (cadr new-layers) 1))
        (gimp-selection-none new-img)
        (gimp-edit-copy (aref (cadr new-layers) 1))

        (gimp-layer-add-mask mylayer mymask)
        (gimp-floating-sel-anchor (car (gimp-edit-paste mymask 1)))

        (let ((result (car (gimp-layer-new-from-drawable
                  (car (gimp-image-merge-visible-layers new-img2 2)) img))))
          (gimp-image-add-layer img result -1)
          (gimp-drawable-set-name result "BW Labs - HSL / HSV")
        )
        (gimp-image-delete new-img)
        (gimp-image-delete new-img2)
      (gimp-displays-flush)
      (gimp-image-undo-group-end img)
   )
)

(script-fu-register "script-fu-bw-labs5"
                    "Black & White HSL / HSV"
                    "Black & White HSL / HSV"
                    "Thomas Baruchel"
                    "Thomas Baruchel"
                    "2008"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer to convert" 0)
(script-fu-menu-register "script-fu-bw-labs5"
                         "<Image>/Script-Fu/Photography/BW Labs")

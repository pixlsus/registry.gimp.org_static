; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

(define (script-fu-float-each-linked-with-retention image drawable)
  (define (image-get-linked-layers image)
    (let* (
        (all-layers (gimp-image-get-layers image))
        (i (car all-layers))
        (linked 0)
        (tmp FALSE)
        )
      (set! all-layers (cadr all-layers))
      (while (> i 0)
        (set! tmp (car (gimp-drawable-get-linked (aref all-layers (- i 1)))))
        (if (= tmp TRUE)
          (set! linked (append linked (list (aref all-layers (- i 1)))))
          )
        (set! i (- i 1))
        )
      (reverse linked)
      )
    )
  (let* (
      (layers (image-get-linked-layers image))
      (select 0)
      (layer 0)
      (floated 0)
      )
    (gimp-image-undo-group-start image)
    (set! select (car (gimp-selection-save image)))
    (while (pair? layers)
      (gimp-selection-load select)
      (if (= (car (gimp-drawable-mask-bounds (car layers))) 1)
        (begin
          (set! floated (car (gimp-selection-float (car layers) 0 0)))
          (define layer (car (gimp-layer-new-from-drawable floated image)))
	  (define layer2 (car (gimp-layer-new-from-drawable floated image)))
	  (gimp-image-remove-layer image floated)
	  (gimp-image-add-layer image layer2 -1)
          (gimp-image-add-layer image layer -1)
          (gimp-drawable-set-linked (car layers) FALSE)
          (gimp-drawable-set-linked layer TRUE)
          (gimp-layer-set-mode layer (car (gimp-layer-get-mode (car layers))))
	  (gimp-layer-set-mode layer2 (car (gimp-layer-get-mode (car layers))))
	  (gimp-image-merge-down image layer2 EXPAND-AS-NECESSARY)
          )
        )
      (set! layers (cdr layers))
      )
    (gimp-selection-load select)
    (gimp-image-remove-channel image select)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
    )
  )
  
(define (script-fu-float-each-linked image drawable)
  (define (image-get-linked-layers image)
    (let* (
        (all-layers (gimp-image-get-layers image))
        (i (car all-layers))
        (linked 0)
        (tmp FALSE)
        )
      (set! all-layers (cadr all-layers))
      (while (> i 0)
        (set! tmp (car (gimp-drawable-get-linked (aref all-layers (- i 1)))))
        (if (= tmp TRUE)
          (set! linked (append linked (list (aref all-layers (- i 1)))))
          )
        (set! i (- i 1))
        )
      (reverse linked)
      )
    )
  (let* (
      (layers (image-get-linked-layers image))
      (select 0)
      (layer 0)
      (floated 0)
      )
    (gimp-image-undo-group-start image)
    (set! select (car (gimp-selection-save image)))
    (while (pair? layers)
      (gimp-selection-load select)
      (if (= (car (gimp-drawable-mask-bounds (car layers))) 1)
        (begin
          (set! floated (car (gimp-selection-float (car layers) 0 0)))
          (define layer (car (gimp-layer-new-from-drawable floated image)))
	  (gimp-image-remove-layer image floated)
          (gimp-image-add-layer image layer -1)
          (gimp-drawable-set-linked (car layers) FALSE)
          (gimp-drawable-set-linked layer TRUE)
          (gimp-layer-set-mode layer (car (gimp-layer-get-mode (car layers))))
          )
        )
      (set! layers (cdr layers))
      )
    (gimp-selection-load select)
    (gimp-image-remove-channel image select)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
    )
  )
  
(define (script-fu-merge-each-linked image drawable)
  (define (image-get-layers image)
    (let* (
        (all-layers (gimp-image-get-layers image))
        (i (car all-layers))
        (layers 0)
        )
      (set! all-layers (cadr all-layers))
      (while (> i 0)
        (set! layers (append layers (list (aref all-layers (- i 1)))))
        (set! i (- i 1))
        )
      (reverse layers)
      )
    )
  (define (image-get-linked-layers image)
    (let* (
        (all-layers (gimp-image-get-layers image))
        (i (car all-layers))
        (linked 0)
        (tmp FALSE)
        )
      (set! all-layers (cadr all-layers))
      (while (> i 0)
        (set! tmp (car (gimp-drawable-get-linked (aref all-layers (- i 1)))))
        (if (= tmp TRUE)
          (set! linked (append linked (list (aref all-layers (- i 1)))))
          )
        (set! i (- i 1))
        )
      (reverse linked)
      )
    )
  (let* (
      (layers (image-get-layers image))
      (linked (image-get-linked-layers image))
      (layer 0)
      (mode 0)
      )
    (gimp-image-undo-group-start image)
    (while (pair? linked)
      (set! layer (member (car linked) layers))
      (if (pair? (cdr layer))
        (begin
          (set! layer (cadr layer))
          (set! mode (car (gimp-layer-get-mode layer)))
          (gimp-drawable-set-visible (car linked) TRUE)
          (gimp-drawable-set-visible layer TRUE)
          (set! layer (car (gimp-image-merge-down image (car linked) EXPAND-AS-NECESSARY)))
          (gimp-drawable-set-linked layer TRUE)
          (gimp-layer-set-mode layer mode)
          )
        )
      (set! linked (cdr linked))
      )
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
    )
  )
  
(script-fu-register "script-fu-float-each-linked"
 "<Image>/Script-Fu/Multi-Layer-Cut-Copy-Paste/Float Linked Layers (Cut)"
 "Float the selection on each linked layer"
 "Saul Goode"
 "Saul Goode"
 "12/16/2006"
 "*"
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 )
  
(script-fu-register "script-fu-float-each-linked-with-retention"
 "<Image>/Script-Fu/Multi-Layer-Cut-Copy-Paste/Float Linked Layers (Copy)"
 "Float the selection on each linked layer - retaining a copy in the original. Derived from Saul Goode's Float Linked Layers from 12/16/2006"
 "Michael B. McLaughlin"
 "Michael B. McLaughlin"
 "04/03/2010"
 "*"
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 )
  
(script-fu-register "script-fu-merge-each-linked"
 "<Image>/Script-Fu/Multi-Layer-Cut-Copy-Paste/Merge Down Linked Layers (Paste)"
 "Merge each linked layer with the one beneath it"
 "Saul Goode"
 "Saul Goode"
 "12/16/2006"
 "*"
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 )
  

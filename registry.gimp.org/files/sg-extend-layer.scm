; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; Scales the layer to match the image size.
; If a selection is present, only the area outside the bounds
; of the selection are scaled.

(define (sg-layer-extend-to-image-size image drawable)
  (define (sf-get-visible-layers image)
    (let loop ( (visible's '())
                (layer's (vector->list (cadr (gimp-image-get-layers image)))))
              (if (null? layer's)
                (if (null? visible's) '() (reverse visible's))
                (loop 
                  (if (= (car (gimp-drawable-get-visible (car layer's))) 1)
                    (cons (car layer's) visible's)
                    visible's)
                  (cdr layer's)))))
  (define (scale-to-rect layer fx1 fy1 fx2 fy2 tx1 ty1 tx2 ty2)
    (let* ((floated 0)
            (image (car (gimp-drawable-get-image layer))))
      (gimp-rect-select image fx1 fy1 (- fx2 fx1) (- fy2 fy1) CHANNEL-OP-REPLACE FALSE 0)
      (if (= (car (gimp-selection-is-empty image)) FALSE)
        (begin
          (set! floated (car (gimp-selection-float layer 0 0)))
          (gimp-layer-scale floated (- tx2 tx1) (- ty2 ty1) TRUE)
          (gimp-layer-set-offsets floated tx1 ty1 )
          (gimp-floating-sel-to-layer floated)
          (set! layer (car (gimp-image-merge-down image floated CLIP-TO-IMAGE))))
        layer)))
  (let* (
    (layer (car (gimp-image-get-active-layer image)))
    (floated 0)
    (bounds (cdr (gimp-drawable-mask-bounds layer)))
    (width (car (gimp-image-width image)))
    (height (car (gimp-image-height image)))
    (lx1 (car (gimp-drawable-offsets layer)))
    (ly1 (cadr (gimp-drawable-offsets layer)))
    (lx2 (+ lx1 (car (gimp-drawable-width layer))))
    (ly2 (+ ly1 (car (gimp-drawable-height layer))))
    (sx1 (+ lx1 (max 1 (car bounds))))
    (sy1 (+ ly1 (max 1 (cadr bounds))))
    (sx2 (+ lx1 (min (- (car (gimp-drawable-width layer)) 1) (caddr bounds))))
    (sy2 (+ ly1 (min (- (car (gimp-drawable-height layer)) 1) (cadddr bounds))))
    (orig-sel 0)
    (visible's '())
    )
    
    (gimp-image-undo-group-start image)
    (gimp-drawable-set-visible layer FALSE)
    (set! visible's (sf-get-visible-layers image))
    (map (lambda (x) (gimp-drawable-set-visible x FALSE)) visible's) 
    (gimp-drawable-set-visible layer TRUE)
    (if (= (car (gimp-drawable-mask-bounds layer)) FALSE)
      (set! layer (scale-to-rect layer (max lx1 0) (max ly1 0) (min lx2 width) (min ly2 height) 0 0 width height))
      (if (= (car (gimp-drawable-mask-intersect layer)) FALSE)
        (begin
          (gimp-layer-scale layer width height TRUE)
          (gimp-layer-set-offsets layer 0 0)
          )
        (begin
          (set! orig-sel (car (gimp-selection-save image)))
          (set! layer (scale-to-rect layer lx1 ly1 sx1 sy1    0   0   sx1   sy1    ))
          (set! layer (scale-to-rect layer sx1 ly1 sx2 sy1    sx1 0   sx2   sy1    ))
          (set! layer (scale-to-rect layer sx2 ly1 lx2 sy1    sx2 0   width sy1    ))
          (set! layer (scale-to-rect layer sx2 sy1 lx2 sy2    sx2 sy1 width sy2    ))
          (set! layer (scale-to-rect layer sx2 sy2 lx2 ly2    sx2 sy2 width height ))
          (set! layer (scale-to-rect layer sx1 sy2 sx2 ly2    sx1 sy2 sx2   height ))
          (set! layer (scale-to-rect layer lx1 sy2 sx1 ly2    0   sy2 sx1   height ))
          (set! layer (scale-to-rect layer lx1 sy1 sx1 sy2    0   sy1 sx1   sy2    ))
          (gimp-selection-load orig-sel)
          (map (lambda (x) (gimp-drawable-set-visible x TRUE)) visible's) 
          (gimp-image-remove-channel image orig-sel)
          )
        )
      )
    (gimp-image-set-active-layer image layer)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
    )
  )

(script-fu-register "sg-layer-extend-to-image-size"
 "Extend to Image Size"
 "Scale the unselected region of the layer to the image size (does not scale selected region)"
 "Saul Goode"
 "Saul Goode"
 "12/7/2009"
 ""
 SF-IMAGE    "Image"    0
 SF-DRAWABLE "Drawable" 0
 )
(script-fu-menu-register "sg-layer-extend-to-image-size"
 "<Image>/Layer/Resize"
 )
 

;;; pixel-dot-grid.scm
;;; Date: <2012-06-17 04:35 roan.horning@gmail.com>
;;; Author: Roan Horning <roan.horning@gmail.com>
;;; Version 1.1.1

;;; Version 1.1 for Gimp 2.2 and later

;;; This implements a method to create a pixelized dot mask effect 
;;; described by caligari87 in the /r/gimp subreddit:
;;; http://www.reddit.com/r/GIMP/comments/uggfq/
;;; help_how_to_dot_raster_an_image/.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (diamond-get-vector pixel-size)
  (let* ((x0 (/ (- pixel-size 1) 2))
         (y0 1)
         (x1 (- pixel-size 1))
         (y1 x0)
         (x2 x0)
         (y2 x1)
         (x3 1)
         (y3 y1))
    (vector  x0 y0 x0 y0 x0 y0 
             x1 y1 x1 y1 x1 y1 
             x2 y2 x2 y2 x2 y2 
             x3 y3 x3 y3 x3 y3)))

(define (diamond-select img pixel-size)
  (let* ((diamond-vector (car (gimp-vectors-new img "diamond")))
         )
    (gimp-vectors-stroke-new-from-points diamond-vector ; vectors 
                                         0              ; Bezier type 
                                         24             ; 4*2*3 num-points 
                                         (diamond-get-vector pixel-size)
                                                        ; controlpoints 
                                         TRUE           ; closed
                                         )
    
    ; test for version 2.8 and above, and, if true, 
    ; call new, required, function to insert vector into image
    (if (not (string<=? (car (gimp-version)) "2.8"))
      (gimp-image-insert-vectors img diamond-vector 0 0))
    
    (gimp-vectors-to-selection diamond-vector ; vectors 
                               0              ; Bezier-curve operation 
                               FALSE          ; antialias 
                               FALSE          ; feather 
                               0              ; feather-radius-x 
                               0              ; feather-radius-y
                               )))

(define (script-fu-pixel-dot-grid img drw pixel-dot-size dot-shape)
  (let* ((drawable-width (car (gimp-drawable-width drw)))
         (drawable-height (car (gimp-drawable-height drw)))
         (drawable-type (car (gimp-drawable-type drw)))
         (old-selection 0)
         (dot-shape-circle 0)
         ;;; layer for the pixel dot mask effect
         (layer-pixel-dot (car (gimp-layer-new img 
                                               pixel-dot-size 
                                               pixel-dot-size 
                                               0 
                                               "Dot mask" 
                                               100 
                                               0)))
         ;;; dot size within the mask is two pixels less in diameter
         ;;; than the width of the mask pixel.
         (dot-size (- pixel-dot-size 2))
         )
    (gimp-image-add-layer img layer-pixel-dot -1)
    (if (> dot-shape dot-shape-circle)
      (diamond-select img pixel-dot-size)  ; looks best when pixel-dot-size
                                           ; is odd  
      (gimp-round-rect-select img
                              1
                              1
                              dot-size
                              dot-size
                              dot-size
                              dot-size
                              0
                              FALSE
                              FALSE
                              0
                              0))
    (gimp-edit-fill layer-pixel-dot 2)
    (gimp-selection-none img)
    (plug-in-tile 1 img layer-pixel-dot drawable-width drawable-height FALSE)
    (let* ((mask-pixel-dot (car (gimp-layer-create-mask layer-pixel-dot 5))))
           (gimp-layer-add-mask layer-pixel-dot mask-pixel-dot)
           (gimp-invert mask-pixel-dot)
           )
    (plug-in-pixelize 1 img drw pixel-dot-size)
    (gimp-displays-flush)))

(script-fu-register
  "script-fu-pixel-dot-grid"
  "_Pixel Dot Mask"
  "Create a pixelized dot mask effect. Looks best when the pixel dot size is a multiple of the image width and height."
  "Roan Horning <roan.horning@gmail.com>"
  "Roan Horning"
  "20. 06. 2012"
  "RGB RGBA GRAY GRAYA"
  SF-IMAGE "Image" 0
  SF-DRAWABLE "Drawable" 0
  SF-ADJUSTMENT "Pixel dot size" '(10 5 100 1 1 1 0)
  SF-OPTION "Dot shape" '("circle" "diamond")
  )

; new in 2.2, for older versions give the full menu path in the
; second parameter of the script-fu-register call.
(script-fu-menu-register "script-fu-pixel-dot-grid"
                         "<Image>/Script-Fu/Artistic")

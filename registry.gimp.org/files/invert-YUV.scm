;
; invert-YUV.scm
;
; Inverts the Luma channel (YCC colorspace) of a selected layer.
;   (similar to the HSV Value invert, but without the color distortion)
;
; Copyright (C) 2008 "Stratadrake", strata_ranger@hotmail.com
;
; Version 0.0   05-23-2008  Written in GIMP 2.4.4
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
  
(define (script-fu-invert-YUV image drawable)

  (if (= 0 (car (gimp-drawable-is-layer drawable)))
  ; then
    (gimp-message "No active layer to perform this function on.")

  ; else
  
  ; (Note: YCbCr_ITU_R709 is specified here, but YCbCr_ITU_R470 or other YCC variants
  ;        can also be used.  There is a tiny variation in color tones between them)
  (let* ((temp-image (car (plug-in-decompose 1 image drawable "YCbCr_ITU_R709" 1))))
    (gimp-image-undo-group-start image)

    ; Invert Luma layer
    (gimp-invert (vector-ref (cadr (gimp-image-get-layers temp-image)) 0))
    
    ; Put it back together
    (plug-in-recompose 1 temp-image drawable)

    ; Cleanup
    (gimp-image-delete temp-image)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
  ; endif
  ))
  
)

(script-fu-register "script-fu-invert-YUV"
		    _"_Luma Invert"
		    "Inverts an image's YCC brightness"
		    "'Stratadrake' (strata_ranger@hotmail.com)"
		    "May 2008"
		    ""
		    "RGB* "
		    SF-IMAGE       "Image"               0
		    SF-DRAWABLE    "Drawable"            0
)

(script-fu-menu-register "script-fu-invert-YUV"
			 _"<Image>/Colors/Components"
)

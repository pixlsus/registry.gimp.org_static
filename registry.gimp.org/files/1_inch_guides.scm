; script-fu-inch-guides
; Add Guides Vertically and Horizontally @ 1 inch intervals 
;
; GIMP - The GNU Image Manipulation Program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
;
;============================================================================================
;== GIMP Add Guides Vertically and Horizontally @ 1 inch intervals                         ==
;== Revision 1.0 Allows irregular image resolutions                                        ==
;== Script by Gregory M. Ross                                                              ==
;============================================================================================

(define (script-fu-inch-guides img drawable)
  
    (let* (
          (ImageW (car (gimp-image-width img)))                    ; Image width
	      (ImageH (car (gimp-image-height img)))                   ; Image height
          (ImageResW (car (gimp-image-get-resolution img)))        ; ppi resolution of image
          (ImageResH (car (cdr (gimp-image-get-resolution img))))  ; ppi resolution of image
          (Vguide 0)                                               ; initial Vguide at 0
          (Hguide 0)                                               ; initial Hguide at 0
    )
   
    (gimp-image-undo-group-start img)

    (while (<= Vguide ImageW)
           (gimp-image-add-vguide img Vguide)
           (set! Vguide (+ Vguide ImageResW))
    )    

    (while (<= Hguide ImageH)
           (gimp-image-add-hguide img Hguide)
           (set! Hguide (+ Hguide ImageResH))
    )    

    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))


(script-fu-register "script-fu-inch-guides"
		    "<Image>/View/Inch Guides"
		    "Add Guides at 1 inch intervals"
		    "Gregory M. Ross"
		    "Gregory M. Ross"
		    "September 2009"
		    ""
		    SF-IMAGE    "Image" 0
		    SF-DRAWABLE "Drawable" 0)
		    
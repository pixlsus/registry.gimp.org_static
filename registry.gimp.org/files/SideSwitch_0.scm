; script-fu-switch-sides
; Switch Image Halfs Horizontally For Cross View 3D 
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
;== GIMP Switch Image Halfs Horizontally For Cross View 3D                                 ==
;== Revision 1.0 Allows Horizontal Switching. Later versions will give options for vert.   ==
;== Revision 1.1 Allows the use of an alpha channel in the image.                          ==
;== Script by Gregory M. Ross   April 28, 2010                                             ==
;============================================================================================

(define (script-fu-switch-sides img baselayer)

    (let* (
          (ImageW (car (gimp-image-width img)))                                     ; Image width
	      (ImageH (car (gimp-image-height img)))                                    ; Image height
          (HalfW (* ImageW 0.5))
          (baseline (car (gimp-layer-new img ImageW ImageH 1 "Switch" 100 0)))
          (Switch (car (gimp-layer-new img ImageW ImageH 1 "Switch" 100 0)))        ; Layer for Switch
		  )
	    (gimp-image-undo-group-start img)
        (gimp-layer-add-alpha baselayer)
        (gimp-selection-all img)
		(gimp-edit-copy baselayer)
		(gimp-selection-none img)
        (gimp-image-add-layer img Switch -1)
		(gimp-floating-sel-anchor (car(gimp-edit-paste Switch 1)))
		(gimp-rect-select img 0 0 HalfW ImageH 0 0 0)
		(gimp-edit-cut Switch)
		(gimp-selection-invert img)
		(gimp-edit-cut baselayer)
		(gimp-selection-none img)
        (gimp-drawable-offset Switch 1 1 HalfW 0)
        (gimp-drawable-offset baselayer 1 1 HalfW 0)
;        (gimp-image-flatten	img)	
		(gimp-image-merge-down img Switch 0)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))

(script-fu-register "script-fu-switch-sides"
		    "<Image>/Filters/Distorts/Switch Sides"
		    "Switch Image Halfs Horizontally For Cross View 3D"
		    "Gregory M. Ross"
		    "Gregory M. Ross"
		    "April 2010"
		    ""
		    SF-IMAGE    "Image" 0
		    SF-DRAWABLE "Drawable" 0)
		    
	

; 60's text effect Rev. 2.2
; creates text with psychedelic color patterns
;
; GIMP - The GNU Image Manipulation Program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; 3d-outline creates outlined border of a text with patterns
; Copyright (C) 1998 Hrvoje Horvat
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

;===============================================================
;== 60's Text Script based on my tutorial of the same name    ==
;== Revision 2.0 Made compatable with Gimp 2.4                ==
;== Revision 2.1 Gave gradient selections                     ==
;== Revision 2.2 Gave gradient types and reversals            ==
;== Script by Gregory M. Ross                                 ==
;===============================================================

(define (script-fu-60's-text string font-name font-size bg-color gradient1 reverse1 style1 gradient2 reverse2 style2)

  (let* ((text-ext (gimp-text-get-extents-fontname string font-size 0 font-name)) ; Get extents of the bounding box for the specified text in pixels.
	 (wide (+ (car text-ext) 20))                                               ; wide = text width + 20 pixels
	 (high (+ (cadr text-ext) 20))                                              ; high = text height + 20 pixels
	 (img (car (gimp-image-new wide high 0)))                                   ; define new rgb image called "img"
	 (bg-layer (car (gimp-layer-new img wide high 0 "Background" 100 0)))       ; define new rgb layer named "Background" 100% Opacity Normal mode
       (gradient-layer (car (gimp-layer-new img wide high 1 "Gradient" 100 10)))  ; define new rgba layer named "Gradient"
       (dupe-layer (car (gimp-layer-new img wide high 1 "Gradient Map" 100 6)))   ; define new rgba layer named "Gradient Map"
       (text-layer (car (gimp-layer-new img wide high 1 "Text" 100 0)))           ; define new rgba layer named "Text"
       (shadow-layer (car (gimp-layer-new img wide high 1 "Shadow" 80 0)))        ; define new rgba layer named "Shadow" with 80% opacity
     	 )
 


(gimp-image-undo-disable img)                                                     ; disable undo since there doesn't need to be one

; Set colors and create layers

(gimp-context-set-default-colors)
(gimp-palette-set-background bg-color)
(gimp-image-add-layer img bg-layer 1)
(gimp-image-add-layer img shadow-layer -1)
(gimp-image-add-layer img text-layer -1)
(gimp-image-add-layer img gradient-layer -1)
(gimp-palette-set-foreground '(000 000 000))
(gimp-edit-clear bg-layer)
(gimp-edit-clear shadow-layer)
(gimp-edit-clear text-layer)
(gimp-edit-clear gradient-layer)
(gimp-floating-sel-anchor (car (gimp-text-fontname img text-layer 10 10 string 0 TRUE font-size PIXELS font-name)))

; Create gradient effect

(gimp-context-set-gradient gradient1)
(gimp-edit-blend gradient-layer 3 0 style1 100 0 0 reverse1 FALSE 0 0 TRUE 0 0 wide high)
(gimp-context-set-gradient gradient2)
(gimp-edit-blend gradient-layer 3 6 style2 100 0 0 reverse2 FALSE 0 0 TRUE 0 0 0 high)

; Create Gradient Map 

(gimp-context-set-gradient "Three bars sin")
(gimp-drawable-set-visible bg-layer FALSE)
(gimp-edit-copy-visible img) 
(gimp-image-add-layer img dupe-layer -1)
(gimp-edit-clear dupe-layer)
(gimp-image-raise-layer-to-top img dupe-layer)
(gimp-edit-paste dupe-layer img)
(gimp-floating-sel-anchor (car (gimp-image-get-floating-sel img)))
(plug-in-gradmap 1 img dupe-layer)

; Clean up jagged edges

(gimp-selection-layer-alpha text-layer)
(gimp-selection-shrink img 1)
(gimp-selection-invert img)
(gimp-edit-clear dupe-layer)
(gimp-edit-clear gradient-layer)
(gimp-edit-clear text-layer)
(gimp-selection-invert img)

; Create drop shadow

(gimp-bucket-fill shadow-layer 0 0 100 0 FALSE 0 0)
(gimp-selection-none img)
(plug-in-gauss 1 img shadow-layer 20 20 0)
(gimp-drawable-offset shadow-layer FALSE 1 8 8)

; Turn on background layers visibility

(gimp-drawable-set-visible bg-layer TRUE)

; Re-enable undo and display new image

(gimp-image-undo-enable img)
(gimp-display-new img)))

; Setup user interface

(script-fu-register "script-fu-60's-text"
		    _"60's Text"
                    "Give text a psychadelic 60's effect"
                    "Gregory M. Ross"
                    "Gregory M. Ross"
                    "March 23, 2009"
                    ""
                    SF-STRING     _"Text" "Gimp Rocks!"
                    SF-FONT       _"Font" "Cooper Black"
                    SF-ADJUSTMENT _"Font Size (pixels)" '(100 2 1000 1 10 0 1)
                    SF-COLOR      _"Background Color" '(255 255 255)
                    SF-GRADIENT   _"Gradient 1"   "Full saturation spectrum CCW"
                    SF-TOGGLE     _"Gradient reverse"   FALSE
                    SF-OPTION     _"Gradient Style"      '("Linear" "Bi-linear" "Radial" "Square"
                                        "Conical (sym)" "Conical (asym)"
                                        "Shaped (angular)" "Shaped (spherical)"
                                        "Shaped (dimpled)" "Spiral (cw)"
                                        "Spiral (ccw)")
                    SF-GRADIENT   _"Gradient 2"   "Three bars sin"
                    SF-TOGGLE     _"Gradient reverse"   FALSE
                    SF-OPTION     _"Gradient Style"      '("Linear" "Bi-linear" "Radial" "Square"
                                        "Conical (sym)" "Conical (asym)"
                                        "Shaped (angular)" "Shaped (spherical)"
                                        "Shaped (dimpled)" "Spiral (cw)"
                                        "Spiral (ccw)")
)
(script-fu-menu-register "script-fu-60's-text"
			 "<Toolbox>/Xtns/Logos")




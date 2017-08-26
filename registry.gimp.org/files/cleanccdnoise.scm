; CleanCCDNoise is a script for Gimp
;
; This script helps fix noise found in digital camera pictures
;
; The script is located in "<Image> / Script-Fu / Enhance / Clean CCD Noise"
;
; Last changed: 2009 July 06
;
; Copyright (C) 2009 Jonathan Denning <jon@gfxcoder.us>
;
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 1.0 (2009 June 16)
;    - Created!
;  Version 1.1 (2009 July 06)
;    - Tempered darkening noise found in browns (wood)
;
; --------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.  
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;
;
;

(define (script-fu-CleanCCDNoise inImage inLayer)
	(let*
		(
			(recolorLayer 0)
			(mask 0)
			(subtract (car (gimp-layer-copy inLayer FALSE)))
			(original (car (gimp-layer-copy inLayer FALSE)))
		)
		
		(gimp-image-undo-group-start inImage)
		
		(gimp-image-add-layer inImage original -1)
		(gimp-image-add-layer inImage subtract -1)
		(gmic RUN-INTERACTIVE inImage subtract)
		(set! recolorLayer (car (gimp-layer-copy subtract FALSE)))
		(gimp-layer-set-mode subtract DIFFERENCE-MODE)
		(set! subtract (car (gimp-image-merge-down inImage subtract 1)))
		(gimp-curves-spline subtract 0 8 #(0 0 8 192 32 255 255 255))
		(gimp-edit-copy subtract)
		(gimp-image-remove-layer inImage subtract)
		
		(gimp-image-add-layer inImage recolorLayer -1)
		(set! mask (car (gimp-layer-create-mask recolorLayer ADD-COPY-MASK)))
		(gimp-layer-add-mask recolorLayer mask)
		(gimp-floating-sel-anchor (car (gimp-edit-paste mask FALSE)))
		(gimp-layer-set-mode recolorLayer COLOR-MODE)
		(gimp-layer-set-name recolorLayer "Clean CCD Noise")
		
		(gimp-image-undo-group-end inImage)
		(gimp-displays-flush)
		(list recolorLayer)
	)
)

(script-fu-register "script-fu-CleanCCDNoise"
	"<Image>/Script-F_u/Enhance/Clean CCD Noise"
	"Cleans CCD Noise"
	"Jon Denning <jon@gfxcoder.us>"
	"Jon Denning"
	"2009-067-06"
	""
	SF-IMAGE	"Image"	  		0
	SF-DRAWABLE	"Layer"			0
)

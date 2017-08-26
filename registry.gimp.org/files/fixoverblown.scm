; FixOverblown is a script for Gimp
;
; This script helps fix overblown areas of an image.
;
; The script is located in "<Image> / Script-Fu / Enhance / Fix Overblown"
;
; Last changed: 2009 June 18
;
; Copyright (C) 2009 Jonathan Denning <jon@gfxcoder.us>
;
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 1.0 (2009 June 16)
;    - Created!
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

(define (script-fu-FixOverblown inImage inLayer)
	(let*
		(
			(overlayLayer (car (gimp-layer-copy inLayer FALSE)))
			(mask 0)
		)
		
		(gimp-image-undo-group-start inImage)
		
		(gimp-image-add-layer inImage overlayLayer -1)
		(gimp-layer-set-mode overlayLayer OVERLAY-MODE)
		(gimp-layer-set-name overlayLayer "Fix Overblown")
		(set! mask (car (gimp-layer-create-mask overlayLayer ADD-COPY-MASK)))
		(gimp-layer-add-mask overlayLayer mask)
		(plug-in-vinvert RUN-NONINTERACTIVE inImage overlayLayer)
		(gimp-curves-spline mask 0 6 #(0 0 128 0 255 255))
		
		(gimp-image-undo-group-end inImage)
		(gimp-displays-flush)
		(list overlayLayer)
	)
)

(script-fu-register "script-fu-FixOverblown"
	"<Image>/Script-F_u/Enhance/Fix Overblown"
	"Helps fix overblown areas"
	"Jon Denning <jon@gfxcoder.us>"
	"Jon Denning"
	"2009-06-18"
	""
	SF-IMAGE	"Image"	  		0
	SF-DRAWABLE	"Layer"			0
)

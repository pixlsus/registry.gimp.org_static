; Defringe is a script for Gimp
;
; This script removes the blue/purple fringe found in digital camera
; pictures.
;
; The script is located in "<Image> / Script-Fu / Enhance / Defringe"
;
; Last changed: 2009 July 5
;
; Copyright (C) 2009 Jonathan Denning <jon@gfxcoder.us>
;
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 1.0 (2009 June 16)
;    - Created!
;  Version 1.1 (2009 June 18)
;    - Fixed some blotchiness found in overblown areas
;    - Cleared bright areas instead of value invert for better
;      recoloring
;    - Added multiply layer to darken the fringe a bit
;  Version 1.2 (2009 July 5)
;    - Masked the darkening layer so "big" blue objects aren't
;      darkened unnecessarily
;
; To Do:
;   - Comment!  Where are my comments??
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

(define (zeroRedGreen inLayer)
	(gimp-curves-spline inLayer 1 4 #(0 0 255 0))
	(gimp-curves-spline inLayer 2 4 #(0 0 255 0))
)
(define (zeroBlue inLayer)
	(gimp-curves-spline inLayer 3 4 #(0 0 255 0))
)

(define (script-fu-Defringe inImage inLayer)
	(let*
		(
			(zeroRedGreenLayer (car (gimp-layer-copy inLayer FALSE)))
			(zeroBlueLayer (car (gimp-layer-copy inLayer FALSE)))
			(vizeroRedGreenLayer (car (gimp-layer-copy inLayer FALSE)))
			(coloredLayer (car (gimp-layer-copy inLayer FALSE)))
			(maskLayer (car (gimp-layer-copy inLayer FALSE)))
			(fringeLayer 0)
		)
		
		(gimp-context-push)
		(gimp-image-undo-group-start inImage)
		
		; Create mask to single out blue fringe
		(gimp-image-add-layer inImage zeroRedGreenLayer -1)
		(zeroRedGreen zeroRedGreenLayer)
		(gimp-desaturate-full zeroRedGreenLayer DESATURATE-LIGHTNESS)
		(gimp-image-add-layer inImage zeroBlueLayer -1)
		(zeroBlue zeroBlueLayer)
		(gimp-desaturate-full zeroBlueLayer DESATURATE-LIGHTNESS)
		(gimp-layer-set-mode zeroBlueLayer SUBTRACT-MODE)
		
		(gimp-image-add-layer inImage vizeroRedGreenLayer -1)
		(plug-in-vinvert RUN-NONINTERACTIVE inImage vizeroRedGreenLayer)
		(let*
			((vizeroBlueLayer (car (gimp-layer-copy vizeroRedGreenLayer FALSE))))
			(zeroRedGreen vizeroRedGreenLayer)
			(gimp-desaturate-full vizeroRedGreenLayer DESATURATE-LIGHTNESS)
			(gimp-image-add-layer inImage vizeroBlueLayer -1)
			(zeroBlue vizeroBlueLayer)
			(gimp-desaturate-full vizeroBlueLayer DESATURATE-LIGHTNESS)
			(gimp-layer-set-mode vizeroBlueLayer SUBTRACT-MODE)
			(let*
				(
					(fringe1Layer (car (gimp-image-merge-down inImage zeroBlueLayer 1)))
					(fringe2Layer (car (gimp-image-merge-down inImage vizeroBlueLayer 1)))
				)
				(gimp-layer-set-mode fringe2Layer OVERLAY-MODE)
				(set! fringeLayer (car (gimp-image-merge-down inImage fringe2Layer 1)))
				
				;(gimp-threshold fringeLayer 1 255)
				(gimp-curves-spline fringeLayer 0 6 #(0 0 5 255 255 255))
				(plug-in-blur RUN-NONINTERACTIVE inImage fringeLayer)
				
				(gimp-edit-copy fringeLayer)
				;(gimp-image-remove-layer inImage fringeLayer)
				(gimp-layer-set-mode fringeLayer MULTIPLY-MODE)
				(gimp-layer-set-opacity fringeLayer 25)
				(gimp-invert fringeLayer)
				
				;undarken the solid blue stuff
				(let*
					(
						(mask (car (gimp-layer-create-mask fringeLayer ADD-COPY-MASK)))
					)
					(gimp-layer-add-mask fringeLayer mask)
					(gimp-by-color-select mask '(255 255 255) 0 CHANNEL-OP-ADD FALSE FALSE 0 FALSE)
					(gimp-selection-invert inImage)
					(gimp-selection-shrink inImage 10.0)
					(gimp-selection-grow inImage 12.0)
					(gimp-selection-feather inImage 4.0)
					(gimp-context-set-foreground '(0 0 0))
					(gimp-edit-fill mask FOREGROUND-FILL)
					(gimp-selection-invert inImage)
					(gimp-context-set-foreground '(255 255 255))
					(gimp-edit-fill mask FOREGROUND-FILL)
					(gimp-selection-none inImage)
				)
				(gimp-layer-set-name fringeLayer "Darken Fringe")
				
				(gimp-image-add-layer inImage coloredLayer -1)
				(let*
					((mask (car (gimp-layer-create-mask coloredLayer ADD-WHITE-MASK))))
					(gimp-layer-add-mask coloredLayer mask)
					(let*
						((selection (car (gimp-edit-paste mask FALSE))))
						(gimp-floating-sel-anchor selection)
					)
				)
			)
		)
		
		(gimp-image-add-layer inImage maskLayer -1)
		(gimp-threshold maskLayer 0 220)
		(gimp-by-color-select maskLayer '(0 0 0) 0 CHANNEL-OP-ADD FALSE FALSE 0 FALSE)
		(gimp-selection-grow inImage 3)
		(gimp-selection-feather inImage 3.0)
		(gimp-image-remove-layer inImage maskLayer)
		(gimp-image-set-active-layer inImage coloredLayer)
		
		;(gimp-edit-fill coloredLayer FOREGROUND-FILL)
		;(plug-in-vinvert RUN-NONINTERACTIVE inImage coloredLayer)
		(gimp-edit-clear coloredLayer)
		
		(plug-in-gauss RUN-NONINTERACTIVE inImage coloredLayer 180.0 180.0 1)
		(plug-in-gauss RUN-NONINTERACTIVE inImage coloredLayer 180.0 180.0 1)
		(plug-in-gauss RUN-NONINTERACTIVE inImage coloredLayer 180.0 180.0 1)
		(gimp-selection-none inImage)
		(plug-in-gauss RUN-NONINTERACTIVE inImage coloredLayer 30.0 30.0 1)
		(gimp-layer-set-mode coloredLayer COLOR-MODE)
		(gimp-layer-set-name coloredLayer "Recolor Fringe")
		
		(gimp-image-undo-group-end inImage)
		(gimp-displays-flush)
		(gimp-context-pop)
		(list fringeLayer coloredLayer)
	)
)

(script-fu-register "script-fu-Defringe"
	"<Image>/Script-F_u/Enhance/Defringe"
	"Removes most blue/purple fringing in an image"
	"Jon Denning <jon@gfxcoder.us>"
	"Jon Denning"
	"2009-07-05"
	""
	SF-IMAGE	"Image"	  		0
	SF-DRAWABLE	"Layer"			0
)

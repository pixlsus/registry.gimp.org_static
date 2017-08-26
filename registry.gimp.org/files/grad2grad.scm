; This script is intended for use with
; GIMP - The GNU Image Manipulation Program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; Gradient to Gradient
; This script generates a new image containing a gradient based on the colors of two other GIMP gradients.
; Copyright (C) 2009 Charles Belov
; docorbit@sonic.net
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
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; 1.0 initial version
; 1.0.2 move to Filters menu


(define (script-fu-grad-to-grad width height uGradient lGradient vertBlend)

	(let* (
			(img (car (gimp-image-new width height RGB)))
			(layer (car (gimp-layer-new img width height RGB "layer 1" 100 NORMAL)))
			(colX 0)
		  )
		
		(gimp-context-push)
		(gimp-image-undo-group-start img)
		(gimp-image-add-layer img layer 0)
 
; Main routine
; blend in first row
		(gimp-context-set-gradient uGradient)
		(gimp-rect-select img 0 0 width 1 CHANNEL-OP-REPLACE 0 0)
		(gimp-edit-blend layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 0 0 TRUE 0 0 (- width 1) 0)
 
; blend in last row
		(gimp-context-set-gradient lGradient)
		(gimp-rect-select img 0 (- height 1) width 1 CHANNEL-OP-REPLACE 0 0)
		(gimp-edit-blend layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 0 0 TRUE 0 (- height 1) (- width 1) (- height 1))

; blend in each column		
		(while (< colX width)
			(gimp-context-set-foreground (car (gimp-image-pick-color img layer colX 0 FALSE FALSE 0)))
			(gimp-context-set-background (car (gimp-image-pick-color img layer colX (- height 1) FALSE FALSE 0)))
			(gimp-rect-select img colX 0 1 height CHANNEL-OP-REPLACE 0 0)
			(gimp-edit-blend layer vertBlend NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 0 0 TRUE colX 0 colX (- height 1))
			(set! colX (+ colX 1))
		)

; end main routine
 
		(gimp-selection-none img)
		(gimp-image-undo-group-end img)
		(gimp-context-pop)
		(gimp-display-new img)

	)
)

(script-fu-register "script-fu-grad-to-grad"
                    "Gradient to Gradient..."
					"This script generates a new image containing a gradient based on the colors of two other GIMP gradients."
					"Charles Belov <docorbit@sonic.net>"
					"Charles Belov"
					"2009-03-22"
					""
					SF-VALUE "Width" "100"
					SF-VALUE "Height" "100"
					SF-GRADIENT "Upper gradient" "Golden"
					SF-GRADIENT "Lower gradient" "Land and Sea"
					SF-OPTION _"Vertical color blend" '(_"RGB" _"HSV")
)

(script-fu-menu-register "script-fu-grad-to-grad" 
                         "<Image>/Filters/Render/Gradients")


; glass-selection.scm
;
; version 1.0 [gimphelp.org]
; last modified/tested by Paul Sherman
; 01/01/2010 on GIMP-2.6.8
;
; Create Glass Effect of Selection
; based upon glass-text.scm by Scott Mosteller
;
; LICENSE
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; Define Function For Glass Translucency Values
;
(define (get-glass-trans-curve parm)
  (let* ((curve-value (make-vector 4 'byte)))
   (aset curve-value 0 0)
   (aset curve-value 1 0)
   (aset curve-value 2 255)
   (aset curve-value 3 parm)
   curve-value     
   )
)

(define (script-fu-glass-selection image
			drawable

			glass-color
			glass-depth
			glass-trans
			shadow-color
			shx
			shy
			shb
			sho
			dsh
			flat)
				
    				(if (= (car (gimp-selection-is-empty image)) TRUE)
					(begin
						(gimp-message "The current image doesn't have a SELECTION.\n\nThis plugin works on a \nSELECTED AREA of the image.")
					)
								(begin; START OF PROCESSING
								       (gimp-image-undo-group-start image)

										(let* (
    										(theHeight (car (gimp-image-height image)))
    										(theWidth (car (gimp-image-width image))) 
											(theSelection (car (gimp-selection-save image)))
											(glass-layer3 -1)
											(glass-layer4 -1)
    										(glass-layer)
    										(trns)
    										(spvalues (make-vector 4 'byte)))

    										(gimp-context-set-foreground glass-color)
											(set! glass-layer4 (car (gimp-layer-new image theWidth theHeight 1 "Copied 2" 100 NORMAL-MODE)))
											(gimp-image-add-layer image glass-layer4 -1) 
											(gimp-edit-fill glass-layer4 0)

											(gimp-context-set-foreground '( 255 255 255))
											(set! glass-layer3 (car (gimp-layer-new image theWidth theHeight 1 "Copied 1" 100 NORMAL-MODE)))
											(gimp-image-add-layer image glass-layer3 -1) 
											(gimp-selection-feather image 2)
											(gimp-edit-fill glass-layer3 0)

										    (gimp-image-resize-to-layers image)

										; Create Glass Area
										    (gimp-image-set-active-layer image glass-layer3)
										    (gimp-layer-resize-to-image-size glass-layer3)
										    (gimp-selection-layer-alpha glass-layer3)   
										    (plug-in-gauss 1 image glass-layer3 5 5 1)
										    (gimp-drawable-set-visible glass-layer3 0)

										    (gimp-image-set-active-layer image glass-layer4)
										    (gimp-layer-resize-to-image-size glass-layer4)
										    (gimp-invert glass-layer4)
										    (plug-in-bump-map 1 image glass-layer4 glass-layer3 135 45 (+ glass-depth 0) 0 0 0 0 0 0 0)
										    (gimp-selection-shrink image glass-depth)
										    (gimp-selection-feather image (- glass-depth 1))

										    (gimp-curves-spline glass-layer4 4 4 (get-glass-trans-curve glass-trans))
										    (set! glass-layer (car (gimp-layer-copy glass-layer4 1)))
										    (gimp-image-add-layer image glass-layer -1)

										    (gimp-edit-clear glass-layer4)
										    (gimp-selection-none image)
										    (gimp-hue-saturation glass-layer4 0 0 0 -100)
										    (gimp-invert glass-layer)
										 ;
										 ; Create shadow layer on request
										    (if (= dsh TRUE)
										    (begin
    										    (script-fu-drop-shadow image glass-layer4 shx shy shb shadow-color sho TRUE)
										    ))
										 ;
										 ; Clean up & delete layers as needed
											(gimp-image-remove-layer image glass-layer3)
											(gimp-image-remove-layer image glass-layer4)
											(gimp-selection-none image)
											(gimp-layer-resize-to-image-size glass-layer)

										 ; Flatten on request
											(if (= flat TRUE)
											(begin
												(gimp-image-flatten image)
											))

									);end of LET

									;Finish the undo group for the process
    									(gimp-image-undo-group-end image)   
    									(gimp-displays-flush)

								); end of BEGIN
    				); end of first IF (checking for a selection)
)

(script-fu-register "script-fu-glass-selection"
		    "<Image>/Script-Fu/Effects Selection/Glass Selection"
		    "Makes the current selection into glass"
		    "Paul Sherman"
		    "Paul Sherman"
		    "12/30/2009"
		    "RGB RGBA GRAY GRAYA"
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
			SF-COLOR      _"Glass color"         '(119 171 234)
			SF-ADJUSTMENT _"Glass Depth (1-10)"        '(3 1 10 1 1 0 1)
			SF-ADJUSTMENT _"Glass Opacity (0-255)" '(140 0 255 1 1 0 1)
			SF-COLOR      _"Shadow color"       '(0 0 0)
			SF-ADJUSTMENT _"Shadow Offset X"    '(12 -25 25 1 1 0 1)
			SF-ADJUSTMENT _"Shadow Offset Y"    '(12 -25 25 1 1 0 1)
			SF-ADJUSTMENT _"Shadow Blur"        '(8 0 25 1 1 0 1)
			SF-ADJUSTMENT _"Shadow Opacity"     '(60 0 100 1 1 0 1)
			SF-TOGGLE     _"Include Shadow?"     TRUE
			SF-TOGGLE     "Flatten Image?"      TRUE)

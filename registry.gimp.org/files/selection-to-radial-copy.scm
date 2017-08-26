;;; Paul Kotronis  
;;; 
;;; 
;;; Plugin  : selection-to-radial-copy.scm
;;; Author  : Paul Kotronis
;;; Date    : April 15, 2009 - Georgia, USA
;;; Revision: None
;;; 					
;;; Version : 1.0
;;; Latest version at: http://registry.gimp.org
;;; Required : Gimp 2.4 or later
;;;
;;; Description: 
;;; Draw a true polar array from the active selection.
;;; -----------------------------------------------------------------------------
;;;
;;; License:
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Define the function:


(define (script-fu-selection-to-radial-copy inImage inLayer xCenter yCenter Radius StartAngle AngleBetween Objects)

	(let* (	(drawable 0)
					(BoxWidth 0)
					(BoxHeight 0)
					(countdown 0) (xoffset 0) (yoffset 0) (arc 0)
					(angle 0) (alpha 0) (beta 0)
					(sel-float 0)
				)
	
	(set! drawable inLayer)
  (gimp-edit-copy drawable)

;;Establish variables, Convert degrees to radians

  (set! BoxWidth (cadr (gimp-selection-bounds inImage)))
  (set! BoxHeight (caddr (gimp-selection-bounds inImage)))
  (set! countdown 0)
  (set! xoffset BoxWidth)
  (set! yoffset BoxHeight)
  (set! arc (* Objects (* AngleBetween (/ 3.14159 180))))
	(set! angle AngleBetween)    
	(set! alpha (* StartAngle (/ 3.14159 180)))
	(set! beta (* AngleBetween (/ 3.14159 180)))
	
; Copy objects loop      
  (while (< countdown Objects) 
    (begin
			(set! xoffset (+ xCenter ( * Radius (cos alpha)) ))
			(set! yoffset (- yCenter ( * Radius (sin alpha)) ))
			(set! sel-float (car (gimp-edit-paste drawable FALSE)))
  		(gimp-layer-set-offsets sel-float xoffset yoffset)
		(gimp-drawable-transform-rotate-default sel-float arc TRUE xoffset yoffset TRUE 0)
		(gimp-floating-sel-anchor sel-float)
  		(set! alpha (+ alpha beta))
		(set! arc (- arc beta))
  		(set! countdown (+ countdown 1))
    );begin
  );while		

       (gimp-displays-flush)
	);let
) ;;def



(script-fu-register
 "script-fu-selection-to-radial-copy"
 _"<Image>/_ATG/_Array/_Selection to radial copy"
 "Draw a true polar array from the active selection."
 "Paul Kotronis <camper5@mail.com>"
 "Paul Kotronis"
 "4/15/2009" 
 "RGB* GRAY* INDEXED*"
 SF-IMAGE "The Image" 0
 SF-DRAWABLE "The Layer" 0
 SF-ADJUSTMENT "Center (x-coordinate)" '(0 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Center (y-coordinate)" '(0 0 9999 1 10 0 1)
 SF-ADJUSTMENT "Radius" '(0 -9999 9999 1 10 0 1)
 SF-ADJUSTMENT "Starting Angle" '(0 0 360 0.1 1 1 1) 
 SF-ADJUSTMENT "Degrees Between" '(0 0 360 0.1 1 1 1)
 SF-ADJUSTMENT "Number of Objects" '(0 -9999 9999 1 10 0 1)
)

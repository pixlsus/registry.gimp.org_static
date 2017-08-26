;
; Filtered B&W for GIMP
; Copyright (C) 2006-8 Lukasz Komsta http://www.komsta.net/
; 
; --------------------------------------------------------------------
; version 0.1  2006/07/12
;     - Initial relase
; version 0.11 2008/08/08
;     - a hack to work with GIMP 2.4 (local variables issue)
;
; --------------------------------------------------------------------
;
; This script converts color images to B&W using channel mixer settings,
; which simulate the use of color filters in B&W photography.
;
; Avoid setting intensity to 1, besause it totally filters one of
; RGB channels and increases noise.
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


(define (script-fu-filtered-bw img drawable filter saturation)
(define (floor x) (- x (fmod x 1))) 

; This function taken from the other scripts

(define (hsv-to-rgb color)
  (let* ((h (car color))
		 (s (cadr color))
		 (v (caddr color)))
	(if (= s 0)
		(list v v v)
		(let* ((quad (/ h 60))
			   (i (floor quad))
			   (f (- quad i))
			   (p (* v (- 1 s)))
			   (q (* v (- 1 (* s f))))
			   (t (* v (- 1 (* s (- 1 f))))))
		  (cond ((= i 0) (list v t p))
				((= i 1) (list q v p))
				((= i 2) (list p v t))
				((= i 3) (list p q v))
				((= i 4) (list t p v))
				(t (list v p q)))))))



(let* (
        (saturation (/ saturation 100))
	(hue (* filter 30))
	(rgb (hsv-to-rgb (list hue saturation 1)))
	(r (car rgb))
	(g (cadr rgb))
	(b (caddr rgb))
	(sum (+ (+ r g) b) )
	(r (/ r sum))
	(g (/ g sum))
	(b (/ b sum))
      )

(gimp-image-undo-group-start img)
(gimp-selection-none img)
(plug-in-colors-channel-mixer 1 img drawable TRUE r g b r g b r g b)
(gimp-image-undo-group-end img)
(gimp-displays-flush)
)

)

(script-fu-register "script-fu-filtered-bw"
		    "<Image>/Script-Fu/Color/Filtered BW"
		    "Simulate B&W photo with color filter"
		    "Lukasz Komsta"
		    "Lukasz Komsta"
		    "2006"
		    "RGB*, GRAY*"
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Layer to blur" 0
                    SF-OPTION "Filter" '("Orange" "Yellow" "Lime-green" "Green" "Cold-green" "Cyan" "Light blue" "Blue" "Cold-violet" "Hot-violet" "Raspberry" "Red") 
		    SF-ADJUSTMENT "Saturation" '(75 0 100 1 1 1 0)
)

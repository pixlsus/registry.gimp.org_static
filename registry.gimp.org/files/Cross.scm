;;  ***************************************************************************
;;  *   Copyright (C) 2008 by James Sambrook                                  *
;;  *   sambrook@va.metrocast.net                                             *
;;  *                                                                         *
;;  *   This program is free software; you can redistribute it and/or modify  *
;;  *   it under the terms of the GNU General Public License as published by  *
;;  *   the Free Software Foundation; either version 2 of the License, or     *
;;  *   (at your option) any later version.                                   *
;;  *                                                                         *
;;  *   This program is distributed in the hope that it will be useful,       *
;;  *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
;;  *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
;;  *   GNU General Public License for more details.                          *
;;  *                                                                         *
;;  *   You should have received a copy of the GNU General Public License     *
;;  *   along with this program; if not, write to the                         *
;;  *   Free Software Foundation, Inc.,                                       *
;;  *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
;;  ***************************************************************************


(define (script-fu-thatchedcross inWidth inHeight numStrokes pctTop pctLeft inColor showAxes)

  (let* (
        (theImage (car (gimp-image-new inWidth inHeight RGB)))
        (baseLayer (car (gimp-layer-new theImage inWidth inHeight RGB-IMAGE "Cross" 100 NORMAL-MODE)))
	  (leftIntersect (* inWidth pctLeft))
	  (topIntersect (* inHeight pctTop))
	  (leftSpace (/ leftIntersect numStrokes))
	  (rightSpace (/ (- inWidth leftIntersect) numStrokes))
	  (topSpace (/ topIntersect numStrokes))
	  (bottomSpace (/ (- inHeight topIntersect) numStrokes))

        (pointArray (make-vector 4 0.0))
        (crossArray (make-vector 10 0.0))

        (flag 0)

        )

	(gimp-context-push)

; Add the three layers of the image in reverse order so they appear properly
	(gimp-image-add-layer theImage baseLayer 0)

; Set the foreground color to chosen color
	(gimp-context-set-foreground '(0 0 0))

; Set the background color to white
	(gimp-context-set-background '(255 255 255))

; Fill the initial image with white
	(gimp-drawable-fill baseLayer 2)

; Set the brush to be three pixels
	(gimp-context-set-brush "Circle (03)")

; Draw the X and Y Axes in Black
	(vector-set! pointArray 0 0)
	(vector-set! pointArray 1 topIntersect)
	(vector-set! pointArray 2 inWidth)
	(vector-set! pointArray 3 topIntersect)
	(if (= showAxes TRUE)
		(gimp-pencil baseLayer 4 pointArray))

	(vector-set! pointArray 0 leftIntersect)
	(vector-set! pointArray 1 0)
	(vector-set! pointArray 2 leftIntersect)
	(vector-set! pointArray 3 inHeight)
	(if (= showAxes TRUE)
		(gimp-pencil baseLayer 4 pointArray))

; Set the pen to the chosen color
	(gimp-context-set-foreground inColor)

; Set the pen size to one pixel
	(gimp-context-set-brush "Circle (01)")

	(while (< flag numStrokes)

; Upper Left Quadrant
		(vector-set! crossArray 0 (* flag leftSpace))
		(vector-set! crossArray 1 topIntersect)
		(vector-set! crossArray 2 leftIntersect)
		(vector-set! crossArray 3 (* (- numStrokes flag) topSpace))

;Upper Right Quadrant
		(vector-set! crossArray 4 (+ leftIntersect (* (- numStrokes flag) rightSpace)))
		(vector-set! crossArray 5 topIntersect)

; Lower Right Quadrant
		(vector-set! crossArray 6 leftIntersect)
		(vector-set! crossArray 7 (- inHeight (* (- numStrokes flag) bottomSpace)))

; Lower Left Quadrant
		(vector-set! crossArray 8 (* flag leftSpace))
		(vector-set! crossArray 9 topIntersect)

; Draw the points
		(gimp-pencil baseLayer 10 crossArray)

; Increment the loop counter
		(set! flag (+ flag 1))
	)

; Show the new image.  TAH-DAH!
	(gimp-display-new theImage)
   )
)

; Register the function with GIMP:

(script-fu-register
  "script-fu-thatchedcross"
  _"_Cross..."
  _"Makes a nice thatched cross design.  Can be symmetric or non-symmetric along either the X- or Y-axes."
  "James Sambrook"
  "26 September 2008"
  "James Sambrook.  King George, VA, USA"
  ""
  SF-ADJUSTMENT _"Image width"        '(600 300 3000 1 50 0 0)
  SF-ADJUSTMENT _"Image height"       '(600 300 3000 1 50 0 0)
  SF-ADJUSTMENT _"Number of Strokes"  '(40 30 100 1 10 0 0)
  SF-ADJUSTMENT _"Percent on top"     '(0.5 0.1 0.9 0.01 0.1 1 0)
  SF-ADJUSTMENT _"Percent on left"    '(0.5 0.1 0.9 0.01 0.1 1 0)
  SF-COLOR      _"Pencil Color"       '(0 127 127)
  SF-TOGGLE     _"Show axes"          TRUE
  

)

(script-fu-menu-register "script-fu-thatchedcross"
                         "<Toolbox>/Xtns/Patterns")
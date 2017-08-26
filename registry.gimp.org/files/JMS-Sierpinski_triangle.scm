;; Sierpinski Triangle
;; (c) 2011, James Sambrook
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

; -------------------------------------------------------

(define (drawLine layer x1 y1 x2 y2 x3 y3)
	(let* (
		(tempArray (make-vector 8 0.0))
		)

; Make the calculations easier to decipher

		(vector-set! tempArray 0 x1)
		(vector-set! tempArray 1 y1)
		(vector-set! tempArray 2 x2)
		(vector-set! tempArray 3 y2)
		(vector-set! tempArray 4 x3)
		(vector-set! tempArray 5 y3)
		(vector-set! tempArray 6 x1)
		(vector-set! tempArray 7 y1)

		(gimp-pencil layer 8 tempArray)
	)
)

; -------------------------------------------------------

(define (drawSubtri image layer number recursions x1 y1 x2 y2 x3 y3)
	(let* (
		(x1px2 (/ (+ x1 x2) 2))
		(x1px3 (/ (+ x1 x3) 2))
		(x2px3 (/ (+ x2 x3) 2))
		(y1py2 (/ (+ y1 y2) 2))
		(y1py3 (/ (+ y1 y3) 2))
		(y2py3 (/ (+ y2 y3) 2))

		(x1mx2 (/ (- x1 x2) 2))
		(x2mx1 (* -1 x1mx2))
		(x1mx3 (/ (- x1 x3) 2))
		(x3mx1 (* -1 x1mx3))
		(x3mx2 (/ (- x3 x2) 2))
		(x2mx3 (* -1 x3mx2))

		(y1my2 (/ (- y1 y2) 2))
		(y2my1 (* -1 y1my2))
		(y1my3 (/ (- y1 y3) 2))
		(y3my1 (* -1 y1my3))
		(y3my2 (/ (- y3 y2) 2))
		(y2my3 (* -1 y3my2))
		)

		(drawLine layer x1 y1 x2 y2 x3 y3)

		(if (> recursions number)
			(begin

; First smaller triangle
				(drawSubtri
					image			; Image
					layer			; Layer
					(+ number 1)		; Next layer for recursion
					recursions		; Total number of recursions
					(+ x1px2 x2mx3)	; x coordinate for first corner
					(+ y1py2 y2my3)	; y coordinate for first corner
					(+ x1px2 x1mx3)	; x coordinate for second corner
					(+ y1py2 y1my3)	; y coordinate for second corner
					x1px2			; x coordinate for third corner
					y1py2			; y coordinate for third corner
				)

; Second smaller triangle
				(drawSubtri
					image			; Image
					layer			; Layer
					(+ number 1)		; Next layer for recursion
					recursions		; Total number of recursions
					(+ x2px3 x2mx1)	; x coordinate for first corner
					(+ y2py3 y2my1)	; y coordinate for first corner
					(+ x2px3 x3mx1)	; x coordinate for second corner
					(+ y2py3 y3my1)	; y coordinate for second corner
					x2px3			; x coordinate for third corner
					y2py3			; y coordinate for third corner
				)

; Third smaller triangle
				(drawSubtri
					image			; Image
					layer			; Layer
					(+ number 1)		; Next layer for recursion
					recursions		; Total number of recursions
					(+ x1px3 x3mx2)	; x coordinate for first corner
					(+ y1py3 y3my2)	; y coordinate for first corner
					(+ x1px3 x1mx2)	; x coordinate for second corner
					(+ y1py3 y1my2)	; y coordinate for second corner
					x1px3			; x coordinate for third corner
					y1py3			; y coordinate for third corner
				)

			)
		)
	)
)

; -------------------------------------------------------

(define (script-fu-SierpinskiTriangle
	width		; Width of the image
	height		; Height of the image
	border		; Border around the triangle
	recursions	; Levels used in making the carpet
	fgColor		; Color of the "holes"
	bgColor		; Background color
	alphaYN		; Transparent holes
	)

	(let* (
		(inWidth (max (expt 2 (+ 2 recursions)) width))
		(inHeight (max (expt 2 (+ 2 recursions)) height))
		(theImage (car (gimp-image-new inWidth inHeight 0)))
		(bgLayer (car (gimp-layer-new theImage inWidth inHeight RGBA-IMAGE "Triangle" 100 NORMAL-MODE)))
		(pointArray (make-vector 8 0.0))
		(calcHeight (- inHeight 1))
		(xc1 0)
		(xc2 inWidth)
		(xc3 (/ inWidth 2))
		(yc1 calcHeight)
		(yc2 calcHeight)
		(yc3 0)

		)

		(gimp-context-set-foreground fgColor)
		(gimp-context-set-background bgColor)

		(gimp-image-add-layer theImage bgLayer 0)
		(gimp-drawable-fill bgLayer BACKGROUND-FILL)

		(gimp-context-set-brush "Circle (01)")

		(drawLine bgLayer xc1 yc1 xc2 yc2 xc3 yc3)

		(drawSubtri
			theImage
			bgLayer
			1
			recursions
			(/ (+ xc1 xc2) 2)
			(/ (+ yc1 yc2) 2)
			(/ (+ xc1 xc3) 2)
			(/ (+ yc1 yc3) 2)
			(/ (+ xc3 xc2) 2)
			(/ (+ yc3 yc2) 2)
		)

		(gimp-image-resize theImage (+ inWidth (* 2 border)) (+ inHeight (* 2 border)) border border)
		(gimp-layer-resize-to-image-size bgLayer)

		(if (= alphaYN TRUE)
			(plug-in-colortoalpha RUN-NONINTERACTIVE theImage bgLayer bgColor)
			(gimp-image-flatten theImage)
		)

; Show the new image.
		(gimp-display-new theImage)

	)
)

(script-fu-register
	"script-fu-SierpinskiTriangle"
	_"Triangle..."
	_"Makes a Sierpinski Triangle"
	"James Sambrook"
	"GNU GPL"
	"1 September 2011"
	""
	SF-ADJUSTMENT _"Triangle width"			'(1260 300 4000 1 50 0 0)
	SF-ADJUSTMENT _"Triangle height"		'(1004 300 4000 1 50 0 0)
	SF-ADJUSTMENT _"Border around image"	'(10 0 100 1 10 0 0)
	SF-ADJUSTMENT _"Number of iterations"	'(6 1 10 1 1 0 0)
	SF-COLOR	  _"Triangle Color"			'(0 0 0)
	SF-COLOR	  _"Background Color"		'(255 255 255)
	SF-TOGGLE	  _"Transparent holes?"		FALSE
)

(script-fu-menu-register "script-fu-SierpinskiTriangle"
	"<Image>/Filters/SambrookJM/Sierpinski/")
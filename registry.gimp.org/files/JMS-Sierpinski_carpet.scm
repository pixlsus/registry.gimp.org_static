;; Sierpinski Carpet
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

(define (script-fu-SierpinskiCarpet
	width		; width of the smallest "hole" in the carpet
	height		; height of the smallest "hole" in the carpet
	recursions	; Levels used in making the carpet
	fgColor		; Color of the "holes"
	bgColor		; Background color
	alphaYN		; Transparent holes
	)

	(let* (
		(theImage (car (gimp-image-new width height 0)))
		(bgLayer (car (gimp-layer-new theImage width height RGBA-IMAGE "Carpet" 100 NORMAL-MODE)))
		(flag 0)	; iteration number we are currently on
		)

		(gimp-context-set-foreground fgColor)
		(gimp-context-set-background bgColor)

		(gimp-image-add-layer theImage bgLayer 0)
		(gimp-drawable-fill bgLayer BACKGROUND-FILL)

		(while (> recursions flag)
			(set! width (car (gimp-drawable-width bgLayer)))
			(set! height (car (gimp-drawable-height bgLayer)))
			(plug-in-tile RUN-NONINTERACTIVE theImage bgLayer (* width 3) (* height 3) 0)
			(set! bgLayer (car (gimp-image-get-active-layer theImage)))
			(gimp-rect-select theImage width height width height CHANNEL-OP-REPLACE 0 0)
			(gimp-edit-bucket-fill bgLayer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)		
			(gimp-selection-none theImage)
			(set! flag (+ 1 flag))
		)

		(if (= alphaYN TRUE)
			(plug-in-colortoalpha RUN-NONINTERACTIVE theImage bgLayer fgColor)
		)

; Show the new image.
		(gimp-display-new theImage)
	)
)

(script-fu-register
	"script-fu-SierpinskiCarpet"
	_"Carpet..."
	_"Makes a Sierpinski carpet"
	"James Sambrook"
	"GNU GPL"
	"1 September 2011"
	""
	SF-ADJUSTMENT _"Initial Block Width"	'(1 1 10 1 1 0 0)
	SF-ADJUSTMENT _"Initial Block Height"	'(1 1 10 1 1 0 0)
	SF-ADJUSTMENT _"Number of iterations (final size of image will be width*3^x by height*3^x)"	'(6 1 8 1 1 0 0)
	SF-COLOR	  _"Color of the 'holes'"	'(255 255 255)
	SF-COLOR	  _"Background Color"		'(0 0 0)
	SF-TOGGLE	  _"Transparent holes?"		FALSE
)

(script-fu-menu-register "script-fu-SierpinskiCarpet"
	"<Image>/Filters/SambrookJM/Sierpinski/")
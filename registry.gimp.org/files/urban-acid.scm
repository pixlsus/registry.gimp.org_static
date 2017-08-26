; Urban Acid
;
; A script to fake the "Urban Acid" look within The Gimp version 2.
;
; This script is based on:
;
; http://www.scrapjazz.com/community/jazzclub/showthread.php?t=89073&page=3&am p;pp=15
;
; --------------------------------------------------------------------
;  
; Changelog:
;  Version 1.0 (8th June 2008) - Harry Phillips
;     - Added the ability to select the opacity
;     - Fixed the problems with the original script by David Hathaway
;
;  Version 0.6 (date unknown) - David Hathaway
;    - modified to work on GIMP 2.4
; 
;  Version 0.5 (date unknown) - David Hathaway
;    - first working version
;
; --------------------------------------------------------------------
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;


(define	(script-fu-urban-acid 	theImage
				theLayer
				opacityLevel
			)

    ;Set some system variables
    (let* (
	(effectLayer (car (gimp-layer-copy theLayer 0)))
	(splineValue)
	(splineRed)
	(splineGreen)
	(splineBlue)
	(a)
    )

    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)


    ;Add a copy of the base layer.
    (gimp-image-add-layer theImage effectLayer 0)

    ;Rename the layer
    (gimp-layer-set-name effectLayer "Effect layer")

    ; define the set-pt procedure
    ; from http://adrian.gimp.org/scripts/shagadelic.scm
    (define (set-pt a index x y)
	(prog1
	(aset a (* index 2) x)
	(aset a (+ (* index 2) 1) y)))

    ;Setup the Value spline
    (define (splineValue)
	(let* ((a (cons-array 10 'byte)))
	(set-pt a 0 0 0)
	(set-pt a 1 44 27)
	(set-pt a 2 99 117)
	(set-pt a 3 195 229)
	(set-pt a 4 255 255)
	a))

    ;Apply the Value spline
    (gimp-curves-spline effectLayer 0 10 (splineValue))

    ;Setup the Red spline
    (define (splineRed)
	(let* ((a (cons-array 10 'byte)))
	(set-pt a 0 0 0)
	(set-pt a 1 51 6)
	(set-pt a 2 151 137)
	(set-pt a 3 204 228)
	(set-pt a 4 255 255)
	a))

    ;Apply the Red spline
    (gimp-curves-spline effectLayer 1 10 (splineRed))

    ;Setup the green spline
    (define (splineGreen)
	(let* ((a (cons-array 10 'byte)))
	(set-pt a 0 0 0)
	(set-pt a 1 38 31)
	(set-pt a 2 125 129)
	(set-pt a 3 197 223)
	(set-pt a 4 255 255)
	a))

    ;Apply the Green spline
    (gimp-curves-spline effectLayer 2 10 (splineGreen))

    ;Setup the blue spline
    (define (splineBlue)
	(let* ((a (cons-array 10 'byte)))
	(set-pt a 0 0 0)
	(set-pt a 1 22 33)
	(set-pt a 2 149 126)
	(set-pt a 4 255 255)
	a))

    ;Apply the Blue spline
    (gimp-curves-spline effectLayer 3 10 (splineBlue))

    ;Change the mode of the effect layer
    (gimp-layer-set-mode effectLayer HARDLIGHT-MODE)

    ;Set the opacity of the effect layer
    (gimp-layer-set-opacity effectLayer opacityLevel)

    ;Finish the undo group for the process
    (gimp-image-undo-group-end theImage)

    ;Ensure the updated image is displayed now
    (gimp-displays-flush)

))

(script-fu-register	"script-fu-urban-acid"
			_"<Image>/Script-Fu/Artistic/Urban Acid..."
			"This script sets the curves based on the Urban Acid formula"
			"David Hathaway, Harry Phillips"
			"David Hathaway, Harry Phillips"
			"08 Jun 2008"
			"RGB*"
			SF-IMAGE	"Image"     0
			SF-DRAWABLE	"Drawable"  0
			SF-ADJUSTMENT	_"Opacity"      '(80 0 100 1 10 1 0)
)



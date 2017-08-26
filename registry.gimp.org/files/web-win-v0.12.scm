; Website Window Creator
; Version 0.12
; By David Nickerson
; Copyright 2009 David Nickerson

; Description:
; This script creates a new image with a window useful for web design

; License:
; This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
; The GNU Public License is available at <http://www.gnu.org/licenses/>.

; Section 1

(define (web-window width height title border border2 radius color1 color2 bgcolor1 bgcolor2 shine-color shine-strength shadow-length flatten)

	; Create an image and layers
	(let* (
		(img (car (gimp-image-new (+ width 100) (+ height 100) RGB)))
		(layer1 (car (gimp-layer-new img (+ width 100) (+ height 100) RGB-IMAGE "Background" 100 NORMAL-MODE)))
		(layer2 (car (gimp-layer-new img (+ width 100) (+ height 100) RGB-IMAGE "Outer Border" 100 NORMAL-MODE)))
		(layer3 (car (gimp-layer-new img (+ width 100) (+ height 100) RGB-IMAGE "Border" 100 NORMAL-MODE)))
		(layer4 (car (gimp-layer-new img (+ width 100) (+ height 100) RGB-IMAGE "Shine" 100 NORMAL-MODE)))
		(layer5 (car (gimp-layer-new img (+ width 100) (+ height 100) RGB-IMAGE "Inner Border" 100 NORMAL-MODE)))
		(layer6 (car (gimp-layer-new img (+ width 100) (+ height 100) RGB-IMAGE "Inside" 100 NORMAL-MODE)))
		(orig-FG (car (gimp-context-get-foreground)))
		(top-left-x 50)
		(top-left-y 50)
		(img-width (+ width 100))
		(img-height(+ height 100))
		)


	(gimp-image-undo-disable img)


	; prepare the layers
	(gimp-image-add-layer img layer1 0)
	(gimp-image-add-layer img layer2 0)
	(gimp-image-add-layer img layer3 0)
	(gimp-image-add-layer img layer4 0)
	(gimp-image-add-layer img layer5 0)
	(gimp-image-add-layer img layer6 0)
	(gimp-layer-add-alpha layer2)
	(gimp-layer-add-alpha layer3)
	(gimp-layer-add-alpha layer4)
	(gimp-layer-add-alpha layer5)
	(gimp-layer-add-alpha layer6)
	(gimp-drawable-fill layer2 TRANSPARENT-FILL)
	(gimp-drawable-fill layer3 TRANSPARENT-FILL)
	(gimp-drawable-fill layer4 TRANSPARENT-FILL)
	(gimp-drawable-fill layer5 TRANSPARENT-FILL)
	(gimp-drawable-fill layer6 TRANSPARENT-FILL)


	; Paint the background layer
	(gimp-palette-set-foreground bgcolor2)
	(gimp-edit-fill layer1 FOREGROUND-FILL)


	; Section 2. This section paints the window.

	; Create a rounded rectangle selection and paint it
	(gimp-round-rect-select img top-left-x top-left-y width height radius radius CHANNEL-OP-REPLACE TRUE FALSE 0 0)
	(gimp-palette-set-foreground color2)
	(gimp-edit-fill layer2 FOREGROUND-FILL)

	; Subtract the border's border from the rounded rectangle and paint it
	(set! top-left-x (if (> (+ top-left-x border2) (- img-width 1)) (- img-width 1) (+ top-left-x border2)))
	(set! top-left-y (if (> (+ top-left-y border2) (- img-height 1)) (- img-height 1) (+ top-left-y border2)))
	(set! width (if (< (- width (* 2 border2)) 1) 1 (- width (* 2 border2))))
	(set! height (if (< (- height (* 2 border2)) 1) 1 (- height (* 2 border2))))
	(set! radius (if (< (- radius border2) 0) 0 (- radius border2)))
	(gimp-round-rect-select img top-left-x top-left-y width height radius radius CHANNEL-OP-REPLACE TRUE FALSE 0 0)
	(gimp-palette-set-foreground color1)
	(gimp-edit-fill layer3 FOREGROUND-FILL)

	; Paint a shiny gradient on top
	(gimp-palette-set-foreground shine-color)
	(gimp-edit-blend layer4 FG-TRANSPARENT-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 1 0 FALSE 0 50 0 (+ 50 title (* 2 border) (* 4 border2) radius))
	(gimp-layer-set-opacity layer4 shine-strength)

	; Subtract the border from the rounded rectangle and title from the top and paint it
	(set! top-left-x (if (> (+ top-left-x border) (- img-width 1)) (- img-width 1) (+ top-left-x border)))
	(set! top-left-y (if (> (+ top-left-y border title) (- img-height 1)) (- img-height 1) (+ top-left-y border title)))
	(set! width (if (< (- width (* 2 border)) 1) 1 (- width (* 2 border))))
	(set! height (if (< (- height (* 2 border) title) 1) 1 (- height (* 2 border) title)))
	(set! radius (if (< (- radius border) 0) 0 (- radius border)))
	(gimp-round-rect-select img top-left-x top-left-y width height radius radius CHANNEL-OP-REPLACE TRUE FALSE 0 0)
	(gimp-palette-set-foreground color2)
	(gimp-edit-fill layer5 FOREGROUND-FILL)

	; Subtract the border's border from the rounded rectangle and paint it
	(set! top-left-x (if (> (+ top-left-x border2) (- img-width 1)) (- img-width 1) (+ top-left-x border2)))
	(set! top-left-y (if (> (+ top-left-y border2) (- img-height 1)) (- img-height 1) (+ top-left-y border2)))
	(set! width (if (< (- width (* 2 border2)) 1) 1 (- width (* 2 border2))))
	(set! height (if (< (- height (* 2 border2)) 1) 1 (- height (* 2 border2))))
	(set! radius (if (< (- radius border2) 0) 0 (- radius border2)))
	(gimp-round-rect-select img top-left-x top-left-y width height radius radius CHANNEL-OP-REPLACE TRUE FALSE 0 0)
	(gimp-palette-set-foreground bgcolor1)
	(gimp-edit-fill layer6 FOREGROUND-FILL)

	; Remove the selection
	(gimp-selection-none img)

	; Section 3. This section adds a dropshadow and finishes up.

	; Make a dropshadow
	(script-fu-drop-shadow img layer2 shadow-length shadow-length 15 '(0 0 0) 30 FALSE)

	; Display the image and re-enable undo
	(if (= flatten TRUE) (gimp-image-flatten img))
	(gimp-context-set-foreground orig-FG)
	(gimp-display-new img)
	(gimp-image-undo-enable img)

))

(script-fu-register "web-window"
					"Website Window"
					"Creates a window for website design"
					"David Nickerson"
					"David Nickerson"
					"2009"
					""
					SF-VALUE	"Width"						"200"
					SF-VALUE	"Height"					"200"
					SF-VALUE	"Title bar width"			"25"
					SF-VALUE	"Border width"				"2"
					SF-VALUE	"Border's border width"		"1"
					SF-VALUE	"Corner radius"				"15"
					SF-COLOR	"Border color"				'(0 93 181)
					SF-COLOR	"Border's border color"		'(0 67 155)
					SF-COLOR	"Inside background"			'(255 255 255)
					SF-COLOR	"Outside background"		'(245 249 255)
					SF-COLOR	"Shine color"				'(255 255 255)
					SF-VALUE	"Shine strength (0-100)"	"50"
					SF-VALUE	"Shadow length"				"8"
					SF-TOGGLE	"Flatten Image"				TRUE)

(script-fu-menu-register "web-window"
                         "<Image>/File/Create")
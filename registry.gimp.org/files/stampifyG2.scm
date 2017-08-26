;adapted for GIMP-2 by Eddy Verlinden
;adapted for GIMP-2.6 by Guillaume Duwelz-Rebert

(define (script-fu-stampify img drawable paper hole diameter gap marg)
		(let*	(
			(owidth (car (gimp-image-width img)))
			(oheight (car (gimp-image-height img)))
			(nw (+ diameter (+ owidth (* 2 marg))))
			(nh (+ diameter (+ oheight (* 2 marg))))
			(img2 (car (gimp-image-new nw nh RGB)))
			(layer1 (car (gimp-layer-new img2 nw nh 0 "Layer1" 100 NORMAL)))
                        (nholes 0)
                        (pos 0)
                        (dist 0)
                        (i 0)
			)

	(gimp-image-add-layer img2 layer1 0)
	(gimp-image-set-active-layer img2 layer1)
	(gimp-palette-set-background paper)
	(gimp-drawable-fill layer1 1)
	(gimp-selection-none img2)

;; calculate number of horisontal holes
	(set! nholes (/ (+ nw gap) (+ diameter gap)))
	(set! pos 0)
	(set! i 0)

;; loop horisontally
	(while (< i nholes)
		(gimp-ellipse-select img2 pos 0 diameter diameter ADD 1 0 0)
		(set! pos (+ pos diameter gap))
		(set! i (+ i 1))
	)

;; calculate number of vertical holes
	(set! nholes (/ (+ nh gap) (+ diameter gap)))
	(set! pos 0)
	(set! i 0)

;; loop vertically
	(while (< i nholes)
		(gimp-ellipse-select img2 0 pos diameter diameter ADD 1 0 0)
		(set! pos (+ pos diameter gap))
		(set! i (+ i 1))
	)

;; and fill the holes with a colour
	(gimp-palette-set-background hole)
	(gimp-edit-fill layer1 1)
	(gimp-selection-none img2)

;; and here comes the clever part:
;; offset horis and vert holes by half the diameter
	(set! dist (* -1 (/ diameter 2)))
	(gimp-drawable-offset layer1 1 0 dist dist)

;; insert old image into a new layer in img2
	(gimp-selection-all img)
	(gimp-edit-copy drawable)
	(let 	((floating-sel (car (gimp-edit-paste layer1 0))))
		(gimp-floating-sel-anchor floating-sel)
	)
;; and return command to The Gimp
	(gimp-image-clean-all img2)
	(gimp-display-new img2)
)
)

(script-fu-register "script-fu-stampify"
	"<Image>/Script-Fu/Decor/Stampify"
	"Will hopefully make an image look like a postage stamp."
	"Claes G Lindblad <claesg@algonet.se>"
	"Claes G Lindblad <claesg@algonet.se>"
	"990330"
	"*"
	SF-IMAGE "Input Image" 0
	SF-DRAWABLE "Input Drawable" 0
	SF-COLOR "Paper colour" '(242 242 242)
	SF-COLOR "Hole colour" '(153 153 153)
	SF-VALUE "Diameter of perforation" "10"
	SF-VALUE "Gap between perforations" "5"
	SF-VALUE "Marginal between art and perforations" "7"
)

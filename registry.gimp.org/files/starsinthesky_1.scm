; Script-fu Stars in the sky
; Version 1.0
; October 2010
; Creates a new layer sparkling stars.
; This script is under CC-BY-SA license, you can find it here : http://creativecommons.org/licenses/by-sa/2.0/fr/deed.en
; Copyleft 2010 Harfangdesneiges
(define (script-fu-stars-in-the-sky img width height spikes-lenght flare-intensity)
	(let*
		(
			(layer (car (gimp-layer-new img width height 0 "Sparkling stars" 100 NORMAL-MODE) ))
			(background-color (gimp-context-get-background))
		)
		(gimp-image-add-layer img layer 0)
		(gimp-context-set-background '(0 0 0) )
		(gimp-drawable-fill layer BACKGROUND-FILL)
		(plug-in-hsv-noise 1 img layer 4 32 10 194)
		(gimp-brightness-contrast layer -48 42)
		(plug-in-sparkle 1 img layer 0.009 flare-intensity spikes-lenght 4 15 1 0 0 0 FALSE FALSE FALSE 0)
		(gimp-context-set-background (car background-color))
	)
)
(script-fu-register
	"script-fu-stars-in-the-sky"
	"Stars in the sky"
	"Creates a new layer with sparkling stars."
	"Harfangdesneiges"
	"CC-BY-SA"
	"October 16, 2010"
	""
	SF-IMAGE "Image" 0
	SF-ADJUSTMENT "Layer's width" '(800 10 10000 1 1 0 SF-SPINNER)
	SF-ADJUSTMENT "Layer's height" '(600 10 10000 1 1 0 SF-SPINNER)
	SF-ADJUSTMENT "Spikes' length" '(26 0 100 1 1 0 SF-SPINNER)
	SF-ADJUSTMENT "Flare intensity" '(0.6 0 1 0.01 0.01 2 SF-SPINNER)
)
(script-fu-menu-register "script-fu-stars-in-the-sky" "<Toolbox>/Xtns/Space Art")

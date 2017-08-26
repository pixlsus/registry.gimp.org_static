; google-logo.scm: Create a 3D text effect similar to that of the
; Google logo.
; Martin Ultima <multima@ultimapcs.dyndns.org>
;
; Version 0.2.
; This script supersedes google-effect.scm.
;
; Based loosely on gradient-bevel v0.1, drop-shadow.scm version 1.04
;
; This code is in the public domain.  I don't care WHAT you do with it,
; but if you create any neat patches please send 'em to me.
;
; QUICK DESCRIPTION:
; This is an attempt to (partially) automate my GIMP logo how-to at
; http://ultimapcs.dyndns.org/~multima/google-logo.html.
;
; Note that you have to create and colorize your logo text MANUALLY, since
; I'm not sure how to automatically implement that.  Then run Filters ->
; Alpha to Logo -> Google Effect from the image window.
;
; Since this is my first attempt at a GIMP script (or *anything* in Scheme,
; for that matter), the code's probably a little bit messy.
;
; Questions, comments, suggestions, etc. to <multima@ultimapcs.dyndns.org>
; (And patches.  I love when people send me patches.  It saves me the trouble
; of fixing my own code.)


; Add the 3D effect:
(define (apply-google-logo-effect img
		logo-layer
		feather-radius
		light-opacity
		dark-opacity
		light-offset-x
		light-offset-y
		dark-offset-x
		dark-offset-y
		logo-blur-radius)

	(let*	(
		(width (car (gimp-drawable-width logo-layer)))
		(height (car (gimp-drawable-height logo-layer)))
		(light-layer (car (gimp-layer-copy logo-layer TRUE)))
		(dark-layer (car (gimp-layer-copy logo-layer TRUE)))
		)

		(gimp-context-push)

		; Foreground white, background black:
		(gimp-context-set-foreground '(255 255 255))
		(gimp-context-set-background '(0 0 0))

		; Rename layers:
		(gimp-drawable-set-name light-layer "Light")
		(gimp-drawable-set-name dark-layer "Dark")

		; Add layers to image:
		(gimp-image-add-layer img light-layer -1)
		(gimp-image-add-layer img dark-layer 1)

		; Layers to image size:
		(gimp-layer-resize-to-image-size light-layer)
		(gimp-layer-resize-to-image-size dark-layer)

		; Add blur BEFORE adding a mask:
		; (it works interactively afterwards, but not scripted)
		(plug-in-gauss-rle RUN-NONINTERACTIVE img light-layer logo-blur-radius TRUE TRUE)
		(plug-in-gauss-rle RUN-NONINTERACTIVE img dark-layer logo-blur-radius TRUE TRUE)

		; Light layer:
		(gimp-image-set-active-layer img light-layer)
		(gimp-selection-layer-alpha light-layer)
		(gimp-selection-feather img feather-radius)
		(gimp-selection-invert img)
		(gimp-edit-fill light-layer FOREGROUND-FILL)
		(gimp-selection-none img)
		(gimp-layer-set-opacity light-layer light-opacity)
		(gimp-selection-layer-alpha light-layer)
		(gimp-selection-invert img)
		(gimp-layer-add-mask light-layer (car (gimp-layer-create-mask light-layer ADD-SELECTION-MASK)))
		(gimp-selection-none img)
		(gimp-layer-translate light-layer light-offset-x light-offset-y)
		(plug-in-autocrop-layer 1 img light-layer)
		(gimp-layer-set-mode light-layer LIGHTEN-ONLY-MODE)

		; Dark layer:
		(gimp-image-set-active-layer img dark-layer)
		(gimp-selection-layer-alpha dark-layer)
		(gimp-selection-feather img feather-radius)
		(gimp-selection-invert img)
		(gimp-edit-fill dark-layer BACKGROUND-FILL)
		(gimp-selection-none img)
		(gimp-layer-set-opacity dark-layer dark-opacity)
		(gimp-selection-layer-alpha dark-layer)
		(gimp-selection-invert img)
		(gimp-layer-add-mask dark-layer (car (gimp-layer-create-mask dark-layer ADD-SELECTION-MASK)))
		(gimp-selection-none img)
		(gimp-layer-translate dark-layer dark-offset-x dark-offset-y)
		(plug-in-autocrop-layer 1 img dark-layer)
		(gimp-layer-set-mode dark-layer DARKEN-ONLY-MODE)
	)
)


; Apply the 3D effect to the current image:
(define (script-fu-google-effect-logo img
	logo-layer
	feather-radius
	light-opacity
	dark-opacity
	light-offset-x
	light-offset-y
	dark-offset-x
	dark-offset-y
	logo-blur-radius
	add-drop-shadow
	shadow-offset-x
	shadow-offset-y
	shadow-blur-radius
	shadow-color
	shadow-opacity
	merge-and-crop)

	;(gimp-image-undo-disable img)
	(apply-google-logo-effect img logo-layer feather-radius light-opacity dark-opacity light-offset-x light-offset-y dark-offset-x dark-offset-y logo-blur-radius)
	(if	(= add-drop-shadow TRUE)
		(script-fu-drop-shadow img logo-layer shadow-offset-x shadow-offset-y shadow-blur-radius shadow-color shadow-opacity TRUE))
	(if	(= merge-and-crop TRUE)
		(plug-in-autocrop 1 img (car (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY))))
	;(gimp-image-undo-enable img)
)


; Create a new (monochrome) 3D logo:
(define (script-fu-google-logo text
	font-name
	font-size
	font-color
	use-gradient
	font-gradient
	reverse-gradient
	feather-radius
	light-opacity
	dark-opacity
	light-offset-x
	light-offset-y
	dark-offset-x
	dark-offset-y
	logo-blur-radius
	add-drop-shadow
	shadow-offset-x
	shadow-offset-y
	shadow-blur-radius
	shadow-color
	shadow-opacity)

	(let*	(
		(img (car (gimp-image-new 1024 768 RGB)))
		(logo-layer (car (gimp-text-fontname img -1 0 0 text 10 TRUE font-size PIXELS font-name)))
		)

		(gimp-image-undo-disable img)

		(plug-in-autocrop 1 img logo-layer)

		; Color the logo text:
		(gimp-context-set-foreground font-color)
		(gimp-selection-layer-alpha logo-layer)
		;(gimp-selection-invert img)
		(if (= use-gradient TRUE)
			(begin
				(gimp-context-set-gradient font-gradient)
				(gimp-edit-blend logo-layer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE reverse-gradient FALSE 0 0 TRUE 0 0 (car (gimp-drawable-width logo-layer)) 0)))
		(if (= use-gradient FALSE)
			(gimp-edit-fill logo-layer FOREGROUND-FILL))
		(gimp-selection-none img)

		(apply-google-logo-effect img logo-layer feather-radius light-opacity dark-opacity light-offset-x light-offset-y dark-offset-x dark-offset-y logo-blur-radius)
		(if	(= add-drop-shadow TRUE)
			(script-fu-drop-shadow img logo-layer shadow-offset-x shadow-offset-y shadow-blur-radius shadow-color shadow-opacity TRUE))
		(plug-in-autocrop 1 img (car (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)))

		(gimp-image-undo-enable img)
		(gimp-display-new img)
	)
)


; Register the script:
; Create an interactive dialog
(script-fu-register "script-fu-google-logo"
	"_Google Logo..."
	"Create a 3D logo similar to Google's"
	"Martin Ultima <multima@ultimapcs.dyndns.org>"
	"Martin Ultima"
	"May 2008"
	""
	; Text, font, size for logo
	SF-STRING	_"Text"				"Google"
	SF-FONT		_"Font"				"Book Antiqua"
	SF-ADJUSTMENT	_"Font size (pixels)"		'(130 2 1000 1 10 0 1)
	SF-COLOR	_"Text color"			"#0039b6"
	SF-TOGGLE	_"Use gradient for text instead of color"	TRUE
	;SF-GRADIENT	_"Blend gradient (text)"	"Full saturation spectrum CCW"
	SF-GRADIENT	_"Blend gradient (text)"	"Google Logo"
	SF-TOGGLE	_"Text gradient reverse"	FALSE
	; For the Google-ification
	SF-ADJUSTMENT	"Feather radius"		'(10 0 100 1 10 0 1)
	SF-ADJUSTMENT	"Light opacity"			'(50 0 100 1 10 0 0)
	SF-ADJUSTMENT	"Dark opacity"			'(100 0 100 1 10 0 0)
	SF-ADJUSTMENT	"Light offset X"		'(4 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Light offset Y"		'(2 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Dark offset X"			'(-2 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Dark offset Y"			'(-1 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Light/dark blur radius"	'(5 0 1024 1 10 0 1)
	SF-TOGGLE	"Add drop shadow"		TRUE
	SF-ADJUSTMENT	"Shadow offset X"		'(2 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Shadow offset Y"		'(8 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Shadow blur radius"		'(5 0 1024 1 10 0 1)
	SF-COLOR	"Shadow color"			"black"
	SF-ADJUSTMENT	"Shadow opacity"		'(50 0 100 1 10 0 0)
)

; Register the script:
; Add to the GIMP image menu
(script-fu-menu-register "script-fu-google-logo"
	"<Toolbox>/Xtns/Logos")


; Register the script:
; Create an interactive dialog
(script-fu-register "script-fu-google-effect-logo"
	"_Google Effect..."
	"Create a 3D text effect similar to that of the Google logo"
	"Martin Ultima <multima@ultimapcs.dyndns.org>"
	"Martin Ultima"
	"May 2008"
	"RGBA"
	; Use the current image
	SF-IMAGE	"Image"				0
	SF-DRAWABLE	"Drawable"			0
	; For the Google-ification
	SF-ADJUSTMENT	"Feather radius"		'(10 0 100 1 10 0 1)
	SF-ADJUSTMENT	"Light opacity"			'(50 0 100 1 10 0 0)
	SF-ADJUSTMENT	"Dark opacity"			'(100 0 100 1 10 0 0)
	SF-ADJUSTMENT	"Light offset X"		'(4 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Light offset Y"		'(2 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Dark offset X"			'(-2 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Dark offset Y"			'(-1 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Light/dark blur radius"	'(5 0 1024 1 10 0 1)
	SF-TOGGLE	"Add drop shadow"		TRUE
	SF-ADJUSTMENT	"Shadow offset X"		'(2 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Shadow offset Y"		'(8 -4096 4096 1 10 0 1)
	SF-ADJUSTMENT	"Shadow blur radius"		'(5 0 1024 1 10 0 1)
	SF-COLOR	"Shadow color"			"black"
	SF-ADJUSTMENT	"Shadow opacity"		'(50 0 100 1 10 0 0)
	SF-TOGGLE	"Merge and crop"		TRUE
)

; Register the script:
; Add to the GIMP image menu
(script-fu-menu-register "script-fu-google-effect-logo"
	"<Image>/Filters/Alpha to Logo")

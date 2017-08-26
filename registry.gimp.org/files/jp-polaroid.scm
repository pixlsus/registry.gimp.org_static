; 17-18, 23-26, 29-30 juillet 2009

(script-fu-register "jp-polaroid"
	"<Image>/Filters/jp/jp-polaroid"
	"Cre'e un faux polaroid/To create a fake polaroid.
Derni`ere version/last version:
http://www.vide.memoire.free.fr/photo/textes/polaroid/jp-polaroid.scm"
"(c) Jean-Pierre Bucciol <jpsspam(at)free.fr>"
	"Published under GPL version 2"
	"17-18, 23-26, 29-30 juillet 2009"
	"*"
	SF-IMAGE "Image" 0
	SF-DRAWABLE "Drawable" 0
	SF-ADJUSTMENT "flou/blur" '(2 0 10 1 2 0 0)
	SF-ADJUSTMENT "vignetage/vignetting" '(0 0 100 1 10 0 0)
	SF-ADJUSTMENT "contraste/contrast" '(20 -100 100 1 10 0 0)
	SF-ADJUSTMENT "balance-cyan-rouge/balance-cyan-red" '(-30 -100 100 1 10 0 0)
	SF-ADJUSTMENT "balance-magenta-vert/balance-magenta-green" '(-15 -100 100 1 10 0 0)
	SF-ADJUSTMENT "balance-jaune-bleu/balance-yellow-blue" '(-50 -100 100 1 10 0 0)
	SF-ADJUSTMENT "luminosite/light" '(25 -100 100 1 10 0 0)
	SF-ADJUSTMENT "saturation/saturation" '(-15 -100 100 1 10 0 0)
	SF-ADJUSTMENT "resolution/resolution" '(120 72 720 10 60 0 0)
	SF-TOGGLE "rotation/rotation" FALSE
	SF-TOGGLE "traits de decoupe/cutting lines" FALSE
	SF-TOGGLE "ombre/shadow" TRUE
)

(define
	(jp-polaroid
		image
		drawable
		flou
		vignetage
		contraste
		balance-cyan-rouge
		balance-magenta-vert
		balance-jaune-bleu
		luminosite
		saturation
		resolution
		rotation
		decoupe
		ombre
	)

	; début groupe d'annulation
	(gimp-undo-push-group-start image)
	(define premierplanorigine (car (gimp-context-get-foreground)))
	(define arriereplanorigine (car (gimp-context-get-background)))

	; passage obligatoire en couleur
	(define gris (car (gimp-drawable-is-gray drawable)))
	(if (= gris TRUE)
		(begin
			(gimp-image-convert-rgb image)
		)
	)
	
	; constantes
	(define largeur (car (gimp-image-width image)))
	(define hauteur (car (gimp-image-height image)))
	(define pouce 2.56)
	(define newlargeurimage (trunc (/ (* 7.7 resolution) pouce)))
	(define newhauteurimage (trunc (/ (* 7.85 resolution) pouce)))
	(define largeurpola (trunc (/ (* 8.85 resolution) pouce)))
	(define hauteurpola (trunc (/ (* 10.75 resolution) pouce)))
	(define margegauche (trunc (/ (* .6 resolution) pouce)))
	(define margehaut (trunc (/ (* .65 resolution) pouce)))

	; découpage
	(if (> largeur hauteur)
		(begin
			(define largeurun (trunc (* hauteur (/ 7.7 7.85))))
			(define hauteurun hauteur)
			(define xoffset (trunc (/  (- largeur largeurun) 2)))
			(define yoffset 0)
		)
		(begin
			(define largeurun largeur)
			(define hauteurun (trunc (* largeur (/ 7.7 7.85))))
			(define xoffset 0)
			(define yoffset (trunc (/  (- hauteur hauteurun) 2)))
		)
	)
	(gimp-image-crop
		image
		largeurun
		hauteurun
		xoffset
		yoffset
	)

	; taille de l'image
	(gimp-image-scale-full
		image
		newlargeurimage
		newhauteurimage
		INTERPOLATION-LANCZOS
	)
	
	; flou
	(define floudeux (trunc (/ (* flou resolution) 300)))
	;(gimp-message (number->string floudeux))
	(if (> floudeux 0)
		(begin
			(plug-in-gauss
				1 ; run-mode
				image ; image
				drawable ; drawable
				floudeux ; horizontal radius
				floudeux ; vertical radius
				0 ; method
			)
		)
	)

	; couleurs
	(gimp-color-balance
		drawable ; drawable
		MIDTONES ; transfer-mode
		TRUE ; preserve-lum
		balance-cyan-rouge ; cyan-red
		balance-magenta-vert ; magenta-green
		balance-jaune-bleu ; yellow-blue
	)
	(gimp-brightness-contrast
		drawable
		luminosite
		contraste
	)
	(gimp-hue-saturation
		drawable
		ALL-HUES ; hue-range
		0 ; hue-offset
		0 ; lightness
		saturation ; saturation
	)

	; vignetage
	(if (> vignetage 0)
		(begin
			(define largeur (car (gimp-image-width image)))
			(define hauteur (car (gimp-image-height image)))
			(define xcentre (/ largeur 2))
			(define ycentre (/ hauteur 2))
			(gimp-edit-blend
				drawable; drawable
				MULTIPLY-MODE ; blend_mode
				NORMAL ; paint_mode
				GRADIENT-RADIAL ; gradient_type
				vignetage ; opacity
				60 ; offset
				REPEAT-NONE ; repeat
				TRUE ; reverse
				0 ; supersample
				0 ; max_depth
				0 ; threshold
				TRUE ; dither
				xcentre ; x1
				ycentre ; y1
				largeur ; x2
				hauteur ; y2
			)
		)
	)

	; filet autour de l'image
	(if (= ombre FALSE)
		(begin
			;(gimp-message "filet !")
			(gimp-context-set-background '(64 64 64))
			(define largeur (car (gimp-image-width image)))
			(define hauteur (car (gimp-image-height image)))
			(define newlargeur (+ largeur 2))
			(define newhauteur (+ hauteur 2))
			(define xoffset 1)
			(define yoffset 1)
			(gimp-image-resize image newlargeur newhauteur xoffset yoffset)
			(gimp-layer-resize-to-image-size drawable)
		)
		(begin
			; construction d'une ombre de 1pt
			;(gimp-message "ombre int !")
			(gimp-context-set-background '(192 192 192))
			(define largeur (car (gimp-image-width image)))
			(define hauteur (car (gimp-image-height image)))
			(define newlargeur (+ largeur 1))
			(define newhauteur (+ hauteur 1))
			(define xoffset 1)
			(define yoffset 1)
			(gimp-image-resize image newlargeur newhauteur xoffset yoffset)
			(gimp-layer-resize-to-image-size drawable)
		)
	)

	; taille de l'image totale pour le calque
	(gimp-image-resize
		image
		largeurpola ; new width
		hauteurpola ; new height
		margegauche ; xoffset
		margehaut ; yoffset	
	)

	; création du calque polaroid
	(gimp-context-set-background '(256 256 256))
	(define jp-polaroid (car (gimp-layer-new
		image ; image
		largeurpola ; width
		hauteurpola ; height
		RGB-IMAGE ; type
		"jp-polaroid" ; name
		100; opacity
		NORMAL-MODE; mode
	)))
	(gimp-image-add-layer image jp-polaroid 1)
	
	; effet de toile pour le calque
	(gimp-invert jp-polaroid)
	(define depth 8)
	;(gimp-message (number->string depth))
	(plug-in-apply-canvas
		1 ; run-mode
		image ; image
		jp-polaroid ; drawable
		1 ; light-direction 1-3
		depth ; depth 1-50
	)
	
	; on colorise la toile
	(gimp-colorize
		jp-polaroid ; layer
		60 ; hue
		10 ; saturation
		60 ; lightness
	)

	; on floute la toile
	(define radius (trunc (/ (* resolution .5) 70)))
	;(gimp-message (number->string radius))
	(if (< radius 2)
		(begin
			(define radius 2)
		)
	)
	;(gimp-message (number->string radius))
	(plug-in-gauss
		1 ; run-mode
		image ; image
		jp-polaroid ; drawable
		radius ; horizontal radius
		radius ; vertical radius
		0 ; method
	)

	; on floute la bordure intérieure
	(if (= ombre FALSE)
		(begin
			(gimp-selection-none image)
			(define selection (trunc (/ (* resolution 1) 70)))
			;(gimp-message (number->string selection))
			(gimp-rect-select
				image ; image
				(- margegauche selection) ; xoffset
				(- margehaut selection) ; yoffset
				(+ newlargeurimagedeux (* 2 selection)) ; width
				(+ newhauteurimagedeux (* 2 selection)) ; height
				CHANNEL-OP-ADD ; operation
				TRUE ; feather
				selection ; feather-radius
			)
			(gimp-rect-select
				image ; image
				(+ margegauche selection) ; xoffset
				(+ margehaut selection) ; yoffset
				(- newlargeurimage (* 2 selection)) ; width
				(- newhauteurimage (* 2 selection)) ; height
				CHANNEL-OP-SUBTRACT ; operation
				TRUE ; feather
				selection ; feather-radius
			)
			(plug-in-gauss
				1 ; run-mode
				image ; image
				drawable ; drawable
				2 ; horizontal radius
				2 ; vertical radius
				0 ; method
			)
			(gimp-selection-none image)
		)
	)

	; rotation
	(if (= rotation TRUE)
		(begin
			(gimp-context-set-foreground '(0 0 0))
			(gimp-context-set-background '(0 0 0))
			(define drawable (car (gimp-image-flatten image)))
			(gimp-drawable-transform-rotate-default
				drawable
				-0.012 ; angle radian
				TRUE ; auto-center
				0
				0
				TRUE ; interpolation
				FALSE ; clip-result
			)
			(gimp-image-flatten image)
		)
	)

	; construction d'une ombre de 1pt
	(if (= ombre TRUE)
		(begin
			;(gimp-message "ombre!")
			(gimp-context-set-background '(192 192 192))
			(define largeur (car (gimp-image-width image)))
			(define hauteur (car (gimp-image-height image)))
			(define largeurdecoupe (+ largeur 1))
			(define hauteurdecoupe (+ hauteur 1))
			(define xoffset 0)
			(define yoffset 0)
			(gimp-image-resize image largeurdecoupe hauteurdecoupe xoffset yoffset)
			(define drawable (car (gimp-image-flatten image)))
			(gimp-layer-resize-to-image-size drawable)
		)
	)

	; construction du filet de découpe de 1pt
	(if (= decoupe TRUE)
		(begin
			(gimp-context-set-background '(192 192 192))
			(define largeur (car (gimp-image-width image)))
			(define hauteur (car (gimp-image-height image)))
			(define largeurdecoupe (+ largeur 2))
			(define hauteurdecoupe (+ hauteur 2))
			(define xoffset 1)
			(define yoffset 1)
			(gimp-image-resize image largeurdecoupe hauteurdecoupe xoffset yoffset)
			(define drawable (car (gimp-image-flatten image)))
			(gimp-layer-resize-to-image-size drawable)
		)
	)

	; on applatit tout au cas où
	(gimp-image-flatten image)

	; resolution
	(gimp-image-set-resolution image resolution resolution)
		
	; fin groupe d'annulation
	(gimp-context-set-foreground premierplanorigine)
	(gimp-context-set-background arriereplanorigine)
	(gimp-undo-push-group-end image)

	; rafraichissement de l'affichage
	(gimp-displays-flush)

)

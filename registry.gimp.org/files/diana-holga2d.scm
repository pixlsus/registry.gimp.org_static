; diana-holga2d
; 21, 22, 23, 24, 25, 28, 29, 9 d'ecembre 2005, 12, 13, 15 mai 2006,
; 15, 18 mars 2007, 29 janvier, 3 juin, 10 octobre 2008, 27 juin 2010
; (c) Jean-Pierre Bucciol <jpsspam(at)free.fr>
; Published under GPL version 2


(script-fu-register "diana-holga2d"
	"<Image>/Filters/jp/diana-holga2d"

"This script-fu for The Gimp is a attempt to simulate the Diana/Holga Toys Cameras effect.
Last version can be found at:
http://www.vide.memoire.free.fr/photo/contrefacons/diana-holga2d.scm
Examples of use can be seen at:
http://www.vide.memoire.free.fr/photo/contrefacons/contrefacons2.php
A tutorial in French at:
http://www.vide.memoire.free.fr/photo/contrefacons/contrefacons.php

Ce script-fu pour The Gimp est une tentative pour simuler l'effet Toy Camera Diana/Holga.
La derni`ere version peut ^etre obtenue `a:
http://www.vide.memoire.free.fr/photo/contrefacons/diana-holga2d.scm
Des exemples d'utilisation peuvent être vus à: http://www.vide.memoire.free.fr/photo/contrefacons/contrefacons2.php
Une description à:
http://www.vide.memoire.free.fr/photo/contrefacons/contrefacons.php"

	"(c) Jean-Pierre Bucciol <jpsspam(at)free.fr>"
	"Published under GPL version 2"
	"June 27, 2010"
	"*"

	SF-IMAGE "Image" 0
	SF-DRAWABLE "Drawable" 0
	SF-TOGGLE "Square/Carre" TRUE
	SF-ADJUSTMENT "Blur/Flou" '(2 0 10 1 1 0 0)
	SF-ADJUSTMENT "Stretch/Etirement" '(2 0 5 1 1 0 0)
	SF-ADJUSTMENT "Zoom/Zoom" '(2 0 5 1 1 0 0)
	SF-ADJUSTMENT "Light/Lumiere" '(3 0 10 1 2 0 0)
	SF-ADJUSTMENT "Vignetting/Vignettage" '(80 0 100 1 10 0 0)
	SF-TOGGLE "Mask/Masque" TRUE
)

(define
	(diana-holga2d
		VarImage
		VarDrawable
		VarCarre
		VarFlou
		VarAngle
		VarZoom
		VarLumiere
		VarVignettage
		VarMasque
	)

	; d'ebut groupe d'annulation
	(gimp-undo-push-group-start VarImage)

	; r'ecup'eration des couleurs de fond et plume pour restauration `a la fin
	(define VarBackOrigine (car (gimp-context-get-background)))
	(define VarForeOrigine (car (gimp-context-get-foreground)))
	
	; mise au carr'e
	(if (= VarCarre TRUE)
		(begin
			(define largeur (car (gimp-image-width VarImage)))
			(define hauteur (car (gimp-image-height VarImage)))
			(if (> largeur hauteur)
				(begin
					(define newlargeur hauteur)
					(define newhauteur hauteur)
					(define xoffset (trunc (/  (- largeur newlargeur) 2)))
					(define yoffset 0)
				)
				(begin
					(define newlargeur largeur)
					(define newhauteur largeur)
					(define xoffset 0)
					(define yoffset (trunc (/  (- hauteur newhauteur) 2)))
				)
			)
			(gimp-image-crop
				VarImage
				newlargeur
				newhauteur
				xoffset
				yoffset
			)
			
		)
	)

	; variables
	(define VarReduction (/ 2 (+ 1 VarFlou)))
	(define VarLargeur (car (gimp-image-width VarImage)))
	(define VarHauteur (car (gimp-image-height VarImage)))
	(define VarCentreX (/ VarLargeur 2))
	(define VarCentreY (/ VarHauteur 2))
	(define VarDiagonale
		(sqrt (+ (* VarLargeur VarLargeur) (* VarHauteur VarHauteur)))
	)
	(define VarCalque (car (gimp-layer-new
		VarImage
		VarLargeur
		VarHauteur
		1 ; type
		"Calque" ; nom
		VarVignettage ; opacit'e
		MULTIPLY-MODE
	)))
	(define VarGris (car (gimp-drawable-is-gray VarDrawable)))
	(define VarMarge (* 0.008 VarDiagonale))
	(define VarCorAngle (/ VarAngle 100))
	(define VarCropLargeur (* VarCorAngle VarLargeur 2.7))
	(define VarCropHauteur (* VarCorAngle VarHauteur 2.7))
	(define VarCorBrouillage (* VarFlou 10))
	(define VarFakeArray (cons-array 256 'byte))
	(define VarZoomTrunc (trunc VarZoom))
	(define VarForeOrigine (car (gimp-context-get-foreground)))
	(define VarBackOrigine (car (gimp-context-get-background)))

	; conversion en rgb si gris
	(if (= VarGris TRUE)
		(begin
			(gimp-image-convert-rgb VarImage)
		)
	)

	; taille/flou
	(if (> VarFlou 0)
		(begin
			(gimp-image-scale VarImage (* VarReduction VarLargeur) (* VarReduction VarHauteur))
			(gimp-image-scale VarImage VarLargeur VarHauteur)
		)
	)

	; distorsion selon une courbe
	(if (> VarAngle 0)
		(begin
			(plug-in-curve-bend
				1 ; 1: run_mode
				VarImage ; 2: image
				VarDrawable ; 3: drawable
				0 ; 4: rotation
				TRUE ; 5: smoothing
				TRUE ; 6: antialias
				FALSE ; 7: work_on_copy
				0 ; 8: curve_type
				3 ; 9: argc_upper_point_x
				(float-array 0 0.5 1) ; 10: upper_point_x
				3 ; 11: argc_upper_point_y
				(float-array 0.5 (- 0.5 VarCorAngle) 0.5) ; 12: upper_point_y
				3 ; 13: argc_lower_point_x
				(float-array 0 0.5 1) ; 14: lower_point_x
				3 ; 15: argc_lower_point_y
				(float-array 0.5 (+ 0.5 VarCorAngle) 0.5) ; 16: lower_point_y
				256 ; 17: argc_upper_val_y
				VarFakeArray ; 18: upper_val_y
				256 ; 19: argc_lower_val_y
				VarFakeArray ; 20: lower_val_y
			)
			(plug-in-curve-bend
				1 ; 1: run_mode
				VarImage ; 2: image
				VarDrawable ; 3: drawable
				90 ; 4: rotation
				TRUE ; 5: smoothing
				TRUE ; 6: antialias
				FALSE ; 7: work_on_copy
				0 ; 8: curve_type
				3 ; 9: argc_upper_point_x
				(float-array 0 0.5 1) ; 10: upper_point_x
				3 ; 11: argc_upper_point_y
				(float-array 0.5 (- 0.5 VarCorAngle) 0.5) ; 12: upper_point_y
				3 ; 13: argc_lower_point_x
				(float-array 0 0.5 1) ; 14: lower_point_x
				3 ; 15: argc_lower_point_y
				(float-array 0.5 (+ 0.5 VarCorAngle) 0.5) ; 16: lower_point_y
				256 ; 17: argc_upper_val_y
				VarFakeArray ; 18: upper_val_y
				256 ; 19: argc_lower_val_y
				VarFakeArray ; 20: lower_val_y
			)
			(gimp-displays-flush)

			; d'ecoupage du surplus
			(if (= VarMasque TRUE)
				(begin
					(set! VarCropLargeur (* VarCropLargeur 0.5))
					(set! VarCropHauteur (* VarCropHauteur 0.5))
				)
			)
			(gimp-image-crop
				VarImage
				(- VarLargeur (* VarCropLargeur 2))
				(- VarHauteur (* VarCropHauteur 2))
				VarCropLargeur
				VarCropHauteur
			)
			(gimp-displays-flush)
		)
	)

	; flou zoom cin'etique
	(if (> VarZoomTrunc 0)
		(begin
			(plug-in-mblur
				1 ; run_mode (noninteractive=1)
				VarImage ; image
				VarDrawable ; drawable
				2 ; type (zoom=2)
				VarZoomTrunc ; length (longueur)
				0 ; angle (inutile avec zoom)
				VarCentreX ; center_x
				VarCentreY ; center_y
			)
			(gimp-displays-flush)
		)
	)

	; Luminosit'e
	(gimp-levels-stretch VarDrawable)
	(if (> VarLumiere 0)
		(begin
			(gimp-curves-spline VarDrawable 0 6 (spline VarLumiere))
		)
	)

	; Vignettage
	(if (> VarVignettage 0)
		(begin
			(gimp-context-set-foreground '(0 0 0))
			(gimp-context-set-background '(255 255 255))
			(gimp-image-add-layer VarImage VarCalque -1)
			(gimp-drawable-fill VarCalque WHITE-FILL)
			(gimp-edit-blend
				VarCalque; drawable
				FG-BG-RGB-MODE ; blend_mode
				NORMAL ; paint_mode
				GRADIENT-RADIAL ; gradient_type
				100 ; opacity
				35 ; offset
				REPEAT-NONE ; repeat
				TRUE ; reverse
				0 ; supersample
				0 ; max_depth
				0 ; threshold
				TRUE ; dither
				VarCentreX ; x1
				VarCentreY ; y1
				(* VarLargeur 0.9) ; x2
				(* VarHauteur 0.9) ; y2
			)
			(gimp-image-flatten VarImage)
			; WARNING: gimp-edit-blend change le Drawable !
			(set! VarDrawable (car (gimp-image-get-active-drawable VarImage)))
			(gimp-displays-flush)
		)
	)

	; marge/masque
	(if (= VarMasque TRUE)
		(begin
			(script-fu-fuzzy-border
				;0 ; run_mode indiqu'e par erreur dans le navigateur !
				VarImage ; image
				VarDrawable ; drawable
				'(0 0 0) ; color
				VarMarge ; value, taille
				TRUE ; toggle, bord flou
				5 ; value, granularit'e
				FALSE; toggle, ombre
				0 ; value; poid de l'ombre
				FALSE ; toggle, travail copie
				TRUE ; toggle; aplatir l'image
			)
			(gimp-image-flatten VarImage)
			; WARNING: script-fu-fuzzy-border change le Drawable !
			(set! VarDrawable (car (gimp-image-get-active-drawable VarImage)))
			(gimp-displays-flush)
		)
	)

	; conversion en gris si origine gris
	(if (= VarGris TRUE)
		(begin
			(gimp-image-convert-grayscale VarImage)
		)
	)

	; restauration des couleurs de fond et de plume
	(gimp-context-set-foreground VarForeOrigine)
	(gimp-context-set-background VarBackOrigine)

	; fin groupe d'annulation
	(gimp-undo-push-group-end VarImage)

	; rafraichissement de l'affichage
	(gimp-displays-flush)

)

(define float-array
	(lambda stuff
		(letrec ((kernel (lambda (array pos remainder)
			(if (null? remainder) array
				(begin
					(aset array pos (car remainder))
					(kernel array (+ pos 1) (cdr remainder))
				)
			))))
			(kernel (cons-array (length stuff) 'double) 0 stuff)
		)
	)
)

(define (set-pt a index x y)
	(begin
		(aset a (* index 2) x)
		(aset a (+ (* index 2) 1) y)
	)
)

(define (spline VarLumiere)
	(let*
		(
			(a (cons-array 6 'byte))
			(VarSplit (- 128 (* 12.8 VarLumiere)))
		)
		(set-pt a 0 0 0)
		(set-pt a 1 VarSplit (- 255 VarSplit))
		(set-pt a 2 255 255)
		a
	)
)

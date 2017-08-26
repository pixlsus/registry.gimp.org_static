;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; itext.scm
; Version 08.05.28 (For The Gimp 2.4) 28.05.2008
; Create a logo with an "Apple iMac logo" effect.
; Based in a tutorial: "iText tutorial" of
; Craig Marshall & Phil Harper. Published in
; http://gug.sunsite.dk/tutorials/itext2/
; 
;
; Copyright (C) 2008 Marcos Pereira (majpereira) <majpereira@hotmail.com>
;----------------------------------------------------------------------------------
; Baseado no tutorial:
;    This script was inspired by Craig Marshall's Tutorial - iText tutorial:
;    (C) 2004 Craig Marshall and Phil Harper
;    http://gug.sunsite.dk/tutorials/itext2/
;    Any comments? e-mail me at: majpereira@hotmail.com
;----------------------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;==================================================================================

(define (script-fu-itext-logo text font size border text-color bg-color bg? flat?)

  (let* (
	(old-fg-color (car (gimp-context-get-foreground)))
	(old-bg-color (car (gimp-context-get-background)))
	(img (car (gimp-image-new 256 256 0)))
	(tmp (gimp-palette-set-foreground text-color))
	(layer1-text (car (gimp-text-fontname img -1 0 0 text border TRUE size 0 font)))
	(width (car (gimp-drawable-width layer1-text)))
	(height (car (gimp-drawable-height layer1-text)))
	(layer0-bg (car (gimp-layer-new img width height 1 "Background" 100 0)))
	(layer2-refr1 (car (gimp-layer-new img width height 1 "Refract 1" 100 5)))
	(layer3-refr2 (car (gimp-layer-new img width height 1 "Refract 2" 100 5)))
	(layer4-refl (car (gimp-layer-new img width height 1 "Reflect" 50 4)))
	(layer5-blur (car (gimp-layer-new img width height 1 "Blur" 100 5)))
	(layer6-light (car (gimp-layer-new img width height 1 "Specular Light" 80 4)))
	(layer-mask 0)
	(flo 0)
	(flo2 0)

	; variáveis fixas
	(xygaus (* size (/ 20 402)))
	(gausblur (* size (/ 12 402)))
	(gausligh (* size (/ 3 402)))
	(shria (* size (/ 10 402)))
	(shrib (* size (/ 6 402)))
	(shric (* size (/ 3 402)))
	(third (/ height 3))
	(duothird (* 2 third))
	(halfwid (/ width 2))
	(halfhei (/ height 2))
	(ligh (* height 0.5))
	)

	(gimp-image-undo-disable img)
	(gimp-image-resize img width height 0 0)

	(gimp-image-add-layer img layer0-bg 1)
	(gimp-context-set-background bg-color)
	(if (eqv? bg? TRUE)
	(gimp-edit-clear layer0-bg)
	(gimp-edit-fill layer0-bg 1))
	(gimp-image-set-active-layer img layer1-text)
	(gimp-selection-layer-alpha layer1-text)
	(set! layer-mask (car (gimp-selection-save img)))
	(gimp-selection-none img)
	(plug-in-gauss-iir xygaus img layer-mask xygaus xygaus xygaus)
	(gimp-drawable-set-name layer-mask "layer-mask")

	(gimp-image-add-layer img layer2-refr1 -2)
	(gimp-edit-clear layer2-refr1)
	(gimp-image-set-active-layer img layer1-text)
	(gimp-selection-layer-alpha layer1-text)
	(gimp-selection-shrink img shria)
	(gimp-image-set-active-layer img layer2-refr1)
	(gimp-context-set-default-colors)
	(gimp-edit-fill layer2-refr1 1)
	(gimp-selection-none img)
	(plug-in-gauss-iir xygaus img layer2-refr1 xygaus xygaus xygaus)

	(gimp-image-add-layer img layer3-refr2 -3)
	(gimp-edit-clear layer3-refr2)
	(gimp-edit-copy layer2-refr1)
	(set! flo (car (gimp-edit-paste layer3-refr2 0)))
	(gimp-floating-sel-anchor flo)
	(gimp-selection-none img)
	(plug-in-gauss-iir xygaus img layer3-refr2 xygaus xygaus xygaus)

	(gimp-image-add-layer img layer4-refl -4)
	(gimp-edit-clear layer4-refl)
	(gimp-image-set-active-layer img layer1-text)
	(gimp-selection-layer-alpha layer1-text)
	(gimp-rect-select img 0 duothird width third 1 FALSE 0)
	(gimp-image-set-active-layer img layer4-refl)
	(gimp-selection-shrink img shrib)
	(gimp-edit-blend layer4-refl 0 2 0 100 1 0 FALSE FALSE 0 0 TRUE halfwid duothird halfwid 0)
	(gimp-selection-none img)

	(gimp-image-add-layer img layer5-blur -5)
	(gimp-edit-clear layer5-blur)
	(gimp-image-set-active-layer img layer1-text)
	(gimp-selection-layer-alpha layer1-text)
	(gimp-edit-fill layer5-blur 0)
	(gimp-image-set-active-layer img layer5-blur)
	(gimp-selection-shrink img shric)
	(gimp-edit-clear layer5-blur)
	(gimp-selection-none img)
	(plug-in-gauss-iir gausblur img layer5-blur gausblur gausblur gausblur)

	(gimp-image-add-layer img layer6-light -6)
	(gimp-edit-fill layer6-light 0)
	(plug-in-lighting
	1		;run_mode		Interactive (0), non-interactive (1)
	img		;image			Input image
	layer6-light	;drawable		Input drawable
	layer-mask	;bumpdrawable		Bumpmap drawable (set to 0 if disabled)
	0		;envdrawable		Environmentmap drawable (set to 0 if disabled)
	TRUE		;dobumpmap		Enable bumpmapping (TRUE/FALSE)
	FALSE		;doenvmap		Enable envmapping (TRUE/FALSE)
	0		;bumpmaptype		Type of mapping (0=linear,1=log, 2=sinusoidal, 3=spherical)
	1		;lighttype		Type of lightsource (0=point,1=directional,3=spot,4=none)
	'(255 255 255)	;lightcolor		Lightsource color (r,g,b)
	height		;lightposition_x	Lightsource position (x,y,z)
	(- height)	;lightposition_y	Lightsource position (x,y,z)
	ligh		;lightposition_z	Lightsource position (x,y,z)
	(* -2 height)	;lightdirection_x	Lightsource direction [x,y,z]
	(- height)	;lightdirection_y	Lightsource direction [x,y,z]
	ligh		;lightdirection_z	Lightsource direction [x,y,z]
	0		;ambient_intensity	Material ambient intensity (0..1)
	0.25		;diffuse_intensity	Material diffuse intensity (0..1)
	0		;diffuse_reflectivity	Material diffuse reflectivity (0..1)
	1		;specular_reflectivity	Material specular reflectivity (0..1)
	10		;highlight		Material highlight (0..->), note: it's expotential
	TRUE		;antialiasing		Apply antialiasing (TRUE/FALSE)
	FALSE		;newimage		Create a new image (TRUE/FALSE)
	FALSE		;transparentbackground	Make background transparent (TRUE/FALSE)
	)
	(plug-in-gauss-iir gausligh img layer6-light gausligh gausligh gausligh)

	(gimp-image-set-active-layer img layer1-text)
	(gimp-selection-layer-alpha layer1-text)
	(gimp-selection-invert img)
	(gimp-edit-clear layer5-blur)
	(gimp-edit-clear layer6-light)
	(gimp-selection-none img)

	(if (eqv? flat? TRUE)
		(gimp-drawable-set-name (car (gimp-image-flatten img)) text))

	(gimp-context-set-foreground old-fg-color)
	(gimp-context-set-background old-bg-color)
	(gimp-image-undo-enable img)
	(gimp-display-new img)
	
    )
)

(script-fu-register "script-fu-itext-logo"
  _"itext..."
  _"(en) Create a logo with an 'Apple iMac logo' effect. \
(pt-BR) Cria um logo com um efeito 'Apple iMac logo'"
  "Marcos Pereira <majpereira@hotmail.com>"
  "Marcos Pereira (majpereira)"
  "28.05.2008"
  ""
  SF-STRING     _"Text"                   "itext"
  SF-FONT       _"Font"                   "Nimbus Roman No9 L" ; linux
  SF-ADJUSTMENT _"Font size (pixels)"     '(200 5 1000 1 10 0 1)
  SF-ADJUSTMENT _"Border (pixels)"        '(0 0 1000 1 10 0 1)
  SF-COLOR      _"Text Color"             '(102 153 0) ; #669900 - verde oliva
  SF-COLOR      _"Background Color"       '(255 255 255)
  SF-TOGGLE     _"Background Transparent" TRUE
  SF-TOGGLE     _"Flatten"                TRUE
)

(script-fu-menu-register "script-fu-itext-logo"
                         "<Toolbox>/Xtns/Logos")

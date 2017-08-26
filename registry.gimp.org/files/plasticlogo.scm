;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; plasticlogo.scm
; Version 0.8.1 (For The Gimp 2.4)
; A Script-Fu that create a Polished Plastic Text or Shape
;
; Copyright (C) 2004-2008 Denis Bodor <lefinnois@lefinnois.net>
; All rights reserved.
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;     * Neither the name of the University of California, Berkeley nor the
;       names of its contributors may be used to endorse or promote products
;       derived from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-plastic-logo-effect	img
					basetext
					back-color
					type
					color
					pattern
					gradient
					direction)
  (let* ((width (car (gimp-drawable-width basetext)))
	 (height (car (gimp-drawable-height basetext)))
	 (fond (car (gimp-layer-new   img
				      width height RGB-IMAGE
				      "Background" 100 NORMAL-MODE)))
	 (olight (car (gimp-layer-new img
				      width height RGBA-IMAGE
				      "Light Outline" 90 SCREEN-MODE)))
	 (border (car (gimp-layer-new img
				      width height RGBA-IMAGE
				      "Border" 100 NORMAL-MODE)))
	 (refl (car (gimp-layer-new   img
				      width height RGBA-IMAGE
				      "Refl" 67 NORMAL-MODE)))
	 (mapeux (car (gimp-layer-new img
				      width height RGBA-IMAGE
				      "Mapper" 100 NORMAL-MODE)))
	 (shad (car (gimp-layer-new   img
				      width height RGBA-IMAGE
				      "Shadow" 100 NORMAL-MODE)))
	(chantext)
	(basetextmask)
	(xdest)
	(ydest)
	(reflmask)
	)
 

    (gimp-context-push)

    ; filling back with background
    (gimp-context-set-background back-color)
    (gimp-selection-none img)
    (script-fu-util-image-resize-from-layer img basetext)
    (gimp-image-add-layer img fond 1)
    (gimp-edit-clear fond)
    
    ; composite text and channel
    (gimp-selection-layer-alpha basetext)
    (set! chantext (car (gimp-selection-save img)))
    (set! basetextmask (car (gimp-layer-create-mask basetext ADD-ALPHA-MASK)))
    (gimp-layer-add-mask basetext basetextmask)
    (gimp-selection-all img)
    ; choose gradient destination
    (if (= direction 0)
      (begin
	(set! xdest 0)
	(set! ydest (car (gimp-drawable-height basetext)))
      ))
    (if (= direction 1)
      (begin
	(set! xdest (car (gimp-drawable-width basetext)))
	(set! ydest 0)
      ))
    (if (= direction 2)
      (begin
	(set! xdest (car (gimp-drawable-width basetext)))
	(set! ydest (car (gimp-drawable-height basetext)))
      ))
    (if (= type 0)
      (begin
	(gimp-context-set-foreground color)
	(gimp-edit-fill basetext FOREGROUND-FILL)))
    (if (= type 1) 
      (begin
	(gimp-context-set-pattern pattern)
	(gimp-edit-bucket-fill basetext PATTERN-BUCKET-FILL 0 100 0 0 1 1)))
    (if (= type 2) 
      (begin
	(gimp-context-set-gradient gradient)
	(gimp-edit-blend basetext 
				    CUSTOM-MODE		;  
				    NORMAL-MODE		; 
				    GRADIENT-LINEAR	; gradient type
				    100			; opacity
				    0			; offset
				    REPEAT-NONE		; repeat
				    FALSE		; reverse
				    FALSE		; supersampling
				    0 0			; 
				    FALSE		; dithering
				    0 0			; x1 y1
				    xdest		; x2
				    ydest		; y2
				    )))
    
    ; Adding light effect on edge
    (gimp-selection-none img)
    (gimp-image-add-layer img olight 0)
    (gimp-edit-clear olight)
    (gimp-selection-load chantext)
    (gimp-selection-shrink img 3)
    (gimp-selection-invert img)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-edit-fill olight FOREGROUND-FILL)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img olight 18 18)
    (gimp-selection-load chantext)
    (gimp-selection-invert img)
    (gimp-edit-cut olight)

    ; creating black border
    (gimp-image-add-layer img border -1)
    (gimp-edit-clear border)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-selection-load chantext)
    (gimp-edit-fill border FOREGROUND-FILL)
    (gimp-selection-shrink img 1)
    (gimp-edit-cut border)

    ; adding light reflect
    (gimp-image-add-layer img refl -1)
    (gimp-edit-clear refl)
    (gimp-ellipse-select img 
			 (- 0 (/ width 2))
			 (- 0 height)
			 (* width 2)
			 (* height 1.54)
			 2 
			 TRUE
			 0
			 0)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-edit-blend refl
		     FG-TRANSPARENT-MODE
		     NORMAL-MODE
		     GRADIENT-LINEAR
		     100
		     0
		     REPEAT-NONE
		     FALSE
		     FALSE
		     0
		     0
		     FALSE
		     (/ width 2)
		     (* height 0.54)
		     (/ width 2)
		     (* height 0.05))
    (gimp-selection-load chantext)
    (set! reflmask (car (gimp-layer-create-mask refl ADD-SELECTION-MASK)))
    (gimp-layer-add-mask refl reflmask)

    ; creating bumpmap map
    (gimp-image-add-layer img mapeux -1)
    (gimp-edit-clear mapeux)
    (gimp-selection-none img)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill mapeux FOREGROUND-FILL)
    (gimp-selection-load chantext)
    (gimp-selection-shrink img 5)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-edit-fill mapeux FOREGROUND-FILL)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img mapeux 18 18)

    ; bumpmapping:displacing reflect to follow shape
    (plug-in-displace 1
		      img
		      refl
		      1.5
		      1.5
		      TRUE
		      TRUE
		      mapeux
		      mapeux
		      0)
    (gimp-image-remove-layer img mapeux)    
    
    ; back shadow
    (gimp-image-add-layer img shad 4)
    (gimp-edit-clear shad)
    (gimp-selection-load chantext)
    (gimp-selection-translate img 0 12)
    (gimp-context-set-foreground '(50 50 50))
    (gimp-edit-fill shad FOREGROUND-FILL)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img shad 15 15)
    
    ; correcting resizing effect on background
    (gimp-context-set-foreground back-color)
    (gimp-layer-resize-to-image-size fond)
    (gimp-edit-fill fond FOREGROUND-FILL)
    
    (gimp-context-pop)))



(define (script-fu-plastic-logo-alpha img
				      text-layer
				      fond-color
				      type
				      color
				      pattern
				      gradient
				      direction
				      )
  (begin
    (gimp-image-undo-disable img)
    (apply-plastic-logo-effect img text-layer fond-color type color pattern gradient direction)
    (gimp-image-undo-enable img)
    (gimp-displays-flush)))



(script-fu-register 	"script-fu-plastic-logo-alpha"
			"Polished Plastic Fu..."
			"Create a polished plastic logo"
			"Denis Bodor <lefinnois@lefinnois.net>"
			"Denis Bodor"
			"03/31/2005"
			""
			SF-IMAGE	"Image"			0
			SF-DRAWABLE	"Drawable"		0
			SF-COLOR "Background" '(255 255 255)
			SF-OPTION "Fill with" '("Color"
						"Pattern"
						"Gradient")
			SF-COLOR "Color" '(255 0 255)
			SF-PATTERN "Pattern" "Wood"
			SF-GRADIENT "Gradient" "Golden"
			SF-OPTION "Direction" '("Vertical"
						"Horizontal"
						"Diagonal")
			)

(script-fu-menu-register "script-fu-plastic-logo-alpha"
			 "<Image>/Filters/Alpha to Logo")

(define (script-fu-plastic-logo		font
					text
					fond-color
					size
					type		; Color=0 Pattern=1 Gradient=2
					color
					pattern
					gradient
					direction
					)
  
  (let* ((img (car (gimp-image-new 256 256 RGB)))	; nouvelle image -> img
	 (border (/ size 4))
	 (text-layer (car (gimp-text-fontname img
					      -1 0 0 text border TRUE 
					      size PIXELS font)))
	 (width (car (gimp-drawable-width text-layer)))
	 (height (car (gimp-drawable-height text-layer)))
	 )
    
    (gimp-image-undo-disable img)
    (gimp-drawable-set-name text-layer text)
    (apply-plastic-logo-effect img text-layer fond-color type color pattern gradient direction)
    (gimp-image-undo-enable img)
    (gimp-display-new img)    
    ))


(script-fu-register 	"script-fu-plastic-logo"
			"Polished Plastic Fu"
			"Create a polished plastic logo"
			"Denis Bodor <lefinnois@lefinnois.net>"
			"Denis Bodor"
			"03/31/2005"
			""
			SF-FONT "Font Name" "Blippo Heavy"
			SF-STRING "Enter your text" "PLASTIC FUN"
			SF-COLOR "Background" '(255 255 255)
			SF-ADJUSTMENT "Font size (pixels)" '(150 2 1000 1 10 0 1)
			SF-OPTION "Fill with" '("Color"
						"Pattern"
						"Gradient")
			SF-COLOR "Color" '(255 0 255)
			SF-PATTERN "Pattern" "Wood"
			SF-GRADIENT "Gradient" "Golden"
			SF-OPTION "Direction" '("Vertical"
						"Horizontal"
						"Diagonal")
			)

(script-fu-menu-register "script-fu-plastic-logo"
			 "<Toolbox>/Xtns/Logos")

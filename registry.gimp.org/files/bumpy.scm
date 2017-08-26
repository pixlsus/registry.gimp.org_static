;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; bumpy.scm
; Version 0.3 (For The Gimp 2.0 and 2.2 and 2.4)
; A Script-Fu that create a bumpmapped Text or Shape
;
; Copyright (C) 2005-2008 Denis Bodor <lefinnois@lefinnois.net>
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
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (apply-bumpy-logo-effect	img
					basetext
					text-color
					boolrippleh
					boolripplev
				        bnoise
				        bplasma
					)

  (let* ((width (car (gimp-drawable-width basetext)))
	 (height (car (gimp-drawable-height basetext)))

	 (fond (car (gimp-layer-new   img
				      width height RGBA-IMAGE
				      "Background" 100 NORMAL-MODE)))
	 (damap (car (gimp-layer-new  img
				      width height RGB-IMAGE
				      "Map" 100 NORMAL-MODE)))
	 (innermap (car (gimp-layer-new  img
				      width height RGB-IMAGE
				      "iMap" 100 NORMAL-MODE)))
	 (chantext)
	 (masktext)
	 )
 

    (gimp-context-push)

    ; filling back with background
    (gimp-context-set-background '(255 255 255))
    (gimp-selection-none img)
    (script-fu-util-image-resize-from-layer img basetext)
    (gimp-image-add-layer img fond 1)
    (gimp-edit-clear fond)
    
    ; correcting resizing effect on background
    (gimp-context-set-foreground '(255 255 255))
    (gimp-layer-resize-to-image-size fond)
    (gimp-edit-fill fond FOREGROUND-FILL)

    ;(gimp-message (number->string width))
    ;(gimp-message (number->string height))

    ; waving/rippling the text
    (if (= boolrippleh TRUE) (plug-in-ripple 1 img basetext 26 2 0 0 0 TRUE FALSE)) ; Horiz
    (if (= boolripplev TRUE) (plug-in-ripple 1 img basetext 26 2 1 0 0 TRUE FALSE)) ; Vert
    (plug-in-gauss-rle2 1 img basetext 1 1)
       
    ; save se selection
    (gimp-selection-layer-alpha basetext)
    (set! chantext (car (gimp-selection-save img)))
    (gimp-selection-none img)

    ; creating map
    (gimp-image-add-layer img damap 1)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-edit-fill damap FOREGROUND-FILL)

    (gimp-selection-load chantext)
    (gimp-selection-grow img 15)
    (gimp-selection-invert img)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill damap FOREGROUND-FILL)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img damap 27 27)
    (gimp-selection-load chantext)
    (gimp-edit-fill damap FOREGROUND-FILL)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img damap 2 2)
  
    (gimp-context-set-foreground '(128 128 128))
    (gimp-selection-all img)
    (gimp-edit-fill fond FOREGROUND-FILL)
    (gimp-selection-none img)
    
    (if (= bplasma TRUE) (plug-in-plasma 1 img fond 0 1.0) (gimp-desaturate fond))
    (if (= bnoise TRUE)  (plug-in-noisify 1 img fond 1 0.2 0.2 0.2 0))

    (gimp-desaturate fond)

    ; apply bumpmap
    (plug-in-bump-map 1
		      img
		      fond
		      damap
		      135
		      42 ; elevation
		      33
		      0
		      0
		      0
		      0
		      1
		      0
		      LINEAR)

    ; creating second map (inner shape)
    (gimp-image-add-layer img innermap 1)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-edit-fill innermap FOREGROUND-FILL)
    (gimp-selection-load chantext)
    (gimp-selection-shrink img 3)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill innermap FOREGROUND-FILL)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img innermap 6 6)
    
    (gimp-context-set-foreground text-color)
    (gimp-edit-fill basetext FOREGROUND-FILL)
    (plug-in-bump-map 1
		      img
		      basetext
		      innermap
		      135
		      32
		      5
		      0
		      0
		      0
		      0
		      1
		      1
		      LINEAR)
    (gimp-selection-load chantext)
    (gimp-selection-shrink img 2)
    (set! masktext (car (gimp-layer-create-mask basetext ADD-SELECTION-MASK)))
    (gimp-layer-add-mask basetext masktext)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img masktext 1 1)

    (gimp-image-raise-layer img fond)
    (gimp-image-raise-layer img fond)
    
    (gimp-context-pop)))



(define (script-fu-bumpy-logo-alpha   img
				      text-layer
				      text-color
				      boolrippleh
				      boolripplev
				      bnoise
				      bplasma
				      )
  (begin
    (gimp-image-undo-disable img)
    (apply-bumpy-logo-effect img text-layer text-color boolrippleh boolripplev bnoise bplasma)
    (gimp-image-undo-enable img)
    (gimp-displays-flush)))

(gimp-message-set-handler 1)

(script-fu-register 	"script-fu-bumpy-logo-alpha"
			"Bumpy..."
			"Create a bumpmapped logo"
			"Denis Bodor <lefinnois@lefinnois.net>"
			"Denis Bodor"
			"05/14/2005"
			""
			SF-IMAGE	"Image"			0
			SF-DRAWABLE	"Drawable"		0
			SF-COLOR 	"Shape Color" '(200 200 40)
			SF-TOGGLE	"Ripple Horiz." TRUE
			SF-TOGGLE	"Ripple Vert."  TRUE
			SF-TOGGLE 	"Back Noise" TRUE
			SF-TOGGLE 	"Back Plasma." TRUE
			)

(script-fu-menu-register "script-fu-bumpy-logo-alpha"
			 "<Image>/Filters/Alpha to Logo")

(define (script-fu-bumpy-logo		font
					text
					text-color
					boolrippleh
					boolripplev
					bnoise
					bplasma
					size
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
    (apply-bumpy-logo-effect img text-layer text-color boolrippleh boolripplev bnoise bplasma)
    (gimp-image-undo-enable img)
    (gimp-display-new img)    
    ))


(script-fu-register 	"script-fu-bumpy-logo"
			"Bumpy"
			"Create a bumpmapped logo"
			"Denis Bodor <lefinnois@lefinnois.net>"
			"Denis Bodor"
			"03/31/2005"
			""
			SF-FONT "Font Name" "Blippo Heavy"
			SF-STRING "Enter your text" "BUMPY"
			SF-COLOR "Text Color" '(200 200 40)
			SF-TOGGLE "Ripple Horiz." TRUE
			SF-TOGGLE "Ripple Vert." TRUE
			SF-TOGGLE "Back Noise" TRUE
			SF-TOGGLE "Back Plasma." TRUE
			SF-ADJUSTMENT "Font size (pixels)" '(150 2 1000 1 10 0 1))

(script-fu-menu-register "script-fu-bumpy-logo"
			 "<Toolbox>/Xtns/Logos")

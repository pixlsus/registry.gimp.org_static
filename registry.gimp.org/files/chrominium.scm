;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; chrominium.scm
; Version 0.2.1 (For The Gimp 2.0 and 2.2 and 2.4)
; A Script-Fu that create a Chrome effect with gradient shadow
;
; Copyright (C) 2007-2008 Denis Bodor <lefinnois@lefinnois.net>
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

(define (apply-chrominium-logo-effect img
				      basetext
				      bgradient
				      gradient
				      color
				      bg-color
				      bsparkles)
  ; compatibility hack
  (define (set-pt a index x y)
    (begin
      (aset a (* index 2) x)
      (aset a (+ (* index 2) 1) y)))

  (define (chrome-spline1)
    (let* ((a (cons-array 12 'byte)))
      (set-pt a 0 0 0)
      (set-pt a 1 63 254)
      (set-pt a 2 124 59)
      (set-pt a 3 170 255)
      (set-pt a 4 220 125)
      (set-pt a 5 255 255)
      a))


  (let* ((width (car (gimp-drawable-width basetext)))
	 (height (car (gimp-drawable-height basetext)))
	 (fond (car (gimp-layer-new   img
				      width height RGB-IMAGE
				      "Background" 100 NORMAL-MODE)))
	 (mapeux (car (gimp-layer-new img
				      width height RGBA-IMAGE
				      "Mapper" 100 NORMAL-MODE)))
	 (shad (car (gimp-layer-new   img
				      width height RGBA-IMAGE
				      "Shadow" 100 NORMAL-MODE)))
	 (spark (car (gimp-layer-new  img
				      width height RGBA-IMAGE
				      "Sparkle" 100 NORMAL-MODE)))
	 (XXbmapshrink (/ height 75))	; blur size for bumpmap 175 ?
	 (XXbmapblur (/ height 25))	; blur size for bumpmap 45 ?
	 (XXshadgrow (/ height 62))	; blur size for bumpmap
	 (XXshadblur (/ height 17))	; blur size for bumpmap
	 (XXsparklen (/ height 12))	; blur size for bumpmap
	 (chantext)
	 (basetextmask)
	 (shadmask)
	 (sparkmask)
	 )

    (gimp-context-push)

    (gimp-image-add-layer img shad 2)
    (gimp-edit-clear shad)

    (if(= bsparkles TRUE)
      (begin
	(gimp-image-add-layer img spark 3)
	(gimp-edit-clear spark)
	)
      )

;    (if(= bsparkles FALSE)
;      (begin
;	(set! XXshadgrow (* XXshadgrow 1.5))
;	(set! XXshadblur (* XXshadblur 1.1))
;	)
;      )

    (gimp-image-add-layer img fond 4)
    (gimp-edit-clear fond)
    (gimp-image-add-layer img mapeux 5)
    (gimp-edit-clear mapeux)

    ; filling back with background
    (gimp-context-set-foreground bg-color)
    (gimp-edit-fill fond FOREGROUND-FILL)
    (gimp-selection-none img)

    (script-fu-util-image-resize-from-layer img basetext)

    ; create channel for text
    (gimp-selection-layer-alpha basetext)
    (set! chantext (car (gimp-selection-save img)))
    (set! basetextmask (car (gimp-layer-create-mask basetext ADD-ALPHA-MASK)))
    (gimp-layer-add-mask basetext basetextmask)
    (gimp-selection-all img)
    (gimp-context-set-foreground '(200 200 200))
    (gimp-edit-fill basetext FOREGROUND-FILL)

    ; create bumpmap
    (gimp-selection-none img)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-edit-fill mapeux FOREGROUND-FILL)
    (gimp-selection-load chantext)
    (gimp-selection-shrink img XXbmapshrink) ; 4
    (gimp-context-set-foreground '(255 255 255))
    (gimp-edit-fill mapeux FOREGROUND-FILL)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img mapeux XXbmapblur XXbmapblur) ; 10 10

    ; bumpmapping
    (plug-in-bump-map 1
		      img
		      basetext
		      mapeux
		      135.00	; azimuth
		      45.00	; elevation
		      8		; depth
		      0		; x offset
		      0		; y offset
		      0		; water level
		      0		; ambient
		      TRUE	; compensate darky
		      FALSE	; invert
		      0		; TYPE 0=LINEAR, 1=SPHERICAL, 2=SINUS
		      )

    ; curving to gain chrome effect
    (gimp-curves-spline basetext 
			0 		; channel to modify 0=VALUE
			12 		; nbr of values (point*2)
			(chrome-spline1); array of points
			)

    ; back shadow
    (if (= bgradient TRUE)
      (begin
	(gimp-context-set-gradient gradient)
	(gimp-edit-blend shad 
			 CUSTOM-MODE
			 NORMAL-MODE
			 GRADIENT-LINEAR	; gradient type
			 100		; opacity
			 0			; offset
			 REPEAT-NONE	; repeat
			 FALSE		; reverse
			 FALSE		; supersampling
			 0 0		; 
			 FALSE		; dithering
			 0 0		; x1 y1
			 width		; x2
			 height		; y2
			 )
	))
    (if (= bgradient FALSE)
      (begin
	(gimp-context-set-foreground color)
	(gimp-edit-fill shad FOREGROUND-FILL)
      ))
    (gimp-selection-load chantext)
    (gimp-selection-grow img XXshadgrow) ; 5
    (set! shadmask (car (gimp-layer-create-mask shad 4))) ; 4=SELECTION MASK
    (gimp-layer-add-mask shad shadmask)
    (gimp-selection-none img)
    (plug-in-gauss-rle2 1 img shadmask XXshadblur XXshadblur)

    ; back shadow sparkle
    (if(= bsparkles TRUE)
      (begin
	(if (= bgradient TRUE)
	  (begin
	    (gimp-context-set-gradient gradient)
	    (gimp-edit-blend spark 
			     CUSTOM-MODE
			     NORMAL-MODE
			     GRADIENT-LINEAR	; gradient type
			     100		; opacity
			     0			; offset
			     REPEAT-NONE	; repeat
			     FALSE		; reverse
			     FALSE		; supersampling
			     0 0		; 
			     FALSE		; dithering
			     0 0		; x1 y1
			     width		; x2
			     height		; y2
			     )
	    ))
	(if (= bgradient FALSE)
	  (begin
	    (gimp-context-set-foreground color)
	    (gimp-edit-fill spark FOREGROUND-FILL)
	    ))
	(set! sparkmask (car (gimp-layer-create-mask spark 1))) ; 1=BLACK MASK
	(gimp-layer-add-mask spark sparkmask)
	(gimp-selection-load chantext)
	(gimp-selection-grow img XXshadgrow) ; 5
	(plug-in-randomize-hurl	RUN-NONINTERACTIVE
				img
				sparkmask
				50		; randomization percentage
				1		; repeat
				FALSE		; random seed
				10		; seed (graine)
				)
	(gimp-selection-none img)
	(gimp-threshold	sparkmask
			253	; low
			255	; high
			)

	(plug-in-sparkle	RUN-NONINTERACTIVE
				img
				sparkmask
				0.001		; lum
				0.50		; intensity
				XXsparklen	; spike len ; 20
				4		; spike nbr
				15		; angle
				1.00		; density
				0		; opacity
				0		; random hue
				0		; random saturation
				FALSE		; preserve lum
				FALSE		; invert
				FALSE		; add border
				0		; color type 0=NATURAL
				)

	(gimp-context-pop))
      )
    )
  )


(define (script-fu-chrominium-logo-alpha img
				      basetext
				      bgradient
				      gradient
				      color
				      bg-color
				      bsparkles)
  (begin
    (gimp-image-undo-group-start img)
    (apply-chrominium-logo-effect img basetext bgradient gradient color bg-color bsparkles)
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)))

(script-fu-register "script-fu-chrominium-logo-alpha"
		    _"Chrominium..."
		    "Chrominium logo"
		    "Denis bodor"
		    "Denis bodor"
		    "2007"
		    "RGBA"
                    SF-IMAGE      "Image"                     0
                    SF-DRAWABLE   "Drawable"                  0
		    SF-TOGGLE     _"Gradient Fill"       TRUE
		    SF-GRADIENT   _"Gradient"            "Full saturation spectrum CCW"
		    SF-COLOR	  _"Color Fill"		'(0 250 0)
		    SF-COLOR      _"Background color"   '(0 0 0)
		    SF-TOGGLE	  _"Sparkles"		TRUE
		    )

(script-fu-menu-register "script-fu-chrominium-logo-alpha"
			 _"<Image>/Filters/Alpha to Logo")


(define (script-fu-chrominium-logo text
				   font
				   size
				   bgradient
				   gradient
				   color
				   bg-color
				   bsparkles)
  (let* ((img (car (gimp-image-new 256 256 RGB)))
	 (border (/ size 4))
	 (basetext (car (gimp-text-fontname   img 
					      -1 0 0 text border TRUE 
					      size PIXELS font))))
    (gimp-image-undo-disable img)
    (apply-chrominium-logo-effect img basetext bgradient gradient color bg-color bsparkles)
    (gimp-image-undo-enable img)
    (gimp-display-new img)))

(script-fu-register "script-fu-chrominium-logo"
		    _"Chrominium..."
		    "Chrominium logo"
		    "Denis bodor"
		    "Denis bodor"
		    "2007"
		    ""
		    SF-STRING     _"Text"               "Chrome"
		    SF-FONT       _"Font"               "Ethnocentric"
		    SF-ADJUSTMENT _"Font size (pixels)" '(150 2 1000 1 10 0 1)
		    SF-TOGGLE     _"Gradient Fill"       TRUE
		    SF-GRADIENT	  _"Gradient" 		"Full saturation spectrum CCW"
		    SF-COLOR	  _"Color Fill"		'(0 250 0)
		    SF-COLOR      _"Background color"   '(0 0 0)
		    SF-TOGGLE	  _"Sparkles"		TRUE
                    )

(script-fu-menu-register "script-fu-chrominium-logo"
			 _"<Toolbox>/Xtns/Logos")

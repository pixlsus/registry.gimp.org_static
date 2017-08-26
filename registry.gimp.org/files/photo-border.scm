; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; Copyright (C) 2009 Egil Kvaleberg <egil@kvaleberg.no>
;
(script-fu-register 
    "script-fu-photo-border"
   _"_Photo Border..."
    "Add white border to photo. \
The extra lower border is for a 'Polaroid' effect, in which case a caption may also be added.
A drop shadow further adds the effect of a photo placed on a background.

Use 'Lomo...' or 'Old Photo...' to distort the photo itself.
"
    "Egil Kvaleberg"
    "copyright 2009, Egil Kvaleberg"
    "4 May, 2009"
    "RGB* GRAY*"
    SF-IMAGE    "Image" 0
    SF-DRAWABLE "Layer" 0
				    ; value lower upper step page digits type
    SF-ADJUSTMENT _"Border (%)"            '(7.0 0 20 0.1 1.0 1 0)
    SF-ADJUSTMENT _"Extra Lower Border (0 for none) (%)" '(21.0 0 100 0.1 1.0 1 0)
    SF-ADJUSTMENT _"Drop Shadow (0 to disable) (%)"  '(3.0 0 50 0.1 1.0 1 0)
    SF-COLOR   _"Border Color" '(250 250 240)
    SF-ADJUSTMENT _"Border Shading Feather (0 for none) (%)" '(10.0 0 50 0.1 1.0 1 0)
    SF-ADJUSTMENT _"Border Shading Transparency (%)" '(25.0 0 100 0.5 5.0 1 0)
    SF-COLOR   _"Border Shading Color" '(0 0 0)
    SF-STRING  _"Caption Text (needs lower border)" ""
    SF-FONT    _"Caption Font" (if (= 0 (car (gimp-fonts-get-list "Comic"))) "Serif Bold" (caadr (gimp-fonts-get-list "Comic")))
    SF-ADJUSTMENT _"Caption Height (%)"    '(8.0 0 50 0.1 1.0 1 0)
)

(script-fu-menu-register 
    "script-fu-photo-border"
    "<Toolbox>/Filters/Decor"
)

(define (script-fu-photo-border image photo-layer border lower shadow border-color
	 border-shading-feather border-shading-transparency border-shading-color
	 text-string text-font text-height)
    (define w (car (gimp-image-width image)))
    (define h (car (gimp-image-height image)))
  ; (define fg (car (gimp-context-get-foreground)))
    (define th (/ (* h text-height) 100))
    (define bw (/ (* w border) 100))
    (define lh (/ (* w lower) 100))
    (define sw (/ (* w shadow) 100))
    (define bsw (/ (* w border) 400))
    (define bsf (/ (* w border-shading-feather) 100))
    (define with-border-width (+ bw w bw))
    (define with-border-height (+ bw h bw lh))

    (define text-layer #f)
    (define border-shading-layer #f)

    (gimp-context-push)
    (gimp-image-undo-group-start image)

    (gimp-layer-add-alpha photo-layer)

    ;make room for border
    (gimp-image-resize image with-border-width with-border-height bw bw)

    (define border-layer (car 
	(gimp-layer-new image with-border-width with-border-height 0 "Photo-Border" 100 NORMAL)
    ))
    (gimp-image-add-layer image border-layer 1)
    (gimp-context-set-foreground border-color)
    (gimp-drawable-fill border-layer 0) 

    (unless (= border-shading-feather 0)
	(set! border-shading-layer (car
	    (gimp-layer-new image with-border-width with-border-height 0 "Photo-Border-Shadow"
						border-shading-transparency NORMAL)
	))
	(gimp-image-add-layer image border-shading-layer -1)
	(gimp-context-set-foreground border-shading-color)
	(gimp-drawable-fill border-shading-layer 0)
	(gimp-image-set-active-layer image border-shading-layer)
	(gimp-rect-select image bsw bsw
			    (- with-border-width (* 2 bsw)) (- with-border-height (* 2 bsw))
			    CHANNEL-OP-REPLACE
			    TRUE ;feather
			    bsf ;feather-blur
	)
	(gimp-edit-clear border-shading-layer)
	(gimp-selection-none image)
	(gimp-image-set-active-layer image photo-layer)
    )

    (unless (= lh 0)
        ; Polaroid style has shadow around frame (not in the same plane)
	(script-fu-drop-shadow image photo-layer 0 0 (/ bw 5) '(0 0 0) 80 1)
    )    
    (unless (= sw 0)
	(script-fu-drop-shadow image border-layer (/ sw 4) (/ sw 4) sw '(0 0 0) 80 1)
    )

    (unless (= (string-length text-string) 0)
	(set! text-layer 
	    (gimp-text-fontname image border-layer bw (+ bw h (/ (- (+ bw lh) th) 2)) text-string 0 TRUE th PIXELS text-font)
	)
    )
  ; (gimp-context-set-foreground fg)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
    (gimp-context-pop)
)



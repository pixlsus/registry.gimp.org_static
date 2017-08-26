; alien aura logo
; Copyright (C) 2002-2003 Michael Schalla
; Copyright (C) 2004-2008 Eric Lamarque

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
; --------------------------------------------------------------------
; version 1.0 by Michael Schalla 2003/02/17
; version 2.0 by Eric Lamarque 2004/08/20
; version 2.4 by Eric Lamarque 2007/05/17
; version 2.4.1 by Eric Lamarque 2008/11/03 - License Fix + script register
; --------------------------------------------------------------------
;

(define (script-fu-alien-aura-logo inText inFont inFontSize inAuraX inAuraY inSpread inGrow inFeather inBackGroundColor inAuraColor1 inAuraColor2 inTextColor inAbsolute inImageWidth inImageHeight inFlatten)
  (let*
    (
      ; Definition unserer lokalen Variablen

      ; Erzeugen des neuen Bildes

      (img  ( car (gimp-image-new 10 10 RGB) ) )
      (theText)
      (theTextWidth)
      (theTextHeight)
      (imgWidth)
      (imgHeight)
      (theRand)
      (theBufferX)
      (theBufferY)
      (ellWidth)
      (ellHeight)
      (ellBufferX)
      (ellBufferY)

      ; Erzeugen einer neuen Ebene zum Bild
      (theLayer (car (gimp-layer-new img 10 10 RGB-IMAGE "Ebene 1" 100 NORMAL) ) )
      (theAura1Layer (car (gimp-layer-new img 10 10 RGBA-IMAGE "Aura 1" 100 NORMAL) ) )
      (theAura2Layer (car (gimp-layer-new img 10 10 RGBA-IMAGE "Aura 2" 100 NORMAL) ) )
      (theTextLayer (car (gimp-layer-new img 10 10 RGBA-IMAGE "Text" 100 NORMAL) ) )

      (old-fg (car (gimp-context-get-foreground) ) )
      (old-bg (car (gimp-context-get-background) ) )
      ; Ende unserer lokalen Variablen
    )

    (gimp-image-add-layer  img theLayer 0)
    (gimp-image-add-layer  img theAura1Layer 0)
    (gimp-image-add-layer  img theAura2Layer 0)
    (gimp-image-add-layer  img theTextLayer 0)

    ; zum Anzeigen des leeren Bildes
    ; (gimp-display-new img)

    (gimp-context-set-background inBackGroundColor )
    (gimp-context-set-foreground inTextColor)

    (gimp-selection-all  img)
    (gimp-edit-clear     theLayer)
    (gimp-selection-none img)

    (set! theText (car (gimp-text-fontname img theLayer 0 0 inText 0 TRUE inFontSize PIXELS inFont)))

    (set! theTextWidth  (car (gimp-drawable-width  theText) ) )
    (set! theTextHeight (car (gimp-drawable-height theText) ) )

		(set! theRand (* 2 (+ inSpread inGrow inFeather ) ) )
		
    (set! imgWidth inImageWidth )
    (set! imgHeight inImageHeight )

    (if (= inAbsolute FALSE)
			( if ( > inAuraX 100 )
      	(set! imgWidth (+ (/ (* theTextWidth inAuraX ) 100 ) theRand ) )
      	(set! imgWidth (+ theTextWidth theRand ) )
			)
    )

    (if (= inAbsolute FALSE)
			(if ( > inAuraY 100 )
	      (set! imgHeight (+ (/ (* theTextHeight inAuraY ) 100 ) theRand ) )
	      (set! imgHeight (+ theTextHeight theRand ) )
			)
    )

    (set! theBufferX (/ (- imgWidth theTextWidth) 2) )
    (set! theBufferY (/ (- imgHeight theTextHeight) 2) )

    (gimp-image-resize img imgWidth imgHeight 0 0)
    (gimp-layer-resize theLayer imgWidth imgHeight 0 0)
    (gimp-layer-resize theAura1Layer imgWidth imgHeight 0 0)
    (gimp-layer-resize theAura2Layer imgWidth imgHeight 0 0)
    (gimp-layer-resize theTextLayer imgWidth imgHeight 0 0)

    (gimp-layer-set-offsets   theText theBufferX theBufferY)
    (gimp-floating-sel-anchor theText)

    (set! ellWidth (/ (* theTextWidth inAuraX ) 100 ) )
    (set! ellHeight (/ (* theTextHeight inAuraY ) 100 ) )

    (set! ellBufferX (/ (- imgWidth ellWidth ) 2) )
    (set! ellBufferY (/ (- imgHeight ellHeight ) 2) )

    (gimp-context-set-foreground inAuraColor1 )
    (gimp-ellipse-select img ellBufferX ellBufferY ellWidth ellHeight REPLACE TRUE FALSE 0)
    (gimp-edit-bucket-fill theAura1Layer FG-BUCKET-FILL NORMAL 100 0 FALSE 0 0)
    (gimp-selection-none img)

    (plug-in-spread 1 img theAura1Layer inSpread inSpread)
    (plug-in-gauss-iir 1 img theAura1Layer inSpread TRUE TRUE)

    (gimp-context-set-foreground inAuraColor2 )
    (set! theText (car (gimp-text-fontname img theAura2Layer 0 0 inText 0 TRUE inFontSize PIXELS inFont)))
    (gimp-layer-set-offsets   theText theBufferX theBufferY)
    (gimp-floating-sel-anchor theText)

    (gimp-by-color-select theAura2Layer inAuraColor2 15 0 FALSE FALSE 10 FALSE)
    (gimp-selection-grow img inGrow )
    (gimp-selection-feather img inFeather )
    (gimp-edit-bucket-fill theAura2Layer FG-BUCKET-FILL NORMAL 100 0 FALSE 0 0)
    (gimp-selection-none img)

    (gimp-context-set-foreground inTextColor )
    (set! theText (car (gimp-text-fontname img theTextLayer 0 0 inText 0 TRUE inFontSize PIXELS inFont)))
    (gimp-layer-set-offsets   theText theBufferX theBufferY)
    (gimp-floating-sel-anchor theText)

    (if (= inFlatten TRUE)
      (gimp-image-flatten img)
      ()
    )

    (gimp-context-set-background old-bg)
    (gimp-context-set-foreground old-fg)

    (gimp-display-new     img)
    (list  img theLayer theText)

    ; Bereinigen Dirty-Flag
    (gimp-image-clean-all img)

  )
)

(script-fu-register
  "script-fu-alien-aura-logo"
  "MS - Alien Aura..."
  "Creates a simple alien glow logo."
  "Michael Schalla"
  "Michael Schalla"
  "October 2002"
  ""
  SF-STRING "Text"              "Alien Aura"
  SF-FONT   "Font"              "-*-Arial Black-*-r-*-*-24-*-*-*-p-*-*-*"
  SF-VALUE  "Font size"         "100"
  SF-ADJUSTMENT "Aura1 X (%)"	  '(120 0 200 1 1 0 1)
  SF-ADJUSTMENT "Aura1 Y (%)"   '(80 0 200 1 1 0 1)
  SF-ADJUSTMENT "Aura1 Spread"  '(20 1 100 1 1 0 1)
  SF-ADJUSTMENT "Aura2 Grow"    '(2 1 20 1 1 0 1)
  SF-ADJUSTMENT "Aura2 Feather" '(5 1 50 1 1 0 1)
  SF-COLOR  "BG Color"          '(0 0 0)
  SF-COLOR  "Aura Color 1"      '(0 255 0)
  SF-COLOR  "Aura Color 2"      '(255 255 0)
  SF-COLOR  "Text Color"        '(0 0 0)
  SF-TOGGLE "Absolute Size?"    FALSE
  SF-VALUE  "Image Width"       "300"
  SF-VALUE  "Image Height"      "100"
  SF-TOGGLE "Flatten Layers?"   FALSE
)

(script-fu-menu-register "script-fu-alien-aura-logo"
			 "<Toolbox>/Xtns/Logos")

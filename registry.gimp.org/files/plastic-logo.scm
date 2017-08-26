; plastic logo
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
; version 2.4 by Eric Lamarque 2007/05/21
; version 2.4.1 by Eric Lamarque 2008/11/03 - License Fix + script register
; --------------------------------------------------------------------
;

(define (script-fu-plastic-logo inText inFont inFontSize inBackGroundColor inHighlightColor inTextColor inDarkColor inHighlightValue inThreshold inShrink inFeather inShadow inAbsolute inImageWidth inImageHeight inFlatten)
  (let*
    (
      ; Definition unserer lokalen Variablen

      ; Erzeugen des neuen Bildes

      (img  ( car (gimp-image-new 10 10 RGB) ) )
      (theText)
      (theTextWidth)
      (theTextHeight)
	  (theFloatSel)
      (imgWidth)
      (imgHeight)
      (theBufferX)
      (theBufferY)

      ; Erzeugen einer neuen Ebene zum Bild
      (theLayer (car (gimp-layer-new img 10 10 RGB-IMAGE "Ebene 1" 100 NORMAL) ) )
      (theTextLayer (car (gimp-layer-new img 10 10 RGBA-IMAGE "Text" 100 NORMAL) ) )
      (theTextMask)

      (old-fg (car (gimp-context-get-foreground) ) )
      (old-bg (car (gimp-context-get-background) ) )
      ; Ende unserer lokalen Variablen
    )

    (gimp-image-add-layer  img theLayer 0)
    (gimp-image-add-layer  img theTextLayer 0)

    ; zum Anzeigen des leeren Bildes
    ; (gimp-display-new img)

    (gimp-context-set-background '(255 255 255) )
    (gimp-context-set-foreground '(0 0 0) )

    (gimp-selection-all  img)
    (gimp-edit-clear     theLayer)
    (gimp-selection-none img)

    (gimp-context-set-foreground '(255 255 255) )
    (set! theText (car (gimp-text-fontname img -1 0 0 inText -1 TRUE inFontSize PIXELS inFont)))
    (gimp-context-set-foreground '(0 0 0) )

    (set! theTextWidth  (car (gimp-drawable-width  theText) ) )
    (set! theTextHeight (car (gimp-drawable-height theText) ) )

    (set! imgWidth inImageWidth )
    (set! imgHeight inImageHeight )

    (if (= inAbsolute FALSE)
      (set! imgWidth (+ theTextWidth 20 ) )
    )

    (if (= inAbsolute FALSE)
      (set! imgHeight (+ theTextHeight 20 ) )
    )

    (set! theBufferX      (/ (- imgWidth theTextWidth) 2) )
    (set! theBufferY      (/ (- imgHeight theTextHeight) 2) )

    (gimp-image-resize img imgWidth imgHeight 0 0)
    (gimp-layer-resize theLayer imgWidth imgHeight 0 0)
    (gimp-layer-resize theTextLayer imgWidth imgHeight 0 0)

    (gimp-layer-set-offsets   theText theBufferX theBufferY)
	(gimp-selection-layer-alpha theText)
    (gimp-edit-blend theLayer FG-BG-RGB NORMAL LINEAR 100 0 REPEAT-NONE FALSE 0 0 FALSE FALSE 0 (+ theBufferY theTextHeight) 0  theBufferY)
	(gimp-selection-none img)

    (plug-in-gauss-iir 1 img theLayer 5 TRUE TRUE)

    (gimp-context-set-foreground inTextColor )
	(gimp-selection-layer-alpha theText)
    ;(set! theText (car (gimp-text-fontname img theTextLayer 0 0 inText 0 TRUE inFontSize PIXELS inFont)))
    ;(gimp-layer-set-offsets   theText theBufferX theBufferY)
	;(set! theFloatSel (car (gimp-edit-paste theTextLayer FALSE)))
    ;(gimp-floating-sel-anchor theFloatSel)
	(gimp-edit-bucket-fill theTextLayer FG-BUCKET-FILL NORMAL 100 0 FALSE 0 0)
	(gimp-selection-none img)

    (gimp-by-color-select theLayer (list inHighlightValue inHighlightValue inHighlightValue) inThreshold 0 FALSE FALSE 10 FALSE)

		(gimp-selection-shrink img inShrink )
		(gimp-selection-feather img inFeather )

    (gimp-context-set-foreground inHighlightColor )
    (gimp-edit-bucket-fill theTextLayer FG-BUCKET-FILL NORMAL 100 0 FALSE 0 0)
    (gimp-selection-none img)

    (gimp-selection-all  img)
    (gimp-edit-clear     theLayer)
    (gimp-selection-none img)

    (gimp-context-set-foreground '(0 0 0) )
	(gimp-selection-layer-alpha theText)
    ;(set! theText (car (gimp-text-fontname img theLayer 0 0 inText 0 TRUE inFontSize PIXELS inFont)))

    ;(gimp-layer-set-offsets   theText theBufferX theBufferY)

    (gimp-edit-blend theLayer FG-BG-RGB NORMAL LINEAR 100 0 REPEAT-NONE FALSE 0 0 FALSE FALSE 0 theBufferY 0 (+ theBufferY theTextHeight) )
	;(set! theFloatSel (car (gimp-edit-paste theLayer FALSE)))
    ;(gimp-floating-sel-anchor theFloatSel)
	(gimp-selection-none img)

    (plug-in-gauss-iir 1 img theLayer 5 TRUE TRUE)

    (gimp-by-color-select theLayer (list inHighlightValue inHighlightValue inHighlightValue) inThreshold 0 FALSE FALSE 10 FALSE)

		(gimp-selection-shrink img inShrink )
		(gimp-selection-feather img inFeather )

    (gimp-context-set-foreground inDarkColor )
    (gimp-edit-bucket-fill theTextLayer FG-BUCKET-FILL NORMAL 100 0 FALSE 0 0)
    (gimp-selection-none img)

    (gimp-layer-add-alpha theTextLayer)
	(set! theTextMask (car (gimp-layer-create-mask theTextLayer ADD-BLACK-MASK)))
    (gimp-layer-add-mask theTextLayer theTextMask)

    (gimp-context-set-foreground '(255 255 255) )
    ;(set! theText (car (gimp-text-fontname img -1 0 0 inText 0 TRUE inFontSize PIXELS inFont)))
	(gimp-layer-resize-to-image-size theText)
    ;(set! theText (car (gimp-text-fontname img theTextMask 0 0 inText 0 TRUE inFontSize PIXELS inFont)))

    ;(gimp-layer-set-offsets   theText theBufferX theBufferY)
	(gimp-edit-copy theText)

	(set! theFloatSel (car (gimp-edit-paste theTextMask FALSE)))
    (gimp-floating-sel-anchor theFloatSel)

    (gimp-layer-remove-mask theTextLayer APPLY)
	(gimp-image-remove-layer img theText)

    (if (= inShadow TRUE)
      (script-fu-drop-shadow img theTextLayer 2 2 5 '(0 0 0) 100 FALSE )
      ()
    )

	(if (= TRUE FALSE) (begin
    (gimp-context-set-background inBackGroundColor )
    (gimp-selection-all  img)
    (gimp-edit-clear     theLayer)
    (gimp-selection-none img)
    ))
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
  "script-fu-plastic-logo"
  "MS - Plastic..."
  "Creates a plastic-like logo."
  "Michael Schalla"
  "Michael Schalla"
  "October 2002"
  ""
  SF-STRING "Text"              			"Plastic"
  SF-FONT   "Font"              			"-*-Fillmore-*-r-*-*-24-*-*-*-p-*-*-*"
  SF-VALUE  "Font size"         			"100"
  SF-COLOR  "BG Color"          			'(255 255 255)
  SF-COLOR  "Color 1"       					'(224 224 255)
  SF-COLOR  "Color 2"             		'(128 128 255)
  SF-COLOR  "Color 3"            			'(64 64 192)
  SF-ADJUSTMENT "Highlight Value"			'(208 0 255 1 1 0 1)
  SF-ADJUSTMENT "Highlight Threshold"	'(25 0 255 1 1 0 1)
  SF-ADJUSTMENT "Highlight Shrink"  	'(0 0 10 1 1 0 1)
  SF-ADJUSTMENT "Highlight Feather" 	'(8 0 20 1 1 0 1)
  SF-TOGGLE "Shadow?"    							TRUE
  SF-TOGGLE "Absolute Size?"    			FALSE
  SF-VALUE  "Image Width"       			"300"
  SF-VALUE  "Image Height"      			"100"
  SF-TOGGLE "Flatten Layers?"   			FALSE
)

(script-fu-menu-register "script-fu-plastic-logo"
			 "<Toolbox>/Xtns/Logos")

; feurio logo
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
; version 1.0 by Michael Schalla 2003/02/19
; version 2.0 by Eric Lamarque 2004/08/20
;   - Support Gimp 2.0
; version 2.1 by Eric Lamarque 2005/02/08
;   - problem with gimp 2.2
; version 2.4 by Eric Lamarque 2007/05/21
;   - Support Gimp 2.4
; version 2.4.1  by Eric Lamarque 2008/10/28
;   - do not rely on external def set-pt
; version 2.4.2 by Eric Lamarque 2008/11/03
;   - fix License
;   - update script register
; --------------------------------------------------------------------
;

(define (script-fu-feurio-logo inText inFont inFontSize inTextColor inRed1 inRed2 inGreen1 inGreen2 inBlue1 inBlue2 inHeight inBlurRadius inAbsolute inImageWidth inImageHeight inFlatten)
  (define (set-pt a index x y)
    (begin
      (aset a (* index 2) x)
      (aset a (+ (* index 2) 1) y)
    )
  )

  (let*
    (
      (img  ( car (gimp-image-new 10 10 RGB) ) )
      (theText)
      (theTextWidth)
      (theTextHeight)
      (imgWidth)
      (imgHeight)
      (theBufferX)
      (theBufferY)
      (theSel)

      (theLayer (car (gimp-layer-new img 10 10 RGB-IMAGE "Layer" 100 NORMAL) ) )
      (theTextLayer (car (gimp-layer-new img 10 10 RGB-IMAGE "Text Layer" 100 NORMAL) ) )
      (theFeurioLayer (car (gimp-layer-new img 10 10 RGB-IMAGE "Feurio Layer" 100 NORMAL) ) )
      (theTextMask)
      (theFeurioMask)

      (old-fg (car (gimp-context-get-foreground) ) )
      (old-bg (car (gimp-context-get-background) ) )
    )

    (define (splineRed)
      (let* ((a (cons-array 6 'byte)))
        (set-pt a 0 0 0)
        (set-pt a 1 inRed1 inRed2)
        (set-pt a 2 255 255)
        a
      )
    )
    (define (splineGreen)
      (let* ((a (cons-array 6 'byte)))
        (set-pt a 0 0 0)
        (set-pt a 1 inGreen1 inGreen2)
        (set-pt a 2 255 255)
        a
      )
    )
    (define (splineBlue)
      (let* ((a (cons-array 6 'byte)))
        (set-pt a 0 0 0)
        (set-pt a 1 inBlue1 inBlue2)
        (set-pt a 2 255 255)
        a
      )
    )

    (gimp-image-add-layer  img theLayer 0)
    (gimp-image-add-layer  img theTextLayer 0)
    (gimp-image-add-layer  img theFeurioLayer 0)

    (gimp-context-set-background '(0 0 0) )
    (gimp-context-set-foreground '(255 255 255) )

    (gimp-selection-all  img)
    (gimp-edit-clear     theLayer)
    (gimp-edit-clear     theTextLayer)
    (gimp-edit-clear     theFeurioLayer)
    (gimp-selection-none img)

    (set! theText (car (gimp-text-fontname img theLayer 0 0 inText 0 TRUE inFontSize PIXELS inFont)))

    (set! theTextWidth  (car (gimp-drawable-width  theText) ) )
    (set! theTextHeight (car (gimp-drawable-height theText) ) )

    (set! imgWidth inImageWidth )
    (set! imgHeight inImageHeight )

  	(if (= inAbsolute FALSE)
      (set! imgWidth (+ theTextWidth (* 3 inHeight) ) )
    )

  	(if (= inAbsolute FALSE)
      (set! imgHeight (+ theTextHeight (* 4 inHeight) ) )
    )

    (set! theBufferX (/ (- imgWidth theTextWidth) 2) )
    (set! theBufferY (/ (- imgHeight theTextHeight) 2) )

    (gimp-image-resize img imgWidth imgHeight 0 0)
    (gimp-layer-resize theLayer imgWidth imgHeight 0 0)
    (gimp-layer-resize theTextLayer imgWidth imgHeight 0 0)
    (gimp-layer-resize theFeurioLayer imgWidth imgHeight 0 0)

    (gimp-layer-set-offsets   theText theBufferX theBufferY)
    (gimp-floating-sel-anchor theText)

    (gimp-context-set-foreground inTextColor )
    (gimp-edit-bucket-fill theTextLayer FG-BUCKET-FILL NORMAL 100 0 FALSE 0 0)

    (plug-in-bump-map 1 img theTextLayer theLayer 135.0 45.0 3 0 0 0 0 TRUE FALSE 2)

    (gimp-layer-add-alpha theTextLayer)
	(set! theTextMask (car (gimp-layer-create-mask theFeurioLayer WHITE-MASK)))
    (gimp-layer-add-mask theTextLayer theTextMask)

    (gimp-edit-copy theLayer)
    (set! theSel (car (gimp-edit-paste theTextMask FALSE)))
    (gimp-floating-sel-anchor theSel)

    (gimp-layer-remove-mask theTextLayer APPLY)

    (gimp-context-set-foreground '(255 255 255) )

    (set! theSel (car (gimp-edit-paste theFeurioLayer FALSE)))
    (gimp-floating-sel-anchor theSel)

    (gimp-by-color-select theFeurioLayer '(255 255 255) 15 0 FALSE FALSE 10 FALSE)
    (gimp-selection-grow img (/ inHeight 5))
    (gimp-edit-bucket-fill theFeurioLayer FG-BUCKET-FILL NORMAL 100 0 FALSE 0 0)
    (gimp-context-set-foreground '(128 128 128) )
    (gimp-selection-shrink img (/ inHeight 5))
    (gimp-edit-bucket-fill theFeurioLayer FG-BUCKET-FILL NORMAL 100 0 FALSE 0 0)
    (gimp-selection-none img)
    (gimp-context-set-foreground '(255 255 255) )

    (plug-in-ripple 1 img theFeurioLayer inHeight inHeight 1 0 1 TRUE FALSE)
    (plug-in-ripple 1 img theFeurioLayer (* 2 inHeight) (/ inHeight 3) 1 0 1 TRUE FALSE)
    (plug-in-spread 1 img theFeurioLayer inHeight inHeight)

    (gimp-layer-add-alpha theFeurioLayer)
 	  (set! theFeurioMask (car (gimp-layer-create-mask theFeurioLayer WHITE-MASK)))
    (gimp-layer-add-mask theFeurioLayer theFeurioMask)
    (gimp-edit-blend theFeurioMask FG-BG-RGB NORMAL LINEAR 100 0 REPEAT-NONE FALSE 0 0 FALSE FALSE 0 theBufferY 0 (- imgHeight theBufferY))

    (plug-in-gauss-iir 1 img theFeurioLayer inBlurRadius 1 1)

    (gimp-curves-spline theFeurioLayer BLUE-LUT 6 (splineBlue))
    (gimp-curves-spline theFeurioLayer GREEN-LUT 6 (splineGreen))
    (gimp-curves-spline theFeurioLayer RED-LUT 6 (splineRed))

    (gimp-layer-remove-mask theFeurioLayer APPLY)

    (plug-in-gauss-iir 1 img theLayer inBlurRadius 1 1)

    (gimp-curves-spline theLayer BLUE-LUT 6 (splineBlue))
    (gimp-curves-spline theLayer GREEN-LUT 6 (splineGreen))
    (gimp-curves-spline theLayer RED-LUT 6 (splineRed))

  	(if (= inFlatten TRUE)
      (gimp-image-flatten img)
  		()
  	)

    (gimp-context-set-background old-bg)
    (gimp-context-set-foreground old-fg)

    (gimp-display-new img)
    (list  img theLayer theText)

    ; Bereinigen Dirty-Flag
    (gimp-image-clean-all img)

  )
)

(script-fu-register
  "script-fu-feurio-logo"
  "MS - Feurio..."
  "Creates a burning text logo."
  "Michael Schalla"
  "Michael Schalla"
  "October 2002"
  ""
  SF-STRING "Text"               "Feurio"
  SF-FONT   "Font"               "-*-Arial Black-*-r-*-*-24-*-*-*-p-*-*-*"
  SF-ADJUSTMENT "Font Size"      '(100 2 1000 1 10 0 1)
  SF-COLOR  "Color"              '(224 0 0)
  SF-ADJUSTMENT "Red 1"          '(64 0 255 1 1 0 1)
  SF-ADJUSTMENT "Red 2"          '(224 0 255 1 1 0 1)
  SF-ADJUSTMENT "Green 1"        '(128 0 255 1 1 0 1)
  SF-ADJUSTMENT "Green 2"        '(192 0 255 1 1 0 1)
  SF-ADJUSTMENT "Blue 1"         '(224 0 255 1 1 0 1)
  SF-ADJUSTMENT "Blue 2"         '(64 0 255 1 1 0 1)
  SF-ADJUSTMENT "Flame Height"   '(25 1 1000 1 1 1 1)
  SF-ADJUSTMENT "Blur Radius"    '(5 1 100 1 1 1 1)
  SF-TOGGLE "Absolute Size?"     FALSE
  SF-VALUE  "Image Width"        "400"
  SF-VALUE  "Image Height"       "150"
  SF-TOGGLE "Flatten Layers?"    FALSE
)

(script-fu-menu-register "script-fu-feurio-logo"
                         "<Toolbox>/Xtns/Logos")

;
; Paper Box Creator V1.1
;
; (C) 2010, Marian Kyral (mkyral@email.cz)
;
; This plugin was tested with Gimp 2.6
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
; Changelog
;
; 01.01.2010 - v1.0
; * Initial version
;
; 01.01.2010 - v1.1
; * Maximal Cutout size is 10 mm
; * The main fold was to small in come cases
; * Set minimal allowed dimensions
;

;
; Define the functions
;
; convert milimeters to pixels using choosen DPI
(define (to-pixels x dpi) (/ (* x  dpi ) 25.4))

; return smaller number
(define (get-smaller a b)
  (if (< a b)
      a
      b
  )
)

; return bigger number
(define (get-bigger a b)
  (if (> a b)
      a
      b
  )
)

; draw box
(define (draw-box layer x y w h )
    (let*
      (
        (box (cons-array 10 'double))
      )

      (aset box 0 x)
      (aset box 1 y)

      (aset box 2  x)
      (aset box 3  (+ y h))

      (aset box 4  (+ x w))
      (aset box 5  (+ y h))

      (aset box 6  (+ x w))
      (aset box 7  y)

      (aset box 8  x)
      (aset box 9  y)

      (gimp-pencil layer 10 box)
    )
)

; draw fold verticaly
(define (draw-fold-vert layer x y w h r )
    (let*
      (
        (fold (cons-array 10 'double))
      )

      (aset fold 0 x)
      (aset fold 1 y)

      (aset fold 2  x)
      (aset fold 3  (+ y h))

      (aset fold 4  (+ x  w))
      (aset fold 5  (+ y (- h (* h r))))

      (aset fold 6  (+ x w))
      (aset fold 7  (+ y (* h r)))

      (aset fold 8  x)
      (aset fold 9  y)

      (gimp-pencil layer 10 fold)
    )
)

; draw fold horizontaly
(define (draw-fold-horiz layer x y w h r )
    (let*
      (
        (fold (cons-array 10 'double))
      )

      (aset fold 0 x)
      (aset fold 1 y)

      (aset fold 2  (+ x w))
      (aset fold 3  y)

      (aset fold 4  (+ x (- w (* w r))))
      (aset fold 5  (+ y h))

      (aset fold 6  (+ x (* w r)))
      (aset fold 7  (+ y h))

      (aset fold 8  x)
      (aset fold 9  y)

      (gimp-pencil layer 10 fold)
    )
)

; The main function
(define (script-fu-kym-paper-box-creator InWidth InHeight InDepth InCutout InFont InText InCustomFontSize InFontSize)
  (let*
    (
        ; default variables
        (TheImageBorderSize 5) ; 5 mm
        (TheSpace 0.4) ; 0.4 mm
        (TheDPI 300)
        (TheMaxCutoutDiameterPx (to-pixels 10 TheDPI)) ; Maximal cutout size is 10 mm
        (TheMinimalFoldSizePx (to-pixels 5 TheDPI)) ; Minimal fold size is 5 mm
;
        (TheImage 0)
        (TheLayer 0)

        (TheWidthPx 0)
        (TheHeightPx 0)
        (TheDepthPx 0)
        (TheImageBorderSizePx 0)
        (TheSpacePx 0)
        (TheDiameterPx 0)

        (TheFoldSize 0)
        (TheImageWith 0)
        (TheImageHeight 0)
        (cx 0) ; start X coordinator
        (cy 0) ; start Y coordinator

        (textExtents)
        (textWidth 0)
        (textHeight 0)
        (textBorder (to-pixels 1 TheDPI)) ; 1 mm
        (fontSize 0)
        (maxTextWidth 0)
        (maxTextHeight 0)
        (TheTextLayer -1)

        (oldBgColor (car (gimp-context-get-background)))
        (oldFgColor (car (gimp-context-get-foreground)))
        (oldBrush   (car (gimp-context-get-brush)))
        (oldOpacity (car (gimp-context-get-opacity)))


    )

    ;; convert mm to pixels
    (set! TheWidthPx  (to-pixels InWidth  TheDPI ))
    (set! TheHeightPx (to-pixels InHeight TheDPI ))
    (set! TheDepthPx  (to-pixels InDepth  TheDPI ))

    (set! TheImageBorderSizePx  (to-pixels TheImageBorderSize  TheDPI ))
    (set! TheSpacePx  (to-pixels TheSpace  TheDPI ))


    ; The final image size
    (set! TheImageWith (+ (* TheImageBorderSizePx 2) TheHeightPx (* TheDepthPx 2) (* TheSpacePx 3)
                          (get-smaller (+ TheMaxCutoutDiameterPx (to-pixels 5 TheDPI)) (get-smaller (- TheHeightPx (to-pixels 2 TheDPI)) (get-bigger (* TheDepthPx 0.8) (* TheHeightPx 0.5))))
                       ))

    (set! TheImageHeight (+ (* TheImageBorderSizePx 2) (* TheWidthPx 2) (* TheDepthPx 2.2)  (* TheSpacePx 4)))

    ;; Create new image
    (set! TheImage (car (gimp-image-new TheImageWith TheImageHeight RGB)))
    (set! TheLayer (car (gimp-layer-new TheImage TheImageWith TheImageHeight RGB-IMAGE "Box layer" 100 NORMAL-MODE)))
    (gimp-image-add-layer TheImage TheLayer 0)
    (gimp-image-set-resolution TheImage TheDPI TheDPI)
    (gimp-image-undo-disable TheImage)
    (gimp-selection-none TheImage)

    ; Colors
    (gimp-context-set-background '(255 255 255))
    (gimp-context-set-foreground '(000 000 000))

    (gimp-drawable-fill TheLayer BACKGROUND-FILL)


  ;
  ; Generate Box
  ;

    (gimp-context-set-brush "Circle (03)")
    (gimp-context-set-opacity 100)

    ; box botton
    ;  -
    ; |X|
    ; |X|
    ;  -

    (set! cx TheImageBorderSizePx)
    (set! cy TheImageBorderSizePx)
    (draw-box TheLayer cx cy TheDepthPx TheWidthPx)

    ; box back
    ;  -   ----
    ; | | | XX |
    ; | | | XX |
    ;  -   ----

    (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx))
    (set! cy TheImageBorderSizePx)
    (draw-box TheLayer cx cy TheHeightPx TheWidthPx)

    ; box top
    ;  -   ----   -
    ; | | |    | |X|
    ; | | |    | |X|
    ;  -   ----   -

    (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx TheHeightPx TheSpacePx))
    (set! cy TheImageBorderSizePx)
    (draw-box TheLayer cx cy TheDepthPx TheWidthPx)

    ; box first side
    ;  -   ----   -
    ; | | |    | | |
    ; | | |    | | |
    ;  -   ----   -
    ;     | XX |
    ;      ----

    (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx))
    (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx))
    (draw-box TheLayer cx cy TheHeightPx TheDepthPx)

    ; box front
    ;  -   ----   -
    ; | | |    | | |
    ; | | |    | | |
    ;  -   ----   -
    ;     |    |
    ;      ----
    ;     | XX |
    ;     | XX |
    ;      ----

    (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx))
    (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx TheDepthPx TheSpacePx))
    (draw-box TheLayer cx cy TheHeightPx TheWidthPx)

    ; box second botton
    ;  -   ----   -
    ; | | |    | | |
    ; | | |    | | |
    ;  -   ----   -
    ;     |    |
    ;  -   ----
    ; |X| |    |
    ; |X| |    |
    ;  -   ----

    (set! cx (+ TheImageBorderSizePx ))
    (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx TheDepthPx TheSpacePx))
    (draw-box TheLayer cx cy TheDepthPx TheWidthPx)

    ; box second side
    ;  -   ----   -
    ; | | |    | | |
    ; | | |    | | |
    ;  -   ----   -
    ;     |    |
    ;  -   ----
    ; | | |    |
    ; | | |    |
    ;  -   ----
    ;     | XX |
    ;      ----

    (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx))
    (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx TheDepthPx TheSpacePx TheWidthPx TheSpacePx))
    (draw-box TheLayer cx cy TheHeightPx TheDepthPx)

;
; Folds
;
    (gimp-context-set-brush "Circle (01)")

    ;  -   ----   -
    ; | | |    | | |
    ; | | |    | | |
    ;  -   ----   -
    ;   X |    |
    ;  -   ----
    ; | | |    |
    ; | | |    |
    ;  -   ----
    ;     |    |
    ;      ----

    (set! cx (+ TheImageBorderSizePx TheDepthPx ))
    (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx ))
    (set! TheFoldSize (get-smaller (* TheDepthPx 0.5) (* TheWidthPx 0.5)))
    (draw-fold-vert TheLayer cx cy (- 0 TheFoldSize) TheDepthPx 0.1 )

    ;  -   ----   -
    ; | | |    | | |
    ; | | |    | | |
    ;  -   ----   -
    ;   < |    |
    ;  -   ----
    ; | | |    |
    ; | | |    |
    ;  -   ----
    ;   X |    |
    ;      ----

    (set! cx (+ TheImageBorderSizePx TheDepthPx ))
    (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx TheDepthPx TheSpacePx TheWidthPx TheSpacePx ))
    (set! TheFoldSize (get-smaller (* TheDepthPx 0.5) (* TheWidthPx 0.5)))
    (draw-fold-vert TheLayer cx cy (- 0 TheFoldSize) TheDepthPx 0.1 )

    ;  -   ----   -
    ; | | |    | | |
    ; | | |    | | |
    ;  -   ----   -
    ;   < |    | X
    ;  -   ----
    ; | | |    |
    ; | | |    |
    ;  -   ----
    ;   < |    |
    ;      ----

    (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx TheHeightPx TheSpacePx ))
    (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx ))
    (set! TheFoldSize (get-smaller (* TheDepthPx 0.8) (* TheWidthPx 0.5)))
    (draw-fold-vert TheLayer cx cy TheFoldSize TheDepthPx 0.1 )

    ;  -   ----   -
    ; | | |    | | |
    ; | | |    | | |
    ;  -   ----   -
    ;   < |    | >
    ;  -   ----
    ; | | |    |
    ; | | |    |
    ;  -   ----
    ;   < |    | X
    ;      ----

    (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx TheHeightPx TheSpacePx ))
    (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx TheDepthPx TheSpacePx TheWidthPx TheSpacePx ))
    (set! TheFoldSize (get-smaller (* TheDepthPx 0.8) (* TheWidthPx 0.5)))
    (draw-fold-vert TheLayer cx cy TheFoldSize TheDepthPx 0.1 )

    ;  -   ----   -
    ; | | |    | | | X
    ; | | |    | | | X
    ;  -   ----   -
    ;   < |    | >
    ;  -   ----
    ; | | |    |
    ; | | |    |
    ;  -   ----
    ;   < |    | >
    ;      ----

    (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx TheHeightPx TheSpacePx TheDepthPx TheSpacePx ))
    (set! cy TheImageBorderSizePx )
    (set! TheFoldSize (get-smaller (+ TheMaxCutoutDiameterPx (to-pixels 5 TheDPI)) (get-smaller (- TheHeightPx (to-pixels 2 TheDPI)) (get-bigger (* TheDepthPx 0.8) (* TheHeightPx 0.5)))))
    (draw-fold-vert TheLayer cx cy TheFoldSize TheWidthPx 0.2 )

    ;  -   ----   -
    ; | | |    | | | >
    ; | | |    | | | >
    ;  -   ----   -
    ;   < |    | >
    ;  -   ----
    ; | | |    |
    ; | | |    |
    ;  -   ----
    ;   < |    | >
    ;      ----
    ;       XX

    (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx ))
    (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx TheDepthPx TheSpacePx TheWidthPx TheSpacePx TheDepthPx TheSpacePx ))
    (set! TheFoldSize (get-smaller (get-bigger (* TheDepthPx 0.2) TheMinimalFoldSizePx) TheDepthPx))
    (draw-fold-horiz TheLayer cx cy TheHeightPx TheFoldSize 0.2 )

    ;
    ;     Done
    ;

    ;  -   ----   -
    ; | | |    | | | >
    ; | | |    | | | >
    ;  -   ----   -
    ;   < |    | >
    ;  -   ----
    ; | | |    |
    ; | | |    |
    ;  -   ----
    ;   < |    | >
    ;      ----
    ;       \/

;
; Create cutout
;

  (if (= InCutout TRUE)
    (begin
      (set! TheDiameterPx ( get-smaller TheMaxCutoutDiameterPx (* (get-smaller (* TheHeightPx 0.5) (* TheWidthPx 0.4) ) 2 )))
      (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx TheHeightPx (- 0 (* TheDiameterPx 0.5) )))
      (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx TheDepthPx TheSpacePx (- (* TheWidthPx 0.5) (* TheDiameterPx 0.5))))

      ; create circle
      (gimp-context-set-brush "Circle (03)")
      (gimp-ellipse-select TheImage cx cy TheDiameterPx TheDiameterPx CHANNEL-OP-REPLACE TRUE FALSE 0 )
      (gimp-edit-stroke TheLayer)

      ; erase right half of circle
      (set! cx (+ cx (* TheDiameterPx 0.5) 2 ))
      (set! cy (- cy 1))
      (set! TheDiameterPx (+ TheDiameterPx 4))
      (gimp-rect-select TheImage cx cy TheDiameterPx TheDiameterPx  CHANNEL-OP-REPLACE FALSE 0)
      (gimp-edit-fill TheLayer BACKGROUND-FILL)

      ; Erase remaining line
      (set! cx (- cx 3 ))
      (set! cy (+ cy 3))
      (set! TheDiameterPx (- TheDiameterPx 7))
      (gimp-rect-select TheImage cx cy 3 TheDiameterPx  CHANNEL-OP-REPLACE FALSE 0)
      (gimp-edit-fill TheLayer BACKGROUND-FILL)

      ; cancel selection
      (gimp-selection-none TheImage)

    )
  )
;
; Add text if entered
;
  (if (> (string-length InText) 0)
    (begin
      (if (= InCustomFontSize FALSE)
        (begin
          ;Probe for the maximal size of text
          (set! maxTextWidth (- TheHeightPx (to-pixels 2 TheDPI)))
          (set! maxTextHeight (- TheDepthPx (to-pixels 2 TheDPI)))

          (set! fontSize 8) ; initial font size

          (while (and (< textWidth maxTextWidth) (< textHeight maxTextHeight) (< fontSize 500))

            (set! fontSize (+ fontSize 2))
            (set! textExtents  (gimp-text-get-extents-fontname InText fontSize 0 InFont))
            (set! textWidth  (text-width  textExtents ))
            (set! textHeight (text-height textExtents ))
          )
          (set! fontSize (- fontSize 2))
        )
        (set! fontSize InFontSize)
      )

      (set! textExtents  (gimp-text-get-extents-fontname InText fontSize 0 InFont))
      (set! textWidth  (text-width  textExtents ))
      (set! textHeight (text-height textExtents ))

      ; Center text
      (set! cx (+ TheImageBorderSizePx TheDepthPx TheSpacePx (* (get-bigger (- TheHeightPx textWidth) 0) 0.5) (- 0 textBorder )))
      (set! cy (+ TheImageBorderSizePx TheWidthPx TheSpacePx (* (get-bigger (- TheDepthPx textHeight) 0) 0.5) (- 0 textBorder )))

      ; Write text to first side
      (set! TheTextLayer (car (gimp-text-fontname TheImage -1 cx cy InText textBorder TRUE fontSize 0  InFont )))

      (gimp-image-flatten TheImage)

      ; Move to second side
      (set! cy (+ cy TheDepthPx TheSpacePx TheWidthPx))

      ; Write text to first side
      (set! TheTextLayer (car (gimp-text-fontname TheImage -1 cx cy InText textBorder TRUE fontSize 0  InFont )))

      ; rotate layer (180 Degrees = pi = (* 4 (atan 1.0)) )
      (gimp-drawable-transform-rotate TheTextLayer (* 4 (atan 1.0)) TRUE 0 0 TRANSFORM-FORWARD INTERPOLATION-NONE FALSE 1 TRANSFORM-RESIZE-CROP-WITH-ASPECT)
    )
  )

;
; Finish work
;
    (gimp-context-set-background oldBgColor)
    (gimp-context-set-foreground oldFgColor)
    (gimp-context-set-brush oldBrush)
    (gimp-context-set-opacity oldOpacity)

    (gimp-image-flatten TheImage)
    (gimp-image-clean-all TheImage)
    (gimp-display-new TheImage)
    (gimp-image-undo-enable TheImage)
    (gimp-displays-flush)
  )
)
;
; Register the function with the GIMP
;
(script-fu-register
    "script-fu-kym-paper-box-creator"
    "<Image>/Filters/Utils/Paper box creator"
    "Generate a paper box pattern"
    "Marian Kyral"
    "(c) 2010, Marian Kyral (mkyral@email.cz)"
    "01.01.2010"
    ""
    SF-ADJUSTMENT "Width  (in milimeters)" '(50 5 400 1 0 2 0)
    SF-ADJUSTMENT "Height (in milimeters)" '(50 5 400 1 0 2 0)
    SF-ADJUSTMENT "Depth   (in milimeters)" '(25 3 400 1 0 2 0)
    SF-TOGGLE     "Create cutout" FALSE
    SF-FONT       "Font" "Sans"
    SF-STRING     "Text" ""
    SF-TOGGLE     "Custom font size" FALSE
    SF-ADJUSTMENT "Font size" '(36 4 400 1 0 2 0)
)
;

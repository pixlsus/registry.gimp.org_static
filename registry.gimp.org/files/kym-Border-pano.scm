;
; Copyright, V1.2
;
; Marian Kyral (mkyral@email.cz)
; (C) 2006, 2008, Frydek-Mistek, Czech
;
; This plugin was tested with Gimp 2.4
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
; 08.10.2008 - v1.2
; * CHANGE: Fixed to work with gimp 2.6
;

; Define the function
;
(define (script-fu-kym-Border-pano InImage InLayer BorderSize LineSize LineDist InUseSysColors InLineColor InBorderColor InFeather InWorkOnCopy)
;
    (let*    (
        (TheImage 0)
        (TheLayer 0)
        (TheWidth 0)
        (TheHeight 0)
        (border-size 0)
        (line-size 0)
        (line-distance 0)
        (image-width 0)
        (image-height 0)
        (TheBackupColor 0)
        (TheLineColor 0)
        (TheBorderColor 0)
        )

        (if (= InWorkOnCopy TRUE)
                           (begin
                             (set! TheImage (car (gimp-image-duplicate InImage)))
                             (gimp-image-undo-disable TheImage)
                             (gimp-selection-none TheImage)
                           )
                           (begin
                             (set! TheImage InImage)
                             (gimp-image-undo-group-start TheImage)
                             (gimp-selection-all TheImage)
                           )
        )

        (set! TheLayer (car (gimp-image-flatten TheImage)))
        (set! TheWidth (car (gimp-image-width TheImage)))
        (set! TheHeight (car (gimp-image-height TheImage)))
        (set! border-size  BorderSize)
        (set! line-size LineSize)
        (set! line-distance LineDist)
        (set! image-width TheWidth)
        (set! image-height (+ TheHeight (* 2 border-size)))

        (set! TheBackupColor (car (gimp-context-get-foreground)))

        (if (= InUseSysColors TRUE)
            (begin
              (set! TheLineColor (car (gimp-context-get-foreground) ))
              (set! TheBorderColor (car (gimp-context-get-background) ))
            )
            (begin
              (set! TheLineColor InLineColor )
              (set! TheBorderColor InBorderColor )
            )
        )

        (set! TheLayer ( car (gimp-layer-copy TheLayer TRUE)))
        (gimp-drawable-set-name TheLayer "With Border")

;
; Generate the border
;
        (gimp-image-resize TheImage image-width image-height 0 border-size)
;
        (let*    (
            (BorderLayer (car (gimp-layer-new TheImage image-width image-height RGBA-IMAGE "TempLayer" 100 NORMAL-MODE)))
            )
            (gimp-image-add-layer TheImage BorderLayer -1)
            (gimp-edit-clear BorderLayer)
;
            (gimp-rect-select TheImage 0 0 image-width image-height CHANNEL-OP-REPLACE FALSE 0)
            (gimp-rect-select TheImage 0 border-size TheWidth TheHeight CHANNEL-OP-SUBTRACT FALSE 0)
            (gimp-palette-set-foreground TheBorderColor)
            (gimp-edit-fill BorderLayer FOREGROUND-FILL)
;

            (cond
                ((> LineSize 0)
                    (begin
;
; Make the line
;
                        (gimp-rect-select TheImage 0
                                       (- border-size (+ line-size line-distance))
                                       image-width
                                       line-size
                              CHANNEL-OP-REPLACE InFeather (* 1.2 line-size)
            )
                        (gimp-palette-set-foreground TheLineColor)
                        (gimp-edit-fill BorderLayer FOREGROUND-FILL)

                        (gimp-rect-select TheImage 0
                                       (+ border-size (+ TheHeight line-distance))
                                       image-width
                                       line-size
                              CHANNEL-OP-REPLACE InFeather (* 1.2 line-size)
            )
                        (gimp-palette-set-foreground TheLineColor)
                        (gimp-edit-fill BorderLayer FOREGROUND-FILL)
                    )
                )
            )

;            (gimp-image-merge-down TheImage BorderLayer CLIP-TO-IMAGE)
        )
;
        (gimp-selection-none TheImage)
        (if (= InWorkOnCopy TRUE)
            (begin  (gimp-image-clean-all TheImage)
                    (gimp-display-new TheImage)
                    (gimp-image-undo-enable TheImage)
            )
            (gimp-image-undo-group-end TheImage)
        )
        (gimp-palette-set-foreground TheBackupColor)
    )
;
; Finish work
;
    (gimp-displays-flush)
;
)
;
; Register the function with the GIMP
;
(script-fu-register
    "script-fu-kym-Border-pano"
    "<Image>/Filters/Decor/Borders/Line Border - panorama"
    "Generate a border for a panorama image."
    "Marian Kyral (mkyral@email.cz)"
    "2006, 2008, Marian Kyral, Frydek-Mistek, Czech"
    "21.06.2006, 06.01.2008"
    "RGB*,GRAY*"
    SF-IMAGE    "The Image"    0
    SF-DRAWABLE    "The Layer"    0
    SF-ADJUSTMENT     "Border size (width in pixels)" '(100 1 600 1 0 2 0)
    SF-ADJUSTMENT      "Line size (in pixels)" '(1 0 50 1 0 2 0)
    SF-ADJUSTMENT      "Line to image dist. (in pixels)" '(1 0 50 1 0 2 0)
    SF-TOGGLE "Use actual palette colors" FALSE
    SF-COLOR "Line color" '(255 255 255)
    SF-COLOR "Border color" '(0 0 0)
    SF-TOGGLE "Feather line" FALSE
    SF-TOGGLE "Work on copy" FALSE
)
;

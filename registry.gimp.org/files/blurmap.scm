; GIMP Blur Map
; Copyright (c) 2007 Jonathan Stipe
; JonStipe@prodigy.net

; ---------------------------------------------------------------------

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (script-fu-blur-map img
                            drawable
                            origimg
                            origmap
                            min
                            max
                            numsteps
                            blurtype)
  (gimp-image-undo-group-start img)
  (let* ((blurmap (car (gimp-layer-copy origmap TRUE)))
        (i numsteps)
        (blurDelta (- max min))
        (blurAmts '())
        (cutoffs '())
        (origselection (car (gimp-selection-save img)))
        )
    (gimp-image-add-layer img blurmap -1)
    (gimp-desaturate blurmap)
    (gimp-selection-none img)
    (if (< max min)
      (begin
        (set! i max)
        (set! max min)
        (set! min i)
        (set! i numsteps)
        (set! blurDelta (- max min))
        (gimp-invert blurmap)
      )
    )
    (while (> i 0)
      (set! blurAmts (cons (+ min (* blurDelta (/ (- i 1) (- numsteps 1)))) blurAmts))
      (set! cutoffs (cons (round (* 255 (/ i numsteps))) cutoffs))
      (set! i (- i 1))
    )
    (set! i 0)
    (while (< i 256)
      (gimp-by-color-select blurmap (list i i i) 0 0 FALSE FALSE 0 FALSE)
      (if (= i (car cutoffs))
        (begin
          (if (and (= (car (gimp-selection-is-empty img)) 0) (> (car blurAmts) 0))
            (if (= blurtype 0)
              (plug-in-gauss-iir 1 img origimg (car blurAmts) 1 1)
              (plug-in-gauss-rle 1 img origimg (car blurAmts) 1 1)
            )
          )
          (set! cutoffs (cdr cutoffs))
          (set! blurAmts (cdr blurAmts))
          (gimp-selection-none img)
        )
      )
      (set! i (+ i 1))
    )
    (gimp-image-remove-layer img blurmap)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img origselection)
    (gimp-displays-flush)
  )
  (gimp-image-undo-group-end img)
)

(script-fu-register "script-fu-blur-map"
                    _"<Image>/Script-Fu/_Blur Map..."
                    "Applies a gaussian blur with a different radius in different parts of the image, according to a map."
                    "Jonathan Stipe <JonStipe@prodigy.net>"
                    "Jonathan Stipe"
                    "September 2007"
                    "RGB*"
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Drawable" 0
                    SF-DRAWABLE "Original Image" 0
                    SF-DRAWABLE "Blur Map" 0
                    SF-ADJUSTMENT "Minimum blur radius" '(0 0 20016 1 10 1 1)
                    SF-ADJUSTMENT "Maximum blur radius" '(255 0 20016 1 10 1 1)
                    SF-ADJUSTMENT "Number of steps" '(10 2 255 1 10 0 1)
                    SF-OPTION "Blur type" '("IIR" "RLE"))
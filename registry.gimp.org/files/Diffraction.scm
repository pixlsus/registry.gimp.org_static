;;------ Diffraction -----------------------
; Creates a diffraction pattern by drawing a number of
; gradient circles in 'difference' mode

(define (script-fu-diffraction width height numWaves wavelen pos axis)
  ; Create an img and a layer
  (let* ((img (car (gimp-image-new width height 0)))
         (layer (car (gimp-layer-new img width height 0 "Diffraction" 100 0)))
         (fgPrev (car (gimp-context-get-foreground))) ; Remember previous settings
         (bgPrev (car (gimp-context-get-background)))
         (posX 0) (posY 0)
         (i 0)
        )

  (gimp-image-undo-disable img)
  (gimp-image-add-layer img layer 0)

  ;(gimp-context-set-foreground '(255 255 255))
  ;(gimp-context-set-background '(0 0 0))
  (gimp-edit-bucket-fill layer 1 0 100 255 0 0 0)

  ; Set the position on the chosen axis
  (if (= axis 0)
    (set! posY (* height (/ pos 100)))
    (set! posX (* width (/ pos 100)))
  )

  (set! numWaves (inexact->exact (round numWaves))) ; Convert to exact (integer)
  (while (< i numWaves)
    (gimp-edit-blend layer 0 6 2 100 0 2 0 0 0 0 1 posX posY (+ posX wavelen) posY)
    (if (= axis 0)    ; Increment position along axis
      (set! posX (+ posX (/ width numWaves)))
      (set! posY (+ posY (/ height numWaves)))
    )
    (set! i (+ i 1))
  )

  (if (odd? numWaves)   ; Invert colours if an odd number of operations
    (gimp-edit-bucket-fill layer 0 6 100 255 0 0 0)
    '()
  )

  ; Restore previous settings
  (gimp-context-set-foreground fgPrev)
  (gimp-context-set-background bgPrev)

  (gimp-image-undo-enable img)
  (gimp-display-new img)
))

(script-fu-register "script-fu-diffraction"
    "<Toolbox>/Xtns/Patterns/Diffraction"
    "Creates a diffraction pattern from a number of circles"
    "David Hari"
    "David Hari"
    "2006, 2008"
    ""
    SF-VALUE "Width" "256"
    SF-VALUE "Height" "256"
    SF-ADJUSTMENT "No. of waves" '(5 1 50 1 10 0 0)
    SF-ADJUSTMENT "Wavelength (px)" '(20 1 1024 1 10 0 1)
    SF-ADJUSTMENT "Position (%)" '(50 0 100 1 5 0 0)
    SF-OPTION "Axis" '("X" "Y")
)

; -*-scheme-*-
; Mario Valle 2010.  No copyright.  Public Domain.
; Based on Guillermo Maldonado Rule of Thirds

(define (script-fu-guide-gr image drawable)
    (let* (
        (width (car (gimp-image-width image)))
        (height (car (gimp-image-height image)))
        )

    (gimp-image-add-hguide image (/ (* height 1000) 1618))
    (gimp-image-add-hguide image (/ (* height 382) 1000))
    (gimp-image-add-vguide image (/ (* width 1000) 1618))
    (gimp-image-add-vguide image (/ (* width 382) 1000))

    (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-guide-gr"
  "New Guides _Golden Ratio"
  "Adds Golden Ratio guides"
  "Mario Valle"
  "Mario Valle, 2010"
  "March 2010"
  ""
  SF-IMAGE      "Input Image"      0
  SF-DRAWABLE   "Input Drawable"   0
)

(script-fu-menu-register "script-fu-guide-gr"
                         "<Image>/Image/Guides")

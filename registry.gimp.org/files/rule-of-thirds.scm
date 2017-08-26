; -*-scheme-*-
; Guillermo Maldonado 2009.  No copyright.  Public Domain.
; Script based on guides-new-percent.scm by Alan Horkan

(define (script-fu-guide-rot image drawable)
    (let* (
        (width (car (gimp-image-width image)))
        (height (car (gimp-image-height image)))
        )

    (gimp-image-add-hguide image (/ height 3))
    (gimp-image-add-hguide image (* height (/ 2 3)))
    (gimp-image-add-vguide image (/ width 3))
    (gimp-image-add-vguide image (* width (/ 2 3)))

    (gimp-displays-flush)
  )
)

(script-fu-register "script-fu-guide-rot"
  "New Guides Rule of _Thirds"
  "Adds Rule of Thirds guides"
  "Guillermo Maldonado"
  "Guillermo Maldonado, 2009"
  "January 2009"
  ""
  SF-IMAGE      "Input Image"      0
  SF-DRAWABLE   "Input Drawable"   0
)

(script-fu-menu-register "script-fu-guide-rot"
                         "<Image>/Image/Guides")

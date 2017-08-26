; select_round_rectangle.scm
; Copyright (c) 2013 Michael Morris
; This software is released under MIT Open Source License
; ==============================================================================

; ------------------------------------------------------------------------------
(define (do-select-rectangle-with-elliptical-corners inImage inDrawable inAntialias inFeather inFeatherRadiusX inFeatherRadiusY inOperation inX inY inWidth inHeight inCornerRadiusX inCornerRadiusY)

    ; Store previous values to restore later
    (let* (
        (antialias-previous (car (gimp-context-get-antialias)))
        (feather-previous (car (gimp-context-get-feather)))
        (feather-radius-previous (gimp-context-get-feather-radius)))

        ; Set context during the selection.
        (gimp-context-set-antialias inAntialias)
        (gimp-context-set-feather inFeather)
        (if (= 1 inFeather)
            (gimp-context-set-feather-radius inFeatherRadiusX inFeatherRadiusY))

        ; Selection.
        (gimp-image-select-round-rectangle inImage inOperation inX inY inWidth inHeight inCornerRadiusX inCornerRadiusY)
        
        ; Restore previous context.
        ;(gimp-context-set-antialias antialias-previous)
        (gimp-context-set-feather feather-previous)
        (gimp-context-set-feather-radius (car feather-radius-previous) (cadr feather-radius-previous))))


; ------------------------------------------------------------------------------
(define (select-rectangle-with-elliptical-corners inImage inDrawable inAntialias inFeather inFeatherRadiusX inFeatherRadiusY inOperation inX inY inWidth inHeight inCornerRadiusX inCornerRadiusY)
    (do-select-rectangle-with-elliptical-corners inImage inDrawable inAntialias inFeather inFeatherRadiusX inFeatherRadiusY inOperation inX inY inWidth inHeight inCornerRadiusX inCornerRadiusY)
)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(script-fu-register
    "select-rectangle-with-elliptical-corners"                      ;func name
    "Rectangle with elliptical corners ..."                         ;menu label
    "Creates a rectangular selection with elliptical corners.\
    Makes selections like the Rectangular Select Tool with the\
    Rounded corners option checked, however the x and y radius of\
    the corner can be specified independently to form more of an\
    elliptical corner rather than circular. Also the radius limit\
    is 262144 pixels instead of 100 pixels."                        ;description
    "Michael Morris"                                                ;author
    "Copyright (c) 2013 Michael Morris\         
    This software is released under MIT Open Source License"        ;copyright notice
    "July 20, 2013"                                                 ;date created
    "*"                                                             ;image type that the script works on
    SF-IMAGE     "Image"                                    0
    SF-DRAWABLE  "Drawable"                                 0
    SF-TOGGLE    "Antialiasing" TRUE
    SF-TOGGLE    "Feather edges" FALSE
    SF-VALUE     "Radius of feathering in X direction\n(0 <= radius <= 1000)\n(Ignored if feather edges is unchecked)" "0.0"
    SF-VALUE     "Radius of feathering in Y direction\n(0 <= radius <= 1000)\n(Ignored if feather edges is unchecked)" "0.0"
    SF-OPTION    "The selection operation" '("Add to the current selection" "Subtract from the current selection" "Replace the current selection" "Interest with the current selection")
    SF-VALUE     "X coordinate of top left corner" "0.0"
    SF-VALUE     "Y coordinate of top left corner of rectangle" "0.0"
    SF-VALUE     "Width of selection (>= 0)" "0.0"
    SF-VALUE     "Height of selection (>= 0)" "0.0"
    SF-VALUE     "Radius of rounding X direction in pixels\n(0 <= radius <= 262144)" "0.0"
    SF-VALUE     "Radius of rounding Y direction in pixels\n(0 <= radius <= 262144)" "0.0"
)

(script-fu-menu-register "select-rectangle-with-elliptical-corners" "<Image>/Select")


; ------------------------------------------------------------------------------
(define (select-rectangle-with-circular-corners inImage inDrawable inAntialias inFeather inFeatherRadius inOperation inX inY inWidth inHeight inCornerRadius)
    (do-select-rectangle-with-elliptical-corners inImage inDrawable inAntialias inFeather inFeatherRadius inFeatherRadius inOperation inX inY inWidth inHeight inCornerRadius inCornerRadius))
     
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(script-fu-register
    "select-rectangle-with-circular-corners"                        ;func name
    "Rectangle with circular corners ..."                           ;menu label
    "Creates a rectangular selection with circular corners. Makes\
    selections like the Rectangular Select Tool with the Rounded\
    corners option checked, however the radius limit is 262144\
    pixels instead of 100 pixels."                                  ;description
    "Michael Morris"                                                ;author
    "Copyright (c) 2013 Michael Morris\         
    This software is released under MIT Open Source License"        ;copyright notice
    "July 20, 2013"                                                 ;date created
    "*"                                                             ;image type that the script works on
    SF-IMAGE     "Image"                                    0
    SF-DRAWABLE  "Drawable"                                 0
    SF-TOGGLE    "Antialiasing" TRUE
    SF-TOGGLE    "Feather edges" FALSE
    SF-VALUE     "Radius of feathering\n(0 <= radius <= 1000)\n(Ignored if feather edges is unchecked)" "0.0"
    SF-OPTION    "The selection operation" '("Add to the current selection" "Subtract from the current selection" "Replace the current selection" "Interest with the current selection")
    SF-VALUE "X coordinate of top left corner" "0.0"
    SF-VALUE "Y coordinate of top left corner of rectangle" "0.0"
    SF-VALUE "Width of selection (>= 0)" "0.0"
    SF-VALUE "Height of selection (>= 0)" "0.0"
    SF-VALUE "Radius of rounding in pixels\n(0 <= radius <= 262144)" "0.0"
)

(script-fu-menu-register "select-rectangle-with-circular-corners" "<Image>/Select")
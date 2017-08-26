;;; cycle-layer-visibility.scm -*-scheme-*-
;;; Author: Edward Hutchins & Randall Sawyer
;;; Version 0.2
;;; I typically bind these to Ctl+Home, Ctl+PgUp, Ctl+PgDn and Ctl+End
;;; so I can do simple layer-based animations

(define (cycle-layer-visibility img select-layer)
    (let*
        (
            (active-layer (car (gimp-image-get-active-layer img)))
            (layer-pos (car (gimp-image-get-layer-position img active-layer)))
            (new-layer-pos (select-layer layer-pos))
            (layers (gimp-image-get-layers img))
            (num-layers (car layers))
            (layer-array (cadr layers))
            (i (- num-layers 1))
        )

        (if (< new-layer-pos 0) (set! new-layer-pos i))
        (if (> new-layer-pos i) (set! new-layer-pos 0))

        (gimp-image-undo-group-start img)

        (gimp-image-set-active-layer img (aref layer-array new-layer-pos))

        (while (>= i 0)
            (let
                (
                    (layer (aref layer-array i))
                )
                (gimp-drawable-set-visible layer (if (= i new-layer-pos) TRUE FALSE))
                (set! i (- i 1))
            )
        )

        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

(define (cycle-layer-visibility-prev img)
    (cycle-layer-visibility img (lambda (layer) (- layer 1)))
)

(define (cycle-layer-visibility-next img)
    (cycle-layer-visibility img (lambda (layer) (+ layer 1)))
)

(define (cycle-layer-visibility-top img)
    (cycle-layer-visibility img (lambda () '0))
)

(define (cycle-layer-visibility-bottom img)
    (cycle-layer-visibility img (lambda () '-1))
)

(script-fu-register "cycle-layer-visibility-prev"
    _"<Image>/Layer/Stack/Select Previous Layer Only"
    "Select the layer above the current layer, making it the only visible layer"
    "Edward Hutchins & Randall Sawyer"
    "Edward Hutchins & Randall Sawyer"
    "2009"
    "RGB RGBA GRAY GRAYA"
    SF-IMAGE     "Image to use"       0
)

(script-fu-register "cycle-layer-visibility-next"
    _"<Image>/Layer/Stack/Select Next Layer Only"
    "Select the layer below the current layer, making it the only visible layer"
    "Edward Hutchins & Randall Sawyer"
    "Edward Hutchins & Randall Sawyer"
    "2009"
    "RGB RGBA GRAY GRAYA"
    SF-IMAGE     "Image to use"       0
)

(script-fu-register "cycle-layer-visibility-top"
    _"<Image>/Layer/Stack/Select Top Layer Only"
    "Select the top layer, making it the only visible layer"
    "Edward Hutchins & Randall Sawyer"
    "Edward Hutchins & Randall Sawyer"
    "2009"
    "RGB RGBA GRAY GRAYA"
    SF-IMAGE     "Image to use"       0
)

(script-fu-register "cycle-layer-visibility-bottom"
    _"<Image>/Layer/Stack/Select Bottom Layer Only"
    "Select the bottom layer, making it the only visible layer"
    "Edward Hutchins & Randall Sawyer"
    "Edward Hutchins & Randall Sawyer"
    "2009"
    "RGB RGBA GRAY GRAYA"
    SF-IMAGE     "Image to use"       0
)

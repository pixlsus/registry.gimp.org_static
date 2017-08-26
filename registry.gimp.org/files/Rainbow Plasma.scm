;;------ Rainbow Plasma --------------------
; Create an image using rainbow gradient circles

(define (script-fu-rainbow-plasma width height numCircles)
  ; Create an img and a layer
  (let* ((img (car (gimp-image-new width height 0)))
         (layer (car (gimp-layer-new img width height 0 "Rainbow Plasma" 100 0)))
         (i 0)
        )

  (gimp-image-undo-disable img)
  (gimp-image-add-layer img layer 0)

  (gimp-edit-bucket-fill layer 1 0 100 255 0 0 0)
  (gimp-context-set-gradient "Full saturation spectrum CCW")
  (while (< i numCircles)
     (gimp-edit-blend layer 3 6 2 100 0 0 0 0 0 0 1
              (rand width) (rand height) (rand width) (rand height))

     (set! i (+ i 1))
  )

  (gimp-display-new img)
  (gimp-image-undo-enable img)
))

(script-fu-register "script-fu-rainbow-plasma"
    "<Toolbox>/Xtns/Patterns/Rainbow Plasma"
    "Creates a rainbow coloured plasma pattern"
    "David Hari"
    "David Hari"
    "2006, 2008"
    ""
    SF-VALUE "Width" "256"
    SF-VALUE "Height" "256"
    SF-ADJUSTMENT "No. of circles" '(20 1 50 1 10 0 0)
)

;;------ Rainbow Plasma Animation ----------
; Create an image using rainbow gradient circles

(define (script-fu-anim-rainbow-plasma width height frames
                                 numCircles maxSpeed minSpeed)
  ; Create an img and a layer
  (let* ((img (car (gimp-image-new width height 0)))
         (circleArray (make-vector numCircles '(0 0 0 0)))
         (frameName "")
         (frameNum 0)
         (i 0)
         (j 0)
        )

  (gimp-image-undo-disable img)

  ; Create array of random circles.
  ; Circles are a list of variables:
  ;   x pos, y pos, length and speed in pixels per frame
  (while (< i numCircles)
    (vector-set! circleArray i
            (cons (rand width) (cons (rand height)
            (cons (rand (max width height))
            (cons (+ (rand (- maxSpeed minSpeed)) minSpeed) '())))))
    (set! i (+ i 1))
  )

  (gimp-context-set-gradient "Full saturation spectrum CCW")

  (while (< j frames)
    (set! frameName (string-append "Frame " (number->string (+ j 1) frameNum)))
    (let* ((layer (car (gimp-layer-new img height width 0 frameName 100 0))))

    ; Add layer to image
    (gimp-image-add-layer img layer -1)
    (gimp-edit-bucket-fill layer 1 0 100 255 0 0 0)

    (set! i 0)     ; Don't forget to reset counter!
    (while (< i numCircles)
      (gimp-edit-blend layer 3 6 2 100 0 0 0 0 0 0 1
              (car (vector-ref circleArray i))     ; get x pos of i'th element
              (cadr (vector-ref circleArray i))    ; y pos
              (+
                (car (vector-ref circleArray i))   ; Add length to x pos to get
                (caddr (vector-ref circleArray i)) ; the end pos (x2)
                (* (cadddr (vector-ref circleArray i)) j); Also add speed increment
              )
              (cadr (vector-ref circleArray i)))   ; end y pos is same as start

      (set! i (+ i 1))
    ))
    (set! j (+ j 1))
  )

  (gimp-display-new img)
  (gimp-image-undo-enable img)
))

(script-fu-register "script-fu-anim-rainbow-plasma"
    "<Toolbox>/Xtns/Anim/Rainbow Plasma"
    "Creates a rainbow coloured plasma pattern"
    "David Hari"
    "David Hari"
    "2008"
    ""
    SF-VALUE "Width" "256"
    SF-VALUE "Height" "256"
    SF-ADJUSTMENT "No. of frames" '(10 1 100 1 5 0 1)
    SF-ADJUSTMENT "No. of circles" '(20 1 50 1 10 0 0)
    SF-ADJUSTMENT "Max speed" '(6 -10 10 1 5 0 0)
    SF-ADJUSTMENT "Min speed" '(-1 -10 10 1 5 0 0)
    ;SF-OPTION "Output to" '("Layers" "XCF files")
)

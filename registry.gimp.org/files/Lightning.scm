;;------ Lightning -------------------------
; Create a lightning effect using the plasma plug-in

(define (script-fu-lightning width height hue sat turbulence desat)
  ; Create an img and a layer. width <-> height so we can rotate later
  (let* ((img (car (gimp-image-new height width 0)))
         (mainLayer (car (gimp-layer-new img height width 0 "Lightning" 100 0)))
         (secondLayer (car (gimp-layer-new img height width 0 "Plasma Layer" 100 6)))
        )

  (gimp-image-undo-disable img)
  (gimp-image-add-layer img mainLayer 0)
  (gimp-image-add-layer img secondLayer -1)

  ;(gimp-context-set-gradient "FG to BG (RGB)") -- set in gimp-edit-blend
  (gimp-context-set-foreground '(0 0 0))
  (gimp-context-set-background '(191 191 191))
  (gimp-edit-blend mainLayer 0 0 0 100 0 0 FALSE FALSE 1 0.0 TRUE 0 0 0 height)

  (plug-in-plasma 1 img secondLayer (rand) turbulence)
  (gimp-desaturate-full secondLayer desat)
  ; Merge the second with the first and assign the resulting layer back to the first
  (set! mainLayer (car (gimp-image-merge-down img secondLayer 0)))

  (gimp-invert mainLayer)
  (gimp-levels mainLayer 0 127 255 0.1 0 255)
  (gimp-colorize mainLayer hue sat 0) ;; TODO - convert RGB to HSV and use SF-COLOR

  (gimp-image-rotate img 0)  ; Rotate so the lightning bolt is vertical
  (gimp-display-new img)
  (gimp-image-undo-enable img)
))

(script-fu-register "script-fu-lightning"
    "<Toolbox>/Xtns/Patterns/Lightning"
    "Creates lightning"
    "David Hari"
    "Taken from tomcat's lightning tutorial - http://gug.sunsite.dk/tutorials/tomcat9/"
    "2008"
    ""
    SF-VALUE "Width" "256"
    SF-VALUE "Height" "256"
    SF-ADJUSTMENT "Hue"        '(250 0 360 1 5 0 0)
    SF-ADJUSTMENT "Saturation" '(100 0 100 1 10 0 0)
    SF-ADJUSTMENT "Turbulence" '(0.4 0.1 2.0 0.1 0.1 1 0)
    SF-OPTION "Desaturation method" '("Lightness" "Luminosity")
)


;;------ Lightning Animation ---------------
; Renders the lightning effect as an animation.

(define (script-fu-anim-lightning width height hue sat frames
                           turbStart turbEnd randMovement desat)
  ; Create the img. width <-> height so we can rotate later
  (let* ((img (car (gimp-image-new height width 0)))
         (randNum (rand))
         (turbVal turbStart)
         (i 0)
        )

  (gimp-image-undo-disable img)

  (gimp-context-set-foreground '(0 0 0))
  (gimp-context-set-background '(191 191 191))

  (while (< i frames)                 ;; TODO - put frame no. in string
    (let* ((mainLayer (car (gimp-layer-new img height width 0 "Frame" 100 0)))
           (secondLayer (car (gimp-layer-new img height width 0 "Plasma Layer" 100 6)))
          )

    (gimp-image-add-layer img mainLayer -1)
    (gimp-image-add-layer img secondLayer -1)
    (gimp-edit-blend mainLayer 0 0 0 100 0 0 FALSE FALSE 1 0.0 TRUE 0 0 0 height)

    ; Increment the turbulence to reach the end value
    (set! turbVal (+ turbVal (/ (- turbEnd turbStart) frames)))
    (plug-in-plasma 1 img secondLayer randNum turbVal)
    (gimp-desaturate-full secondLayer desat)
    ; Merge the second with the first and assign the resulting layer back to the first
    (set! mainLayer (car (gimp-image-merge-down img secondLayer 0)))

    (gimp-invert mainLayer)
    (gimp-levels mainLayer 0 127 255 0.1 0 255)
    (gimp-colorize mainLayer hue sat 0)    ;; TODO - convert RGB to HSV

    (if (= randMovement TRUE)
      (set! randNum (rand))  ; Pick a new random number
      '()
    )
    (set! i (+ i 1))
    )
  )

  (gimp-image-rotate img 0)  ; Rotate so the lightning bolt is vertical
  (gimp-display-new img)
  (gimp-image-undo-enable img)
))

(script-fu-register "script-fu-anim-lightning"
    "<Toolbox>/Xtns/Anim/Lightning"
    "Creates the lightning effect as an animation."
    "David Hari"
    ""
    "2008"
    ""
    SF-VALUE "Width" "256"
    SF-VALUE "Height" "256"
    SF-ADJUSTMENT "Hue"        '(250 0 360 1 5 0 0)
    SF-ADJUSTMENT "Saturation" '(100 0 100 1 10 0 0)
    SF-ADJUSTMENT "No. of frames" '(10 1 100 1 5 0 1)
    SF-ADJUSTMENT "Turbulence start" '(0.4 0.1 2.0 0.1 0.1 1 0)
    SF-ADJUSTMENT "Turbulence end"   '(1.0 0.1 2.0 0.1 0.1 1 0)
    SF-TOGGLE "Random movement" TRUE
    SF-OPTION "Desaturation method" '("Lightness" "Luminosity")
)

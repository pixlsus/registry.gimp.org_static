;;------ Flames ----------------------------
; Create the effect of flames rising from the bottom of
; the image


(define (script-fu-flames width height numFlames gradient)
  (let* ((img (car (gimp-image-new width height 0)))
         (layer (car (gimp-layer-new img width height 0 "Flames" 100 0)))
         (fgOld (car (gimp-context-get-foreground))) ; May be used for gradient
         (bgOld (car (gimp-context-get-background)))
         (numPoints (+ (* numFlames 4) 2)) ; number of points
         (pointArray (make-vector numPoints 0.0)) ; points for pencil tool
         (posX 0) (posY (- height 2))   ; x, y pos for pencil tool
         (spkInc (/ width numFlames))      ; The increment for each spike
         (i 0)
        )

;;;;;;;;
;; FIXME - 'Error: make-vector: argument 1 must be: non-negative integer'
;;         for some values of 'numFlames' and width/height
;;;;;;;;
  (gimp-context-push)   ; Remember previous settings
  (gimp-image-undo-disable img)
  (gimp-image-add-layer img layer 0)
  (gimp-context-set-foreground '(255 255 255))
  (gimp-context-set-background '(0 0 0))
  (gimp-edit-clear layer)

  ; Create the spikes that are going to be our flames
  (while (< i numPoints)
    (vector-set! pointArray i posX)
    (vector-set! pointArray (+ i 1) posY)
    (if (> i (- numPoints 6))
      (set! posX width)   ; Last point is at the very edge
      (set! posX (+ (* (/ i 4) spkInc) (- (rand (* spkInc 0.6)) (* spkInc 0.3))))
    )
    (if (> (remainder i 4) 1)    ; If we need a peak or a trough
      (set! posY (+ (rand (* height 0.3)) (* height 0.2)))
      (set! posY (- (+ (rand (* height 0.2)) (* height 0.8)) 2))
    )

    (set! i (+ i 2))
  )
  ; Now draw the spikes and fill them in
  (gimp-context-set-brush "Circle (01)")
  (gimp-pencil layer numPoints pointArray)
  (gimp-edit-bucket-fill layer 0 0 100 0 FALSE 0 (- height 1))

  ; Set fg/bg to previous in case the user selects "FG to BG" as gradient
  (gimp-context-set-foreground fgOld)
  (gimp-context-set-background bgOld)

  ; Ripple and blur the spikes to look like flames
  (plug-in-ripple 1 img layer (/ height 4) (/ width 50) 0 1 0 TRUE FALSE)
  (plug-in-gauss-rle2 1 img layer spkInc spkInc)

  ; Now map the specified grapient
  (gimp-context-set-gradient gradient)
  (plug-in-gradmap 1 img layer)

  (gimp-context-pop)   ; Restore previous settings
  (gimp-image-undo-enable img)
  (gimp-display-new img)
))

(script-fu-register "script-fu-flames"
    "<Toolbox>/Xtns/Patterns/Flames..."
    "Create the effect of flames rising from the bottom of the image"
    "David Hari"
    "Taken from tomcat's fire tutorial - http://gug.sunsite.dk/tutorials/tomcat13/"
    "2008"
    ""
    SF-VALUE "Width"  "256"
    SF-VALUE "Height" "256"
    SF-ADJUSTMENT "No. of flames" '(7 1 50 1 2 0 0)
    SF-GRADIENT "Gradient" "German flag smooth"
)


;;------ Flames Animation ------------------
; Renders the flames effect as an animation.

(define (script-fu-anim-flames width height frames numFlames gradient)
  (let* ((img (car (gimp-image-new width height 0)))
         (layer (car (gimp-layer-new img width height 0 "Flames" 100 0)))
         (fgOld (car (gimp-context-get-foreground))) ; May be used for gradient
         (bgOld (car (gimp-context-get-background)))
         (numPoints (+ (* numFlames 4) 2)) ; number of points
         (pointArray (make-vector numPoints 0.0)) ; points for pencil tool
         (period (/ height 4)) (amp (/ width 50)) ; Period and amplitude of ripple
         (posX 0) (posY (- height 2))  ; x, y pos for pencil tool
         (spkInc (/ width numFlames))  ; The increment for each spike
         (rippleOffset 0)              ; Wave period offset for each frame
         (frameName "") (frameNum 0)
         (i 0) (j 0)
        )

  (gimp-context-push)   ; Remember previous settings
  (gimp-image-undo-disable img)
  (gimp-image-add-layer img layer 0)
  (gimp-context-set-foreground '(255 255 255))
  (gimp-context-set-background '(0 0 0))
  (gimp-edit-clear layer)

  ; Create the spikes that are going to be our flames
  (while (< i numPoints)
    (vector-set! pointArray i posX)
    (vector-set! pointArray (+ i 1) posY)
    (if (> i (- numPoints 6))
      (set! posX width)   ; Last point is at the very edge
      (set! posX (+ (* (/ i 4) spkInc) (- (rand (* spkInc 0.6)) (* spkInc 0.3))))
    )
    (if (> (remainder i 4) 1)    ; If we need a peak or a trough
      (set! posY (+ (rand (* height 0.3)) (* height 0.2)))
      (set! posY (- (+ (rand (* height 0.2)) (* height 0.8)) 2))
    )

    (set! i (+ i 2))
  )
  ; Now draw the spikes and fill them in
  (gimp-context-set-brush "Circle (01)")
  (gimp-pencil layer numPoints pointArray)
  (gimp-edit-bucket-fill layer 0 0 100 0 FALSE 0 (- height 1))

  ; Set fg/bg to previous in case the user selects "FG to BG" as gradient
  (gimp-context-set-foreground fgOld)
  (gimp-context-set-background bgOld)

  (while (< j frames)
    (let* ((newLayer (car (gimp-layer-copy layer FALSE))))

    (gimp-image-add-layer img newLayer -1)

    ; Name the layer "Frame X"
    (set! frameName (string-append "Frame " (number->string (+ j 1) frameNum)))
    (gimp-drawable-set-name newLayer frameName)

    ; Because plug-in-ripple has no offset, we have to make our own by adding
    ; a bit to the image height so the ripple effect starts from there instead
    (gimp-image-resize img width (+ height rippleOffset) 0 rippleOffset)
    (gimp-layer-resize-to-image-size newLayer)

    ; Ripple and blur the spikes to look like flames
    (plug-in-ripple 1 img newLayer period amp 0 0 0 TRUE FALSE)
    (plug-in-gauss-rle2 1 img newLayer spkInc spkInc)

    ; Now map the specified grapient
    (gimp-context-set-gradient gradient)
    (plug-in-gradmap 1 img newLayer)

    ; Now we have to re-resize the image and layer back to the original size
    (gimp-image-resize img width height 0 (- 0 rippleOffset))
    (gimp-layer-resize-to-image-size newLayer)

    (set! rippleOffset (+ rippleOffset (/ period frames)))
    (set! j (+ j 1))
  ))
  (gimp-image-remove-layer img layer) ; Remove the original layer

  (gimp-context-pop)   ; Restore previous settings
  (gimp-image-undo-enable img)
  (gimp-display-new img)
))

(script-fu-register "script-fu-anim-flames"
    "<Toolbox>/Xtns/Anim/Flames..."
    "Creates the flames effect as an animation."
    "David Hari"
    ""
    "2008"
    ""
    SF-VALUE "Width"  "256"
    SF-VALUE "Height" "256"
    SF-ADJUSTMENT "No. of frames" '(10 1 100 1 5 0 1)
    SF-ADJUSTMENT "No. of flames" '(7 1 50 1 2 0 0)
    SF-GRADIENT "Gradient" "German flag smooth"
)

;; -*- mode: gimp -*-
(define (random-in-range a b)
  (let ((r (random (max a b))))
    (if (>= r a)
	r
	(random-in-range a b))))

(define (script-fu-sunray img sun-radius amount thickness start-angle randomize outer-radius flowery)

  (gimp-image-undo-group-start img)

  (let* ((img-width (car (gimp-image-width img)))
         (img-height (car (gimp-image-height img)))
         (radius (or (and (> outer-radius 0) outer-radius) 
                     (sqrt (* (pow (/ img-height 2) 2)
                              (pow (/ img-width 2) 2)))))
         (angle (/ 360 amount 2))
         (offset-x (/ img-width 2))
         (offset-y (/ img-height 2))
         ;; reset 'booleans' to true lisp booleans:
         (randomize (= TRUE randomize))	
         (flowery (= TRUE flowery)))

    (letrec ((deg->rad 
              (lambda (d)
                (* (/ *pi* 180.0) d)))
             (point-on-circle
              (lambda (r angle)
                (let ((angle (deg->rad angle)))
                  (list (round (* r (cos angle)))
                        (round (* r (sin angle)))))))
             (adjust
              (lambda (point) 
                (list (+ (car point) offset-x)
                      (+ (cadr point) offset-y))))
             (make-piece-of-pie
              (lambda (radius from-angle angle offset-x offset-y)
                (append (adjust (point-on-circle radius from-angle))
                        (adjust (point-on-circle radius (+ from-angle angle)))
                        (list offset-x offset-y))))
             (draw-figure
              (lambda (img fig)
                (gimp-free-select img (length fig) 
                                  (list->vector fig)
                                  CHANNEL-OP-ADD 
                                  0
                                  0
                                  0)))
             (define-flower-for-pie 
               (lambda (pie)
                 (let ((diameter (sqrt 
                                  (+ (pow (abs (-  (nth 0 pie) 
                                                   (nth 2 pie))) 2)
                                     (pow (abs (-  (nth 1 pie) 
                                                   (nth 3 pie))) 2)))))
                   (list (- (/ (+ (nth 0 pie)
                                  (nth 2 pie)) 2) 
                            (/ diameter 2))
                         (- (/ (+ (nth 1 pie)
                                  (nth 3 pie)) 2)
                            (/ diameter 2))
                         diameter
                         diameter
                         CHANNEL-OP-ADD TRUE FALSE 0)))))

      (gimp-selection-none img)

      (let loop ((s-a start-angle)
                 (angle angle))
        (let ((pie (make-piece-of-pie radius
                                      s-a 
                                      (if randomize
                                          (random-in-range 5 (- (* 2 angle) 5))
                                          (* angle thickness))
                                      offset-x offset-y))) 
          (draw-figure img pie)
          (when flowery 
            (apply gimp-ellipse-select img (define-flower-for-pie pie))))
        
        (when (< (+ s-a (* 2 angle)) (+ 360 start-angle))
          (loop (+ s-a (* 2 angle)) angle offset-x offset-y)))
      
      (gimp-ellipse-select img   
                           (- (/ (car (gimp-image-width img))
                                 2) 
                              sun-radius)
                           (- (/ (car (gimp-image-height img))
                                 2) 
                              sun-radius)
                           (* 2 sun-radius)
                           (* 2 sun-radius)
                           CHANNEL-OP-SUBTRACT
                           TRUE
                           FALSE
                           0))

    (gimp-image-undo-group-end img)))

(script-fu-register "script-fu-sunray"
                    _"<Image>/Tools/Selection Tools/Sunray Pattern..."
                    "Sets the selection to a sunray-like pattern."
                    "Niels Giesen (niels.giesen@gmail.com)"
                    "Niels Giesen"
                    "2008-04-27"
                    "" 
                    SF-IMAGE _"Image to work on" 1
		    SF-ADJUSTMENT _"Radius of the Sun (Pixels)" '(35 0 1000 1 50 0 1)
		    SF-ADJUSTMENT _"Amount of rays" '(8 2 40 1 5 0 1)
		    SF-ADJUSTMENT _"Thickness (Thickness of rays 0 =< 2)"	'(.5 0 2 .01 .1 2 1)
		    SF-ADJUSTMENT _"Start angle degrees" '(0 0 90 1 15 0 1)
		    SF-TOGGLE _"Randomize? (Give rays a random thickness)" FALSE
		    SF-ADJUSTMENT _"Outer radius (Value of 0 means image size)" '(0 0 2000 10 100 0 1)
		    SF-TOGGLE _"Flowery? (Only has effect when outer radius is smaller than image size)" FALSE)




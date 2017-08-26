(define DEFAULT-POS (list 0 0))
(define DEFAULT-DIR 0)
(define DEFAULT-STEP 5)
(define DEFAULT-ANGLE 90)
(define DEFAULT-NAME "auto turtle")
(define number-of-auto-turtle 0)
(define empty '())

;a turtle is born!
(define (new-turtle inName)  
  (cond ((eqv? inName empty)
         (turtle-name! empty (turtle-name empty)))
        ('else
          (turtle-name! empty inName)) )
  )

;turtle-fields gets
(define (turtle-name inTurtle)
  (cond 
    ((or (eqv? inTurtle empty)
         (not (string? (car inTurtle))))
     (name-new-turtle DEFAULT-NAME))
    ('else (car inTurtle)))
  )

(define (name-new-turtle inName)
  (begin
    (set! number-of-auto-turtle (+ 1  number-of-auto-turtle))
    (string-append inName (number->string  number-of-auto-turtle))
    )
  )

(define (turtle-coor inTurtle)
  (cond 
    ((or (eqv? inTurtle empty) 
         (eqv? (cdr inTurtle) empty) 
         ) 
     (list DEFAULT-POS DEFAULT-DIR))
    ('else (list (coor-pos (cadr inTurtle))
                 (coor-dir (cadr inTurtle))) ))
  )

(define (coor-pos inCoor)
  (cond ((eqv? empty inCoor) DEFAULT-POS)
        ('else (list (pos-x(car inCoor)) (pos-y(car inCoor)))))
  )

(define (pos-x inPos)
  (cond ((eqv? empty inPos) (car DEFAULT-POS))
        ('else (car inPos)) )
  )

(define (pos-y inPos)
  (cond ((or(eqv? empty inPos)
            (eqv? empty (cdr inPos)))
         (cadr DEFAULT-POS))
        ('else (cadr inPos)) )
  )

(define (coor-dir inCoor)
  (cond ((or(eqv? empty inCoor)
            (eqv? empty (cdr inCoor))) DEFAULT-DIR)
        ('else (cadr inCoor)))
  )

(define (turtle-pos inTurtle)
  (coor-pos (turtle-coor inTurtle)))

(define (turtle-pos-x inTurtle)
  (pos-x (coor-pos (turtle-coor inTurtle))))

(define (turtle-pos-y inTurtle)
  (pos-y (coor-pos (turtle-coor inTurtle))))

(define (turtle-dir inTurtle)
  (coor-dir (turtle-coor inTurtle)))

(define (turtle-status inTurtle)
  (cond ((or (eqv? empty inTurtle)
             (eqv? empty (cdr inTurtle))
             (eqv? empty (cddr inTurtle)))
         (list DEFAULT-STEP DEFAULT-ANGLE TRUE))
        ('else 
          (list (status-step  (caddr inTurtle))
                (status-angle (caddr inTurtle))
                (status-pen (caddr inTurtle)) )))                
  )

(define (status-step inStatus)
  (cond ((eqv? empty inStatus) DEFAULT-STEP)
        ('else (car inStatus)))
  )

(define (status-angle inStatus)
  (cond ((or(eqv? empty inStatus)
            (eqv? empty (cdr inStatus)))
         DEFAULT-ANGLE)
        ('else (cadr inStatus)) )
  )

(define (status-pen inStatus)
  (cond ((or(eqv? empty inStatus)
            (eqv? empty (cdr inStatus))
            (eqv? empty (cddr inStatus)))
         TRUE)
        ('else (caddr inStatus)) )
  )

(define (turtle-step inTurtle)
  (status-step (turtle-status inTurtle)))

(define (turtle-angle inTurtle)
  (status-angle (turtle-status inTurtle))) 

(define (turtle-pen inTurtle)
  (status-pen (turtle-status inTurtle))) 

;turtle-fields sets
(define (turtle-name! inTurtle inName)
  (list inName (turtle-coor inTurtle) (turtle-status inTurtle)))

(define (turtle-coor! inTurtle inCoor)
  (list (turtle-name inTurtle) inCoor (turtle-status inTurtle)))

(define (turtle-status! inTurtle inStatus)
  (list (turtle-name inTurtle) (turtle-coor inTurtle) inStatus))

(define (coor-pos! inCoor inPos)
  (list inPos (coor-dir inCoor)))

(define (pos-x! inPos inX)
  (list inX (pos-y inPos)))

(define (pos-y! inPos inY)
  (list (pos-x inPos) inY))

(define (coor-dir! inCoor inDir)
  (list (coor-pos inCoor) inDir))

(define (status-step! inStatus inStep)
  (list inStep (status-angle inStatus) (status-pen inStatus)))

(define (status-angle! inStatus inAngle)
  (list (status-step inStatus) inAngle (status-pen inStatus)))

(define (status-pen! inStatus inAct)
  (list (status-step inStatus) (status-angle inStatus) inAct))

;turtle-move

(define (forward inTurtle)
  (let ( 
        (x (turtle-pos-x inTurtle))
        (y (turtle-pos-y inTurtle))
        (dir (deg->rad (turtle-dir inTurtle)))
        )   
    (turtle-coor! inTurtle
                  (coor-pos! (turtle-coor inTurtle) 
                             (list 
                              (+ x (* (turtle-step inTurtle) (cos dir))) 
                              (+ y (* (turtle-step inTurtle) (sin dir)))
                              )))
    ))

(define (backward inTurtle)
  (let ( 
        (x (turtle-pos-x inTurtle))
        (y (turtle-pos-y inTurtle))
        (dir (deg->rad (turtle-dir inTurtle)))
        )   
    (turtle-coor! inTurtle
                  (coor-pos! (turtle-coor inTurtle) 
                             (list 
                              (- x (* (turtle-step inTurtle) (cos dir))) 
                              (- y (* (turtle-step inTurtle) (sin dir)))
                              )))
    ))

(define (right inTurtle)
  (let (
        (dir (norm-angle (+ (turtle-dir inTurtle) (turtle-angle inTurtle))))
        )
    (turtle-coor! inTurtle
                  (coor-dir! (turtle-coor inTurtle) dir))
    ))

(define (left inTurtle)
  (let (
        (dir (norm-angle (- (turtle-dir inTurtle) (turtle-angle inTurtle))))
        )   
    (turtle-coor! inTurtle
                  (coor-dir! (turtle-coor inTurtle) dir))    
    ))

(define (up inTurtle)
  (turtle-status! inTurtle (status-pen! (turtle-status inTurtle) FALSE))
  )

(define (down inTurtle)
  (turtle-status! inTurtle (status-pen! (turtle-status inTurtle) TRUE))
  )

;vector-oper

(define (vect-append inVect1 inVect2)
  (letrec ((loop (lambda (theVect aInd)
                   (cond ((< aInd (length inVect1))
                          (begin
                            (aset theVect aInd (aref inVect1 aInd))
                            (loop theVect (+ 1 aInd)) ))
                         ((< aInd (length theVect))
                          (begin
                            (aset theVect aInd (aref inVect2 (- aInd (length inVect1)))) 
                            (loop theVect (+ 1 aInd)) ))
                         ('else theVect))
                   )) )
    (loop (cons-array (+ (length inVect1) (length inVect2)) 'double) 0)
    )
  )

;conversion

(define (string->list inString)
  (letrec (
           (string->list_aux 
            (lambda (inString inList) 
              (cond ((= 0 (string-length inString)) inList)
                    ('else 
                      (string->list_aux (substring inString 1 (string-length inString))
                                        (append inList (list (substring inString 0 1))) )
                      ))
              ))
           )
    (string->list_aux inString empty)
    )   
  )

;from point x,y coordinates #(s1.x s1.y ... sn.x sn.y) 
;to path format - point + 2 control points

(define (pos->path-vect inPos)
  (letrec (
           (pos->path-vect-aux 
            (lambda (inPos inVect)
              (cond ((null? inPos) inVect)
                    ('else 
                      (let* (
                             (thePath (cons-array 9 'double)) 
                             )
                        (aset thePath 0 (pos-x inPos))
                        (aset thePath 1 (pos-y inPos))
                        (aset thePath 2 2.0)            ;control point
                        (aset thePath 3 (pos-x inPos))
                        (aset thePath 4 (pos-y inPos))
                        (aset thePath 5 1.0)            ;drawing point
                        (aset thePath 6 (pos-x inPos))
                        (aset thePath 7 (pos-y inPos))
                        (aset thePath 8 2.0)            ;control point
                        (cond ((null? inVect) (pos->path-vect-aux  (cddr inPos) thePath))
                              ('else (pos->path-vect-aux  
                                      (cddr inPos) (vect-append inVect thePath)) ))
                        ))
                    ))
            ))
    (pos->path-vect-aux inPos empty)
    )
  )

(define (pos->draw-vect inPos)
  (letrec (
           (pos->draw-vect-aux 
            (lambda (inPos inVect)
              (cond ((null? inPos) inVect)
                    ('else 
                      (let* (
                             (theVect (cons-array 2 'double)) 
                             )
                        (aset theVect 0  (pos-x inPos))
                        (aset theVect 1  (pos-y inPos))
                        (cond ((null? inVect) (pos->draw-vect-aux (cddr inPos) theVect))
                              ('else (pos->draw-vect-aux (cddr inPos)
                                                         (vect-append inVect theVect)) ))
                        ))
                    ))
            ))
    (pos->draw-vect-aux inPos empty)
    )
  )

;angle-oper
(define (deg->rad inAngle)
  (* inAngle (/  *pi* 180))
  )

(define (norm-angle inAngle)
  (cond ((> *pi* 180) (norm-angle (- inAngle 360)))
        ((< *pi* -180) (norm-angle (+ inAngle 360)))
        ('else inAngle)) 
  )

(script-fu-register
 "script-fu-turtle"
 "<Image>/Filters/Render/Turtle graphics/Turtle"
 "turtle graphics version 1.2\nPath - the turtle move commands\n F : forward, pen down \n B : backward, pen down \n G: forward, pen up\n C : backward, pen up\n + : turn left Angle \n - : turn right Angle \nAngle - in degrees\nSteps Number - steps number to draw a straight line of image width length\nCenter - if no, the first step will be at the center of the \"Scale\" object\nScale - if no, the turtle step will be image width /Steps Number"
 "Annamaria Trincardi"
 "2004, Annamaria Trincardi"
 "2004-12-04"
 "*"
 SF-IMAGE 	"image"				0
 SF-DRAWABLE 	"drawable"			0
 SF-STRING     "Path"               "F-F+F-F"
 SF-ADJUSTMENT 	"Angle" '(60 15 180 15 30 0 0)
 SF-ADJUSTMENT 	"Steps Number"  '(4 1 10 1 2 0 0)  
 SF-OPTION	"Center" '("Image" "Layer" "Mask" "No")
 SF-OPTION	"Scale" '("Image" "Layer" "Mask" "No")
 SF-OPTION	"Keep Aspect Ratio" '("Yes" "No")
 SF-OPTION	"Tool" '("Path" "Pencil" "Brush" "Airbrush")
 )

(define (script-fu-turtle
         inImage 
         inDrawable
         inString
         inAngle
         inStep
         inCenter
         inResizeTo
         inRatio
         inTool
         )
  ;; init
  ; create image  
  ; (turn undo off)
  (gimp-image-undo-group-start inImage )
  ;create, add and clear layer 
  ;;main 
  ;set defaults
  (set! DEFAULT-ANGLE inAngle)
  (set! DEFAULT-STEP (/ (car (gimp-image-width inImage)) inStep)) 
  (let* (
         ;"original" path list - start pos DEFAULT-POS, step DEFAULT-STEP
         (theTrail (turtle-trail (new-turtle empty) (string->list inString) empty))
         
         (theTrailBounds (trail-bounds theTrail))
         (theTrailDim (list
                       (- (pos-x (cddr theTrailBounds)) (pos-x theTrailBounds))
                       (- (pos-y (cddr theTrailBounds)) (pos-y theTrailBounds))
                       ))
         (theTrailCenter (list
                          (+ (pos-x theTrailBounds) (/ (pos-x theTrailDim) 2))
                          (+ (pos-y theTrailBounds) (/ (pos-y theTrailDim) 2))
                          ))
         
         (theCenterToDimOff (dim-center inImage inDrawable inCenter))
         (theResizeToDimOff (dim-center inImage inDrawable inResizeTo))
  
         (theBorder (cond ((= 1 inStep) 0)
                          ('else DEFAULT-STEP)))
         (theScale (list
                    (cond ((= 3 inResizeTo) 1)
                          ((= 0 (pos-x theTrailDim)) 1)
                          ('else (/ (- (pos-x (car theResizeToDimOff)) theBorder)
                                    (pos-x theTrailDim)) ))
                    (cond ((= 3 inResizeTo) 1)
                          ((= 0 (pos-y theTrailDim)) 1)
                          ('else (/ (- (pos-y (car theResizeToDimOff)) theBorder)
                                    (pos-y theTrailDim)) ))
                    ))                   
         (theMinScale (min (pos-x theScale) (pos-y theScale)) )
         )
    (cond ((= 0 inRatio)
           (set! theScale (list theMinScale theMinScale)) ))
    (let* (
           (theShift (cond ((= 3 inCenter) 
                            (list
                             (- (pos-x (cadr theResizeToDimOff))
                                (* (pos-x theScale) (pos-x (car theTrail))))
                             (- (pos-y (cadr theResizeToDimOff))
                                (* (pos-y theScale) (pos-y (car theTrail))))) )
                           ('else 
                             (list 
                              (- (pos-x (cadr theCenterToDimOff))
                                 (* (pos-x theScale) (pos-x theTrailCenter)))
                              (- (pos-y (cadr theCenterToDimOff))
                                 (* (pos-y theScale) (pos-y theTrailCenter))))) 
                           )) 
           ;"drawable" path list - transformed to fit image/drawable
           (theDrawTrail (trail-transform theTrail theScale theShift))
           )
      (draw-trail inImage inDrawable theDrawTrail inTool) 
      )
    )
  ;; exit
  ; (turn dirty off)
  ; display inImage
  (gimp-displays-flush)
  ; (turn undo on)
  (gimp-image-undo-group-end inImage)
  )

(define (turtle-trail inTurtle inAction inTrail)
   (cond ((eqv? empty inTrail)
         (turtle-trail inTurtle inAction (list (turtle-pos inTurtle))) )
         ((eqv? empty inAction) inTrail)
        ('else   
          (let* (
                 (theAction (car inAction))
                 (newTurtle 
                  (cond ((equal? "+" theAction) (right inTurtle)) 
                        ((equal? "-" theAction) (left inTurtle)) 
                        ((equal? "F" theAction) (forward(down inTurtle)))
                        ((equal? "B" theAction) (backward(down inTurtle)))               
                        ((equal? "G" theAction) (forward(up inTurtle)))
                        ((equal? "C" theAction) (backward(up inTurtle)))
                        ('else inTurtle)
                        ))
                 )
            (cond ((equal? (turtle-pos newTurtle) (turtle-pos inTurtle)) 
                   (turtle-trail newTurtle (cdr inAction) inTrail) )
                  ;pen down - draws a line 
                  ((equal? (turtle-pen newTurtle) TRUE)
                   (turtle-trail 
                    newTurtle (cdr inAction)
                    (append (list (append (turtle-pos newTurtle) (car inTrail)))
                            (cdr inTrail)))
                   )
                  ;pen up - draws start/stop points
                  ('else
                    (turtle-trail 
                     newTurtle (cdr inAction)
                     (append (list (turtle-pos newTurtle)) inTrail)) 
                    ))
            )
          ))
  )

;draws trail using selected tool
(define (draw-trail inImage inDrawable inTrail inTool)
  (cond ((null? inTrail) empty)
        ('else
          ;from list to vector 
          (let* (
                 (theVect (cond ((= 0 inTool) (pos->path-vect (car inTrail))) 
                                ('else (pos->draw-vect  (car inTrail)) )) )
                 )
            (print theVect)
            (cond ((= 0 inTool)
                   (gimp-path-set-points inImage "turtle path" 1 
                                        (- (length theVect) 3) theVect) )
                  ('else
                    (cond
                      ((= 1 inTool) 
                       (gimp-pencil inDrawable (length theVect) theVect) )
                      ((= 2 inTool) 
                       (gimp-paintbrush-default inDrawable (length theVect) theVect) )
                      ((= 3 inTool) 
                       (gimp-airbrush-default inDrawable (length theVect) theVect) )
                      )))
            )
          (draw-trail inImage inDrawable (cdr inTrail) inTool) ))
  )

(define (trail-bounds inTrail)
  (letrec (
           (trail-bounds-aux 
            (lambda (inTrail inBounds)
              (cond ((null? inTrail) inBounds)
                    ('else
                      (trail-bounds-aux (cdr inTrail)
                                        (pos-bounds (car inTrail) inBounds)) )))
            ))
    (trail-bounds-aux inTrail empty)
    )
  )

(define (pos-bounds inPos inBounds)
  (cond ((null? inPos) inBounds)
        ((null? inBounds) (pos-bounds inPos (list (pos-x inPos) (pos-y inPos)
                                                  (pos-x inPos) (pos-y inPos))) )
        ('else (pos-bounds (cddr inPos)
                           (list (min (pos-x inPos) (pos-x inBounds))
                                 (min (pos-y inPos) (pos-y inBounds))
                                 (max (pos-x inPos) (pos-x (cddr inBounds)))
                                 (max (pos-y inPos) (pos-y (cddr inBounds))) ))
          ))                      
  )

(define (trail-transform inTrail inScale inShift)
  (letrec (
           (trail-transform-aux
            (lambda (inTrail inTransform)
              (cond ((null? inTrail) inTransform)
                    ('else 
                      (trail-transform-aux
                       (cdr inTrail) 
                       (append inTransform
                               (list(pos-transform (car inTrail) inScale inShift empty))
                               ))
                      ))
              ))
           )
    (trail-transform-aux inTrail empty)
    )
  )

(define (pos-transform inPos inScale inShift inTransform)
  (cond ((null? inPos) inTransform)
        ('else
          (pos-transform 
           (cddr inPos) inScale inShift
           (append inTransform
                   (list (+ (* (pos-x inPos) (pos-x inScale)) (pos-x inShift))
                         (+ (* (pos-y inPos) (pos-y inScale)) (pos-y inShift)) )) ) ))
  )


(define (dim-center inImage inDrawable inType)
  (let* (
         (theOffset
          (cond ((= 0 inType) 
                 (list (- 0 (pos-x (gimp-drawable-offsets inDrawable)))
                       (- 0 (pos-y (gimp-drawable-offsets inDrawable)))) )
                ((= 1 inType) DEFAULT-POS)
                ((= 2 inType) 
                 (list
                  (pos-x (cdr(gimp-drawable-mask-bounds inDrawable)))
                  (pos-y (cdr(gimp-drawable-mask-bounds inDrawable)))) )
                ('else DEFAULT-POS) ))
         (theDim
          (cond ((= 0 inType) (list (car (gimp-image-width inImage)) 
                                    (car (gimp-image-height inImage))) )
                ((= 1 inType) (list (car (gimp-drawable-width inDrawable))
                                    (car (gimp-drawable-height inDrawable))) )
                ((= 2 inType) 
                 (list
                  (- (pos-x (cdddr(gimp-drawable-mask-bounds inDrawable)))
                     (pos-x (cdr(gimp-drawable-mask-bounds inDrawable)))) 
                  (- (pos-y (cdddr(gimp-drawable-mask-bounds inDrawable)))
                     (pos-y (cdr(gimp-drawable-mask-bounds inDrawable))))) ) 
                ('else (list (car (gimp-image-width inImage)) 
                             (car (gimp-image-height inImage))) )))
         (theCenter (list 
                     (+ (pos-x theOffset) (/ (pos-x theDim) 2))
                     (+ (pos-y theOffset) (/ (pos-y theDim) 2)) ))
         )
    (list theDim theCenter)
    )
  )

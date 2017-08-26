;conversion
(define (list->string inList)  
  (cond ((eqv? inList empty) "")
        ('else
          (string-append (car inList) 
                         (list->string (cdr inList))) ))
  )

(define (vector->string inList inInd)
  (cond ((< inInd (length inList)) 
         (string-append (number->string(aref inList inInd)) 
                        " "
                        (vector->string inList (+ 1 inInd))))
        ('else ""))
  ) 

(define (string->list inString)
  (letrec (
           (string->list_aux 
            (lambda (inString inList) 
               (cond ((= 0 (string-length inString)) inList)
                    ('else 
                      (string->list_aux (substring inString 1 (string-length inString))
                                        (append inList
                                                (list (substring inString 0 1))
                                                ))
                      ))
              ))
           )
    (string->list_aux inString empty)
    )   
  )


(define (string->mlist inString inSkip inBreak)
   (letrec (
           (string->list_aux 
            (lambda (inString inMList) 
                (cond ((= 0 (string-length inString)) inMList)
                    ((equal? inSkip (substring inString 0 1))
                     (string->list_aux (substring inString 1 (string-length inString))
                                       inMList) )
                    ((equal? inBreak (substring inString 0 1))
                     (string->list_aux (substring inString 1 (string-length inString))
                                       (append inMList (list empty))) )
                    ('else 
                      (string->list_aux (substring inString 1 (string-length inString))
                                        (mlist_append inMList
                                                      (list (substring inString 0 1))
                                                      ))
                      ))
              ))
           )
    (string->list_aux inString empty)
    )   
  )

(define (mlist_append inMList inList)
  (cond ((null? inMList) (list inList))
        ('else
          (letrec (
                   (mlist_append_aux 
                    (lambda (inMList inMList-append)
                      (cond ((null? inMList) inMList-append)
                            ((null? (cdr inMList))
                             (mlist_append_aux (cdr inMList) 
                                               (append inMList-append 
                                                       (list (append (car inMList) 
                                                                     inList)) )) )
                            ('else
                              (mlist_append_aux (cdr inMList) 
                                                (append inMList-append 
                                                        (list (car inMList)) ))
                              ))
                      ))
                   )
            (mlist_append_aux inMList empty)
            )
          ))
  )

;L-system

(define (L-system inIter inAxiom inRules)
   (cond 
    ((eqv? empty inRules) inAxiom)
    ((= 0 inIter) inAxiom)
    ('else 
      (L-system 
       (- inIter 1 ) 
       (L-system-iter inAxiom inRules empty)
       inRules )
      ))
  )

(define (L-system-iter inList inRules inTransf)
  (cond
    ((eqv? empty inList) inTransf) 
    ((eqv? empty inRules) (append inTransf inList))
    ('else
      (L-system-iter (cdr inList) inRules
                     (append inTransf (L-system-transform (car inList) inRules))) 
      ))
  )

(define (L-system-transform inElement inRules )
  (cond
    ((eqv? empty inRules) (list inElement) )
    ((equal? inElement (caar inRules)) (cdar inRules) )
    ('else 
      (L-system-transform inElement (cdr inRules) )
      ))
  )

(script-fu-register
 "script-fu-turtle-L-system"
 "<Image>/Filters/Render/Turtle graphics/L-system"
 "L-system version 1.1"
 "Annamaria Trincardi"
 "2004, Annamaria Trincardi"
 "2004-12-11"
 "*"
 SF-IMAGE 	"image"				0
 SF-DRAWABLE 	"drawable"			0
 SF-STRING     "Axiom"               "F++F++F"
 SF-STRING     "Rules"               "F:F-F++F-F"
 SF-STRING     "Last rule"               ""
 SF-ADJUSTMENT 	"Iter" '(4 0 10 1 2 1 1)
 SF-ADJUSTMENT 	"Angle" '(60 15 180 15 30 0 0)
 SF-ADJUSTMENT 	"Steps Number"  '(4 1 10 1 2 0 0)  
 SF-OPTION	"Center" '("Image" "Layer" "Mask" "No")
 SF-OPTION	"Scale" '("Image" "Layer" "Mask" "No")
 SF-OPTION	"Keep Aspect Ratio" '("Yes" "No")
 SF-OPTION	"Tool" '("Path" "Pencil" "Brush" "Airbrush")
 )

(define (script-fu-turtle-L-system
         inImage 
         inDrawable
         inAxiom
         inRules
         inLastRule
         inIter
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
  (let* (
         (L-System-path
          (cond ((> (string-length inRules) 2)
                 (list->string
                  (L-system 1 
                            (L-system inIter 
                                      (string->list inAxiom)
                                      (string->mlist inRules ":" ";"))
                            (string->mlist inLastRule ":" ";"))) )
                ('else
                  (list->string
                   (L-system inIter 
                             (string->list inAxiom)
                             (string->mlist inRules ":" ";")))
                  
                  ))
          )
         )
    (script-fu-turtle inImage inDrawable 
                      L-System-path inAngle inStep 
                      inCenter inResizeTo inRatio inTool
                      )
    )
  ;; exit
  ; (turn dirty off)
  ; display inImage
  (gimp-displays-flush)
  ; (turn undo on)
  (gimp-image-undo-group-end inImage)
  )

(script-fu-register
 "script-fu-turtle-shapes"
 "<Image>/Filters/Render/Turtle graphics/shapes"
 "shapes version 1.1"
 "Annamaria Trincardi"
 "2004, Annamaria Trincardi"
 "2004-12-11"
 "*"
 SF-IMAGE 	"image"				0
 SF-DRAWABLE 	"drawable"			0
 SF-OPTION      "Shape"              '("Poly" "Star" "Spyro" )
 SF-ADJUSTMENT 	"Sides" '(4 3 10 1 2 1 1)
 SF-ADJUSTMENT 	"Side/Spyro to draw" '(0 0 10 1 2 1 1)
 SF-ADJUSTMENT 	"Steps Number"  '(4 1 10 1 2 0 0)
 SF-OPTION	"Center" '("Image" "Layer" "Mask" "No")
 SF-OPTION	"Scale" '("Image" "Layer" "Mask" "No")
 SF-OPTION	"Keep Aspect Ratio" '("Yes" "No")
 SF-OPTION	"Tool" '("Path" "Pencil" "Brush" "Airbrush")
 )

(define (script-fu-turtle-shapes
         inImage 
         inDrawable
         inShape
         inSides
         inIter
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
  (let* (
         (theAngle (/ 360 inSides))
         (theAxiom "a")
         (theRules
          (cond ((= 0 inShape) "a:a+F")             ;poly
                ((= 1 inShape) "a:a+F--F")          ;star
                ((= 2 inShape) "a:a+F;+:+F")        ;spyro
                
                ))
         (theLastRule "")
         (theIter 
          (cond ((= 0 inIter) inSides)                ;default
                ((= 2 inShape) (* inSides inIter))
                ('else (min inIter inSides))))
         )
    (script-fu-turtle-L-system
     inImage 
     inDrawable
     theAxiom
     theRules
     theLastRule
     theIter
     theAngle
     inStep
     inCenter
     inResizeTo
     inRatio
     inTool
     )
    )
  ;; exit
  ; (turn dirty off)
  ; display inImage
  (gimp-displays-flush)
  ; (turn undo on)
  (gimp-image-undo-group-end inImage)
  )

(script-fu-register
 "script-fu-turtle-L-system-preset"
 "<Image>/Filters/Render/Turtle graphics/L-system preset"
 "L-system preset version 1.1"
 "Annamaria Trincardi"
 "2004, Annamaria Trincardi"
 "2004-12-11"
 "*"
 SF-IMAGE 	"image"				0
 SF-DRAWABLE 	"drawable"			0
 SF-OPTION      "Shape"              '("Koch" "C" "dragon" "Peano")
 SF-ADJUSTMENT 	"Iter" '(4 0 10 1 2 1 1)
 SF-ADJUSTMENT 	"Steps Number"  '(4 1 10 1 2 0 0)
 SF-OPTION	"Center" '("Image" "Layer" "Mask" "No")
 SF-OPTION	"Scale" '("Image" "Layer" "Mask" "No")
 SF-OPTION	"Keep Aspect Ratio" '("Yes" "No")
 SF-OPTION	"Tool" '("Path" "Pencil" "Brush" "Airbrush")
 )

(define (script-fu-turtle-L-system-preset
         inImage 
         inDrawable
         inShape
         inIter
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
  (let* (
         (theShape 
          (cond ((= 0 inShape) Koch)       
                ((= 1 inShape) C)
                ((= 2 inShape) dragon)
                ((= 3 inShape) Peano)
                ))
         (theAngle (caddr theShape))
         (theAxiom (car (cadr theShape)))
         (theRules (cadr (cadr theShape)))
         (theLastRule (caddr (cadr theShape)))
         )
    (script-fu-turtle-L-system
     inImage 
     inDrawable
     theAxiom
     theRules
     theLastRule
     inIter
     theAngle
     inStep
     inCenter
     inResizeTo
     inRatio
     inTool
     )
    )
  ;; exit
  ; (turn dirty off)
  ; display inImage
  (gimp-displays-flush)
  ; (turn undo on)
  (gimp-image-undo-group-end inImage)
  )

(define Koch 
  (list
   "Koch"
   (list "F"
         "F:F-F++F-F"
         "")
   60)
  )

(define C 
  (list
   "C"
   (list "F"
         "F:F-F+"
         "")
   90)
  )

(define dragon
  (list
   "dragon"
   (list "A"
         "A:-B++A-;B:+B--A+"
         "A:F;B:F")
   45)
  )

(define Peano 
  (list
   "Peano"
   (list "A"
         "A:-BF+AFA+FB-;B:+AF-BFB-FA+"
         "A:;B:")
   90)
  )


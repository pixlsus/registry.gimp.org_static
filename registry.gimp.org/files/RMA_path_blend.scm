; RMA_path_blend.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.3 (20120418)

; Description
;
; Fills a gradient blend between two paths.
;

; Changelog
;
; V1.1 added more math to find the orthagonal blending point for eash segment...yeah baby - I gots the maths!
; V1.2 reversing the the path selection reverses the blend, and an overlap slider
; Supporting paths with multiple segments, so long as the mumber of segments are equal.

; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

; Helper function to return the length of a path stroke
(define (path_length p s)
  (let*
    (
      (varStrokeObj 0)
      (varLength 0)
      (varCheck 0)
    )

    (when (< s (car (gimp-vectors-get-strokes p))) ; stroke in path
      (set! varStrokeObj (aref (cadr (gimp-vectors-get-strokes p)) s))
      (set! varLength (car (gimp-vectors-stroke-get-length p varStrokeObj 1))) ; get length
      (if (<= varLength 0)
        (set! varLength 0)
        (begin
          (set! varCheck (list-ref (gimp-vectors-stroke-get-point-at-dist p varStrokeObj varLength 1) 3)) ; check if length OK
          (while (= varCheck FALSE)       ;backtrack to get last good length
            (set! varLength (- varLength 0.001))
            (set! varCheck (list-ref (gimp-vectors-stroke-get-point-at-dist p varStrokeObj varLength 1) 3))
          )
        )
      )
    )
    varLength ; return length
  )
)

(define (RMA_path_blend img inLayer inP1 inP2 inGrad inLap)
  (let*
    ( (width (car (gimp-image-width img)))
	  (height (car (gimp-image-height img)))
      (selbounds (gimp-selection-bounds img))
      (inGrad (if (equal? inGrad "") (car (gimp-context-get-gradient)) inGrad))
      (origimg img)
      (origlayer inLayer)
      (img (car (gimp-image-duplicate img)))
      (inLayer (car (gimp-image-get-active-layer img)))
      (selchannel 0)
      (strokes1 (gimp-vectors-get-strokes inP1))
      (strokes2 (gimp-vectors-get-strokes inP2))
      (L1 0)
      (L2 0)
      (counter 0)
      (strokecount 0)
      (selsize 0)
      (temp 0)
      (rev FALSE)
      (errhandler (car (gimp-message-get-handler)))
	)
    
    (gimp-message-set-handler MESSAGE-BOX)
    ;(gimp-message-set-handler ERROR-CONSOLE)
    
    (gimp-image-undo-group-start origimg)
    (gimp-image-undo-freeze img)
    (gimp-context-push)
    (gimp-context-set-gradient inGrad)
	   
    ;(gimp-display-new img)   ;uncomment for debugging...
    (set! selchannel (car (gimp-selection-save img))) 
    
    (if (= inP1 inP2) 
       (gimp-message "You must select two different paths!")
       (if (<> (car strokes1) (car strokes2))
         (gimp-message "Paths must have the same number of segments!")
         (while (< strokecount (car strokes1))
           ;(gimp-message (string-append "Path Segment: " (number->string strokecount)))
           (set! L1 (path_length inP1 strokecount))
           (set! L2 (path_length inP2 strokecount))

           (when (> (* L1 L2) 0) ; neither zero length
               ;make inP1 the shorter
               (when (> L1 L2)      
                 (set! temp L1)
                 (set! L1 L2)
                 (set! L2 temp)
                 (set! temp inP1)
                 (set! inP1 inP2)
                 (set! inP2 temp)
                 (set! rev TRUE)
               )

                          
               (while (< (+ counter 1 inLap) L1)
                 (let*
                   (
                   (P1a (gimp-vectors-stroke-get-point-at-dist inP1 (aref (cadr (gimp-vectors-get-strokes inP1)) strokecount) counter 1))
                   (P1b (gimp-vectors-stroke-get-point-at-dist inP1 (aref (cadr (gimp-vectors-get-strokes inP1)) strokecount) (+ counter 1 inLap) 1))
                   (P2a (gimp-vectors-stroke-get-point-at-dist inP2 (aref (cadr (gimp-vectors-get-strokes inP2)) strokecount) (* counter (/ L2 L1)) 1))
                   (P2b (gimp-vectors-stroke-get-point-at-dist inP2 (aref (cadr (gimp-vectors-get-strokes inP2)) strokecount) (+ (* (+ counter 1) (/ L2 L1)) inLap) 1))
                   (x1 (car P2a))
                   (y1 (cadr P2a))
                   (x2 (car P2b))
                   (y2 (cadr P2b))
                   (x3 (/ (+ (car P1a) (car P1b)) 2))
                   (y3 (/ (+ (cadr P1a) (cadr P1b)) 2))
                   (xQ 0)
                   (yQ 0)
                   )
            
                 (gimp-free-select img 8 (vector (car P1a) (cadr P1a) (car P1b) (cadr P1b) (car P2b) (cadr P2b) (car P2a) (cadr P2a) ) CHANNEL-OP-REPLACE TRUE FALSE 0)
                 
                 (when (= (car (gimp-selection-is-empty img)) FALSE)                 
                    (when (= x2 x1) ; vertical
                      (set! xQ x1)
                      (set! yQ y3)
                    )
                    (when (= y2 y1) ; horizontal
                      (set! xQ x3)
                      (set! yQ y1)
                    )
                    (unless (or (= x2 x1) (= y2 y1))
                      (set! xQ (/ (+ y3 (* x3 (/ (- x2 x1) (- y2 y1))) (- y1) (* x1 (/ (- y2 y1) (- x2 x1)))) (+ (/ (- y2 y1) (- x2 x1)) (+ (/ (- x2 x1) (- y2 y1))))))
                      (set! yQ (+ (* xQ (/ (- x1 x2) (- y2 y1))) y3 (* x3 (/ (- x2 x1) (- y2 y1)))))
                    )
                    
                    (if (equal? rev FALSE)
                      (gimp-edit-blend inLayer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE TRUE 3 0.2 TRUE x3 y3 xQ yQ)          
                      (gimp-edit-blend inLayer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE TRUE 3 0.2 TRUE xQ yQ x3 y3)          
                    )   
                 ) 
                 )
                 (set! counter (+ counter 1))
               )

       
               ;last segment
               (let*
                 (
                   (P1a (gimp-vectors-stroke-get-point-at-dist inP1 (aref (cadr (gimp-vectors-get-strokes inP1)) strokecount) counter 1))
                   (P1b (gimp-vectors-stroke-get-point-at-dist inP1 (aref (cadr (gimp-vectors-get-strokes inP1)) strokecount) L1 1))
                   (P2a (gimp-vectors-stroke-get-point-at-dist inP2 (aref (cadr (gimp-vectors-get-strokes inP2)) strokecount) (* counter (/ L2 L1)) 1))
                   (P2b (gimp-vectors-stroke-get-point-at-dist inP2 (aref (cadr (gimp-vectors-get-strokes inP2)) strokecount) L2 1))
                   (x1 (car P2a))
                   (y1 (cadr P2a))
                   (x2 (car P2b))
                   (y2 (cadr P2b))
                   (x3 (/ (+ (car P1a) (car P1b)) 2))
                   (y3 (/ (+ (cadr P1a) (cadr P1b)) 2))
                   (u 0)
                   (xQ 0)
                   (yQ 0)
                 )
                 
                 (gimp-free-select img 8 (vector (car P1a) (cadr P1a) (car P1b) (cadr P1b) (car P2b) (cadr P2b) (car P2a) (cadr P2a) ) CHANNEL-OP-REPLACE TRUE FALSE 0)
       
                 (when (= (car (gimp-selection-is-empty img)) FALSE)
                    (when (= x2 x1) ; vertical
                      (set! xQ x1)
                      (set! yQ y3)
                    )
                    (when (= y2 y1) ; horizontal
                      (set! xQ x3)
                      (set! yQ y1)
                    )
                    (unless (or (= x2 x1) (= y2 y1))
                      (set! xQ (/ (+ y3 (* x3 (/ (- x2 x1) (- y2 y1))) (- y1) (* x1 (/ (- y2 y1) (- x2 x1)))) (+ (/ (- y2 y1) (- x2 x1)) (+ (/ (- x2 x1) (- y2 y1))))))
                      (set! yQ (+ (* xQ (/ (- x1 x2) (- y2 y1))) y3 (* x3 (/ (- x2 x1) (- y2 y1)))))
                    )
                    
                    (if (equal? rev FALSE)
                      (gimp-edit-blend inLayer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE TRUE 3 0.2 TRUE x3 y3 xQ yQ)          
                      (gimp-edit-blend inLayer CUSTOM-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE TRUE 3 0.2 TRUE xQ yQ x3 y3)          
                    )       
                  )
               )        
               
               (set! counter 0)
           )
           (set! strokecount (+ strokecount 1)) 
           ;update display after each stroke
           (if (zero? (car selbounds))
             (gimp-selection-none img)
             (gimp-selection-load selchannel)
           )
           (gimp-edit-copy inLayer)
           (gimp-floating-sel-anchor (car (gimp-edit-paste origlayer TRUE)))
  	       (gimp-displays-flush)
         )
       )
    )
    
  	(gimp-displays-flush)
	(gimp-context-pop)
  	(gimp-image-undo-thaw img)
    (gimp-image-delete img)
    
  	(gimp-image-undo-group-end origimg)
    
    (gimp-message-set-handler errhandler)
    
  )
)

(script-fu-register "RMA_path_blend"
        		    "Path Blend..."
                    "Fils the area between two paths with a gradient,"
                    "Rob Antonishen"
                    "Rob Antonishen"
                    "April 2012"
                    "RGB*, GREY*"
                    SF-IMAGE      "image"              0
                    SF-DRAWABLE   "Layer"              0
                    SF-VECTORS    "Blend from"         0
                    SF-VECTORS    "Blend to"           0
	                SF-GRADIENT   "Gradient"           ""
                    SF-ADJUSTMENT "Segment Overlap"    (list 1.0 0.0 2.0 0.1 1 1 SF-SLIDER)
)

(script-fu-menu-register "RMA_path_blend"
                         "<Image>/Filters/Render")
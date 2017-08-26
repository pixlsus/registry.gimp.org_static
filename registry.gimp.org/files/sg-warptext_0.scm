; warptext.scm - a Script-fu for warping text to fill a region defined by
;                a four-point bezier path
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
;

; Transform the supplied stroke using the transformation matrix (m).
; The original stroke is removed from the path and "replaced" by the
; newly created transformed stroke.
; Returns the ID of the newly created stroke.
;
(define (warptext-transform-stroke m path stroke)
  (let* ((stroke-info (gimp-vectors-stroke-get-points path stroke))
         (type (car stroke-info))
         (v-length (cadr stroke-info))
         (points (vector->list (caddr stroke-info)))
         (closed?* (cadddr stroke-info))
         (trans-points nil)
         (coords nil)
         )
    (while (pair? points)
      (set! coords (transform-point m (car points) (cadr points)))
      (set! trans-points (cons (cadr coords)
                               (cons (car coords)
                                     trans-points)))
      (set! points (cddr points))
      )
    (set! points (list->vector (reverse trans-points)))
    (gimp-vectors-remove-stroke path stroke)
    (gimp-vectors-stroke-new-from-points path type v-length points closed?*)
    )
  )

; Transform all strokes within a path. Original strokes are deleted and
; replaced by transformed substitutes.
;
(define (warptext-transform-path m path)
  (let loop ((strokes (vector->list (cadr (gimp-vectors-get-strokes path)))))
    (if (null? strokes)
        path
        (begin
          (warptext-transform-stroke m path (car strokes))
          (loop (cdr strokes))))
    )
  )

; Transforms a 4-point envelope path such that the four control points
; are repositioned over the four corners of a rectangle that circumscribes
; the supplied 'text-path' (text-path does not actually have to represent
; text). THE IMAGE CANVAS IS RESIZED to fit the circumscribed rectangle
; to facillitate subsequent normalization of the text path (the original
; image canvas must eventually be restored). NOTE: the original envelope
; stroke is deleted, replaced by a new transformed stroke (in the same
; envelope path).
;
; Returns a list containing the correction transformation matrix and a
; list containing information needed to restore the original canvas 
; (using 'gimp-image-resize').
;
(define (warptext-square-off-envelope image text-path env-path env-stroke padding)
  (let* ((points #())
         (trans-points nil)
         (width  (car (gimp-image-width  image)))
         (height (car (gimp-image-height image)))
         (layer (car (gimp-layer-new image 
                                     width 
                                     height 
                                     (+ (* (car (gimp-image-base-type image)) 2) 1) 
                                     "resize" 
                                     100 
                                     NORMAL-MODE)))
         (x 0)
         (y 0)
         (w 0)
         (h 0)
         (m (mat3-identity))
         )
    (gimp-image-add-layer image layer 0)
    (gimp-drawable-fill layer WHITE-FILL)
    (gimp-context-set-foreground '(0 0 0))
    (gimp-context-set-paint-method "gimp-paintbrush")
    (gimp-context-set-brush "Circle (01)")
    (gimp-context-set-paint-mode NORMAL-MODE)
    (gimp-selection-none image)
    (gimp-edit-stroke-vectors layer text-path)
    (plug-in-autocrop-layer RUN-NONINTERACTIVE image layer)
    (gimp-layer-resize layer 
                       (+ (car (gimp-drawable-width layer))
                          (* 2 padding))
                       (+ (car (gimp-drawable-height layer))
                          (* 2 padding))
                       padding
                       padding)
    (set! x (car  (gimp-drawable-offsets layer)))
    (set! y (cadr (gimp-drawable-offsets layer)))
    (set! w (car (gimp-drawable-width  layer)))
    (set! h (car (gimp-drawable-height layer)))
    (gimp-image-resize image w h (- x) (- y))
    (set! points (caddr (gimp-vectors-stroke-get-points env-path env-stroke)))
    (set! m (mat3-perspective m 0 0 w h 
                              (vector-ref points 2)
                              (vector-ref points 3)
                              (vector-ref points 8)
                              (vector-ref points 9)
                              (vector-ref points 20)
                              (vector-ref points 21)
                              (vector-ref points 14)
                              (vector-ref points 15)))
    (warptext-transform-stroke (mat3-invert m) env-path env-stroke)
    (gimp-image-remove-layer image layer)
    ; return the matrix, and info to restore original image bounds
    (list m (list width height x y))
    )
  )

; Normalize all of the control points in all of the strokes so that
; they can be used as alpha interpolations on the bezier surface.
; 
; Assuming that 'warptext-square-off-envelope' has been run prior to this,
; width and height stem from the image dimensions.
; Warning: Text path is modified by this procedure!
;
(define (warptext-normalize-text-path path width height)
  (let ((m (mat3-scale (mat3-identity) (/ width) (/ height))))
    (let loop ((strokes (vector->list (cadr (gimp-vectors-get-strokes path)))))
      (if (null? strokes)
          (vector->list (cadr (gimp-vectors-get-strokes path)))
          (begin
            (warptext-transform-stroke m path (car strokes))
            (loop (cdr strokes)))))
    )
  )

; Map the control points of the text path to the bezier surface described
; by the envelope path. The text path should be normalized and the envelope
; path should be "squared-off". 
; Returns ID of generated warped path.
    
(define (warptext-map-bezier-envelope image text-path env-path quality)

  (define (make-latitude-stroke curve-path curve-top curve-bot alpha)
    (let 
      loop ((top (vector->list (caddr (gimp-vectors-stroke-get-points curve-path curve-top)))) 
            (bot (vector->list (caddr (gimp-vectors-stroke-get-points curve-path curve-bot))))
            (lat-curve ()))
        (if (null? top)
          (car (gimp-vectors-stroke-new-from-points curve-path 
                                                    0 
                                                    12 
                                                    (list->vector (reverse lat-curve))
                                                    0))
          (loop (cddr top) 
                (cddr bot) 
                (cons (+ (* (- 1 alpha) (cadr top)) (* alpha (cadr bot))) 
                      (cons (+ (* (- 1 alpha) (car top)) (* alpha (car bot)))
                            lat-curve))))))

  (define (reposition-ends curve-path curve-left curve-right length-left length-right lat-stroke alpha)
    (let* (
        (lat-points (caddr (gimp-vectors-stroke-get-points curve-path lat-stroke)))
        (left (gimp-vectors-stroke-get-point-at-dist curve-path 
                                                     curve-left 
                                                     (* alpha length-left) 
                                                     1))
        (leftx (car left))
        (lefty (cadr left))
        (right (gimp-vectors-stroke-get-point-at-dist curve-path 
                                                      curve-right 
                                                      (* alpha length-right) 
                                                      1))
        (rightx (car right))
        (righty (cadr right))
        (left-dx  (- leftx  (vector-ref lat-points 2)))
        (left-dy  (- lefty  (vector-ref lat-points 3)))
        (right-dx (- rightx (vector-ref lat-points 8)))
        (right-dy (- righty (vector-ref lat-points 9)))
        )
      (gimp-vectors-remove-stroke curve-path lat-stroke)
      (car (gimp-vectors-stroke-new-from-points 
               curve-path 
               0 
               12
               (vector
                 leftx ;; outer control handles don't matter
                 lefty
                 leftx ;; (+ (vector-ref lat-points 2) left-dx) 
                 lefty ;; (+ (vector-ref lat-points 3) left-dy) 
                 (+ (vector-ref lat-points 4) left-dx) 
                 (+ (vector-ref lat-points 5) left-dy) 
                 (+ (vector-ref lat-points 6) right-dx) 
                 (+ (vector-ref lat-points 7) right-dy) 
                 rightx ;; (+ (vector-ref lat-points 8) right-dx) 
                 righty ;; (+ (vector-ref lat-points 9) right-dy) 
                 rightx ;; outer handles don't matter
                 righty)
               FALSE))))



  (let* ((warped-path (car (gimp-vectors-new image "warped")))
         (env-stroke (vector-ref (cadr (gimp-vectors-get-strokes env-path)) 0))
         (env-coords (caddr (gimp-vectors-stroke-get-points env-path env-stroke)))
         (curve-path (car (gimp-vectors-new image "curve")))
         (curve-top (car (gimp-vectors-stroke-new-from-points 
                             curve-path 
                             0 
                             12 
                             (vector   
                               (vector-ref env-coords 0) 
                               (vector-ref env-coords 1) 
                               (vector-ref env-coords 2) 
                               (vector-ref env-coords 3) 
                               (vector-ref env-coords 4) 
                               (vector-ref env-coords 5) 
                               (vector-ref env-coords 6) 
                               (vector-ref env-coords 7)
                               (vector-ref env-coords 8)
                               (vector-ref env-coords 9)
                               (vector-ref env-coords 10)
                               (vector-ref env-coords 11))
                             FALSE)))
         (curve-right (car (gimp-vectors-stroke-new-from-points 
                               curve-path 
                               0 
                               12 
                               (vector   
                                 (vector-ref env-coords 6)
                                 (vector-ref env-coords 7)
                                 (vector-ref env-coords 8)
                                 (vector-ref env-coords 9)
                                 (vector-ref env-coords 10)
                                 (vector-ref env-coords 11)
                                 (vector-ref env-coords 12)
                                 (vector-ref env-coords 13)
                                 (vector-ref env-coords 14)
                                 (vector-ref env-coords 15)
                                 (vector-ref env-coords 16)
                                 (vector-ref env-coords 17))
                               FALSE)))
         (curve-bot (car (gimp-vectors-stroke-new-from-points 
                             curve-path 
                             0 
                             12 
                             (vector   
                               (vector-ref env-coords 22)
                               (vector-ref env-coords 23)
                               (vector-ref env-coords 20)
                               (vector-ref env-coords 21)
                               (vector-ref env-coords 18)
                               (vector-ref env-coords 19)
                               (vector-ref env-coords 16)
                               (vector-ref env-coords 17)
                               (vector-ref env-coords 14)
                               (vector-ref env-coords 15)
                               (vector-ref env-coords 12)
                               (vector-ref env-coords 13))
                             FALSE)))
         (curve-left (car (gimp-vectors-stroke-new-from-points 
                              curve-path 
                              0 
                              12 
                              (vector   
                                (vector-ref env-coords 4) 
                                (vector-ref env-coords 5) 
                                (vector-ref env-coords 2) 
                                (vector-ref env-coords 3) 
                                (vector-ref env-coords 0) 
                                (vector-ref env-coords 1)
                                (vector-ref env-coords 22)
                                (vector-ref env-coords 23)
                                (vector-ref env-coords 20)
                                (vector-ref env-coords 21)
                                (vector-ref env-coords 18)
                                (vector-ref env-coords 19))
                              FALSE)))
         (text-strokes (vector->list (cadr (gimp-vectors-get-strokes text-path))))
         (type 0)
         (points nil)
         (interpolated-points nil)
         (v-length 0)
         (closed FALSE)
         (coords nil)
         (alpha-y 0)   
         (trans-points nil)
         (length-left (car (gimp-vectors-stroke-get-length curve-path curve-left 0.5)))
         (length-right (car (gimp-vectors-stroke-get-length curve-path curve-right 0.5)))
         (lat-stroke 0)
         (stroke-info nil)
         )
    (gimp-image-add-vectors image curve-path 0)
    (gimp-vectors-set-visible curve-path TRUE)
    (gimp-image-add-vectors image warped-path 0)
    (gimp-vectors-set-visible warped-path TRUE)
    (while (pair? text-strokes)
      (set! stroke-info (gimp-vectors-stroke-get-points text-path (car text-strokes)))
      (set! type (car stroke-info))
      (set! closed (cadddr stroke-info))
      (set! interpolated-points (gimp-vectors-stroke-interpolate text-path 
                                                                 (car text-strokes)
                                                                 (/ 0.5 quality)))
      (set! closed (caddr interpolated-points))
      (let
        loop ((points (vector->list (cadr interpolated-points)))
              (trans-points nil))
          (if (null? points)
            (begin
              (set! trans-points (reverse trans-points))
              (gimp-vectors-stroke-new-from-points warped-path 
                                                   type
                                                   (length trans-points)
                                                   (list->vector trans-points) 
                                                   closed))
            (begin
              (set! lat-stroke (make-latitude-stroke curve-path 
                                                     curve-top 
                                                     curve-bot 
                                                     (cadr points)))
              (set! lat-stroke (reposition-ends curve-path 
                                                curve-left 
                                                curve-right 
                                                length-left
                                                length-right
                                                lat-stroke 
                                                (cadr points)))
              (set! coords (gimp-vectors-stroke-get-point-at-dist 
                               curve-path
                               lat-stroke 
                               (* (car (gimp-vectors-stroke-get-length curve-path lat-stroke 0.5)) 
                                  (car points))
                               0.5))
              (gimp-vectors-remove-stroke curve-path lat-stroke)
              (loop (cddr points) (cons (cadr coords) 
                                        (cons (car coords) 
                                              (cons (cadr coords)
                                                    (cons (car coords) 
                                                          (cons (cadr coords) 
                                                                (cons (car coords)
                                                                      trans-points))))))))))
      (set! text-strokes (cdr text-strokes)))
    (gimp-image-remove-vectors image curve-path)
    warped-path
    )
  )

; A 'mat3' is a "3x3 list matrix" corresponding to a C-style matrix[y][x]
; Accessing an element is performed with (cXr (cYr matrix)) where
; (cYr m) specifies a particular row in the matrix: car=1st, cadr=2nd, caddr=3rd
; (map cXr m) specifies a particular column: car=1st, cadr=2nd, caddr=3rd
; (cXr (cYr m)) specifies element matrix[y][x]: e.g., (car (cadr m))=2nd element of 1st row
;

(define (mat3 t00 t01 t02 t10 t11 t12 t20 t21 t22)
  (list (list t00 t01 t02)
        (list t10 t11 t12)
        (list t20 t21 t22)))

(define (mat3-identity)
  (mat3 1.0 0.0 0.0
        0.0 1.0 0.0
        0.0 0.0 1.0))

; Transform an xy point using matrix m
;
        
(define (transform-point m x y) 
  (let ((w (apply + (map * (caddr m) (list x y 1)))))
    (set! w (if (zero? w)
              1.0
              (/ w)))
    (list (* (+ (* (caar m) x) (* (cadar m) y) (caddar m)) w) ; newx
          (* (+ (* (caadr m) x) (* (cadadr m) y) (caddr (cadr m))) w)))) ; newy

; 'matrix-perspective' modifies a transform matrix given a source box (xywh)
; and four target corners (x1 y1 x2 y2 x3 y3 x4 y4)
; For a path, the source box would be the image.
;
(define (mat3-perspective m x y w h x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((scalex (if (zero? w) 1.0 (/ w)))
        (scaley (if (zero? h) 1.0 (/ h))))
    (set! m (mat3-scale 
              (mat3-translate m (- x) (- y)) 
              scalex scaley))
    (let ((dx1 (- x2 x4))
          (dx2 (- x3 x4))
          (dx3 (- (+ x1 x4) x2 x3))
          (dy1 (- y2 y4))
          (dy2 (- y3 y4))
          (dy3 (- (+ y1 y4) y2 y3)))
      (mat3-mult (if (and (zero? dx3) (zero? dy3))
                   (mat3 ;; mapping is affine
                     (- x2 x1) (- x4 x2) x1
                     (- y2 y1) (- y4 y2) y1
                     0.0       0.0       (caddr (caddr m)))
                   (let* ((det (- (* dx1 dy2) (* dy1 dx2)))
                          (t20 (if (zero? det) 
                                 1.0 
                                 (/ (- (* dx3 dy2) (* dy3 dx2)) det)))
                          (t21 (if (zero? det) 
                                 1.0 
                                 (/ (- (* dx1 dy3) (* dy1 dx3)) det))))
                     (mat3
                       (+ (- x2 x1) (* t20 x2)) (+ (- x3 x1) (* t21 x3)) x1
                       (+ (- y2 y1) (* t20 y2)) (+ (- y3 y1) (* t21 y3)) y1
                       t20                      t21                      1.0)))
                 m))))

; expanded (slower) version:
;
; (define (mat3-det m)
;   (- (+ (* (car   (car   m)) (cadr  (cadr   m)) (caddr (caddr m)))
;         (* (cadr  (car   m)) (caddr (cadr   m)) (car   (caddr m)))
;         (* (caddr (car   m)) (car   (cadr   m)) (cadr  (caddr m))))
;      (+ (* (car   (caddr m)) (cadr  (cadr   m)) (caddr (car m)))
;         (* (cadr  (caddr m)) (caddr (cadr   m)) (car   (car m)))
;         (* (caddr (caddr m)) (car   (cadr   m)) (cadr  (car m))))))

(define (mat3-det m)
  (- (+ (* (caar         m)  (cadadr        m)  (caddr (caddr m)))
        (* (cadar        m)  (caddr (cadr   m)) (caaddr       m))
        (* (caddar       m)  (caadr         m)  (cadr  (caddr m))))
     (+ (* (caaddr       m)  (cadadr        m)  (caddar       m))
        (* (cadr  (caddr m)) (caddr (cadr   m)) (caar         m))
        (* (caddr (caddr m)) (caadr         m)  (cadar        m)))))


(define (mat3-invert m)
  (let ((det (mat3-det m)))
    (if (zero? det)
      m
      (begin
        (set! det (/ det))
        (mat3
          (* (- (* (cadr (cadr m)) (caddr (caddr m))) 
                (* (caddr (cadr m)) (cadr (caddr m)))) det)
          (* (- (* (caddr (car m)) (cadr (caddr m))) 
                (* (cadr (car m)) (caddr (caddr m)))) det)
          (* (- (* (cadr (car m)) (caddr (cadr m))) 
                (* (caddr (car m)) (cadr (cadr m)))) det)
          
          (* (- (* (caddr (cadr m)) (car (caddr m))) 
                (* (car (cadr m)) (caddr (caddr m)))) det)
          (* (- (* (car (car m)) (caddr (caddr m))) 
                (* (caddr (car m)) (car (caddr m)))) det)
          (* (- (* (caddr (car m)) (car (cadr m))) 
                (* (car (car m)) (caddr (cadr m)))) det)
          
          (* (- (* (car (cadr m)) (cadr (caddr m))) 
                (* (cadr (cadr m)) (car (caddr m)))) det)
          (* (- (* (cadr (car m)) (car (caddr m))) 
                (* (car (car m)) (cadr (caddr m)))) det)
          (* (- (* (car (car m)) (cadr (cadr m))) 
                (* (cadr (car m)) (car (cadr m)))) det))))))
            
; multiplies two matrices and returns result.
;
(define (mat3-mult m1 m2)
  (mat3
    (apply + (map * (car m1) (map car m2)))
    (apply + (map * (car m1) (map cadr m2)))
    (apply + (map * (car m1) (map caddr m2)))

    (apply + (map * (cadr m1) (map car m2)))
    (apply + (map * (cadr m1) (map cadr m2)))
    (apply + (map * (cadr m1) (map caddr m2)))

    (apply + (map * (caddr m1) (map car m2)))
    (apply + (map * (caddr m1) (map cadr m2)))
    (apply + (map * (caddr m1) (map caddr m2)))))

(define (mat3-translate matrix x y)
  (list
    (map + (car matrix)  (map * (make-list 3 x) (caddr matrix)))
    (map + (cadr matrix) (map * (make-list 3 y) (caddr matrix)))
    (caddr matrix)
    )
  )

(define (mat3-scale matrix x y)
  (list
    (map * (car matrix) (make-list 3 x))
    (map * (cadr matrix) (make-list 3 y))
    (caddr matrix)
    )
  )

(define (script-fu-sg-warp-text image layer use-path orig-path padding quality)
  (let* ((env-path 0)
         (env-stroke nil)
         (recovery-info nil)
         (warped-path 0)
         (text-path 0)
         (env-name "")
         )
    (gimp-image-undo-group-start image)
    (gimp-context-push)
    (set! env-name (car (gimp-vectors-get-name (car (gimp-image-get-active-vectors image)))))
    (set! env-path (car (gimp-vectors-copy 
                           (car (gimp-image-get-active-vectors image)))))
    (gimp-image-add-vectors image env-path 0)
    (gimp-vectors-set-visible env-path FALSE)
    (set! env-stroke (vector-ref (cadr (gimp-vectors-get-strokes env-path)) 0))
    (if (= use-path TRUE)
      (set! text-path (car (gimp-vectors-copy orig-path)))
      (set! text-path (car (gimp-vectors-new-from-text-layer image layer))))
    (gimp-image-add-vectors image text-path 0)
    (set! recovery-info (warptext-square-off-envelope image text-path env-path env-stroke padding))
    (warptext-normalize-text-path text-path 
                         (car (gimp-image-width image)) 
                         (car (gimp-image-height image)))
    (set! warped-path (warptext-map-bezier-envelope image text-path env-path quality))
    (warptext-transform-path (car recovery-info) warped-path)
    (set! recovery-info (cadr recovery-info))
    (gimp-image-resize image 
                       (car recovery-info) 
                       (cadr recovery-info) 
                       (caddr recovery-info) 
                       (cadddr recovery-info))
    (gimp-image-remove-vectors image text-path)
    (gimp-image-remove-vectors image env-path)
    (gimp-context-pop)      
    (gimp-vectors-set-visible warped-path TRUE)
    (gimp-vectors-set-name warped-path (string-append "warped - " env-name))
    (gimp-displays-flush)
    (gimp-image-undo-group-end image)
    )
  )
  
(script-fu-register "script-fu-sg-warp-text"
  "Warp text..."
  "Warp text to a four-point Bezier patch"
    "Saul Goode"
  "Saul Goode"
  "July 2010"
  "*"
  SF-IMAGE    "Image"    0
  SF-DRAWABLE "Layer" 0
  SF-TOGGLE "Use alternate path" FALSE
  SF-VECTORS "Path" 0
  SF-ADJUSTMENT "Padding" (list 0 0 25 1 10 0 SF-SPINNER)
  SF-ADJUSTMENT "Quality" (list 60 1 250 1 10 0 SF-SPINNER)
  )
(script-fu-menu-register "script-fu-sg-warp-text"
  "<Image>/Filters/Distorts"
  )



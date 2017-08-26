; ------------------------------------------------------------------
; Original information ---------------------------------------------
; 
; islamic-design-6.scm for GIMP 2.6
; Copyright (C) 2011 Simin Massoudi <simin.massoudi@yahoo.com> & Moona Vafadoost <moona.vafadoost@gmail.com>
;
;End original Information--------------------------------------------
;--------------------------------------------------------------------
(define (intersection2 l1 l2)
	(let* (
		(x1 (vector-ref l1 0))
		(y1 (vector-ref l1 1))
		(x2 (vector-ref l1 2))
		(y2 (vector-ref l1 3))
		(x3 (vector-ref l2 0))
		(y3 (vector-ref l2 1))
		(x4 (vector-ref l2 2))
		(y4 (vector-ref l2 3))
		(m1 0)
		(m2 0)
		(tmp1 0)
		(tmp2 0)
		(x 0)
		(y 0)
		(r (make-vector 2 'double))
		)
		
		(cond ((= x1 x2)
				(set! x x1)
				(set! m2 (/ (- y4 y3) (- x4 x3)))
				(set! y (+ (* m2 x) (- y3 (* m2 x3))))
			)
			((= x3 x4)
				(set! x x3)
				(set! m1 (/ (- y2 y1) (- x2 x1)))
				(set! y (+ (* m1 x) (- y1 (* m1 x1))))
			)
			((= 1 1)
				(set! m1 (/ (- y2 y1) (- x2 x1)))
				(set! m2 (/ (- y4 y3) (- x4 x3)))
				(set! tmp1 (- (- y3 (* m2 x3)) (- y1 (* m1 x1))))
				(set! tmp2 (- m1 m2))
				(set! x (/ tmp1 tmp2))
				(set! y (+ (* m1 x) (- y1 (* m1 x1))))
			)
		)
		;(draw-line image drawable x1 y1 x y)
		(vector-set! r 0 x)
        	(vector-set! r 1 y)
		r

	)

)

(define (islamic-design-6 image drawable width merg) 
; start the undo group
	(gimp-undo-push-group-start image)
  (let* (
	  (x1 0)
	  (y1 0)
	  (x2 0)
	  (y2 0)
	  (x3 0)
	  (y3 0)
	  (x4 0)
	  (y4 0)
	  (from (make-vector 2 'double))
	  (to (make-vector 2 'double))
	;
      (l21 (make-vector 4 'double))
	  (l22 (make-vector 4 'double))
	  (l23 (make-vector 4 'double))
	  (l24 (make-vector 4 'double))
	;
	  (l31 (make-vector 4 'double))
	  (l32 (make-vector 4 'double))
	  (l33 (make-vector 4 'double))
	  (l34 (make-vector 4 'double))
	;
	  (left (make-vector 4 'double))
	  (top (make-vector 4 'double))
	  (right (make-vector 4 'double))
	  (down (make-vector 4 'double))
	;
	  (radical2 1.414213562)
	  (z3 0.1464466095)
	  (s-value1 0)
	  (tmp1 0)
	  (tmp2 0)
	  (myl2 0)
)
;------------------------------
	(set! s-value1 (* z3 width))
;------------------------------
	;--------------------------------stage 1------------------
	;left
	(vector-set! left 0 0)
	(vector-set! left 1 0)
	(vector-set! left 2 0)
	(vector-set! left 3 width)
	;top
	(vector-set! top 0 0)
	(vector-set! top 1 0)
	(vector-set! top 2 width)
	(vector-set! top 3 0)
	;right
	(vector-set! right 0 width)
	(vector-set! right 1 0)
	(vector-set! right 2 width)
	(vector-set! right 3 width)
	;down
	(vector-set! down 0 0)
	(vector-set! down 1 width)
	(vector-set! down 2 width)
	(vector-set! down 3 width)
	;---
	(set! tmp1 (/ width 2))
	(set! tmp2 (- width s-value1))
	;l21
	(vector-set! l21 0 tmp1)
	(vector-set! l21 1 0)
	(vector-set! l21 2 0)
	(vector-set! l21 3 tmp1)
	
	;l22
	(vector-set! l22 0 0)
	(vector-set! l22 1 tmp1)
	(vector-set! l22 2 tmp1)
	(vector-set! l22 3 width)
	
	;l23
	(vector-set! l23 0 tmp1)
	(vector-set! l23 1 width)
	(vector-set! l23 2 width)
	(vector-set! l23 3 tmp1)
	
	;l24
	(vector-set! l24 0 width)
	(vector-set! l24 1 tmp1)
	(vector-set! l24 2 tmp1)
	(vector-set! l24 3 0)
	;---
	;l31
	(vector-set! l31 0 s-value1)
	(vector-set! l31 1 s-value1)
	(vector-set! l31 2 s-value1)
	(vector-set! l31 3 tmp2)
	
	;l32
	(vector-set! l32 0 s-value1)
	(vector-set! l32 1 tmp2)
	(vector-set! l32 2 tmp2)
	(vector-set! l32 3 tmp2)
	
	;l33
	(vector-set! l33 0 tmp2)
	(vector-set! l33 1 tmp2)
	(vector-set! l33 2 tmp2)
	(vector-set! l33 3 s-value1)
	
	;l34
	(vector-set! l34 0 tmp2)
	(vector-set! l34 1 s-value1)
	(vector-set! l34 2 s-value1)
	(vector-set! l34 3 s-value1)
	;--------------------------------stage 2
	(set! myl2 
		(car 
			;(gimp-layer-new-from-drawable drawable image)
			(gimp-layer-new image width width RGB-IMAGE "stage-2" 100 NORMAL-MODE)
		)
	   )
	(gimp-image-add-layer image myl2 -1)
	(gimp-layer-add-alpha myl2)
	(gimp-drawable-fill myl2 3)
	;---
	(set! from (intersection2 left l21))
	(set! to (intersection2 l31 l21))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l34 l21))
	(set! to (intersection2 top l21))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 left l22))
	(set! to (intersection2 l31 l22))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l32 l22))
	(set! to (intersection2 down l22))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 down l23))
	(set! to (intersection2 l32 l23))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l33 l23))
	(set! to (intersection2 right l23))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 right l24))
	(set! to (intersection2 l33 l24))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l34 l24))
	(set! to (intersection2 top l24))
	(draw-line2 myl2 from to)
	;ok
	;--------------------------------stage 3
	(set! myl2 
		(car 
			;(gimp-layer-new-from-drawable drawable image)
			(gimp-layer-new image width width RGB-IMAGE "stage-2" 100 NORMAL-MODE)
		)
	   )
	(gimp-image-add-layer image myl2 -1)
	(gimp-layer-add-alpha myl2)
	(gimp-drawable-fill myl2 3)
    ;---
	(set! from (intersection2 l34 l31))
	(set! to (intersection2 l21 l31))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l22 l31))
	(set! to (intersection2 l32 l31))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l31 l32))
	(set! to (intersection2 l22 l32))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l23 l32))
	(set! to (intersection2 l33 l32))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l32 l33))
	(set! to (intersection2 l23 l33))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l24 l33))
	(set! to (intersection2 l34 l33))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l33 l34))
	(set! to (intersection2 l24 l34))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l21 l34))
	(set! to (intersection2 l31 l34))
	(draw-line2 myl2 from to)
	;ok
	(when (= merg TRUE)
            (set! tmp1 2)
            (while (< tmp1 3)
            	(set! myl2
                    (car
                        (gimp-image-merge-down image myl2 0)
                    )
                )
                (set! tmp1 (+ tmp1 1))
            )
        )
	; Complete the undo group
  	(gimp-undo-push-group-end image)
	; Flush output
  	(gimp-displays-flush)
  )
)
	;***********************************************************
(script-fu-register "islamic-design-6"
                    "Design-6"
                    "This function get a value (WIDTH) from user and draw some regular lines."
                    "Moona Vafadoost <moona.vafadoost@gmail.com> AND Simin Massoudi <simin.massoudi@yahoo.com>" 
                    "GNU General Public License"
                    "2011-01-12"
                    "RGB*, GRAY*"

        SF-IMAGE "Input Image" 0
	SF-DRAWABLE "Input Drawable" 0
	SF-VALUE "Width" "400"
	SF-TOGGLE "Merge Layers?" TRUE
)
(script-fu-menu-register "islamic-design-6"
                          "<Image>/Islamic-Design")
; ------------------------------------------------------------------
; Original information ---------------------------------------------
; 
; islamic-design-8.scm for GIMP 2.6
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
(define (draw-line2 drawable from to) 
  (let* (
          (points (make-vector 4 'double))
        )

        (vector-set! points 0 (vector-ref from 0))
        (vector-set! points 1 (vector-ref from 1))
        (vector-set! points 2 (vector-ref to 0))
        (vector-set! points 3 (vector-ref to 1))
	(gimp-paintbrush-default drawable 4 points)
        ;(gimp-pencil drawable 4 points)
	; Flush output
  	;(gimp-displays-flush)
  )
)
(define (islamic-design-8 image drawable width merg) 
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
	;
	  (l41 (make-vector 4 'double))
	  (l42 (make-vector 4 'double))
	  (l43 (make-vector 4 'double))
	  (l44 (make-vector 4 'double))
	;
	  (l51 (make-vector 4 'double))
	  (l52 (make-vector 4 'double))
	  (l53 (make-vector 4 'double))
	  (l54 (make-vector 4 'double))
	;
	  (l61 (make-vector 4 'double))
	  (l62 (make-vector 4 'double))
	  (l63 (make-vector 4 'double))
	  (l64 (make-vector 4 'double))
	;

	  (left (make-vector 4 'double))
	  (top (make-vector 4 'double))
	  (right (make-vector 4 'double))
	  (down (make-vector 4 'double))
	;
	  (z1 0.2071106781)
	  (z2 0.121320343)
	  (z3 0.1464466095)
	  (s-value1 0)
	  (s-value2 0)
	  (tmp1 0)
	  (tmp2 0)
	  (tmp3 0)
	  (tmp4 0)
	  (tmp5 0)
	  (tmp6 0)	
	  (myl2 0)
)

;------------------------------
	(set! s-value1 (* z1 width))
	(set! tmp3 (* z3 width))
	(set! s-value2 (- (/ width 2) tmp3))
	
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
	(set! tmp1 (/ width 4))
	(set! tmp2 (- width tmp1))
	;l21
	(vector-set! l21 0 tmp1)
	(vector-set! l21 1 0)
	(vector-set! l21 2 tmp1)
	(vector-set! l21 3 width)
	;l22
	(vector-set! l22 0 tmp2)
	(vector-set! l22 1 0)
	(vector-set! l22 2 tmp2)
	(vector-set! l22 3 width)
	;l23
	(vector-set! l23 0 0)
	(vector-set! l23 1 tmp1)
	(vector-set! l23 2 width)
	(vector-set! l23 3 tmp1)
	;l24
	(vector-set! l24 0 0)
	(vector-set! l24 1 tmp2)
	(vector-set! l24 2 width)
	(vector-set! l24 3 tmp2)
	;---
	(set! tmp4(- width tmp3))
	(set! tmp5(- width s-value2))
	;l31
	(vector-set! l31 0 s-value2)
	(vector-set! l31 1 tmp3)
	(vector-set! l31 2 tmp5)
	(vector-set! l31 3 tmp4)
	;l32
	(vector-set! l32 0 tmp5)
	(vector-set! l32 1 tmp3)
	(vector-set! l32 2 s-value2)
	(vector-set! l32 3 tmp4)
	;---
	;l41
	(vector-set! l41 0 s-value2)
	(vector-set! l41 1 tmp3)
	(vector-set! l41 2 0)
	(vector-set! l41 3 width)
	;l42
	(vector-set! l42 0 s-value2)
	(vector-set! l42 1 tmp4)
	(vector-set! l42 2 0)
	(vector-set! l42 3 0)
	;l43
	(vector-set! l43 0 tmp5)
	(vector-set! l43 1 tmp3)
	(vector-set! l43 2 width)
	(vector-set! l43 3 width)
	;l44
	(vector-set! l44 0 tmp5)
	(vector-set! l44 1 tmp4)
	(vector-set! l44 2 width)
	(vector-set! l44 3 0)
	;---
	(set! tmp6 (/ width 2))
	;l51
	(vector-set! l51 0 tmp3)
	(vector-set! l51 1 tmp3)
	(vector-set! l51 2 width)
	(vector-set! l51 3 tmp6)
	;l52
	(vector-set! l52 0 width)
	(vector-set! l52 1 tmp6)
	(vector-set! l52 2 tmp3)
	(vector-set! l52 3 tmp4)
	;l53
	(vector-set! l53 0 0)
	(vector-set! l53 1 tmp6)
	(vector-set! l53 2 tmp4)
	(vector-set! l53 3 tmp3)
	;l54
	(vector-set! l54 0 0)
	(vector-set! l54 1 tmp6)
	(vector-set! l54 2 tmp4)
	(vector-set! l54 3 tmp4)
	;---
	(set! tmp6 (/ width 2))
	;l61
	(vector-set! l61 0 tmp3)
	(vector-set! l61 1 tmp3)
	(vector-set! l61 2 tmp6)
	(vector-set! l61 3 0)
	;l62
	(vector-set! l62 0 tmp6)
	(vector-set! l62 1 0)
	(vector-set! l62 2 tmp4)
	(vector-set! l62 3 tmp3)
	;l63
	(vector-set! l63 0 tmp3)
	(vector-set! l63 1 tmp4)
	(vector-set! l63 2 tmp6)
	(vector-set! l63 3 width)
	;l64
	(vector-set! l64 0 tmp6)
	(vector-set! l64 1 width)
	(vector-set! l64 2 tmp4)
	(vector-set! l64 3 tmp4)
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
	(set! from (intersection2 top l42))
	(set! to (intersection2 l23 l42))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 down l41))
	(set! to (intersection2 l24 l41))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 down l43))
	(set! to (intersection2 l24 l43))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 top l44))
	(set! to (intersection2 l23 l44))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l42 l23))
	(set! to (intersection2 l31 l23))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l32 l23))
	(set! to (intersection2 l44 l23))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l41 l24))
	(set! to (intersection2 l32 l24))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l31 l24))
	(set! to (intersection2 l43 l24))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l23 l31))
	(set! to (intersection2 l24 l31))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l23 l32))
	(set! to (intersection2 l24 l32))
	(draw-line2 myl2 from to)
	;--------------------------------stage 3
	(set! myl2 
		(car 
			;(gimp-layer-new-from-drawable drawable image)
			(gimp-layer-new image width width RGB-IMAGE "stage-3" 100 NORMAL-MODE)
		)
	   )
	(gimp-image-add-layer image myl2 -1)
	(gimp-layer-add-alpha myl2)
	(gimp-drawable-fill myl2 3)
	;---
	(set! from (intersection2 l21 l61))
	(set! to (intersection2 top l61))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 top l62))
	(set! to (intersection2 l22 l62))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l21 l63))
	(set! to (intersection2 down l63))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 down l64))
	(set! to (intersection2 l22 l64))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l61 l21))
	(set! to (intersection2 l53 l21))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l54 l21))
	(set! to (intersection2 l63 l21))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l62 l22))
	(set! to (intersection2 l51 l22))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l52 l22))
	(set! to (intersection2 l64 l22))
	(draw-line2 myl2 from to)
	;ok
    (set! from (intersection2 left l53))
	(set! to (intersection2 l21 l53))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 left l54))
	(set! to (intersection2 l21 l54))
	(draw-line2 myl2 from to)
	;ok
    (set! from (intersection2 l22 l51))
	(set! to (intersection2 right l51))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l22 l52))
	(set! to (intersection2 right l52))
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
(script-fu-register "islamic-design-8"
                    "Design-8"
                    "This function get a value (WIDTH) from user and draw some regular lines."
                    "Moona Vafadoost <moona.vafadoost@gmail.com> AND Simin Massoudi <simin.massoudi@yahoo.com>" 
                    "GNU General Public License , Please use it and pass it to another !!!"
                    "2011-01-12"
                    "RGB*, GRAY*"

        SF-IMAGE "Input Image" 0
	SF-DRAWABLE "Input Drawable" 0
	SF-VALUE "Width" "400"
	SF-TOGGLE "Merge Layers?" TRUE
)
(script-fu-menu-register "islamic-design-8"
                          "<Image>/Islamic-Design")

; ------------------------------------------------------------------
; Original information ---------------------------------------------
; 
; islamic-design-5.scm for GIMP 2.6
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
(define (islamic-design-5 image drawable width merg) 
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
	  (l25 (make-vector 4 'double))
	  (l26 (make-vector 4 'double))
	  (l27 (make-vector 4 'double))
	  (l28 (make-vector 4 'double))
	;
	  (l31 (make-vector 4 'double))
	  (l32 (make-vector 4 'double))
	  (l33 (make-vector 4 'double))
	  (l34 (make-vector 4 'double))
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
	  (l71 (make-vector 4 'double))
	  (l72 (make-vector 4 'double))
	  (l73 (make-vector 4 'double))
	  (l74 (make-vector 4 'double))
	;
	  (left (make-vector 4 'double))
	  (top (make-vector 4 'double))
	  (right (make-vector 4 'double))
	  (down (make-vector 4 'double))
	;
	  (radical2 1.414213562)
	  (z1 0.2071106781)
	  (z2 0.121320343)
	  (z3 0.1464466095)
	  (s-value1 0)
	  (s-value2 0)
	  (s-value3 0)
	  (tmp1 0)
	  (tmp2 0)
	  (tmp3 0)
	  (tmp4 0)
	  (myl2 0)
)
;------------------------------
	(set! s-value1 (* z1 width))
	(set! s-value2 (* z3 width))
	(set! s-value3 (- (/ width 2)(* z2 width)))
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
	(vector-set! l21 0 0)
	(vector-set! l21 1 tmp1)
	(vector-set! l21 2 s-value1)
	(vector-set! l21 3 0)
	;l22
	(vector-set! l22 0 tmp1)
	(vector-set! l22 1 0)
	(vector-set! l22 2 0)
	(vector-set! l22 3 s-value1)
	;l23
	(vector-set! l23 0 tmp1)
	(vector-set! l23 1 0)
	(vector-set! l23 2 width)
	(vector-set! l23 3 s-value1)
	;l24
	(vector-set! l24 0 width)
	(vector-set! l24 1 tmp1)
	(vector-set! l24 2 tmp2)
	(vector-set! l24 3 0)
	;l25
	(vector-set! l25 0 width)
	(vector-set! l25 1 tmp1)
	(vector-set! l25 2 tmp2)
	(vector-set! l25 3 width)
	;l26
	(vector-set! l26 0 tmp1)
	(vector-set! l26 1 width)
	(vector-set! l26 2 width)
	(vector-set! l26 3 tmp2)
	;l27
	(vector-set! l27 0 tmp1)
	(vector-set! l27 1 width)
	(vector-set! l27 2 0)
	(vector-set! l27 3 tmp2)
	;l28
	(vector-set! l28 0 0)
	(vector-set! l28 1 tmp1)
	(vector-set! l28 2 s-value1)
	(vector-set! l28 3 width)
	;---
	(set! tmp3 (- width s-value2))
	;l31
	(vector-set! l31 0 tmp1)
	(vector-set! l31 1 0)
	(vector-set! l31 2 s-value2)
	(vector-set! l31 3 tmp3)
	;l32
	(vector-set! l32 0 tmp1)
	(vector-set! l32 1 0)
	(vector-set! l32 2 tmp3)
	(vector-set! l32 3 tmp3)
	;l33
	(vector-set! l33 0 tmp1)
	(vector-set! l33 1 width)
	(vector-set! l33 2 s-value2)
	(vector-set! l33 3 s-value2)
	;l34
	(vector-set! l34 0 tmp1)
	(vector-set! l34 1 width)
	(vector-set! l34 2 tmp3)
	(vector-set! l34 3 s-value2)
	;---
	;l51
	(vector-set! l51 0 0)
	(vector-set! l51 1 tmp1)
	(vector-set! l51 2 tmp3)
	(vector-set! l51 3 s-value2)
	;l52
	(vector-set! l52 0 0)
	(vector-set! l52 1 tmp1)
	(vector-set! l52 2 tmp3)
	(vector-set! l52 3 tmp3)
	;l53
	(vector-set! l53 0 width)
	(vector-set! l53 1 tmp1)
	(vector-set! l53 2 s-value2)
	(vector-set! l53 3 s-value2)
	;l54
	(vector-set! l54 0 width)
	(vector-set! l54 1 tmp1)
	(vector-set! l54 2 s-value2)
	(vector-set! l54 3 tmp3)
	;---
	(set! tmp4 (- width s-value3))
	;l61
	(vector-set! l61 0 s-value1)
	(vector-set! l61 1 0)
	(vector-set! l61 2 tmp4)
	(vector-set! l61 3 width)
	;l62
	(vector-set! l62 0 s-value3)
	(vector-set! l62 1 0)
	(vector-set! l62 2 tmp2)
	(vector-set! l62 3 width)
	;l63
	(vector-set! l63 0 tmp4)
	(vector-set! l63 1 0)
	(vector-set! l63 2 s-value1)
	(vector-set! l63 3 width)
	;l64
	(vector-set! l64 0 tmp2)
	(vector-set! l64 1 0)
	(vector-set! l64 2 s-value3)
	(vector-set! l64 3 width)
	;---
	;l71
	(vector-set! l71 0 0)
	(vector-set! l71 1 s-value1)
	(vector-set! l71 2 width)
	(vector-set! l71 3 tmp4)
	;l72
	(vector-set! l72 0 0)
	(vector-set! l72 1 s-value3)
	(vector-set! l72 2 width)
	(vector-set! l72 3 tmp2)
	;l73
	(vector-set! l73 0 0)
	(vector-set! l73 1 tmp4)
	(vector-set! l73 2 width)
	(vector-set! l73 3 s-value1)
	;l74
	(vector-set! l74 0 0)
	(vector-set! l74 1 tmp2)
	(vector-set! l74 2 width)
	(vector-set! l74 3 s-value3)
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
	(set! to (intersection2 l72 l21))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l71 l21))
	(set! to (intersection2 top l21))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 left l22))
	(set! to (intersection2 l61 l22))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l62 l22))
	(set! to (intersection2 top l22))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 top l23))
	(set! to (intersection2 l63 l23))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l64 l23))
	(set! to (intersection2 right l23))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 right l24))
	(set! to (intersection2 l74 l24))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l73 l24))
	(set! to (intersection2 top l24))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 right l25))
	(set! to (intersection2 l71 l25))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l72 l25))
	(set! to (intersection2 down l25))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 down l26))
	(set! to (intersection2 l61 l26))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l62 l26))
	(set! to (intersection2 right l26))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 down l27))
	(set! to (intersection2 l64 l27))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l63 l27))
	(set! to (intersection2 left l27))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 left l28))
	(set! to (intersection2 l73 l28))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l74 l28))
	(set! to (intersection2 down l28))
	(draw-line2 myl2 from to)
	;ok
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
	(set! from (intersection2 l22 l61))
	(set! to (intersection2 l73 l61))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l74 l61))
	(set! to (intersection2 l26 l61))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l22 l62))
	(set! to (intersection2 l73 l62))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l74 l62))
	(set! to (intersection2 l26 l62))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l23 l63))
	(set! to (intersection2 l71 l63))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l72 l63))
	(set! to (intersection2 l27 l63))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l23 l64))
	(set! to (intersection2 l71 l64))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l72 l64))
	(set! to (intersection2 l27 l64))
	(draw-line2 myl2 from to)
	;ok
	;--------------------------------stage 4
	(set! myl2 
		(car 
			;(gimp-layer-new-from-drawable drawable image)
			(gimp-layer-new image width width RGB-IMAGE "stage-4" 100 NORMAL-MODE)
		)
	   )
	(gimp-image-add-layer image myl2 -1)
	(gimp-layer-add-alpha myl2)
	(gimp-drawable-fill myl2 3)
	;---
	(set! from (intersection2 l21 l71))
	(set! to (intersection2 l63 l71))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l64 l71))
	(set! to (intersection2 l25 l71))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l21 l72))
	(set! to (intersection2 l63 l72))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l64 l72))
	(set! to (intersection2 l25 l72))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l28 l73))
	(set! to (intersection2 l61 l73))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l62 l73))
	(set! to (intersection2 l24 l73))
	(draw-line2 myl2 from to)
	;ok
	(set! from (intersection2 l28 l74))
	(set! to (intersection2 l61 l74))
	(draw-line2 myl2 from to)
	(set! from (intersection2 l62 l74))
	(set! to (intersection2 l24 l74))
	(draw-line2 myl2 from to)
	;ok
	(when (= merg TRUE)
            (set! tmp1 2)
            (while (< tmp1 4)
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
(script-fu-register "islamic-design-5"
                    "Design-5"
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
(script-fu-menu-register "islamic-design-5"
                          "<Image>/Islamic-Design")

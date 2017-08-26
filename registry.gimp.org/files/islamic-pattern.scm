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

        (gimp-pencil drawable 4 points)
	; Flush output
  	;(gimp-displays-flush)
  )
)
(define (islamic-pattern11 image drawable width)
	; start the undo group
	(gimp-undo-push-group-start image)
        ;define the local variables
        (let* (
                (A1 (make-vector 2 'double))
                (A2 (make-vector 2 'double))
                (A3 (make-vector 2 'double))
                (A4 (make-vector 2 'double))
             ;
                (D1 (make-vector 2 'double))
                (D2 (make-vector 2 'double))
                (D3 (make-vector 2 'double))
                (D4 (make-vector 2 'double))
                (D5 (make-vector 2 'double))
                (D6 (make-vector 2 'double))
                (D7 (make-vector 2 'double))
                (D8 (make-vector 2 'double))
             ;
                (from (make-vector 2 'double))
                (to (make-vector 2 'double))
             ;
                (w1 0) ;(width/2)
                (w2 0) ;(w1*radical2)
                (w3 0) ;(width-w2)
	        (radical2 0.414213562)
                (w4 0) ;(width-radical2)
                (w5 0) ;[(width*radical2)+(w2)]
                (w6 0) ;(width-w5)
                (myl2 0)
             ;
	        (top (make-vector 4 'double))
	        (left (make-vector 4 'double))
	        (right (make-vector 4 'double))
	        (down (make-vector 4 'double))
             ;
	        (l11 (make-vector 4 'double))
	        (l12 (make-vector 4 'double))
	        (l13 (make-vector 4 'double))
	        (l14 (make-vector 4 'double))
             ;
	        (l21 (make-vector 4 'double))
	        (l22 (make-vector 4 'double))
	        (l23 (make-vector 4 'double))
	        (l24 (make-vector 4 'double))
             ;
	        (lh1 (make-vector 4 'double))
	        (lh2 (make-vector 4 'double))
	        (lh3 (make-vector 4 'double))
	        (lh4 (make-vector 4 'double))
	        (lh5 (make-vector 4 'double))
	        (lh6 (make-vector 4 'double))
	        (lh7 (make-vector 4 'double))
	        (lh8 (make-vector 4 'double))
              )
              ;----------------------
	      (set! w1 (/ width 2))
	      (set! w2 (* w1 radical2))
	      (set! w3 (- width w2))
              (set! w4 (- width radical2))
              (set! w5 (+ (* width radical2) w2))
              (set! w6 (- width w5))
;************************************************
;Lh , L1 , L2***********
              ;lh1
	      (vector-set! lh1 0 w1)
	      (vector-set! lh1 1 0)
	      (vector-set! lh1 2 0)
	      (vector-set! lh1 3 w2)
              ;lh2
	      (vector-set! lh2 0 w1)
	      (vector-set! lh2 1 0)
	      (vector-set! lh2 2 width)
	      (vector-set! lh2 3 w2)
              ;lh3
	      (vector-set! lh3 0 w3)
	      (vector-set! lh3 1 0)
	      (vector-set! lh3 2 width)
	      (vector-set! lh3 3 w1)
              ;lh4
	      (vector-set! lh4 0 width)
	      (vector-set! lh4 1 w1)
	      (vector-set! lh4 2 w3)
	      (vector-set! lh4 3 width)
              ;lh5
	      (vector-set! lh5 0 width)
	      (vector-set! lh5 1 w3)
	      (vector-set! lh5 2 w1)
	      (vector-set! lh5 3 width)
              ;lh6
	      (vector-set! lh6 0 w1)
	      (vector-set! lh6 1 width)
	      (vector-set! lh6 2 0)
	      (vector-set! lh6 3 w3)
              ;lh7
	      (vector-set! lh7 0 w2)
	      (vector-set! lh7 1 width)
	      (vector-set! lh7 2 0)
	      (vector-set! lh7 3 w1)
              ;lh8
	      (vector-set! lh8 0 0)
	      (vector-set! lh8 1 w1)
	      (vector-set! lh8 2 w2)
	      (vector-set! lh8 3 0)
              ;---
              ;l11
	      (vector-set! l11 0 w2 )
	      (vector-set! l11 1 0)
	      (vector-set! l11 2 w5)
	      (vector-set! l11 3 width)
              ;l12
	      (vector-set! l12 0 w6)
	      (vector-set! l12 1 0)
	      (vector-set! l12 2 w3)
	      (vector-set! l12 3 width)
              ;l13
	      (vector-set! l13 0 w5)
	      (vector-set! l13 1 0)
	      (vector-set! l13 2 w2)
	      (vector-set! l13 3 width)
              ;l14
	      (vector-set! l14 0 w3)
	      (vector-set! l14 1 0)
	      (vector-set! l14 2 w6)
	      (vector-set! l14 3 width)
              ;---
              ;l21
	      (vector-set! l21 0 0)
	      (vector-set! l21 1 w2)
	      (vector-set! l21 2 width)
	      (vector-set! l21 3 w5)
              ;l22
	      (vector-set! l22 0 width)
	      (vector-set! l22 1 w2)
	      (vector-set! l22 2 0)
	      (vector-set! l22 3 w5)
              ;l23
	      (vector-set! l23 0 0)
	      (vector-set! l23 1 w3)
	      (vector-set! l23 2 width)
	      (vector-set! l23 3 w6)
              ;l24
	      (vector-set! l24 0 0)
	      (vector-set! l24 1 w6)
	      (vector-set! l24 2 width)
	      (vector-set! l24 3 w3)
;****************************************************
	      (set! myl2 
		    (car 
			  ;(gimp-layer-new-from-drawable drawable image)
			  (gimp-layer-new image width width RGB-IMAGE "final step"
                           100 NORMAL-MODE)
		    )
	      )
	      (gimp-image-add-layer image myl2 -1)
	      (gimp-layer-add-alpha myl2)
	      (gimp-drawable-fill myl2 3)
	      ;---
 ;****************************D8=>K1=>P1=>T1=>A3***************************
             (vector-set! D8 0 0)
             (vector-set! D8 1 w2)
             (set! to (intersection2 l11 lh1))
	     (draw-line2 myl2 D8 to)
             ;
             (set! from (intersection2 l11 lh1))
	     (set! to (intersection2 l22 l11))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l22 l11))
	     (set! to (intersection2 l22 lh7))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l22 lh7))
             (vector-set! A3 0 0) 
             (vector-set! A3 1 w1)
	     (draw-line2 myl2 from A3)
             ;
 ;*****************************A3=>T8=>O4=>K4=>D7*************************
             (vector-set! A3 0 0)
             (vector-set! A3 1 w1)
             (set! to (intersection2 l24 lh8))
	     (draw-line2 myl2 A3 to)
             ;
             (set! from (intersection2 l24 lh8))
	     (set! to (intersection2 l24 l13))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l24 l13))
	     (set! to (intersection2 l13 lh6))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l13 lh6))
             (vector-set! D7 0 0) 
             (vector-set! D7 1 w3)
	     (draw-line2 myl2 from D7)
             ;
 ;*****************************D6=>T2=>P4=>K7=>A2*************************
             (vector-set! D6 0 w2)
             (vector-set! D6 1 width)
             (set! to (intersection2 l23 lh7))
	     (draw-line2 myl2 D6 to)
             ;
             (set! from (intersection2 l23 lh7))
	     (set! to (intersection2 l23 l11))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l23 l11))
	     (set! to (intersection2 l11 lh5))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l11 lh5))
             (vector-set! A2 0 w1) 
             (vector-set! A2 1 width)
	     (draw-line2 myl2 from A2)
             ;
 ;*****************************A2=>K8=>O3=>T3=>D5*************************
             (vector-set! A2 0 w1)
             (vector-set! A2 1 width)
             (set! to (intersection2 l14 lh6))
	     (draw-line2 myl2 A2 to)
             ;
             (set! from (intersection2 l14 lh6))
	     (set! to (intersection2 l24 l14))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l24 l14))
	     (set! to (intersection2 l24 lh4))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l24 lh4))
             (vector-set! D5 0 w3) 
             (vector-set! D5 1 width)
	     (draw-line2 myl2 from D5)
             ;
 ;*****************************D4=>K3=>P3=>T5=>A4*************************
             (vector-set! D4 0 width)
             (vector-set! D4 1 w3)
             (set! to (intersection2 l12 lh5))
	     (draw-line2 myl2 D4 to)
             ;
             (set! from (intersection2 l12 lh5))
	     (set! to (intersection2 l23 l12))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l23 l12))
	     (set! to (intersection2 l23 lh3))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l23 lh3))
             (vector-set! A4 0 width) 
             (vector-set! A4 1 w1)
	     (draw-line2 myl2 from A4)
             ;
 ;*****************************A4=>T4=>O2=>K2=>D3*************************
             (vector-set! A4 0 width)
             (vector-set! A4 1 w1)
             (set! to (intersection2 l21 lh4))
	     (draw-line2 myl2 A4 to)
             ;
             (set! from (intersection2 l21 lh4))
	     (set! to (intersection2 l21 l14))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l21 l14))
	     (set! to (intersection2 l14 lh2))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l14 lh2))
             (vector-set! D3 0 width) 
             (vector-set! D3 1 w2)
	     (draw-line2 myl2 from D3)
             ;
 ;*****************************D2=>T6=>P2=>K5=>A1*************************
             (vector-set! D2 0 w3)
             (vector-set! D2 1 0)
             (set! to (intersection2 l22 lh3))
	     (draw-line2 myl2 D2 to)
             ;
             (set! from (intersection2 l22 lh3))
	     (set! to (intersection2 l22 l12))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l22 l12))
	     (set! to (intersection2 l12 lh1))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l12 lh1))
             (vector-set! A1 0 w1) 
             (vector-set! A1 1 0)
	     (draw-line2 myl2 from A1)
             ;
 ;*****************************A1=>K6=>O1=>T7=>D1*************************
             (vector-set! A1 0 w1)
             (vector-set! A1 1 0)
             (set! to (intersection2 l13 lh2))
	     (draw-line2 myl2 A1 to)
             ;
             (set! from (intersection2 l13 lh2))
	     (set! to (intersection2 l21 l13))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l21 l13))
	     (set! to (intersection2 l21 lh8))
	     (draw-line2 myl2 from to)
             ;
             (set! from (intersection2 l21 lh8))
             (vector-set! D1 0 w2) 
             (vector-set! D1 1 0)
	     (draw-line2 myl2 from D1)
 ;****************** OK *******************
	      ; Complete the undo group
  	      (gimp-undo-push-group-end image)
              ;flush output
              (gimp-displays-flush)
        )
)
(script-fu-register "islamic-pattern11"
                    "islamic-pattern11"
                    "pattern"
                    "zahra saghari <saghari.zahra@gmail.com>"
                    "GNU General Public License"
                    "2011-1-8"
                    "RGB*, GRAY*"

        SF-IMAGE "Input Image" 0
	SF-DRAWABLE "Input Drawable" 0
	SF-VALUE "Width" "400"
)
(script-fu-menu-register "islamic-pattern11"
                          "<Image>/islamic-pattern11")























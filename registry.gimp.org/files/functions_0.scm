(define (my-duplicate-layer image layer)
	(let* (
		(dup-layer (car (gimp-layer-copy layer 1))))
		(gimp-image-add-layer image dup-layer 0)
		dup-layer
	)
)

(define (rgb-to-hsv color)
	(let* (
			(r (car color))
			(g (cadr color))
			(b (caddr color))
			(cmin (min r (min g b)))
			(cmax (max r (max g b)))
			(diff (- cmax cmin))
			(rc (/ (- cmax r) diff))
			(gc (/ (- cmax g) diff))
			(bc (/ (- cmax b) diff))
			(h (/ (if (= r cmax)
						(- bc gc)
						(if (= g cmax)
							(+ 2.0 (- rc bc))
							(+ 4.0 (- gc rc))
						)
					)
					6.0
				)
			)
		)
		(list (if (= cmin cmax)
				0
				(* 360 (if (< h 0.0)
						(+ h 1.0)
						h
					)
				)
			)
			(if (= cmin cmax)
				0
				(/ (- cmax cmin) cmax)
			)
			cmax
		)
	)
)


; Do RGB to HSV in gimp ranges
(define (gimp-rgb-to-hsv color)
	(let*
		(
			(r (car color))
			(g (cadr color))
			(b (caddr color))
			(hsv (rgb-to-hsv (list (/ r 255.0) (/ g 255.0) (/ b 255.0))))
			(h (car hsv))
			(s (cadr hsv))
			(v (caddr hsv))
		)
		(list h (* s 100.0) (* v 100.0))
	)
)


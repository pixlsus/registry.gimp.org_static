(define (test-brush-api)
  (test-suite 'brush-api
	      ; create some
	      (test '(equal?       "foo" (car (gimp-brush-new "foo"))))
	      (test '(equal?       "%s"  (car (gimp-brush-new "%s"))))

	      ; expected errors
	      (test '(equal?      '()    (gimp-brush-new "")))
	      (test '(equal?      '()    (gimp-brush-new 0)))

	      ; do we have an editable and generated brush
	      (test '(equal? TRUE (car (gimp-brush-is-editable "foo"))))
	      (test '(equal? TRUE (car (gimp-brush-is-generated "foo"))))

	      ; this has to return false for other brushes
	      (test '(equal? (FALSE (car (gimp-brush-is-editable  "Pepper")))))
	      (test '(equal? (FALSE (car (gimp-brush-is-generated "Pepper")))))

	      ; and should fail otherwise
	      (test '(equal? '() (gimp-brush-is-editable "")))

	      ; check default properties
	      (test '(equal?       0     (car (gimp-brush-get-angle "foo"))))
	      (test '(equal?       1     (car (gimp-brush-get-aspect-ratio "foo"))))
	      (test '(equal?       0.5   (car (gimp-brush-get-hardness "foo"))))
	      (test '(equal?       11    (car (gimp-brush-get-info "foo"))))
	      (test '(equal?       11    (cadr (gimp-brush-get-info "foo"))))
	      (test '(equal?       1     (caddr (gimp-brush-get-info "foo"))))
	      (test '(equal?       0     (cadddr (gimp-brush-get-info "foo"))))
	      (test '(equal?       11    (car    (gimp-brush-get-pixels "foo"))))
	      (test '(equal?       11    (cadr   (gimp-brush-get-pixels "foo"))))
	      (test '(equal?       1     (caddr  (gimp-brush-get-pixels "foo"))))
	      (test '(equal?       121   (cadddr (gimp-brush-get-pixels "foo"))))
	      (test '(equal?       '#( 0 0 0 0 1 1 1 0 0 0 0 0 0 1 4 15 15 15 4 1 0 0 0 1 9 33 45 59 45 33 9 1 0 0 4 33 75 115 137 115 75 33 4 0 1 15 45 115 179 215 179 115 45 15 1 1 15 59 137 215 250 215 137 59 15 1 1 15 45 115 179 215 179 115 45 15 1 0 4 33 75 115 137 115 75 33 4 0 0 1 9 33 45 59 45 33 9 1 0 0 0 1 4 15 15 15 4 1 0 0 0 0 0 0 1 1 1 0 0 0 0 )                                   
				         (car    (cddddr (gimp-brush-get-pixels "foo")))))
	      (test '(equal?       0     (cadr   (cddddr (gimp-brush-get-pixels "foo")))))
	      (test '(equal?       0     (caddr  (cddddr (gimp-brush-get-pixels "foo")))))
	      (test '(equal?       '#()  (cadddr (cddddr (gimp-brush-get-pixels "foo")))))
	      (test '(equal?       5     (car (gimp-brush-get-radius "foo"))))
	      (test '(equal?       BRUSH-GENERATED-CIRCLE 
				         (car (gimp-brush-get-shape "foo"))))
	      (test '(equal?       20    (car (gimp-brush-get-spacing "foo"))))
	      (test '(equal?       2     (car (gimp-brush-get-spikes  "foo"))))

	      ; modifying existing brushes
	      (test '(equal?       "bar" (car (gimp-brush-rename "foo" "bar"))))
	      (test '(equal?       "foo" (car (gimp-brush-rename "bar" "foo"))))

	      ; some rename errors
	      (test '(equal?       '()   (gimp-brush-rename "foo" "")))

	      ; expected errors
	      (test '(equal?      '()    (gimp-brush-get-angle "")))
	      (test '(equal?      '()    (gimp-brush-get-aspect-ratio "")))
	      (test '(equal?      '()    (gimp-brush-get-hardness "")))
	      (test '(equal?      '()    (gimp-brush-get-info "")))
	      (test '(equal?      '()    (gimp-brush-get-pixels "")))
	      (test '(equal?      '()    (gimp-brush-get-radius "")))
	      (test '(equal?      '()    (gimp-brush-get-shape "")))
	      (test '(equal?      '()    (gimp-brush-get-spacing "")))
	      (test '(equal?      '()    (gimp-brush-get-spikes  "")))

	      ; testing the setters
	      (test '(equal?      42.22999954  (car (gimp-brush-set-angle "foo" 42.23))))
	      (test '(equal?      10     (car (gimp-brush-set-radius "foo" 10))))
	      (test '(equal?      0      (car (gimp-brush-set-radius "foo" -10))))
	      (test '(equal?      10.5   (car (gimp-brush-set-radius "foo" 10.5))))
	      (test '(equal?      5      (car (gimp-brush-set-aspect-ratio "foo" 5))))

	      ; delete some
	      (test '(equal?      '#t    (car (gimp-brush-delete "foo"))))
	      (test '(equal?      '#t    (car (gimp-brush-delete "%s"))))

	      ; expected errors
	      (test '(equal?      '()    (gimp-brush-delete "")))
	      (test '(equal?      '()    (gimp-brush-delete 0)))
	      )
)
(define (anitools-name-find-framerate name start end)
;  (print (string-append name "=" (number->string start) "," (number->string end)))
  (if (and (<= end (string-length name))
	   (<= (+ start 1) (string-length name)))
      (if (string=? "(" (substring name start (+ start 1)))
	  (if (and (>= end (+ start 4))
		   (string=? "ms)" (substring name (- end 3) end)))
	      (list start end)
	      (let ((c (substring name end (+ end 1))))
		(if (or (string=? c "m")
			(string=? c "s")
			(string=? c ")")
			(and (string<=? c "9")
			     (string>=? c "0")))
		    (anitools-name-find-framerate name start (+ end 1))
		    (anitools-name-find-framerate name end (+ end 1)))))
	  (anitools-name-find-framerate name (+ start 1) (+ end 1)))
      #f))
      

(define (anitools-name-set-framerate name rate force)
  (let ((indexes (anitools-name-find-framerate name 0 0)))
    (if indexes
	(if force
	    (string-append (substring name 0 (car indexes)) 
			   "(" 
			   (number->string rate) 
			   "ms)" 
			   (substring name (cadr indexes) (string-length name)))
	    name)
	(string-append name " (" (number->string rate) "ms)"))))

(define (anitools-layer-set-framerate drawable rate)
  (gimp-drawable-set-name drawable (anitools-name-set-framerate (car (gimp-drawable-get-name drawable)) rate #t)))

(define (anitools-for-each-layer image fun)
  (let ((info (gimp-image-get-layers image))
	(index 0))
    (while (< index (car info))
	   (fun (aref (cadr info) index))
	   (set! index (+ index 1)))))
  
(define (anitools-image-set-framerate image rate force)
  (gimp-image-undo-group-start image)
  (anitools-for-each-layer image (lambda (layer) 
				   (gimp-drawable-set-name layer
							   (anitools-name-set-framerate (car (gimp-drawable-get-name layer)) 
											rate (= force TRUE)))))
  (gimp-image-undo-group-end image))

(define (anitools-drawable-make-scanlines drawable)
  (let* ((y (modulo (cadr (gimp-drawable-offsets drawable)) 2))
	 (x (- -1 (car (gimp-drawable-offsets drawable))))
	 (width  (- (+ (car (gimp-drawable-width drawable)) 5) x))
	 (lst nil)
	 (height (car (gimp-drawable-height drawable)))
	 (numcoords 0))
    ;; generate scanline coordinates list for whole drawable
    ;; this is much faster than drawing the scanlines one by one
    (while (< y height)
	   (set! lst (cons x (cons y (cons width (cons y lst)))))
	   (set! y (+ y 2))
	   (set! lst (cons width (cons y (cons x (cons y lst)))))
	   (set! y (+ y 2))
	   (set! numcoords (+ numcoords 8)))
    (gimp-pencil drawable numcoords (list->vector lst))))

(define (anitools-image-make-scanlines image)
  (gimp-image-undo-group-start image)
  (gimp-brushes-set-brush "Circle (01)")
  (anitools-for-each-layer image (lambda (layer) (anitools-drawable-make-scanlines layer)))
  (gimp-image-undo-group-end image)
  (gimp-displays-flush))

(define (anitools-image-optimize-gif image)
  (let ((unop (car (plug-in-animationoptimize RUN-NONINTERACTIVE image 0))))
    (when (not (car (gimp-drawable-is-rgb (aref (cadr (gimp-image-get-layers unop)) 0))))
	  (gimp-image-convert-rgb unop))
    (gimp-convert-indexed unop NO-DITHER MAKE-PALETTE 16 FALSE TRUE "")
    (plug-in-animationoptimize RUN-INTERACTIVE unop 0)
    (gimp-image-delete unop)))

(script-fu-register "anitools-image-set-framerate"
		    "<Image>/Script-Fu/Anitools/Set Image Framerate"
		    "Set the framerate for all frames of an image"
		    "Joost Diepenmaat <joost@zeekat.nl>"
		    "Joost Diepenmaat"
		    "2008-04-10"
		    "RGB*, GRAY*, INDEXED*"
		    SF-IMAGE "Image" 0
		    SF-VALUE "Rate" "40"
		    SF-TOGGLE "Force (overwrite already specified framerates)" 0)

(script-fu-register "anitools-layer-set-framerate"
		    "<Image>/Script-Fu/Anitools/Set Layer Framerate"
		    "Set the framerate for this frame (layer)"
		    "Joost Diepenmaat <joost@zeekat.nl>"
		    "Joost Diepenmaat"
		    "2008-04-10"
		    "RGB*, GRAY*, INDEXED*"
		    SF-DRAWABLE "Frame" 0
		    SF-VALUE "Rate" "40")

(script-fu-register "anitools-image-make-scanlines"
		    "<Image>/Script-Fu/Anitools/Create Scanlines"
		    "Create horizontal 1px scan lines using the current colour and mode"
		    "Joost Diepenmaat <joost@zeekat.nl>"
		    "Joost Diepenmaat"
		    "2008-04-10"
		    "RGB*, GRAY*, INDEXED*"
		    SF-IMAGE "Image" 0)

(script-fu-register "anitools-image-optimize-gif"
		    "<Image>/Script-Fu/Anitools/Make GIF animation"
		    "Create optimized GIF"
		    "Joost Diepenmaat <joost@zeekat.nl>"
		    "Joost Diepenmaat"
		    "2008-04-10"
		    "RGB*, GRAY*, INDEXED*"
		    SF-IMAGE "Image" 0)

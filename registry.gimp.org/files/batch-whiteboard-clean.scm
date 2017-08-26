(define (batch-whiteboard-clean pattern)
  (let* ((filelist (cadr (file-glob pattern 1))))
    (while (not (null? filelist))
           (let* ((filename (car filelist))
                  (image (car (gimp-file-load RUN-NONINTERACTIVE
                                              filename filename)))
                  (drawable (car (gimp-image-get-active-layer image))))
             (plug-in-dog RUN-NONINTERACTIVE
                                   image drawable 6.0 2.0 FALSE TRUE)
             (plug-in-c-astretch RUN-NONINTERACTIVE
                                   image drawable)
	     (gimp-file-save RUN-NONINTERACTIVE
                             image drawable filename filename)
             (gimp-image-delete image))
           (set! filelist (cdr filelist)))))


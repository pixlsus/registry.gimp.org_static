(define (batch-pencil-drawing pattern
                              blurradius
                              strength)
  (let* ((filelist (cadr (file-glob pattern 1))))
    (while (not (null? filelist))
           (let* ((filename (car filelist)))
                (do-pencil-drawing filename
                            blurradius
                            strength))
           (set! filelist (cdr filelist)))))

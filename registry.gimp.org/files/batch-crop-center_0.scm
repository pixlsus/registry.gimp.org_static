; 2012 Ivan Lantes Perez
; Script to crop images and keep centered

(define (batch-crop-center pattern
                           width
                           height)
    (let* ((filelist (cadr (file-glob pattern 1))))
        (while (not (null? filelist))
            (let* ((filename (car filelist))
                (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename )))
                (drawable (car (gimp-image-get-active-layer image))))
                (let* ((original-width (car (gimp-image-width image)))
                (original-height (car (gimp-image-height image)))
                )
                (if (<= (/ original-width original-height) (/ width height))
                    (gimp-image-crop image original-width (* original-width (/ height width)) 0 (/ (- original-height (* original-width (/ height width))) 2) )
                    (gimp-image-crop image (* original-height (/ width height)) original-height (/ (- original-width (* original-height (/ width height))) 2) 0)
                )
                )
                
                (set! drawable (car (gimp-image-get-active-layer image)))
                (gimp-file-save RUN-NONINTERACTIVE image drawable filename filename)
                (gimp-image-delete image))
			
            (set! filelist (cdr filelist))
        )
    )
)

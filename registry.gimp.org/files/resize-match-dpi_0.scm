; Resize-match-dpi - Scales an image and its DPI
; Copyright (C) 2010  odie5533
; This software is released under the terms of the
; GNU General Public License version 3.0 or later
(define (resize-match-dpi image drawable newwidth newheight)
  (let* (
	 (oldwidth (car (gimp-image-width image)))
	 (oldheight (car (gimp-image-height image)))
	 (newdpi 0)
        )
    (if (= newheight 0)
        (set! newheight (round (* (/ oldheight oldwidth) newwidth)))
	()
    )
    (if (= newwidth 0)
	(set! newwidth (round (* (/ oldwidth oldheight) newheight)))
	()
    )
    (set! newdpi (/ (* newwidth (car (gimp-image-get-resolution image))) oldwidth))
    (gimp-image-undo-group-start image)
    (gimp-image-scale image newwidth newheight)
    (gimp-image-set-resolution image newdpi newdpi)
    (gimp-image-undo-group-end image)
))

(define (resize_match_dpi filename output newwidth newheight)
  (let* ((image    (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
         (drawable (car (gimp-image-get-active-layer image)))
        )
    (resize-match-dpi image drawable newwidth newheight)
    (gimp-file-save RUN-NONINTERACTIVE image drawable output output)
    (gimp-image-delete image)
))

(define (resize_match_dpi_multi pattern outdir newwidth newheight)
  (let* ((filelist (cadr (file-glob pattern 1))))
    (while (not (null? filelist))
	   (let* ((filename (car filelist))
		  (image    (car (gimp-file-load RUN-NONINTERACTIVE
						 filename filename)))
		  (drawable (car (gimp-image-get-active-layer image)))
		  (outf (string-append outdir "/" filename)))
	     (resize-match-dpi image drawable newwidth newheight)
	     (gimp-file-save RUN-NONINTERACTIVE image drawable outf outf)
	     (gimp-image-delete image))
	   (set! filelist (cdr filelist)))))

(script-fu-register "resize-match-dpi"
  "<Image>/Script-Fu/Resize Match DPI"
  "Resize and match DPI"
  "odie5533"
  "odie5533"
  "2010-05-16"
  "RGB*"
  SF-IMAGE "Image"  0
  SF-DRAWABLE "Drawable" 0

  SF-VALUE  "New width" "1280"
  SF-VALUE  "New height" "0"
)

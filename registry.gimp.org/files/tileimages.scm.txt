;;; tileimages.scm -- Put n*m pics onto one image in n columns, m rows with padding
;;; if there aren't n*m images passed, the last image is repeated
;;; this is useful for just passing one image, which will be tiled for all n*m positions
;;; if there are more images passed, only the first n*m are used (order not guaranteed)
;;
;; Copyright (C) 2014 Joshua E. Buhl

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

(define (batch-tileimages fileout n m padding gimp-fill-type roundedcornerradius featherselectionradius drawbox drawgrid gridlinewidth dpi jpeg-quality file-glob-pattern)
  (let* ((filelist (cadr (file-glob file-glob-pattern 1)))
	 (filename (car filelist))
	 (numimages (car (file-glob file-glob-pattern 1)))
	 (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
	 ;(image (car (file-jpeg-load 1 filename filename)))

	 (imagewidth (car (gimp-image-width image))) 
	 (imageheight (car (gimp-image-height image)))

	 (i 0)
	 (j 0)

	 (bigwidth (+ (* n imagewidth) (* (* 2 n) padding)))
	 (bigheight (+ (* m imageheight) (* (* 2 m) padding)))
	 (bigimage (car (gimp-image-new bigwidth bigheight RGB)))
	 (big-layer (car (gimp-layer-new bigimage bigwidth bigheight 0 "BigLayer" 100 NORMAL)))
	 ); let*(

    (gimp-image-add-layer bigimage big-layer 0)
    (gimp-drawable-fill (car (gimp-image-get-active-drawable bigimage)) gimp-fill-type)
    (gimp-image-delete image)

    ;;(while (not (null? filelist))
    (while (< j m )
	    (if (< 0 numimages )
	    	(begin 
	    	  (set! filename (car filelist))
	    	  (set! image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
	    	  ;(set! image (car (file-jpeg-load 1 filename filename)))
	    	  (set! filelist (cdr filelist))
	    	  (set! numimages (- numimages 1))
	    	  )
	    	)
	   (let* ((x0 (+ (* i imagewidth) 
			 (+ (* (* i 2 ) padding) padding))
		      ) 
		  (y0 (+ (* j imageheight) 
			 (+ (* (* j 2 ) padding) padding ))
		      ) 
		  )

	     (gimp-image-undo-disable image)
	     ;copy pic into blahimage
	     (gimp-selection-all image)

	     (if (< 0 roundedcornerradius )
		 ;(gimp-drawable-fill (car (gimp-image-get-active-drawable image)) 0)
		 (begin 
		   (gimp-image-select-round-rectangle image 2 0 0 imagewidth imageheight roundedcornerradius roundedcornerradius)
		   )
		 )

	     (if (< 0 featherselectionradius)
	     	 (begin
	     	   ;(gimp-drawable-fill (car (gimp-image-get-active-drawable image)) 0)
	     	   (gimp-selection-feather image featherselectionradius)
	     	   )
	     	 )

	     (gimp-edit-copy (car (gimp-image-get-active-drawable image)))

	     ;select rect in bigimage to paste into
	     (gimp-selection-none bigimage)
	     (gimp-rect-select bigimage x0 y0 imagewidth imageheight 0 0 0)
	     (gimp-edit-paste (car (gimp-image-get-active-drawable bigimage)) 1)
     
	     (gimp-floating-sel-anchor (car (gimp-image-get-active-drawable bigimage)))

	     (gimp-image-undo-enable image)
	     (gimp-image-clean-all image)

	     ;(set! filelist (cdr filelist))
	     (set! i (+ i 1))
	     (while (not (< i n))
		    (set! j (+ j 1))		    
		    (set! i  0)
		)

	     )
	   )


    (gimp-selection-none bigimage)
    ;;gimp scheme has no 'and'?!
    ;; (if (and drawbox drawgrid) 
    ;; 	  (plug-in-grid 1 bigimage (car (gimp-image-get-active-drawable bigimage)) 
    ;; 		  gridlinewidth (/ bigheight m) 0 '(0 0 0) 255
    ;; 		  gridlinewidth (/ bigwidth n) 0 '(0 0 0) 255
    ;; 		  0 0 0 '(255 255 255) 0
    ;; 		  )
    ;; 	  )
    (if (< 0 drawbox)
	  (plug-in-grid 1 bigimage (car (gimp-image-get-active-drawable bigimage)) 
		  gridlinewidth bigheight 0 '(200 200 200) 255
		  gridlinewidth bigwidth  0 '(200 200 200) 255
		  0 0 0 '(255 255 255) 0
		  )
	  )
    ;because of the way the grid plugin works, it doesn't seem
    ;possible to draw the grid w/o the surrounding box.... 
    ;setting the offset to imagewidth is the same as 0 and half the line width appears at the edges
    ;if you set the offset to imagewidth - gridlinewidth/2 then the other lines aren't centered between tiles.
    ;workaround: draw the box again with the background color
    (if (< 0 drawgrid)
	  (plug-in-grid 1 bigimage (car (gimp-image-get-active-drawable bigimage)) 
		  gridlinewidth (/ bigheight m) (/ bigheight m) '(200 200 200) 255
		  gridlinewidth (/ bigwidth n) (/ bigwidth n) '(200 200 200) 255
		  0 0 0 '(255 255 255) 0
		  )
	  )

   (gimp-image-set-filename bigimage fileout)
   (gimp-image-set-resolution bigimage dpi dpi)
   (file-jpeg-save 1 bigimage 
   		  (car (gimp-image-active-drawable bigimage))
   		  (car (gimp-image-get-filename bigimage))
   		  (car (gimp-image-get-filename bigimage))
   		  jpeg-quality
   		  0.0 1 0
   		  "file written by Gimp" 0 1 10 2)
   (gimp-image-undo-enable bigimage)
   (gimp-image-clean-all bigimage)
))




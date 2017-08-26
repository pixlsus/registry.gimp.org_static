; sf-batch-divide-image.scm
; by minyu fei
; 
; version 0.1 (20140122)
;
; description:
;
; loads images from one directory, divides into 2 parts in equal size, vertical or hirizontal, depending on the options, and saves to another directory.
; 
; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (sf-batch-divide-image inLoadDir inLoadType inDirection inSaveDir)
(let* ((pathchar (if (equal? (substring gimp-dir 0 1) "/") "/" "\\"))
       (pathname (string-append inLoadDir pathchar))
       (pathnamelen (string-length pathname))
       (loadTypeStr (cond 
		      (( equal? inLoadType 0 ) (vector ".[pP][nN][gG]" "png" 3))
		      (( equal? inLoadType 1 ) (vector ".[tT][iI][fF]" "tif" 3))
		      (( equal? inLoadType 2 ) (vector ".[tT][iI][fF][fF]" "tiff" 4))
		      (( equal? inLoadType 3 ) (vector ".[jJ][pP][gG]" "jpg" 3))
		      (( equal? inLoadType 4 ) (vector ".[bB][mM][pP]" "bmp" 3))))
       (image-guide-list (if (equal? inDirection 0) 
			   (cons gimp-image-add-vguide gimp-image-width)
			   (cons gimp-image-add-hguide gimp-image-height)))
       (pattern (string-append pathname "*" (vector-ref loadTypeStr 0)))
       (filelist (cadr (file-glob pattern 1))))
  (while (not (null? filelist))
         (let* ((filename (car filelist))
                (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
                (drawable (car (gimp-image-get-active-layer image)))
		(guidelist (let ((image-guide (car image-guide-list))
				 (guide-dir (cdr image-guide-list)))
			     (image-guide image (/ (car (guide-dir image)) 2))))
                (subimagelist (cadr (plug-in-guillotine RUN-NONINTERACTIVE image drawable)))
		(subimageleft (vector-ref subimagelist 0))
		(subimageright (vector-ref subimagelist 1))
		(drawableleft (car (gimp-image-get-active-layer subimageleft)))
		(drawableright (car (gimp-image-get-active-layer subimageright)))
		(shortname (substring filename pathnamelen (- (string-length filename) (vector-ref loadTypeStr 2) 1)))
		(newname (string-append inSaveDir pathchar shortname))
		(newnameleft (string-append newname "_1." (vector-ref loadTypeStr 1)))
		(newnameright (string-append newname "_2." (vector-ref loadTypeStr 1))))
	   (gimp-file-save RUN-NONINTERACTIVE subimageleft drawableleft newnameleft newnameleft)
	   (gimp-file-save RUN-NONINTERACTIVE subimageright drawableright newnameright newnameright)
	   (gimp-image-delete subimageleft)
	   (gimp-image-delete subimageright)
	   (gimp-image-delete image))
	 (set! filelist (cdr filelist))
  )
)
)

(script-fu-register "sf-batch-divide-image"
                    "<Toolbox>/Xtns/Batch Tools/Batch divide Images..."
                    "Batch divide a folder of full page images."
                    "minyu fei"
                    "minyu fei"
                    "jan 2014"
		    ""
                    SF-DIRNAME    "Load from" "C:\\Users\\programmer\\Documents"
                    SF-OPTION     "Load File Type" (list "png" "tif" "tiff" "jpg" "bmp") 
		    SF-OPTION     "Guide Direction" (list "Vertical " "Horizontal")
                    SF-DIRNAME    "Save Directory" "C:\\Users\\programmer\\Documents"
)

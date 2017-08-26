; A Script-Fu Script that Opens all files in a directory ; Creates and store away a 4 different copies of each of the pictures, the filename of the copy
; has a users defined Start
; "Cut Out and Create"
; Written in September 2011

(define (script-fu-batch-cut-and-create globexp)
(let* ((filelist (cadr (file-glob globexp 1))))
(while (not (null? filelist))
     (let* ((fname (car filelist))
		;(image (car (file-tiff-load RUN-NONINTERACTIVE fname fname))))
     	 (image (car (gimp-file-load RUN-NONINTERACTIVE fname fname))))

      (let* (
		(drawable (car (gimp-image-flatten image)))
		(xdrawable (car (gimp-image-get-active-layer image))))

		(gimp-image-rotate image ROTATE-90)

;; Select picture 1 in layer 1
           ;(gimp-rect-select  image x y w h CHANNEL-OP-REPLACE FALSE 0)
		 (gimp-rect-select image 4 4 3520 2355 CHANNEL-OP-REPLACE FALSE 0)
           (gimp-edit-copy-visible image)

		 (let* 
		   (
			(new-image  (car (gimp-edit-paste-as-new)))
		     (ydrawable (car (gimp-image-flatten new-image)))
			(xydrawable   (car (gimp-image-active-drawable new-image)))
             )          
		(gimp-file-save RUN-NONINTERACTIVE new-image xydrawable (string-append (car (strbreakup fname ".")) "_1.jpg") (string-append (car (strbreakup fname ".")) "_1.jpg")))

;; Select picture 2 
		 (gimp-rect-select  image 3531 4 3490 2355 CHANNEL-OP-REPLACE FALSE 0)
		 (gimp-edit-copy-visible image)

		 (let* ((new-image  (car (gimp-edit-paste-as-new)))
		    (ydrawable (car (gimp-image-flatten new-image)))
		    (xydrawable   (car (gimp-image-active-drawable new-image)))
		   )
	
		  (gimp-file-save RUN-NONINTERACTIVE new-image xydrawable (string-append (car (strbreakup fname ".")) "_2.jpg") (string-append (car (strbreakup fname ".")) "_2.jpg")))

;; Select picture 3
		 (gimp-rect-select  image 4 2365 3520 2376 CHANNEL-OP-REPLACE FALSE 0)
		 (gimp-edit-copy-visible image)

		 (let* ((new-image  (car (gimp-edit-paste-as-new)))
		     (ydrawable (car (gimp-image-flatten new-image)))
			(xydrawable   (car (gimp-image-active-drawable new-image)))
		 )

           (gimp-file-save RUN-NONINTERACTIVE new-image xydrawable (string-append (car (strbreakup fname ".")) "_3.jpg") (string-append (car (strbreakup fname ".")) "_3.jpg")))

;; Select picture 4
		 (gimp-rect-select  image 3531 2365 3550 2365 CHANNEL-OP-REPLACE FALSE 0)
		 (gimp-edit-copy-visible image)

		 (let* ((new-image  (car (gimp-edit-paste-as-new)))
		     (ydrawable (car (gimp-image-flatten new-image)))
			(xydrawable   (car (gimp-image-active-drawable new-image)))
           )
		 (gimp-file-save RUN-NONINTERACTIVE new-image xydrawable (string-append (car (strbreakup fname ".")) "_4.jpg") (string-append (car (strbreakup fname ".")) "_4.jpg")))
           
  )        	
  (set! filelist (cdr filelist)))))

)


; Register in Gimp Menu
(script-fu-register "script-fu-batch-cut-and-create"
		    _"_Batch Cut out pictures ..."
		    "Opens a Multi tiff file selects the images and cut them out then store away the copies "
		    "Sven Tryding"
		    "2011, Sven Tryding"
		    "September, 2011"
		    ""
		    SF-STRING "Path" "C:\\Users\\K\ällaren\\Pictures\\Skann Mapp\\*.jpg"
		   


              )            

(script-fu-menu-register "script-fu-batch-cut-and-create"
			 "<Toolbox>/_Filters/_Script-Fu")


        (script-fu-register
                  "script-fu-20centfox"                        ;func name
                  "<Image>/Script-Fu/20 Cent Fox Text"
                   "Makes a 3D representeation of the current (text) layer with 20 Century Fox effect"              ;description
                  "Frans Rijven"                             ;author
                  "copyright 2014, Frans Rijven"             ;copyright notice
                  "May 23, 2014"                          ;date created
                  ""                     ;image type that the script works on
		  SF-IMAGE      "Image"         0
		  SF-DRAWABLE   "Drawable"      0
		  SF-VALUE	"Depth"  "6"
		  SF-VALUE	"Horizontal shrink" "0.99"
		  SF-VALUE	"Vertical shrink" "0.99"
		  SF-VALUE	"move pxls hor"  "0"
		  SF-VALUE	"move pxls vert"  "0"
		  SF-VALUE	"Bump-direction:"  "90"
		  SF-VALUE	"Bump-height:"  "45"
		  SF-VALUE	"Bump-projection depth:"  "2"
        )
 
        (define (script-fu-20centfox beeld laag diepte hkrimp vkrimp hmov vmov brichting bhoogte bprojdiep)
	(let*
		(
		   (orglaag (car (gimp-image-get-active-layer beeld)))
		)
	  (let*
		(
		   (werklaag (car (gimp-layer-copy laag TRUE )))
		   (pos (car (gimp-image-get-layer-position beeld laag)))
		   (nwpos (+  pos 1))
	  	)
	    (gimp-image-add-layer beeld werklaag nwpos)
	  )
	  (let* 
	    (
	      (baslaag (car (gimp-image-get-active-layer beeld)))
	      (lagenteller diepte)
		  (focuslaag baslaag)
		  (hwissel 0)
		  (vwissel 0)
		  (yco 0)
		  (opacity 100)
		  (imhgt (car (gimp-image-height beeld)))
		  (hhgt (/ imhgt 2))
		  (imwdt (car (gimp-image-width beeld)))
		  (hwdt (/ imwdt 2))
		  
	    )
	      (plug-in-bump-map 1 beeld baslaag baslaag brichting bhoogte bprojdiep 0 0 0 0 TRUE FALSE 0) 
		  (while (> lagenteller 0)
			  (let*
			    (
				(focuslaag (car (gimp-image-get-active-layer beeld)))
				(werklaag (car (gimp-layer-copy focuslaag TRUE )))
				(laagpos (car (gimp-image-get-layer-position beeld focuslaag)))
				(nwpos (+ laagpos 1))
			    )
			    (gimp-image-add-layer beeld werklaag nwpos)
			    (set! yco (+ 200 (- diepte lagenteller)))
			    (gimp-item-transform-2d werklaag hwdt hhgt hkrimp vkrimp 0 hwdt hhgt )
;			    (set! opacity (- opacity 0))
;			    (gimp-layer-set-opacity werklaag opacity)
			    (set! hhgt (- hhgt hmov)) 
			    (print hhgt)
			    (set! hwdt (+ hwdt vmov))
			    (set! lagenteller (- lagenteller 1))
			  )
		  )
		  (gimp-image-set-active-layer beeld baslaag)
		  (set! lagenteller diepte)
		  (while (> lagenteller 0)
			  (gimp-image-merge-down beeld baslaag 0)
			  (set! baslaag (car (gimp-image-get-active-layer beeld)))
			  (set! lagenteller (- lagenteller 1))
		  )
		(gimp-image-set-active-layer beeld orglaag)
		(gimp-displays-flush)
	  )  
	  )
	)

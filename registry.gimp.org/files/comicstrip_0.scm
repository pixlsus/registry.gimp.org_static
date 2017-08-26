
;*************************************************************************************** 
; Comic-Strip script  for GIMP 2.2
; Copyright (C) 2019 John Harris john@grynn.com>
; Based on comic-book script by Joe Kitella <joe.kitella@gmail.com
; --------------------------------------------------------------------


(define (script-fu-Comic-Strip
			img
			drawable
	)

  (gimp-image-undo-group-start img)

  (let* (
	 (width (car (gimp-drawable-width drawable)))
	 (height (car (gimp-drawable-height drawable)))
	 (old-selection (car (gimp-selection-save img)))
	 (image-type (car (gimp-image-base-type img)))
	 (layer-type (car (gimp-drawable-type drawable)))
	 (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 NORMAL-MODE)))
	 (theImage)
       ) 

    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection WHITE-IMAGE-FILL)) ; so Empty and All are the same.
    (gimp-selection-none img)
    (gimp-image-add-layer img layer-temp1 -1)
    (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))

    (gimp-hue-saturation layer-temp1 0 0 0 100)
    (gimp-posterize layer-temp1 3) 
    (plug-in-unsharp-mask 0 0 layer-temp1 4 10 0)
    (plug-in-newsprint 0 0 layer-temp1 0 1 20 1 0 15 0 75 0 0 0 1)
   
    (set! layer-temp1 (car (gimp-image-get-active-layer img)))

    (gimp-drawable-set-name layer-temp1 "Comic-Strip")

    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-Comic-Strip"
  "<Image>/Filters/Artistic/Comic-Strip"
  "Creates a Comic Strip Effect."
  "John Harris <john@grynn.com>"
  "John Harris"
  "2010, June"
  "RGB* GRAY*"
  SF-IMAGE      "Image"	            0
  SF-DRAWABLE   "Drawable"          0
)



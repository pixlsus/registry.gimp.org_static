
;*************************************************************************************** 
; Comic-Book1 script  for GIMP 2.2
; Copyright (C) 2009 Joe Kitella <joe.kitella@gmail.com>
; 
; --------------------------------------------------------------------


(define (script-fu-Comic-Book1
			img
			drawable
	)

  (gimp-undo-push-group-start img)

  (let* (
	 (width (car (gimp-drawable-width drawable)))
	 (height (car (gimp-drawable-height drawable)))
	 (old-selection (car (gimp-selection-save img)))
	 (image-type (car (gimp-image-base-type img)))
	 (layer-type (car (gimp-drawable-type drawable)))
	 (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 NORMAL-MODE)))
	 (layer-temp2 (car (gimp-layer-new img width height layer-type "temp2"  100 NORMAL-MODE)))
 	 (layer-temp3 (car (gimp-layer-new img width height layer-type "temp3"  100 NORMAL-MODE)))
       ) 

    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection WHITE-IMAGE-FILL)) ; so Empty and All are the same.
    (gimp-selection-none img)
    (gimp-image-add-layer img layer-temp1 -1)
    (gimp-image-add-layer img layer-temp3 -1)
    (gimp-image-add-layer img layer-temp2 -1)
     (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    (gimp-edit-copy layer-temp1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp2 0)))
    (gimp-edit-copy layer-temp1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp3 0)))

    (plug-in-photocopy 1 img layer-temp2 8.0 1.0 0.0 0.8)
    (plug-in-photocopy 1 img layer-temp3 24.54 1.0 0.0 0.8)
    (gimp-levels layer-temp2 0 215 235 1.0 0 255) 
    (gimp-levels layer-temp3 0 123 212 .44 0 255) 
    (gimp-layer-set-mode layer-temp2 3)
    (gimp-layer-set-mode layer-temp3 3)
    (gimp-levels layer-temp1 0 60 220 1.00 0 255)
    (gimp-levels layer-temp1 0 25 225 2.25 0 255)
    (plug-in-newsprint 1 img layer-temp1  3 0 0 0 0 15 0 75 0 0 0 2)
    (gimp-image-merge-down img layer-temp3 0)
    (gimp-image-merge-down img layer-temp2 0)
   
    (set! layer-temp1 (car (gimp-image-get-active-layer img)))

    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
        (gimp-edit-clear layer-temp1)
        ))

    (gimp-layer-set-name layer-temp1 "Comic-Book")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)


    (gimp-undo-push-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-Comic-Book1"
  _"<Image>/Filters/Artistic/Comic-Book1..."
  "Creates a Comic Book Effect."
  "Joe Kitella <joe.kitella@gmail.com>"
  "Joe Kitella"
  "2009, May"
  "RGB* GRAY*"
  SF-IMAGE      "Image"	            0
  SF-DRAWABLE   "Drawable"          0
)

;*************************************************************************************** 
; Comic-Book2 script  for GIMP 2.2
; Copyright (C) 2009 Joe Kitella <joe.kitella@gmail.com>
; 
; --------------------------------------------------------------------


(define (script-fu-Comic-Book2
			img
			drawable
	)

  (gimp-undo-push-group-start img)

  (let* (
	 (width (car (gimp-drawable-width drawable)))
	 (height (car (gimp-drawable-height drawable)))
	 (old-selection (car (gimp-selection-save img)))
	 (image-type (car (gimp-image-base-type img)))
	 (layer-type (car (gimp-drawable-type drawable)))
	 (layer-temp1 (car (gimp-layer-new img width height layer-type "temp1"  100 NORMAL-MODE)))
	 (layer-temp3 (car (gimp-layer-new img width height layer-type "temp3"  100 NORMAL-MODE)))
       ) 

    (if (eqv? (car (gimp-selection-is-empty img)) TRUE)
        (gimp-drawable-fill old-selection WHITE-IMAGE-FILL)) ; so Empty and All are the same.
    (gimp-selection-none img)
    (gimp-image-add-layer img layer-temp1 -1)
    (gimp-image-add-layer img layer-temp3 -1)
     (gimp-edit-copy drawable)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp1 0)))
    (gimp-edit-copy layer-temp1)
    (gimp-floating-sel-anchor (car (gimp-edit-paste layer-temp3 0)))

    (plug-in-photocopy 1 img layer-temp3 24.54 1.0 0.0 0.8)
    (gimp-levels layer-temp3 0 123 212 .44 0 255) 
    (gimp-layer-set-mode layer-temp3 3)
    (gimp-levels layer-temp1 0 60 220 1.00 0 255)
    (plug-in-cartoon 1 img layer-temp1 7.0 0.200)
    (plug-in-newsprint 1 img layer-temp1  3 0 0 0 0 15 0 75 0 0 0 2)
    (gimp-image-merge-down img layer-temp3 0)
   
    (set! layer-temp1 (car (gimp-image-get-active-layer img)))

    (gimp-selection-load old-selection)
    (gimp-selection-invert img)
    (if (eqv? (car (gimp-selection-is-empty img)) FALSE) ; both Empty and All are denied
        (begin
        (gimp-edit-clear layer-temp1)
        ))

    (gimp-layer-set-name layer-temp1 "Comic-Book")
    (gimp-selection-load old-selection)
    (gimp-image-remove-channel img old-selection)


    (gimp-undo-push-group-end img)
    (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-Comic-Book2"
  _"<Image>/Filters/Artistic/Comic-Book2..."
  "Creates a Comic Book Effect."
  "Joe Kitella <joe.kitella@gmail.com>"
  "Joe Kitella"
  "2009, May"
  "RGB* GRAY*"
  SF-IMAGE      "Image"	            0
  SF-DRAWABLE   "Drawable"          0
)

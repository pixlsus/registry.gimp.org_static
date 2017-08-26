; Split a vector into it's strokes - a new path will be created for each of the original strokes
;
; This script was written by LightningIsMyName (aka LIMN), you can contact me here: <lightningismyname@gmail.com>
;
; This script works on GIMP 2.4 or higher
;
; Last changed on February 21, 2008
;
; You may redistribute this script as long as you do it for free and without any profit 


(define (script-fu-split-vector img layer1)

     (let* (

		(count 0)

		(vector (car(gimp-image-get-active-vectors img)))

		(vector_position (car(gimp-image-get-vectors-position img vector)))

		(vector_name (car(gimp-vectors-get-name vector)))

		(vector_new (car(gimp-vectors-new img vector_name)))

		(strokes (cadr(gimp-vectors-get-strokes vector)))

		(strokes_count (car(gimp-vectors-get-strokes vector)))

		(points (caddr(gimp-vectors-stroke-get-points vector (aref strokes count))))

		(points_num (cadr(gimp-vectors-stroke-get-points vector (aref strokes count))))

		(open (cadddr(gimp-vectors-stroke-get-points vector (aref strokes count)))) 

		(vector_list (cons-array strokes_count 'byte))

     )
	

	;undo group
	(gimp-image-undo-group-start img)

	(gimp-image-add-vectors img vector_new vector_position)

	(gimp-vectors-stroke-new-from-points vector_new 0 points_num points open)

	(gimp-vectors-set-linked vector_new (car(gimp-vectors-get-linked vector)))

	(gimp-vectors-set-visible vector_new (car(gimp-vectors-get-visible vector)))

	(aset vector_list count vector_new)


	(while (< (+ count 1) strokes_count)
	(begin

		(set! count (+ 1 count))

		(set! points (caddr(gimp-vectors-stroke-get-points vector (aref strokes count))))

		(set! points_num (cadr(gimp-vectors-stroke-get-points vector (aref strokes count))))

		(set! open (cadddr(gimp-vectors-stroke-get-points vector (aref strokes count)))) 

		(set! vector_new (car(gimp-vectors-new img vector_name)))

		(gimp-image-add-vectors img vector_new vector_position)

		(gimp-vectors-stroke-new-from-points vector_new 0 points_num points open)

		(gimp-vectors-set-linked vector_new (car(gimp-vectors-get-linked vector)))

		(gimp-vectors-set-visible vector_new (car(gimp-vectors-get-visible vector)))

		(aset vector_list count vector_new)

	))

	

	(print (list strokes_count vector_list))

	;end of undo group
	(gimp-image-undo-group-end img)

	

))

(script-fu-register
      "script-fu-split-vector"
              	"<Vectors>/Split to strokes"
              	"Splits the active vector to it's strokes - each stroke as a new path."
              	"LightningIsMyName (LIMN)"
              	"LightningIsMyName (LIMN)"
		"February 2008"
              	""
		SF-IMAGE	"Image"     0
		SF-VECTORS	"Vector Object"     0
)


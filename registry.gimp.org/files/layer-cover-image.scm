(script-fu-register "layer-cover-image"
	"<Image>/Layer/Scale and move layer to canvas"

        "Make the currently selected layer the same size and position
as the canvas (image)"

	"(c) Kevin Brubeck Unhammer <unhammer(at)gmail.com>"
	"Published under GPL version 2"
	"May 24, 2009"
	"*"

	SF-IMAGE "Image" 0
	SF-DRAWABLE "Drawable" 0
)

(define (layer-cover-image image drawable)

  (gimp-image-undo-group-start image)

  (let* ((width (car (gimp-image-width image)))
         (height (car (gimp-image-height image))))
    (gimp-layer-scale
     drawable
     width
     height   
     1 ))
    
  (gimp-layer-set-offsets drawable 
			  0
			  0)

  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
  )

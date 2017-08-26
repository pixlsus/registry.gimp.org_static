; Luigi Chiesa 2008.  No copyright.  Public Domain.
; Add a grid of guides

(define (script-fu-frame-guides InImage InBorder)
  (gimp-image-undo-group-start InImage)
  (let* (
        (width (car (gimp-image-width InImage)))
      	(height (car (gimp-image-height InImage)))
        )
        
        (gimp-image-add-hguide InImage InBorder)
        (gimp-image-add-hguide InImage (- height InBorder))
        (gimp-image-add-vguide InImage InBorder)
        (gimp-image-add-vguide InImage (- width InBorder))

	(gimp-image-undo-group-end InImage)
  (gimp-displays-flush)
  )
)

(script-fu-register
  "script-fu-frame-guides"
  "<Image>/Image/Guides/Frame"
  "Add a frame of guides"
  "Paxvlae"
  "Public Domain"
  "October 2013"
  "*"
  SF-IMAGE      "Image"   0
	SF-ADJUSTMENT	"Border"	'(2 1 8008135 1 10 0 1)
)

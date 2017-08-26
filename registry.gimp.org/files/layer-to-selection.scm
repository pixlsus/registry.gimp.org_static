(script-fu-register "layer-to-selection"
        "<Image>/Select/_Layer to selection"

        "Just make a layer selected. For some reason this is not built in."

        "(c) Kevin Brubeck Unhammer <unhammer(at)fsfe.org>"
        "Published under GPL version 2"
        "March 8, 2009. Modified by Aralox 7th April 2012 (Fixed depreciated functions)"
        "*"

        SF-IMAGE "Image" 0
        SF-DRAWABLE "Drawable" 0
)

(define
  (layer-to-selection
   image
   drawable)

  (gimp-image-undo-group-start image)

  ;; is there a quicker way to do this?
  (let* ((layer (car (gimp-image-get-active-layer image)))
         (width (car (gimp-drawable-width layer)))
         (height (car (gimp-drawable-height layer)))
         (posx (car (gimp-drawable-offsets layer)))
         (posy (cadr (gimp-drawable-offsets layer))))
    (gimp-image-select-rectangle image
                                 CHANNEL-OP-REPLACE
                                 posx
                                 posy
                                 width
                                 height
                                 ))


  (gimp-image-undo-group-end image)
  (gimp-displays-flush))

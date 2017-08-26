; fancy photo finish gimp script
(define (script-fu-photo-finish   image 
                                  drawable
                                  radius
                                  shadow-transl-x
                                  shadow-transl-y
                                  shadow-blur
                                  shadow-color
                                  shadow-opacity
                                  shadow-allow-resize
                                  bg-color
                                  )

    (let* (
        (layer (gimp-image-get-active-layer image))
        (bg-layer 0)
        (image-width (car (gimp-image-width image)))
        (image-height (car (gimp-image-height image)))
    )

    (gimp-image-undo-group-start image)

    (set! layer drawable)
    (gimp-layer-add-alpha drawable)
    (gimp-selection-all image)
    (script-fu-selection-rounded-rectangle image drawable radius 0)
    (gimp-selection-invert image)
    (gimp-edit-cut drawable)
    (gimp-selection-all image)
    (script-fu-selection-rounded-rectangle image drawable radius 0)
    (script-fu-drop-shadow image drawable shadow-transl-x shadow-transl-y 
        shadow-blur shadow-color shadow-opacity shadow-allow-resize)
    (gimp-selection-none image)
    (set! bg-layer (car (gimp-layer-new image image-width image-height 
        RGB-IMAGE "bg-layer" 100 NORMAL-MODE)))
    (gimp-image-add-layer image bg-layer -1)
    (gimp-context-set-background bg-color)
    (gimp-edit-fill bg-layer BACKGROUND-FILL)
    (gimp-image-raise-layer-to-top image layer)
    (set! drawable (gimp-image-flatten image))

    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
  )
)

; populate script registration information
(script-fu-register "script-fu-photo-finish"
    "Fancy Photo Finish"
    "Round photo edges and apply a shadow border, i.e., 'raise the photo'"
    "Kamil Wojcicki"
    "Kamil Wojcicki"
    "March 2009"
    "*"
    SF-IMAGE        "Image"                           0
    SF-DRAWABLE     "Drawable"                        0
    SF-ADJUSTMENT   "Round Rectangle Edge Radius"     '(5 0 100 1 10 0 0)
    SF-ADJUSTMENT   "Shadow Offset X"                 '(5 -4096 4096 1 10 0 1)
    SF-ADJUSTMENT   "Shadow Offset Y"                 '(5 -4096 4096 1 10 0 1)
    SF-ADJUSTMENT   "Shadow Blur Radius"              '(10 0 1024 1 10 0 1)
    SF-COLOR        "Shadow Color"                    "black"
    SF-ADJUSTMENT   "Shadow Opacity"                  '(70 0 100 1 10 0 0)
    SF-TOGGLE       "Shadow Allow Resize"             TRUE
    SF-COLOR        "Background Color"                "white"
)

; register the script within gimp menu
(script-fu-menu-register "script-fu-photo-finish" "<Image>/Filters")


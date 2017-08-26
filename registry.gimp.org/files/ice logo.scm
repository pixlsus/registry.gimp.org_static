(define (script-fu-ice-logo icetext ice_font ice_fontsize ice_bgcolor ice_density ice_shadow save_to ice_photo_format ice_photo_quality serial_number)
(let*
   (
       (ice_imagewidth 10)
       (ice_imageheight 10)
       (ice_image (car
                     (gimp-image-new
                      ice_imagewidth
                      ice_imageheight
                      RGB
                     )
                  )
       )
       (ice_text)
       (ice_layer1
                 (car
                    (gimp-layer-new
                     ice_image
                     ice_imagewidth
                     ice_imageheight
                     RGB-IMAGE
                     "layer1"
                     100
                     NORMAL
                     )
                 )
       )
       (ice_layer2
                 (car
                    (gimp-layer-new
                     ice_image
                     ice_imagewidth
                     ice_imageheight
                     RGB-IMAGE
                     "layer2"
                     100
                     NORMAL
                     )
                 )
       )
    )

(gimp-image-add-layer ice_image ice_layer1 1)
(gimp-image-add-layer ice_image ice_layer2 0)

(gimp-context-set-foreground '(255 255 255))
(gimp-context-set-background '(0 0 0))
(gimp-drawable-fill ice_layer2 BACKGROUND-FILL)


(set! ice_text
             (car
                (gimp-text-fontname
                 ice_image ice_layer2
                 0 0
                 (if (= (strcmp serial_number "bouc-haib-enou-iti1") 0)  icetext "Demo Version") 
                 30
                 TRUE
                 ice_fontsize PIXELS
                 ice_font)
              )
)

(set! ice_imagewidth
              (car
                 (gimp-drawable-width ice_text)
              )
)

(set! ice_imageheight
              (car
                 (gimp-drawable-height ice_text)
              )
)

(gimp-image-resize ice_image ice_imagewidth ice_imageheight 0 0)
(gimp-layer-resize ice_layer2 ice_imagewidth ice_imageheight 0 0)
(gimp-layer-resize ice_layer1 ice_imagewidth ice_imageheight 0 0)

(gimp-context-set-background ice_bgcolor)
(gimp-drawable-fill ice_layer1 BACKGROUND-FILL)

(let* 
    (
      (ice_float (car
                    (gimp-image-get-floating-sel ice_image)
                 )
      )
    )
(gimp-floating-sel-anchor ice_float)
)

(plug-in-rotate 1 ice_image ice_layer2 1 TRUE) 
(plug-in-wind 1 ice_image ice_layer2 (/ (* ice_fontsize ice_density)100 ) 1 (/ (* ice_fontsize ice_density)100 ) 0 1)   
(plug-in-rotate 1 ice_image ice_layer2 3 TRUE)

(let*
    (
       (ice_layer3 (car
                      (gimp-layer-copy ice_layer2 1)
                   )
       )
     )
(gimp-image-add-layer ice_image ice_layer3 -1)
(plug-in-gauss-iir2 1 ice_image ice_layer2 4 4)
(gimp-color-balance ice_layer2 0 TRUE -77 -10 27)
(plug-in-gauss-iir2 1 ice_image ice_layer3 2 2)
(gimp-layer-set-mode ice_layer3 ADDITION-MODE)
(gimp-image-merge-down ice_image ice_layer3 CLIP-TO-IMAGE)
  
)

(let*
    (
       (ice_layer4 (car
                      (gimp-image-get-active-layer ice_image)
                   )
       )
     )

(gimp-by-color-select ice_layer4 '(0 0 0) (- 105 (* 5 ice_shadow)) CHANNEL-OP-REPLACE TRUE 0 0 0)
(plug-in-threshold-alpha 1 ice_image ice_layer4 255)  
(gimp-selection-none ice_image)
)
(gimp-image-merge-visible-layers ice_image 1)
(let*
    (
       (ice_layer5 (car
                      (gimp-image-get-active-layer ice_image)
                   )
       )
     )
(if (= ice_photo_format 0)
(begin
(gimp-image-convert-indexed ice_image 0 0 256 FALSE FALSE "no name")
(file-gif-save 1 ice_image ice_layer5 (string-append save_to "/image01.gif") (string-append save_to "/image01.gif") 0 0 0 0)
)
)
(if (= ice_photo_format 1)
(file-jpeg-save 1 ice_image ice_layer5 (string-append save_to "/image01.jpg") (string-append save_to "/image01.jpg") ice_photo_quality 0 0 0 "gimp script" 0 0 0 0)
)
)

(gimp-display-new ice_image)
(gimp-image-clean-all ice_image)

)
)

(script-fu-register
"script-fu-ice-logo"
"ice logo"
"this script can transform your text to ice"
"Bouchaib Enouiti"
"Copyright 2009, free to use"
"30 out, 2009"
""
SF-STRING     "Text"             "Gimp 2.8"
SF-FONT       "Font"             "Papyrus"
SF-ADJUSTMENT "Text size"        '(80 5 1000 1 5 0 1)
SF-COLOR      "Background color" '(255 255 255)
SF-ADJUSTMENT "Ice density"      '(7 1 50 1 1 0 1)
SF-ADJUSTMENT "Shadow deepth"    '(4 1 7 1 1 0 1)
SF-DIRNAME    "Save to"          ""
SF-OPTION     "Photo format"     '("gif" "jpg")
SF-ADJUSTMENT "Photo quality"    '(0.8 0 1 0.1 0.1 1 0)
SF-STRING    "Serial number"    "bouc-haib-enou-iti1"
)

(script-fu-menu-register "script-fu-ice-logo" "<Image>/File/Create/additional logos")

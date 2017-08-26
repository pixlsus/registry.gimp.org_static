; export_iOS_icons_of_image.scm
; Version 1.20
; Copyright (c) 2013 Michael Morris
; This software is released under MIT Open Source License
; ==============================================================================

; ------------------------------------------------------------------------------
(define (export-as-ios-image inImage inDrawable inPath inFilename inWidth inHeight)
    (let* (
        (thePathAndFilename (string-append inPath DIR-SEPARATOR inFilename)))
        
        ; Check to make sure thePathAndFilename does not exist
        (if (file-exists? thePathAndFilename)
        
            ; The file exists, then display message and exit
            (gimp-message (string-append "The file:[" thePathAndFilename "] exists, skipping."))
            
            ; The file does not exist, then duplicate, scale, export, and delete duplicate
            (let* (
                (theDuplicateImage (car (gimp-image-duplicate inImage))))
                (gimp-image-scale theDuplicateImage inWidth inHeight)
                (file-png-save
                    RUN-NONINTERACTIVE
                    theDuplicateImage
                    (car (gimp-image-merge-visible-layers theDuplicateImage 1))
                    thePathAndFilename
                    inFilename
                    0               ; Use Adam7 interlacing?
                    9               ; Deflate Compression factor (0--9)
                    1               ; Write bKGD chunk?
                    1               ; Write gAMA chunk?
                    0               ; Write oFFs chunk?
                    0               ; Write pHYs chunk?
                    0)              ; Write tIME chunk?
                (gimp-image-delete theDuplicateImage)))))

; ------------------------------------------------------------------------------
(define (export-retina-resolution-image-as-ios-images inImage inDrawable inPath inFilename inImageExportOption)
    ; Image export: Retina
    (if (or (= 0 inImageExportOption) (= 1 inImageExportOption))
        (let* (
            (filename (string-append inFilename "@2x.png")))
            (export-as-ios-image inImage inDrawable inPath filename (car (gimp-image-width inImage)) (car (gimp-image-height inImage)))))

    ; Image export: Non-Retina
    (if (or (= 0 inImageExportOption) (= 2 inImageExportOption))
        (let* (
            (filename (string-append inFilename ".png")))
            (export-as-ios-image inImage inDrawable inPath filename (round (/ (car (gimp-image-width inImage)) 2)) (round (/ (car (gimp-image-height inImage)) 2))))))
     
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(script-fu-register
    "export-retina-resolution-image-as-ios-images"                  ;func name
    "Image(s) ..."                                                  ;menu label
    "Exports an iOS Retina image and/or an iOS Non-Retina image\
    from the active image. The active image's resolution is\
    equivalent to that of the resulting iOS Retina image.\
    \
    The resulting iOS Retina image has the same width and height\
    as the active image. The resulting iOS Non-Retina image has\
    half the width and half the height as the active image.\
    \
    The recommended resolution of the active image is\
    equivalent to that of the resulting iOS Retina image."          ;description
    "Michael Morris"                                                ;author
    "Copyright (c) 2013 Michael Morris\         
    This software is released under MIT Open Source License"        ;copyright notice
    "July 17, 2013"                                                 ;date created
    "*"                                                             ;image type that the script works on
    SF-IMAGE     "Image"                                    0
    SF-DRAWABLE  "Drawable"                                 0
    SF-DIRNAME   "Path"                                     "/tmp"
    SF-STRING    "Filename\n  (without @2x.png & .png)"     "Image"
    SF-OPTION    "Image(s) exported" '("Retina and Non-Retina" "Retina only" "Non-Retina only")
)

(script-fu-menu-register "export-retina-resolution-image-as-ios-images" "<Image>/File/iOS Export/Retina Resolution Image as")

; ------------------------------------------------------------------------------
(define (export-image-as-ios-images inImage inDrawable inPath inFilename inImageExportOption inImageScalingOption inSize)
    ; Image export: Retina
    (if (or (= 0 inImageExportOption) (= 1 inImageExportOption))
        (let* (
            (filename (string-append inFilename "@2x.png")))
          
            (if (= 0 inImageScalingOption)
                ; Scaling option square: width = size, height = size
                (export-as-ios-image inImage inDrawable inPath filename inSize inSize)

                (if (= 1 inImageScalingOption)
                    ; Scaling option rectangle: width = size, height is proportionate
                    (export-as-ios-image inImage inDrawable inPath filename inSize (round (* (car (gimp-image-height inImage)) (/ inSize (car (gimp-image-width inImage))))))

                    (if (= 2 inImageScalingOption)
                        ; Scaling option rectangle: width is proportionate, height = size
                        (export-as-ios-image inImage inDrawable inPath filename (round (* (car (gimp-image-width inImage)) (/ inSize (car (gimp-image-height inImage))))) inSize))))
            ; Halve the image size for Non-Retina image
            (set! inSize (round (/ inSize 2)))))

    ; Image export: Non-Retina
    (if (or (= 0 inImageExportOption) (= 2 inImageExportOption))
        (let* (
            (filename (string-append inFilename ".png")))
            
            (if (= 0 inImageScalingOption)
                ; Scaling option square: width = size, height = size
                (export-as-ios-image inImage inDrawable inPath filename inSize inSize)

                (if (= 1 inImageScalingOption)
                    ; Scaling option rectangle: width = size, height is proportionate
                    (export-as-ios-image inImage inDrawable inPath filename inSize (round (* (car (gimp-image-height inImage)) (/ inSize (car (gimp-image-width inImage))))))

                    (if (= 2 inImageScalingOption)
                        ; Scaling option rectangle: width is proportionate, height = size
                        (export-as-ios-image inImage inDrawable inPath filename (round (* (car (gimp-image-width inImage)) (/ inSize (car (gimp-image-height inImage))))) inSize)))))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(script-fu-register
    "export-image-as-ios-images"                                    ;func name
    "Image(s) ..."                                                  ;menu label
    "Exports an iOS Retina image and/or an iOS Non-Retina image\
    from the active image. The resulting image(s) are scaled per\
    user specifications.\
    \
    Scaling will be either down, up, or none, depending upon the\
    user specified size and the active image's resolution. Scaling\
    is either square or rectangular. Square scaling results in\
    image(s) that have the width and height equal to the size\
    specified. Rectangular scaling results in image(s) that have\
    either the width or height equal to the size specified and the\
    other dimension scaled proportionally.\
    \
    If the user specifies that both Retina and Non-Retina images\
    are to be exported, the resolution of the resulting iOS Retina\
    image will be equivalent to the size specified while the\
    resolution of the resulting iOS Non-Retina image will be half\
    of the size. If the user specifies only a Retina or only a\
    Non-Retina image is to be exported, resulting iOS image will\
    be equivalent to the size specified."                           ;description
    "Michael Morris"                                                ;author
    "Copyright (c) 2013 Michael Morris\         
    This software is released under MIT Open Source License"        ;copyright notice
    "July 17, 2013"                                                 ;date created
    "*"                                                             ;image type that the script works on
    SF-IMAGE     "Image"                                    0
    SF-DRAWABLE  "Drawable"                                 0
    SF-DIRNAME   "Path"                                     "/tmp"
    SF-STRING    "Filename\n  (without @2x.png & .png)"     "Image"
    SF-OPTION    "Image(s) exported" '("Retina and Non-Retina" "Retina only" "Non-Retina only")
    SF-OPTION    "Image scaling" '("Square (width = size, height = size)" "Rectangle (width = size, height is proportionate)" "Rectangle (width is proportionate, height = size)")
    SF-VALUE     "Size of image in Pixels\n  (if both Retina and Non-Retina\n  images are being exported,\n  the Non-Retina image size\n  will be half of the value)"   "114"
)

(script-fu-menu-register "export-image-as-ios-images" "<Image>/File/iOS Export/Image as")

; ------------------------------------------------------------------------------
(define (export-image-as-app-icons-for-devices inImage inDrawable inPath iniPadIcons iniPhoneIcons)
    
    (if (or (= 1 iniPadIcons) (= 1 iniPhoneIcons))
        (begin
            (export-as-ios-image inImage inDrawable inPath "iTunesArtwork@2x" 1024 1024)
            (export-as-ios-image inImage inDrawable inPath "iTunesArtwork" 512 512)
            (export-as-ios-image inImage inDrawable inPath "Icon-Small@2x.png" 58 58)
            (export-as-ios-image inImage inDrawable inPath "Icon-Small.png" 29 29)))
    (if (= 1 iniPadIcons)
        (begin
            (export-as-ios-image inImage inDrawable inPath "Icon-72@2x.png" 144 144)
            (export-as-ios-image inImage inDrawable inPath "Icon-72.png" 72 72)
            (export-as-ios-image inImage inDrawable inPath "Icon-Small-50@2x.png" 100 100)
            (export-as-ios-image inImage inDrawable inPath "Icon-Small-50.png" 50 50)))
    (if (= 1 iniPhoneIcons)
        (begin
            (export-as-ios-image inImage inDrawable inPath "Icon@2x.png" 114 114)
            (export-as-ios-image inImage inDrawable inPath "Icon.png" 57 57))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(script-fu-register
    "export-image-as-app-icons-for-devices"                         ;func name
    "App Icons for Device(s) ..."                                   ;menu label
    "Exports the required and recommend iOS app icons for the\
    various iOS devices and iOS display types from the active\
    image.\
    \
    The recommended resolution of the active image is\
    1024 x 1024."                                                   ;description
    "Michael Morris"                                                ;author
    "Copyright (c) 2013 Michael Morris\
    This software is released under MIT Open Source License"        ;copyright notice
    "July 17, 2013"                                                 ;date created
    "*"                                                             ;image type that the script works on
    SF-IMAGE    "Image"                             0
    SF-DRAWABLE "Drawable"                          0
    SF-DIRNAME  "Path"                              "/tmp"
    SF-TOGGLE   "Create iPad icons"                 1
    SF-TOGGLE   "Create iPhone/iPod touch icons"    1
)

(script-fu-menu-register "export-image-as-app-icons-for-devices" "<Image>/File/iOS Export/Image as")
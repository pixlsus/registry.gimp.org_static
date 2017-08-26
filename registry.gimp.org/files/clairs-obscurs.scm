(define (script-fu-shadows-highlights image drawable)

; create a highlights layer
(let ((highlights-layer (car (gimp-layer-copy drawable 1))))
(gimp-drawable-set-name highlights-layer "Assombrir les lumières")
(gimp-image-add-layer image highlights-layer -1)

;process shadows/highlights layer
(gimp-desaturate highlights-layer)
(gimp-invert highlights-layer)
(gimp-layer-set-mode highlights-layer 5)
(plug-in-gauss-iir2 1 image highlights-layer 25 25)

;copy highlights layer to create shadows layer
(define shadows-layer (car (gimp-layer-copy highlights-layer 1)))
(gimp-drawable-set-name shadows-layer "Éclaircir les ombres")
(gimp-image-add-layer image shadows-layer -1)

;process highlights layer
(plug-in-colortoalpha 1 image highlights-layer '(255 255 255))
(gimp-layer-set-opacity highlights-layer 0)

;process shadows layer
(plug-in-colortoalpha 1 image shadows-layer '(0 0 0))
(gimp-layer-set-opacity shadows-layer 0)

;update image window
(gimp-displays-flush)))

(script-fu-register "script-fu-shadows-highlights"
                   _"<Image>/Filters/Clairs & Obscurs"
                    "Corrige les ombres et les hautes lumières - adapté depuis http://mailgate.supereva.com/comp/comp.graphics.apps.gimp/msg06394.html"
                    "Arnaud Champollion - d'après Shadows and Highlights de Dennis Bond - grâce au travail de Jozef Trawinski"
                    "Arnaud Champollion - d'après Shadows and Highlights de Dennis Bond - grâce au travail de Jozef Trawinski"
                    "24 octobre 2007"

                    "RGB* GRAY*"
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Drawable" 0)

; B&W Film Emulation via Channel Mixer v0.1
; Created by Patrick David <patdavid@gmail.com>
; http://blog.patdavid.net
;
; Similar in functionality to my previous "Color Decompose" script.
; Instead of decomposing to a bunch of layers, this script will
; decompose a bunch of layers into B&W based on some common
; Channel Mixer settings to emulate various B&W film stocks.
;
; Careful!  This will create ~ 15 new layers of your image, each a different
; B&W film emulation.
;
; If you check the Contact Sheet? option, instead it will scale down
; and create each version all on a single image for inspection.
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

(define (script-fu-patdavid-bw-film-channels Image Drawable ContactSheet)

	(let*
	  (
        (lyr 0)
        (iWidth)
        (iHeight)
        (x-space)
        (y-space)
        (background)
        (forecolor)
	  )

      (set! iWidth (* 5 (* 0.2 (car(gimp-drawable-width Drawable)))))
      (set! iHeight (* 3 (* 0.2 (car(gimp-drawable-height Drawable)))))

      (set! x-space 1.02)
      (set! y-space 1.02)

      (if (= ContactSheet TRUE)
        (let* ()

                (set! Image (car(gimp-image-new iWidth iHeight RGB)))
                (gimp-context-set-interpolation INTERPOLATION-LANCZOS)

                (set! forecolor (car(gimp-context-get-foreground)))
                (gimp-context-set-foreground '(127 127 127))

                (set! background (car(gimp-layer-new Image iWidth iHeight 0 "Background" 100 0)))
                (gimp-image-insert-layer Image background 0 -1)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Agfa 200X")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Agfa 200X" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.18 0.41 0.41 0.18 0.41 0.41 0.18 0.41 0.41)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Agfapan 25")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Agfapan 25" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* x-space (car(gimp-drawable-width lyr))) 0)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.25 0.39 0.36 0.25 0.39 0.36 0.25 0.39 0.36)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Agfapan 100")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Agfapan 100" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 2 (* x-space (car(gimp-drawable-width lyr)))) 0)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.21 0.40 0.39 0.21 0.40 0.39 0.21 0.40 0.39)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Agfapan 400")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Agfapan 400" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 3 (* x-space (car(gimp-drawable-width lyr)))) 0)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.20 0.41 0.39 0.20 0.41 0.39 0.20 0.41 0.39)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford Delta 100")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Ilford Delta 100" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 4 (* x-space (car(gimp-drawable-width lyr)))) 0)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.21 0.42 0.37 0.21 0.42 0.37 0.21 0.42 0.37)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford Delta 400")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Ilford Delta 400" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 0 (* x-space (car(gimp-drawable-width lyr)))) (* 1 (* y-space (car(gimp-drawable-height lyr)))) )
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.22 0.42 0.36 0.22 0.42 0.36 0.22 0.42 0.36)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford Delta 400 Pro")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Ilford Delta 400 Pro" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 1 (* x-space (car(gimp-drawable-width lyr)))) (* 1 (* y-space (car(gimp-drawable-height lyr)))) )
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.31 0.36 0.33 0.31 0.36 0.33 0.31 0.36 0.33)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford FP4")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Ilford FP4" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 2 (* x-space (car(gimp-drawable-width lyr)))) (* 1 (* y-space (car(gimp-drawable-height lyr)))) )
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.28 0.41 0.31 0.28 0.41 0.31 0.28 0.41 0.31)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford HP5")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Ilford HP5" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 3 (* x-space (car(gimp-drawable-width lyr)))) (* 1 (* y-space (car(gimp-drawable-height lyr)))) )
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.23 0.37 0.40 0.23 0.37 0.40 0.23 0.37 0.40)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford Pan F")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Ilford Pan F" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 4 (* x-space (car(gimp-drawable-width lyr)))) (* 1 (* y-space (car(gimp-drawable-height lyr)))) )
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.33 0.36 0.31 0.33 0.36 0.31 0.33 0.36 0.31)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford SFX")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Ilford SFX" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 0 (* x-space (car(gimp-drawable-width lyr)))) (* 2 (* y-space (car(gimp-drawable-height lyr)))) )
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.36 0.31 0.33 0.36 0.31 0.33 0.36 0.31 0.33)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford XP2 Super")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Ilford XP2 Super" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 1 (* x-space (car(gimp-drawable-width lyr)))) (* 2 (* y-space (car(gimp-drawable-height lyr)))) )
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.21 0.42 0.37 0.21 0.42 0.37 0.21 0.42 0.37)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Kodak Tmax 100")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Kodak Tmax 100" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 2 (* x-space (car(gimp-drawable-width lyr)))) (* 2 (* y-space (car(gimp-drawable-height lyr)))) )
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.24 0.37 0.39 0.24 0.37 0.39 0.24 0.37 0.39)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Kodak Tmax 400")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Kodak Tmax 400" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 3 (* x-space (car(gimp-drawable-width lyr)))) (* 2 (* y-space (car(gimp-drawable-height lyr)))) )
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.27 0.36 0.37 0.27 0.36 0.37 0.27 0.36 0.37)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Kodak Tri-X")
                (gimp-image-insert-layer Image lyr 0 -1)
                (gimp-floating-sel-anchor (car (gimp-text-fontname Image lyr 10 10 "Kodak Tri-X" 10 TRUE 100 PIXELS "Arial")))
                (gimp-layer-scale lyr (/ iWidth 5) (/ iHeight 3) FALSE)
                (gimp-layer-translate lyr (* 4 (* x-space (car(gimp-drawable-width lyr)))) (* 2 (* y-space (car(gimp-drawable-height lyr)))) )
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.25 0.35 0.40 0.25 0.35 0.40 0.25 0.35 0.40)

                (gimp-image-resize-to-layers Image)
                (set! iWidth (car(gimp-image-width Image)))
                (set! iHeight (car(gimp-image-height Image)))

                (gimp-image-resize Image (* 1.02 iWidth) (* 1.02 iHeight) (/ (- (* 1.02 iWidth) iWidth) 2) (/ (- (* 1.02 iHeight) iHeight) 2))

                (gimp-layer-resize-to-image-size background)
                (gimp-context-set-foreground '(0 0 0))
                (gimp-drawable-fill background 0)

                (gimp-context-set-foreground forecolor)

                (gimp-display-new Image)
        )
	  )

      (if (= ContactSheet FALSE)
        (let* ()

                (gimp-image-undo-group-start Image)
                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Agfa 200X")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.18 0.41 0.41 0.18 0.41 0.41 0.18 0.41 0.41)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Agfapan 25")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.25 0.39 0.36 0.25 0.39 0.36 0.25 0.39 0.36)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Agfapan 100")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.21 0.40 0.39 0.21 0.40 0.39 0.21 0.40 0.39)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Agfapan 400")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.20 0.41 0.39 0.20 0.41 0.39 0.20 0.41 0.39)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford Delta 100")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.21 0.42 0.37 0.21 0.42 0.37 0.21 0.42 0.37)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford Delta 400")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.22 0.42 0.36 0.22 0.42 0.36 0.22 0.42 0.36)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford Delta 400 Pro")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.31 0.36 0.33 0.31 0.36 0.33 0.31 0.36 0.33)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford FP4")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.28 0.41 0.31 0.28 0.41 0.31 0.28 0.41 0.31)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford HP5")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.23 0.37 0.40 0.23 0.37 0.40 0.23 0.37 0.40)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford Pan F")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.33 0.36 0.31 0.33 0.36 0.31 0.33 0.36 0.31)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford SFX")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.36 0.31 0.33 0.36 0.31 0.33 0.36 0.31 0.33)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Ilford XP2 Super")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.21 0.42 0.37 0.21 0.42 0.37 0.21 0.42 0.37)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Kodak Tmax 100")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.24 0.37 0.39 0.24 0.37 0.39 0.24 0.37 0.39)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Kodak Tmax 400")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.27 0.36 0.37 0.27 0.36 0.37 0.27 0.36 0.37)

                (set! lyr (car(gimp-layer-new-from-drawable Drawable Image)))
                (gimp-item-set-name lyr "Kodak Tri-X")
                (gimp-image-insert-layer Image lyr 0 -1)
                (plug-in-colors-channel-mixer RUN-NONINTERACTIVE Image lyr TRUE 0.25 0.35 0.40 0.25 0.35 0.40 0.25 0.35 0.40)

                (gimp-image-undo-group-end Image)

        )
      )

      (gimp-displays-flush)

    )
)

; Finally register our script with script-fu
(script-fu-register "script-fu-patdavid-bw-film-channels"
                    "B&W Film Channels..."
                    "Convert to multiple B&W images based on popular Channel Mixer settings."
                    "Patrick David <pat@patdavid.net>"
                    "Patrick David"
                    "2013-1-30"
                    "RGB*"
			SF-IMAGE	"Image"		0
			SF-DRAWABLE	"Drawable"	0
            SF-TOGGLE "Contact Sheet?" TRUE
)

(script-fu-menu-register "script-fu-patdavid-bw-film-channels" "<Image>/Colors")

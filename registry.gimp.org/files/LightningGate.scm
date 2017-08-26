;;  ***************************************************************************
;;  *   Copyright (C) 2008 by James Sambrook                                  *
;;  *   sambrook@va.metrocast.net                                             *
;;  *                                                                         *
;;  *   This program is free software; you can redistribute it and/or modify  *
;;  *   it under the terms of the GNU General Public License as published by  *
;;  *   the Free Software Foundation; either version 2 of the License, or     *
;;  *   (at your option) any later version.                                   *
;;  *                                                                         *
;;  *   This program is distributed in the hope that it will be useful,       *
;;  *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
;;  *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
;;  *   GNU General Public License for more details.                          *
;;  *                                                                         *
;;  *   You should have received a copy of the GNU General Public License     *
;;  *   along with this program; if not, write to the                         *
;;  *   Free Software Foundation, Inc.,                                       *
;;  *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
;;  ***************************************************************************

(define (script-fu-lightninggate width height turb properAR xNoise yNoise frames brightLevel satLevel lightLevel)
  (let* ((theImage (car (gimp-image-new width height 0)))
         (layer (car (gimp-layer-new theImage width height 0 "Bottom" 100 0)))
         (frameName "")
	   (frameNum 0)
	   (widthStep (/ width frames))
	   (heightStep (/ height frames))
         (flag 0)
        )

  (gimp-context-push)   ; Remember previous settings
  (gimp-image-undo-disable theImage)
  (gimp-context-set-foreground '(255 255 255))
  (gimp-context-set-background '(0 0 0))

  (while (< flag frames)
	(let* ((newLayer (car (gimp-layer-copy layer FALSE))))
	(gimp-image-add-layer theImage newLayer -1)

;	Name the layer "Frame X"
	(set! frameName (string-append "Frame " (number->string (+ flag 1) frameNum)))
	(gimp-drawable-set-name newLayer frameName)

; Generate Random Noise in the layer
	(plug-in-solid-noise RUN-NONINTERACTIVE
		theImage newLayer 1 0 (rand 4000000000) turb width height)

	(if (= properAR TRUE)
		(set! yNoise (* xNoise (/ width height))))

; Blend a shaped gradient in difference mode on the layer
	(gimp-edit-blend newLayer FG-BG-RGB-MODE DIFFERENCE-MODE GRADIENT-SHAPEBURST-SPHERICAL
		100 0 REPEAT-NONE FALSE FALSE 3 0.2 TRUE 0 0 xNoise yNoise)

; Invert the colors in the layer to make the lightning show up properly
	(gimp-invert newLayer)

; Set the threshhold to highlight the lightning
	(gimp-levels newLayer HISTOGRAM-VALUE brightLevel 255 1 0 255)
	(gimp-layer-set-mode newLayer LIGHTEN-ONLY-MODE)

; Resize the layer, and then rescale it to the correct image size
	(gimp-layer-scale newLayer (* (+ 1 flag) widthStep) (* (+ 1 flag) heightStep) 1)
	(gimp-layer-resize-to-image-size newLayer)

; Incremental colorizing
	(gimp-colorize newLayer (* (/ flag frames) 360) satLevel lightLevel)

; Increment the flag
	(set! flag (+ flag 1))

  ))

  (gimp-context-pop)   ; Restore previous settings
  (gimp-image-undo-enable theImage)
  (gimp-display-new theImage)

))

(script-fu-register
    "script-fu-lightninggate"
    _"_Lightning Gate..."
    _"Creates a tunnel of lightning."
    "James Sambrook"
    "10 October 2008"
    "James Sambrook.  King George, VA, USA"
    ""
    SF-ADJUSTMENT "Width"         '(600 300 3000 10 50 0 0)
    SF-ADJUSTMENT "Height"        '(600 300 3000 10 50 0 0)
    SF-ADJUSTMENT "Turbulence"    '(2 1 15 1 5 0 0)
    SF-TOGGLE     "Aspect Ratio for Noise Matches Aspect Ratio of Image" FALSE
    SF-ADJUSTMENT "X Noise Size"  '(4 1 15 0.1 1 1 0)
    SF-ADJUSTMENT "Y Noise Size"  '(4 1 15 0.1 1 1 0)
    SF-ADJUSTMENT "No. of frames" '(5 2 20 1 5 0 0)
    SF-ADJUSTMENT "Threshhold"    '(200 1 255 5 20 0 0)
    SF-ADJUSTMENT "Saturation"    '(50 0 100 1 10 0 0)
    SF-ADJUSTMENT "Lightness"     '(0 -100 100 5 20 0 0)

)

(script-fu-menu-register "script-fu-lightninggate"
                         "<Image>/Filters/SambrookJM/")

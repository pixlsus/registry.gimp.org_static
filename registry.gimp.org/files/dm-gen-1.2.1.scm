; Script to build displacement map v.1.2.1
; Written by Andrey Lebedenko (Lucidlook) 2009
;; ----------------------------------------------------------------
; 
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.  
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-dm-gen
				theOldImage
				theOldLayer
				mapXuser
				mapYuser
				mapNoise
				map3Dcorr
				mapNoBlue
				mapDecompose
	)

	; Initialize an undo, so the process can be undone with a single undo
;    (gimp-image-undo-group-start theOldImage)

; Variable init
  (let* (
	(imageWidth  (car (gimp-image-width theOldImage)))
	(imageHeight (car (gimp-image-height theOldImage)))
	(mask)
	(theImage)
	(newLayer)
	(red-component 0)
	(green-component 1)
	(blue-component 2)
	(alpha-component 5)
	(i 0)
	(RGBImage)
	(num_layers 0)
	(layer_ids)
	(current_layer)
	(theDisplay)
    )


	(set! theImage (car (gimp-image-new mapXuser mapYuser RGB)))
	(set! newLayer (car (gimp-layer-new   theImage 
                                         mapXuser
                                         mapYuser
                                         RGB-IMAGE
                                         "Displacement map"
                                         100
                                         NORMAL-MODE
						)
                )
	)


; Add new layer
	(gimp-image-add-layer theImage newLayer 0)
	(set! theDisplay (car (gimp-display-new theImage)))
   
	(gimp-context-set-background '(255 255 255))
	(gimp-context-set-foreground '(0 0 0))
	(gimp-drawable-fill newLayer BACKGROUND-FILL)
	(gimp-layer-set-lock-alpha newLayer TRUE)
	
	(while (< i mapNoise)
		(plug-in-rgb-noise RUN-NONINTERACTIVE theImage newLayer 0 1 0.2 0.2 0.2 0)
		(set! i (+ i 1))
	)
	
	(plug-in-gauss-iir 1 theImage newLayer 5 TRUE TRUE)

	(gimp-image-set-component-active theImage green-component FALSE)
	(gimp-image-set-component-active theImage blue-component FALSE)
	(plug-in-emboss 1 theImage newLayer 0 50.0 10 -5) ; horizontal
	
	(gimp-image-set-component-active theImage red-component FALSE)
	(gimp-image-set-component-active theImage green-component TRUE)
	(plug-in-emboss 1 theImage newLayer 90 50.0 10 -5) ; vertical

	(if (= mapNoBlue TRUE)
		(begin
		(gimp-image-set-component-active theImage red-component FALSE)
;		(gimp-image-set-component-visible theImage red-component FALSE)
	
		(gimp-image-set-component-active theImage green-component FALSE)
;		(gimp-image-set-component-visible theImage green-component FALSE)
	
		(gimp-image-set-component-active theImage blue-component TRUE)
;		(gimp-image-set-component-visible theImage blue-component TRUE)
;		(gimp-image-set-component-active theImage alpha-component TRUE)
;		(gimp-image-set-component-visible theImage alpha-component TRUE)
	
		(gimp-edit-bucket-fill-full newLayer FG-BUCKET-FILL NORMAL-MODE 100 255 FALSE TRUE SELECT-CRITERION-COMPOSITE 0 0)
		)
	)
	(gimp-image-set-component-active theImage red-component TRUE)
	(gimp-image-set-component-visible theImage red-component TRUE)
	(gimp-image-set-component-active theImage green-component TRUE)
	(gimp-image-set-component-visible theImage green-component TRUE)

	(set! i 1)
	(while (<= i map3Dcorr)
		(gimp-perspective newLayer TRUE 0 0 mapXuser 0 (- 0 mapXuser) (* mapYuser 4) (* mapXuser 2) (* mapYuser 4))
		(gimp-layer-scale newLayer (* mapXuser 3) mapYuser FALSE)
;		(gimp-drawable-transform-scale-default newLayer 0 0 (car (gimp-drawable-width theLayer)) imageHeight TRUE TRUE)
		
		(gimp-layer-scale newLayer (* mapXuser 3) (+ mapYuser 10) FALSE)
		(gimp-image-crop (car(gimp-drawable-get-image newLayer)) mapXuser mapYuser 0 0)
		
		(set! i (+ i 1))
	)
	
	(gimp-drawable-transform-scale-default newLayer (- 0 mapXuser) 0 (* mapXuser 4) mapYuser TRUE TRUE)
	(gimp-image-crop (car(gimp-drawable-get-image newLayer)) mapXuser mapYuser 0 0)


	(set! mask (car (gimp-layer-create-mask newLayer ADD-WHITE-MASK)))
	(gimp-layer-add-mask newLayer mask)

	(gimp-edit-blend mask FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE TRUE FALSE 1 0 TRUE 0 0 0 mapYuser)

	; mask to selection ( mask-to-selection ) :
	(gimp-selection-load mask)

	(gimp-image-set-component-active theImage red-component TRUE)
	(gimp-image-set-component-active theImage green-component FALSE)
	(gimp-image-set-component-active theImage blue-component FALSE)

	; fill with grey
	(gimp-context-set-foreground '(128 128 128))
	(gimp-edit-bucket-fill-full newLayer FG-BUCKET-FILL NORMAL-MODE 100 255 FALSE TRUE SELECT-CRITERION-COMPOSITE 0 0)


	(gimp-selection-none theImage)
	(gimp-context-set-foreground '(0 0 0))
	(gimp-context-set-background '(255 255 255))
	
	(gimp-edit-blend mask FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE TRUE FALSE 1 0 TRUE 
		0 0 0 (round(* mapYuser 0.2)) )

	; mask to selection ( mask-to-selection ) :
	(gimp-selection-load mask)

	(gimp-image-set-component-active theImage red-component FALSE)
	(gimp-image-set-component-active theImage green-component TRUE)
	(gimp-image-set-component-active theImage blue-component FALSE)

	; fill with grey
	(gimp-context-set-foreground '(128 128 128))
	(gimp-edit-bucket-fill-full newLayer FG-BUCKET-FILL NORMAL-MODE 100 255 FALSE TRUE SELECT-CRITERION-COMPOSITE 0 0)
	
	(gimp-layer-remove-mask newLayer MASK-DISCARD)
	(gimp-selection-none theImage)
	
	(if (= mapDecompose TRUE) 
	(begin
		(set! RGBImage (car (plug-in-decompose RUN-NONINTERACTIVE theImage newLayer "RGB" TRUE)))
		
		; since we decompose, we no loner need original map
		(gimp-display-delete theDisplay)
		; (gimp-image-delete theImage)
		
	
		; delete blue channel/layer
		(set! num_layers (car (gimp-image-get-layers RGBImage)))
		(set! layer_ids (cadr (gimp-image-get-layers RGBImage)))
		(set! i 0)
	
		(while (< i num_layers)
			(set! current_layer (vector-ref layer_ids i))
				(if (= i 0) ; red
					(gimp-drawable-set-name current_layer "X")
			)
			(if (= i 1) ; green
				(gimp-drawable-set-name current_layer "Y")
			)
			(if (= i 2) ; blue
				(if (= mapNoBlue TRUE)
					(gimp-image-remove-layer RGBImage current_layer)
				)
			)
			(set! i (+ i 1))
		)
		(gimp-display-new RGBImage)
	)
	else
	(begin
    ;Finish the undo group for the process and enable all channels
		(gimp-image-set-component-active theImage red-component TRUE)
		(gimp-image-set-component-active theImage green-component TRUE)
		(gimp-image-set-component-active theImage blue-component TRUE)
	)
	)
	
    ;Ensure the updated image is displayed now
    (gimp-displays-flush)
)
)

(script-fu-register "script-fu-dm-gen"
            _"<Image>/Script-Fu/Map/Water reflection map"
            "Generates new image with displacement map. Can be used later for water reflection generation."
            "Andrey Lebedenko"
            "Andrey Lebedenko"
            "19.01.2009"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
            SF-VALUE	_"X Size:" "500"
			SF-VALUE	_"Y Size:" "500"
			SF-ADJUSTMENT   _"Noise level"     '(6 1 20 1 1 0 0)
			SF-ADJUSTMENT   _"3D corrections"     '(2 0 10 1 1 0 0)
            SF-TOGGLE	_"Delete blue channel"	TRUE
            SF-TOGGLE	_"Decompose channels into layers"	TRUE
)


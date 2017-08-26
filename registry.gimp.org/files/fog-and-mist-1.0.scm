; Script to feign mist and fog
; Written by Wilfrid TETARD 2009
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

(define (script-fu-fog-and-mist-1.0
				theOldImage
				theOldLayer
				thickness
				softness
				sheet
				clouds
				contraste
				mistman
				cloudman
				flatim
				
	)

	; Initialize an undo, so the process can be undone with a single undo
    	(gimp-image-undo-group-start theOldImage)

	; Variable init
  (let* (
	(imageWidth  (car (gimp-image-width theOldImage)))
	(mist-layerA)
	(mist-layerB)
	(mist-layerC)
	(cloud-layerA)
	(cloud-layerB)
	(num_layers)
	(layer_ids)
	(masque1)
	(masque2)
	)

	;create "Mist"-layers
	(set! mist-layerA (car(gimp-layer-new-from-drawable theOldLayer theOldImage)))
	(gimp-image-add-layer theOldImage mist-layerA -1)
	(if (= mistman TRUE) (plug-in-solid-noise 0 theOldImage mist-layerA 0 1 (rand 10) 15 0.8 (- 8.8 (/ softness 12.5))))
	(if (= mistman FALSE) (plug-in-solid-noise 1 theOldImage mist-layerA 0 1 (rand 10) 15 0.8 (- 8.8 (/ softness 12.5))))
	(gimp-layer-set-mode mist-layerA 4)
	(set! mist-layerB (car(gimp-layer-new-from-drawable mist-layerA theOldImage)))
	(gimp-image-add-layer theOldImage mist-layerB -1)
	(gimp-layer-set-mode mist-layerB 9)
	(gimp-layer-set-opacity mist-layerB (+ 75 (/ thickness 4)))
	(set! mist-layerC (car(gimp-layer-new-from-drawable mist-layerB theOldImage)))
	(gimp-image-add-layer theOldImage mist-layerC -1)
	(gimp-layer-set-mode mist-layerC 20)
	(gimp-layer-set-opacity mist-layerC softness)

	;create "Clouds"-layers
	(set! cloud-layerA (car(gimp-layer-new-from-drawable theOldLayer theOldImage)))
	(gimp-image-add-layer theOldImage cloud-layerA -1)
	(if (= cloudman TRUE) (plug-in-plasma 0 theOldImage cloud-layerA (rand 100) 2.5))
	(if (= cloudman FALSE) (plug-in-plasma 1 theOldImage cloud-layerA (rand 100) 2.5))	
	(gimp-desaturate-full cloud-layerA 0)
	(gimp-layer-set-mode cloud-layerA 4)
	(set! cloud-layerB (car(gimp-layer-new-from-drawable cloud-layerA theOldImage)))
	(gimp-image-add-layer theOldImage cloud-layerB -1)
	(gimp-layer-set-mode cloud-layerB 5)
	(gimp-layer-set-opacity mist-layerC (+ 20 (/ thickness 3)))

	;merge "Clouds"-layers
	(set! num_layers (car (gimp-image-get-layers theOldImage)))
	(set! layer_ids (cadr (gimp-image-get-layers theOldImage)))
	(gimp-image-merge-down theOldImage (vector-ref layer_ids 0) 2)
	(set! num_layers (car (gimp-image-get-layers theOldImage)))
	(set! layer_ids (cadr (gimp-image-get-layers theOldImage)))
	(gimp-layer-set-mode (vector-ref layer_ids 0) 4)
	(gimp-layer-set-opacity (vector-ref layer_ids 0) clouds)

	;add a layer mask and rename "Clouds"-layer
	(set! masque1 (car (gimp-layer-create-mask (vector-ref layer_ids 0) 0)))
	(gimp-image-add-layer-mask theOldImage (vector-ref layer_ids 0) masque1) 
	(gimp-layer-set-name (vector-ref layer_ids 0) "Clouds")
	(gimp-levels (vector-ref layer_ids 0) 0 0 255 (+ 1 (* 0.003 thickness)) 0 255)

	;merge "Mist"-layers	
	(set! num_layers (car (gimp-image-get-layers theOldImage)))
	(set! layer_ids (cadr (gimp-image-get-layers theOldImage)))
	(gimp-image-merge-down theOldImage (vector-ref layer_ids 2) 2)
	(set! num_layers (car (gimp-image-get-layers theOldImage)))
	(set! layer_ids (cadr (gimp-image-get-layers theOldImage)))
	;(gimp-layer-set-mode (vector-ref layer_ids 2) 4)
	(gimp-levels-stretch (vector-ref layer_ids 2))
	(set! num_layers (car (gimp-image-get-layers theOldImage)))
	(set! layer_ids (cadr (gimp-image-get-layers theOldImage)))
	(gimp-image-merge-down theOldImage (vector-ref layer_ids 1) 2)
	(set! num_layers (car (gimp-image-get-layers theOldImage)))
	(set! layer_ids (cadr (gimp-image-get-layers theOldImage)))
	(gimp-layer-set-mode (vector-ref layer_ids 1) 4)
	(gimp-layer-set-opacity (vector-ref layer_ids 1) sheet)
	(gimp-brightness-contrast (vector-ref layer_ids 1) (+ 0 (* 2 (- thickness 50))) (- 0 (* 2 (- contraste 50))))
	(gimp-levels (vector-ref layer_ids 1) 0 0 255 (+ 1 (* 0.002 thickness)) 0 255)

	;add a layer-mask and rename "Mist"-layer
	(set! masque2 (car (gimp-layer-create-mask (vector-ref layer_ids 1) 0)))
	(gimp-image-add-layer-mask theOldImage (vector-ref layer_ids 1) masque2)  
	(gimp-layer-set-name (vector-ref layer_ids 1) "Mist")
	(gimp-image-undo-group-end theOldImage)

    	;Ensure the updated image is displayed now
    	(gimp-displays-flush)
	(if (= flatim TRUE) (set! theOldLayer (gimp-image-flatten theOldImage)))
	(if (= flatim FALSE) (gimp-message "You can delete partially mist or clouds by setting the opacity of layers, or painting in black on the layer's masks"))
	
  )
)

(script-fu-register "script-fu-fog-and-mist-1.0"
            _"<Image>/Script-Fu/Effects/Harry's Mist"
            "Feign mist and fog with optional cloudy effect."
            "Wilfrid TETARD"
            "Wilfrid TETARD"
            "19.01.2009"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
	    SF-ADJUSTMENT   _"Thickness"     '(80 0 100 1 1 0 0)
	    SF-ADJUSTMENT   _"Mist appearance (Flat : 0 to Round : 100)"     '(50 0 100 1 1 0 0)
	    SF-ADJUSTMENT   _"Sheet layer opacity"     '(80 0 100 1 1 0 0)
	    SF-ADJUSTMENT   _"Clouds layer opacity"     '(80 0 100 1 1 0 0)
	    SF-ADJUSTMENT   _"Sheet uniformity"     '(40 35 100 1 1 0 0)
	    SF-TOGGLE _"Manually set mist appearance" FALSE
	    SF-TOGGLE _"Manually set clouds appearance" FALSE
	    SF-TOGGLE _"Flatten image" FALSE
           
)


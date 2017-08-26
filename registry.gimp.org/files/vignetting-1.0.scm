; Script to create an elliptical vignetting, so that to fit to the image's borders.
; You can choose dark or white vignetting and also blur corners.
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

(define (script-fu-vignetting-1.0
				theOldImage
				theOldLayer
				darkLevel
				softness
				distance
				blurLevel
				flatim
				
	)

	; Initialize an undo, so the process can be undone with a single undo
    	(gimp-image-undo-group-start theOldImage)

	; Variable init
  (let* (
	(imagewidth  (car (gimp-image-width theOldImage)))
	(imageheight  (car (gimp-image-height theOldImage)))
	(OldBackground (car (gimp-context-get-background)))
	(OldForeground (car (gimp-context-get-foreground)))
	(dark-layer)
	(vign-layer)
	(colLayer)
	(sizelayer)
	(num_layers)
	(layer_ids)
	(masque1)
	(masque2)
	)

	(gimp-context-set-background '(255 255 255))
	(gimp-context-set-foreground '(0 0 0))
	(set! dark-layer (car(gimp-layer-new-from-drawable theOldLayer theOldImage)))
	(gimp-image-add-layer theOldImage dark-layer -1)
	(gimp-layer-set-name dark-layer "Blur")
	(if (< 0 blurLevel) (plug-in-gauss 1 theOldImage dark-layer blurLevel blurLevel 0))
	(set! colLayer (car(gimp-layer-new theOldImage imagewidth imageheight 0 "Light" 100 0)))
	(gimp-image-add-layer theOldImage colLayer -1)
	(if (<= darkLevel 0) 
		(begin 
			(gimp-selection-none theOldImage)
			(gimp-edit-fill colLayer FOREGROUND-FILL)
			(gimp-layer-set-opacity colLayer (- 0 darkLevel))
			(gimp-layer-set-mode colLayer 9)
		)
	)
	(if (> darkLevel 0)
		(begin 
			(gimp-selection-none theOldImage) 
			(gimp-edit-fill colLayer BACKGROUND-FILL)
			(gimp-layer-set-opacity colLayer darkLevel)
			(gimp-layer-set-mode colLayer 10)
		)
	)
	(if (< imagewidth imageheight) (set! sizelayer imagewidth))
	(if (>= imagewidth imageheight) (set! sizelayer imageheight))
	(set! vign-layer (car(gimp-layer-new theOldImage sizelayer sizelayer 0 "vignetting" 100 0)))
	(gimp-image-add-layer theOldImage vign-layer -1)
	(gimp-edit-blend vign-layer 0 0 2 100 0 0 FALSE FALSE 1 0 TRUE (/ sizelayer 2) (/ sizelayer 2) (- 0 (* distance (* 0.01 sizelayer))) (/ sizelayer 2))
	(gimp-levels vign-layer 0 (* 1.2 softness) 255 1 0 255)
	(gimp-layer-scale vign-layer imagewidth imageheight FALSE)
	(set! masque1 (car(gimp-layer-create-mask vign-layer 5)))
	(gimp-layer-add-mask colLayer masque1)
	(set! masque2 (car(gimp-channel-copy masque1)))
	(gimp-layer-add-mask dark-layer masque2)
	(gimp-layer-set-opacity vign-layer 0)
	(gimp-image-remove-layer theOldImage vign-layer)
	(gimp-context-set-background OldBackground)
	(gimp-context-set-foreground OldForeground)
	(gimp-image-undo-group-end theOldImage)
	

    	;Ensure the updated image is displayed now
    	(gimp-displays-flush)
	(if (= flatim TRUE) (set! theOldLayer (gimp-image-flatten theOldImage)))
	
  )
)

(script-fu-register "script-fu-vignetting-1.0"
            _"<Image>/Script-Fu/Effects/Harry's Vignetting"
            "Script to create an elliptical vignetting, so that to fit to the image's borders."
	    "You can choose dark or clear vignetting and also blur corners."
            "Wilfrid TETARD"
            "21.10.2009"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
	    SF-ADJUSTMENT   _"Darker (-100) to Clearer (100)"     '(-50 -100 100 1 1 0 0)
	    SF-ADJUSTMENT   _"Vignetting hardness/Durete du vignettage"     '(30 0 100 1 1 0 0)
	    SF-ADJUSTMENT   _"Vignetting distance from borders"     '(0 -100 100 1 1 0 0)
	    SF-ADJUSTMENT   _"Blur corners"     '(0 0 100 1 1 0 0)
	    SF-TOGGLE _"Flatten image" FALSE
           
)


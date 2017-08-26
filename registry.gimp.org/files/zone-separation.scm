;
;   zone-separation.scm - script-fu for The Gimp
;	ZS-Separator - Separate a b/w-photo according to the zone system 
;   Version 0.8 (2013-11-15) 
;   testet with The Gimp 2.8
;   Copyright (C) 2013 Leif Pullich - liclic@gmx.de
; 
; Description
; 
; The script is intended to do some annoying work for a photographer
; who wants to analyze a b/w-photo according to the zone system
; of Ansel Adams.
; 
;   What it does
;
; ZS-Separator creates 11 masked layers, each representing one of the 11
; tonal zones of the zone system. The mask of each layer makes all pixels
; transparent which do not belong to the gray values of the corresponding
; zone. The sum of all layers gives the complete photo.
; Additionally, four differently colored layers are inserted below the
; stack of the zone layers which can be used to highlight pixels belonging
; to selected zones.
;
;   How to use it
;
; Activate the layer you  want to analyze and run the script. You will
; find the menu entry in Filters/Generic/Zone System Separator.
; Turn the colored layers below the layer stack invisible except
; one. Play around with the visibility or opacity of the zone layers.
; 
;   Shortcomings 
;
; - works only with RGB-images
; - does not check whether it has already been run. Hence, if it is
;   run again all layers will be created again
; - suits only for analyzing. There's no dynamic adaption if
;   you make changes to your original layer or to the zone layers 
;   If you did and you want to analyze the result, run the script again
; - excessively commented. Since the author is not a programmer he would
;   have appreciated to find more comments in other scripts while learning
;   script-fu
;
; The value ranges for each zone are inspired from an article by
; Thomas Brotzler on focussiert.com:
; http://fokussiert.com/2013/03/20/digitale-schwarzweisfotografie-das-zonensystem-2/
;
; TODO:
; - make it work with other image types or insert a check and warning
; - check whether the colored layers are already there, to avoid 
;   doubling when the script ist run again
; ----------------------------------------------------------------------
; Change log
;    - released: 2013-11-15
;
; ----------------------------------------------------------------------
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; subs
(define (sub-copy-layer inImage inLayer inLayerName)
	;; copy the layer <inLayer> and set it's name to <inLayerName>
	;; create copy of the orgininal layer, insert copy, set name 
	(let ((nZone 0))
	(set! nZone (car (gimp-layer-copy inLayer FALSE))) 
	(gimp-image-insert-layer inImage nZone 0 -1)
	(gimp-item-set-name nZone inLayerName)
	)
)

(define (sub-add-mask inLayer maskType)
	;; ad a mask of the type <maskType> to the copied layer
	;; create mask, set edit mode
	(gimp-layer-add-mask inLayer (car (gimp-layer-create-mask inLayer maskType)))
	(gimp-layer-set-edit-mask inLayer TRUE)
)

(define (sub-apply-threshold inLayer inValues)
	;; apply threshold-tool to the layer's mask 
	(gimp-threshold (car (gimp-layer-get-mask inLayer)) (car inValues) (cadr inValues))
)				

(define (sub-create-layer inImage inLayer inColVals)
	;; create colored layer
	(let*
		(
		(imageWidth (car(gimp-drawable-width (car(gimp-image-get-active-layer inImage)))))
		(imageHeight(car(gimp-drawable-height(car(gimp-image-get-active-layer inImage)))))
		(drawableType 0)    ;set drawable type to RGB (TODO: insert check of image type)
		(clayParent 0) 		;position new layer in main stack
		(clayPosition -1)	;position new layer directly above the current layer
		(clayOpacity 100)	;set full opacity
		(clayMode 0)		;set layer mode to normal
		(clayFillType 0)	;set fill-type to fill with foreground color
		(clayName (car inColVals))
		(clayColor(cdr inColVals))
		(cLayer)			;anonymous layer object
		)
		(gimp-context-set-foreground clayColor)
		(set! cLayer(car(gimp-layer-new inImage imageWidth imageHeight drawableType clayName clayOpacity clayMode)))
		(gimp-image-insert-layer inImage cLayer clayParent clayPosition)
		(gimp-drawable-fill cLayer clayFillType)
	)
)
;; main
(define (script-fu-separate-zones inImage inLayer)

	(let*
		(
		(baseLayer inLayer)
		;; each element of the following vector definition is a list 
		;; containing the layer's name and values for the threshold
		;; tool
 		;; (STR_LAYER_NAME NUM_THRESHOLD_LOW NUM_THRESHOLD_HIGH)			
		(zones #(				;define vector for zone values
			("Zone 0" 0 13)
			("Zone I" 14 39)
			("Zone II" 40 64)
			("Zone III" 65 89)
			("Zone IV" 90 115)
			("Zone V" 116 140)
			("Zone VI" 141 165)
			("Zone VII" 166 191)
			("Zone VIII" 192 216)
			("Zone IX" 217 242)
			("Zone X" 243 255)
			) 	
		)						;end zones
		(zoneLayer)				;layer for the zone to create
		(zoneLayerName "")		;name of the layer
		(zoneData)				;stores list from vector with the values for zone to create
		(i 10)					;counting variable
		;; (STR_LAYER_NAME NUM_R  NUM_G NUM_B)
		(vColoredLayers #(			;define vector for colored layer values
			("zs red"   255 0 0)
			("zs cyan"  0 255 255)
			("zs black" 0 0 0)
			("zs white" 255 255 255)
			)
		)						;end vColoredLayers
		) 
								;end var definition
		;; preliminaries
		(gimp-context-push)
		(gimp-image-undo-group-start inImage)
		(gimp-progress-init "Creating zones" -1)
		(gimp-progress-pulse)
		
		;; START - work thru all elements of <zones>
		(while (>= i 0)
		
		;; get list from vector for zone to create
		(set! zoneData (aref zones i)) 
		
		;; call sub for copying the original layer
		;; set name for layer, call sub, get layer for next step
		(set! zoneLayerName (car zoneData)) 
		(sub-copy-layer inImage baseLayer zoneLayerName) 
		(set! zoneLayer (car(gimp-image-get-active-layer inImage))) 
				
		;; call sub for generating new mask
		(sub-add-mask zoneLayer 5)
		
		;; call sub for applying threshold-tool to mask with values
		(sub-apply-threshold zoneLayer (cdr zoneData))
		
		;; set original layer as active layer
		(gimp-image-set-active-layer inImage baseLayer)
						
		;; update counter 
		(set! i (- i 1))
		) ;endwhile		
		
		;; insert colored layers
		(let((j 0)) ;; very locally defined counter
			(while (< j 4)
			(sub-create-layer inImage baseLayer (aref vColoredLayers j))
			;; pass each value list from vector-----------^
			(set! j (+ j 1))
			)
		)
		 
		;; cleanup
		(gimp-context-pop)
		(gimp-image-undo-group-end inImage)
		(gimp-progress-set-text "Ready")
		(gimp-progress-update 1.0)
		(gimp-displays-flush)
		
	)

)				
		


(script-fu-register 
	"script-fu-separate-zones"
	"Zone System Separator"
	"Separates a b&w photo according to the zone system. Set selected \
	 zone layers unvisible and use the colored layers to bring out the \
	 pixels belonging to the zone"
	"Leif Pullich"
	"2013 Leif Pullich under \
	terms of CC-License BY-NC Version XX"
	"Oktober 2013"
	"RGB*"
	
	SF-IMAGE "image" 0 
    SF-DRAWABLE "layer" 0 

)
(script-fu-menu-register 
	"script-fu-separate-zones"
	"<Image>/Filters/Generic"
)



;remove-timestampp.scm
;
; by Andreas Schönfelder
;
; Version 1.1 (20101120)
;
; License:
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
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html


(define (script-fu-remove-timestamp img drw color threshold grow1 grow2 spread_count spread offset_count offset)
	(let*
		(
			(layer1 0)
			(layer2 0)
			(layer3 0)
			(layer4 0)
			(-offset (* offset -1))
			(count 0)
		)
		
		; start
		(gimp-context-push)
		(gimp-image-undo-group-start img)
		
		; select color
		(gimp-by-color-select drw color threshold 3 FALSE FALSE 0.0 FALSE)
		
		; grow selection
		(gimp-selection-grow img grow1)
		
		; add alpha channel
		(gimp-layer-add-alpha drw)
		
		; clear selected
		(gimp-edit-clear drw)
		
		; create new layer
		(set! layer1 (car (gimp-layer-copy drw TRUE)))
		
		; add new created layer
		(gimp-image-add-layer img layer1 1)
		
		; grow selection
		(gimp-selection-grow img grow2)
		
		; interchange pixels
		(while (< count spread_count)
			(plug-in-spread TRUE img layer1 spread spread)
			(set! count (+ count 1))
		)
		
		; melt pixels
		(plug-in-randomize-slur TRUE img layer1 100 5 TRUE 1)
		
		; select nothing
		(gimp-selection-none img)
		
		; offset layers
		(set! count 0)
		(while (< count spread_count)
		
			; create new layers
			(set! layer2 (car (gimp-layer-copy layer1 TRUE)))
			(set! layer3 (car (gimp-layer-copy layer1 TRUE)))
			(set! layer4 (car (gimp-layer-copy layer1 TRUE)))
			
			; add new created layers
			(gimp-image-add-layer img layer2 2)
			(gimp-image-add-layer img layer3 3)
			(gimp-image-add-layer img layer4 4)
			
			; offset new created layers
			(gimp-drawable-offset layer1 FALSE 1 -offset -offset)
			(gimp-drawable-offset layer2 FALSE 1 offset -offset)
			(gimp-drawable-offset layer3 FALSE 1 -offset offset)
			(gimp-drawable-offset layer4 FALSE 1 offset offset)
			
			; merge all new layers
			(gimp-drawable-set-visible drw FALSE)
			(set! layer1 (car (gimp-image-merge-visible-layers img 1)))
			(gimp-drawable-set-visible drw TRUE)
			
			(set! count (+ count 1))
		)
			
		; merge all layers
		(gimp-image-merge-visible-layers img 1)
		
		; done
		(gimp-image-undo-group-end img)
		(gimp-displays-flush)
		(gimp-context-pop)
	)
)


(script-fu-register "script-fu-remove-timestamp"
	"<Image>/Filters/Enhance/Remove Timestamp"
	"Delete timestamp within an image"
	"Andreas Schönfelder <passtschu at freenet dot de>"
	"Andreas Schönfelder <passtschu at freenet dot de>"
	"2010-11-20"
	"*"
	SF-IMAGE		"Image"					0
	SF-DRAWABLE		"Drawable"				0
	SF-COLOR		"Color"					'(255 255 0)
	SF-ADJUSTMENT	"Threshold"				'(60 1 255 1 10 0 0)
	SF-ADJUSTMENT	"Grow color selection"	'(3 1 255 1 10 0 0)
	SF-ADJUSTMENT	"Grow fill selection"	'(10 1 255 1 10 0 0)
	SF-ADJUSTMENT	"Spread repeat"			'(3 1 10 1 10 0 0)
	SF-ADJUSTMENT	"Spread amount"			'(10 1 100 1 10 0 0)
	SF-ADJUSTMENT	"Offset repeat"			'(3 1 10 1 10 0 0)
	SF-ADJUSTMENT	"Offset amount"			'(3 1 100 1 10 0 0)
)

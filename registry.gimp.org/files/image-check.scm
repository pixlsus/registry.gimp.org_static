;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
;   Plugin      : Image Check
;   Author      : Hans Schopmeijer
;   Date        : 26-12-2013
;   Version     : 1.0
;   Required    : Gimp 2.8
;   Location    : Tools/Image Check
;   Modes       : RGB and RGBA
;	
;	This plugin creates a transparent layer on top of the active layer highlighting the parts
;	of the image with maximum local contrast, thus the parts witch are in perfect focus.
;   It also show which parts of the image are unrecoverable under,- and overexposed.
;   Finally if there is transparency in the layer(s), the plugin will indicate areas
;   with no image information. 
;   It is optional to sample only the active layer or all visible layers.
;   If the image contains a lot of detail (or noise), completion will take longer.
;
;	Input : - an image on the active layer 
;
;	Output: - a transparent layer directly above the active layer with the following colorcodes:
;             green        : areas in perfect focus
;             red          : areas with unrecoverable overexposure
;             blue         : areas with unrecoverable underexposure   
;             yellow/black : areas without image-information (only for layers with alphachannel)
;             
;
;	Enjoy!
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
	
(define (script-fu-image-check img drawa merged)  
   
   (let* (
        (pos 0)
        (focus-peaking 0)
        (temp 0)
        (chan 0)
        (item (car(gimp-image-get-active-layer img)))
       	 )

	;start undo-group and save context
	(gimp-image-undo-group-start img)
	(gimp-context-push)
	
    ;make working copy of active or all visible layer(s)
    (set! pos (car(gimp-image-get-item-position img item)))
    (if (equal? merged FALSE)
        (begin
    	    (set! focus-peaking (car (gimp-layer-copy item 1)))
    	    (gimp-layer-set-name focus-peaking "Focus Peaking")
    	)
        (set! focus-peaking (car (gimp-layer-new-from-visible img img "Focus Peaking\n     merged")))
    )
   	(gimp-image-insert-layer img focus-peaking 0 pos)

    ;check for "holes" in the image
   	(gimp-image-select-item img 2 focus-peaking)
    (gimp-selection-invert img)
    (set! chan (car(gimp-selection-save img)))
    (gimp-selection-none img) 

    ;make high-pass copy
	(set! temp (car (gimp-layer-copy focus-peaking 1))) 
   	(gimp-image-insert-layer img temp 0 pos)
    (plug-in-gauss-iir2 1 img temp 1 1 )
    (gimp-layer-set-mode temp SUBTRACT-MODE)
    (set! focus-peaking (car(gimp-image-merge-down img temp 0)))          
 
    ; do focus peaking (green)
    (gimp-context-set-sample-threshold-int 9)
    (gimp-image-select-color img 0 focus-peaking '(0 0 0))
    (gimp-edit-clear focus-peaking)
    (gimp-context-set-foreground '(0 255 0))
    (gimp-selection-invert img )
    (gimp-layer-set-lock-alpha focus-peaking TRUE)
    (gimp-edit-fill focus-peaking 0)
    (gimp-context-set-sample-threshold-int 0)
    (gimp-layer-set-lock-alpha focus-peaking FALSE)
    
    ;do unrecoverable highlights (red)  
    (gimp-image-select-color img 2 item '(255 255 255))
    (if (equal? (car (gimp-selection-is-empty img)) FALSE)
        (begin
            (gimp-context-set-foreground '(255 0 0))
            (gimp-edit-fill focus-peaking 0) 
        )
        ()
     )
 
    ;do unrecoverable shadows (blue)  
    (gimp-image-select-color img 2 item '(0 0 0))
    (if (equal? (car (gimp-selection-is-empty img)) FALSE)
        (begin
            (gimp-context-set-foreground '( 0 0 255))
            (gimp-edit-fill focus-peaking 0)
        )
        ()
    )

    ;check for transparent areas
    (gimp-selection-load chan)
    (if (equal? (car(gimp-selection-is-empty img)) FALSE)
        (begin
            (gimp-context-set-pattern "Warning!")
            (gimp-edit-fill focus-peaking 4)
        )
        ()
    )
    (gimp-selection-none img)
    (gimp-image-remove-channel img chan)
    

 	; flush the image, end undo-group and pop context
   	(gimp-displays-flush)
   	(gimp-image-undo-group-end img)
   	(gimp-context-pop)
   )
 )

(script-fu-register "script-fu-image-check"
            _"<Image>/Tools/Image Check"
            "Creates a new transparent layer above the active layer, highlighting parts of the active layer whith are in perfect focus, unrecoverable under,- or over-exposed or lacking any information"
            "Hans Schopmeijer"
            "GNU General Public License"
            "26-12-2013"
            "RGB*"
            SF-IMAGE    	"Input Image"   						0
            SF-DRAWABLE 	"Drawable"                              0
            SF-TOGGLE       "Use merged layers below active layer"  FALSE
)
            

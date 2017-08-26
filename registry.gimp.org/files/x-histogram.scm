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
;   Plugin      : X Histogram
;   Author      : Hans Schopmeijer
;   Date        : 1-2014
;   Version     : 1.0
;   Required    : Gimp 2.8
;   Location    : Tools/X Histogram
;   
;	
;	This plugin creates a transparent layer on top of the active layer with a
;   wavelike representation of the histogram.
;   This idea is directly taken from the excellent Darktable raw-processor
;
;	Input : - an image on the active layer
;
;	Output: - a transparent layer directly above the active layer with the X-histogram
;                         
;
;	Enjoy!
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
   
(define (script-fu-x-histogram img drawa ) 
 
   (let* (
		(width (car(gimp-image-width img)))
		(height (car(gimp-image-height img)))
		(x-hist (car(gimp-layer-new img width height RGBA-IMAGE "X-Histogram" 100 NORMAL-MODE)))
		(radius 0)
		(brush 0)
        (up-leftx 0)
        (segments 0)
        (rect-width 0)
        (end 0)
        (cntr 0)
        (mean 0)
        (ypos 0)
        (item (car(gimp-image-get-active-layer img)))
        (red   0)
        (green 0)
        (blue  0)
         )

	;start undo-group and save context
	(gimp-image-undo-group-start img)
	(gimp-context-push)

    ;limit nr segments to 200 and adjust brush-size to image-resolution
	(if (< width 200)
	    (begin
            (set! segments (round(/ width 2)))
            (set! radius 1)
        )
        (begin
            (set! segments 200)
            (set! radius (round (/ width 1000)))
        )
	)
	
	(set! rect-width (/ width segments))
	(set! end (* segments 2))
	(set! red   (cons-array end 'double))
    (set! green (cons-array end 'double))
    (set! blue  (cons-array end 'double))

    ;generate new brush to make scaling possible
    (gimp-brush-new "temp")
    (gimp-brush-set-shape "temp" BRUSH-GENERATED-CIRCLE )
    (gimp-brush-set-radius "temp" radius)
    (gimp-brush-set-hardness "temp" 1)
        
    ; create transparent layer above the active layer
    (gimp-image-insert-layer img x-hist 0 (car(gimp-image-get-item-position img item)))
    (gimp-context-set-foreground `(25 25 25))
    (gimp-edit-fill x-hist 0)
    (gimp-layer-set-opacity x-hist 80)
    (gimp-context-set-brush "temp")
    
    ;calculate points      
    (while (< cntr end)
        (set! up-leftx (round (* rect-width (+ 0.5 (/ cntr 2)))))
        (aset red cntr up-leftx)
        (aset green cntr up-leftx)
        (aset blue cntr up-leftx)  
        (gimp-image-select-rectangle img 2 up-leftx 0 rect-width height)

        (set! cntr (+ cntr 1))
 
        ;do red 
        (set! mean (car(gimp-histogram item 1 0 255)))
        (set! ypos (round(- height ( * (/ height 255) mean))))
        (aset red cntr ypos)
        
        ;do green
        (set! mean (car(gimp-histogram item 2 0 255)))
        (set! ypos (round(- height ( * (/ height 255) mean))))
        (aset green cntr ypos)
        
        ;do blue
        (set! mean (car(gimp-histogram item 3 0 255)))  
        (set! ypos (round(- height ( * (/ height 255) mean))))
        (aset blue cntr ypos)
                
        (set! cntr (+ cntr 1))
    )
    (gimp-selection-none img)

    ;draw graphs
    (gimp-context-set-foreground `(255 0 0))
    (gimp-paintbrush x-hist 0 end red PAINT-CONSTANT 0)
    (gimp-context-set-foreground `(0 255 0))
    (gimp-paintbrush x-hist 0 end green PAINT-CONSTANT 0)
    (gimp-context-set-foreground `(0 0 255))
    (gimp-paintbrush x-hist 0 end blue PAINT-CONSTANT 0)
         
 	; flush the image, end undo-group, pop context and delete brush
 	(gimp-brush-delete "temp")
   	(gimp-displays-flush)
   	(gimp-image-undo-group-end img)
   	(gimp-context-pop)
   )
 )

(script-fu-register "script-fu-x-histogram"
            _"<Image>/Tools/X-Histogram"
            "Creates a new transparent layer above the active layer, with wavelike 
            representation of the histogram"
            "Hans Schopmeijer"
            "GNU General Public License"
            "1-2014"
            "RGB* GRAY*"
            SF-IMAGE    	"Input Image"   						0
            SF-DRAWABLE 	"Drawable"                              0
                                   
)
            

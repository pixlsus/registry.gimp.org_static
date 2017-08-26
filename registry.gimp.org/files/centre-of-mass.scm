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
;   Plugin      : Centre of Mass
;   Author      : Hans Schopmeijer
;   Date        : 1-14
;   Version     : 1.0
;   Required    : Gimp 2.8
;   Location    : Select/Centre of Mass
;   Modes       : all
;	
;	This script will place 2 guides denoting the centre of mass of a selection. 
;   The selection can consist of several unconnected pixelregions.
;
;	Input : - a selection 
;
;	Output: - 2 guides denoting the centre of mass of the selection.
;             The coordinates of the centre of mass can be found in the Error Console.
;             If the script is called from another script, the output will be a list
;             of the coordinates, (x y)
;
;	Enjoy!
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
	
(define (script-fu-centre-of-mass img drawa)  
   
    ; this procedure from Hevan53 displays text in a popup box and quits the program
    (define (gimp-message-and-quit message)
        (let (  
            ;get your current handler
    	    (old-handler (car (gimp-message-get-handler))) 
    	)
    	(gimp-message-set-handler MESSAGE-BOX)
    	(gimp-message message)
        ;reset handler
    	(gimp-message-set-handler old-handler)
    	(quit)
    ))

    ;find selection that results in 50% counted pixels using binairy search
    ;before use: there is a layer whith a black background and the selection filled with white    
    ;input  : img, layer, coorlist bounding box, horizontal/vertical (TRUE/FALSE)
    ;output : x/y value from horizontal/vertical centre of mass
	(define (find-centre-mass img layer coorlist horizontal?)
		(let*(  
			    (halfpix 0)
			    ;get boundaries of the selection
                (x1 (car coorlist))
                (y1 (cadr coorlist))
                (x2 (caddr coorlist))
                (y2 (cadddr coorlist))
                (border 0)
			    (step 0)
			    (nr-wpix 0)
			    (lastpix 0)
			 )
			 ;get total number of white pixels in selectionbox
			 (gimp-image-select-rectangle img 2 x1 y1 (- x2 x1) (- y2 y1))
			 
			 (set! halfpix (/ (caddr (cddr (gimp-histogram layer 0 128 255))) 2))
			 (if (= horizontal? TRUE )
                (set! step (/ (- x2 x1) 2)) 
                (set! step (/ (- y2 y1) 2))  
			 )
			 (set! border step)
			 (while (> step 1)
			    ;modify selection
				(if (= horizontal? TRUE )
				    (gimp-image-select-rectangle img 2 x1 y1 border (- y2 y1))
				    (gimp-image-select-rectangle img 2 x1 y1 (- x2 x1) border)
				)
				(set! step ( / step 2))
				
				;count number of white pixels in selection
			    (set! nr-wpix (caddr (cddr (gimp-histogram layer 0 128 255))))
			    (if (> nr-wpix halfpix)
					(set! border (- border step))
					(set! border (+ border step))
				)
			 )
           
            ;refine outcome
			(if (= horizontal? TRUE )
			    (begin
			        (if (> nr-wpix halfpix) 
                        (gimp-image-select-rectangle img 2 x1 y1 (- border 1) (- y2 y1))
                        (gimp-image-select-rectangle img 2 x1 y1 (+ border 1) (- y2 y1))
                    )   
                ) 
                (begin
			        (if (> nr-wpix halfpix)
                        (gimp-image-select-rectangle img 2 x1 y1 (- x2 x1) (- border 1))
                        (gimp-image-select-rectangle img 2 x1 y1 (- x2 x1) (+ border 1))
                    )
                )   
			 )
			 (set! lastpix (caddr (cddr (gimp-histogram layer 0 128 255))))
			 (if (= horizontal? TRUE )
                (+ (floor border) x1 (/(- halfpix nr-wpix )(abs(- lastpix nr-wpix))))
                (+ (floor border) y1 (/(- halfpix nr-wpix )(abs(- lastpix nr-wpix))))
			 )
		)
	)

 (let* (
        (working 0)
        (data 0)
        (chan 0)
        (x1 0)
        (x2 0)
        (y1 0)
        (y2 0)
        (borderx 0)
        (bordery 0)
       	 )
    
    ;check if there is a selection, if not, quit
	(if (equal? TRUE (car (gimp-selection-is-empty img)))
		(gimp-message-and-quit "There is no selection! \nPlease make a selection."))

	;start undo-group and save context
	(gimp-image-undo-group-start img)
	(gimp-context-push)

	;save selection, create new layer with black background
    (set! chan (car(gimp-selection-save img)))
    (gimp-selection-none img)
	(set! working (car (gimp-layer-copy drawa 1)))
    (gimp-image-add-layer img working -1)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-context-set-background '(0 0 0))
    (gimp-edit-fill working 1)

    ;retrieve selection and fill selection on black layer with white
    (gimp-image-select-item img 2 chan)
    (gimp-edit-fill working 0)
    
    ;get boundaries of the selection
    (set! data (cdr(gimp-selection-bounds img)))

    ;find centre of mass    
    (set! borderx (find-centre-mass img working data TRUE))
    (gimp-image-add-vguide img (round borderx))
    (set! bordery (find-centre-mass img working data FALSE))
    (gimp-image-add-hguide img (round bordery))
   
    ;cleanup and restore selection
    (gimp-selection-none img)
    (gimp-image-remove-layer img working)
    (gimp-image-select-item img 2 chan)
    (gimp-image-remove-channel img chan)
    
 	; flush the image, end undo-group, pop context and display coordinates in Error Console
   	(gimp-displays-flush)
   	(gimp-image-undo-group-end img)
   	(gimp-context-pop)
   	(gimp-message (strcat "Centre of Mass X : " (number->string borderx) "\nCentre of Mass Y : " (number->string bordery) ))
   	(list borderx bordery)
   )
 )

(script-fu-register "script-fu-centre-of-mass"
            "<Image>/Select/Centre of Mass"
            "This script will place 2 guides denoting the centre of mass of a selection."
            "Hans Schopmeijer"
            "GNU General Public License"
            "1-2014"
            ""
            SF-IMAGE    	"Input Image"   						0
            SF-DRAWABLE 	"Drawable"                              0             
)
            

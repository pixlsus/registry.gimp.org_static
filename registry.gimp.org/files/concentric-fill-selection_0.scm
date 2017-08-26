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
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Plugin      : concentric-fill-selection 
;   Author      : Hans Schopmeijer
;   Date        : 1-4-2013
;   Version     : 1.0
;   Required    : Gimp 2.8
;   Tested on   : Windows Vista
;                 Ubuntu
;   Location    : Select/Concentric-fill-selection
;
;
;   This plugin fills all the selected parts of an image from the outside in with 2 colors
;   or 1 color and a transparent band. The filled selection will be placed on a transparent
;   layer above the active layer.
;   The number of bands is user defined in the number of interations, if however the 
;   selections is totaly filled, the proces will stop before the number of iterations
;   being given is reached.
;   To totaly fill a selection, choose a very high number of iterations. 
;   The width of each band can be increased or decreased by a given amount of pixels in 
;   every iteration. It is also possible to feather one or both colors.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;   
;
(define (script-fu-concentric-fill-selection img draw fgc fgw fgwc fgf transparent? bgc bgw bgwc bgf times)
  ; this procedure  displays text in a popup box and quits the program
  (define (gimp-message-and-quit message)
    (let  
    ;get current handler
	((old-handler (car (gimp-message-get-handler))) )
 	(gimp-message-set-handler MESSAGE-BOX)
 	(gimp-message message)
	;reset handler
 	(gimp-message-set-handler old-handler)
 	(quit)
  ))

  (let* (
	 (y 0)
	 (item 0)
	 (pos 0)
	 (width (car(gimp-image-width img)))
	 (height (car(gimp-image-height img)))
	 (new-layer (car(gimp-layer-new img width height RGBA-IMAGE "striped-selection" 100 NORMAL-MODE)))
	 (channela 0)
	 (channelb 0)
	)

	; check if there is a selection, else quit
	
	(if  ( = TRUE (car(gimp-selection-is-empty img)))
	        (gimp-message-and-quit "There is no selection! \n Please make a selection before using this script.")
	)
   	; save context, start undo-group and create a new transparent layer
	(gimp-image-undo-group-start img)
	(gimp-context-push)
	(set! item (car(gimp-image-get-active-layer img)))
    (set! pos (car(gimp-image-get-item-position img item)))
	(gimp-image-insert-layer img new-layer 0 pos)
	(gimp-layer-set-name new-layer "filled selection")
	(gimp-layer-add-alpha new-layer)
	(gimp-edit-clear new-layer )
	; set new colors
	(gimp-context-set-foreground fgc)
	(gimp-context-set-background bgc)

	; fill selection for each color until the size of the selection < 0
	; or the all the iterations are done.
	(set! times (abs times))
	(set! fgf (abs fgf))
	(set! bgf (abs bgf))
	(set! y 0)
    (while (< y times)
		  ( if ( and (= FALSE (car(gimp-selection-is-empty img))) (> fgw 0))
		   (begin
		     (gimp-selection-feather img fgf)
		     (set! channela (car(gimp-selection-save img)))
			 (gimp-selection-shrink img fgw)
			 (set! channelb (car(gimp-selection-save img)))
			 (gimp-selection-load channela)
		     (gimp-image-remove-channel img channela)			 
			 (gimp-selection-feather img 1)
			 (gimp-image-select-item img CHANNEL-OP-SUBTRACT channelb)
	 	     (gimp-edit-bucket-fill-full new-layer 0 0 100.0 128.0 FALSE TRUE 0 0.0 0.0)
			 (gimp-selection-load channelb)	 	     
		     (gimp-image-remove-channel img channelb)
		   )
		   (set! y times)
	     )
         ( if ( and (= FALSE (car(gimp-selection-is-empty img))) (> bgw 0))
	 	   (begin
		     (gimp-selection-feather img bgf)
		     (set! channela (car(gimp-selection-save img)))
		     (gimp-selection-shrink img bgw)
			 (set! channelb (car(gimp-selection-save img)))
			 (gimp-selection-load channela)
		     (gimp-image-remove-channel img channela)	
			 (gimp-selection-feather img 1)
			 (gimp-image-select-item img CHANNEL-OP-SUBTRACT channelb)
	 	     (gimp-edit-bucket-fill-full new-layer 1 0 (* (- 1 transparent?) 100.0) 128.0 FALSE TRUE 0 0.0 0.0)
	 	     (gimp-selection-load channelb)
		     (gimp-image-remove-channel img channelb) 
		   )
		   (set! y times)
         )
         (set! y (+ y 1))
         (set! fgw (+ fgw fgwc))
         (set! bgw (+ bgw bgwc))
   )

; flush the image, end undo-group and pop context
   	(gimp-displays-flush)
   	(gimp-image-undo-group-end img)
   	(gimp-context-pop)
   )
)

(script-fu-register "script-fu-concentric-fill-selection"
            _"<Image>/Select/Concentric-fill-selection..."
            "fills a selection with dual colored outlines from the outside in"
            "Hans Schopmeijer"
            "GNU General Public License"
            "7-3-2013"
            "RGB* GRAY*"
            SF-IMAGE    "Input Image"     				  		0
            SF-DRAWABLE "Input Drawable" 				  		0
            SF-COLOR    "First color" 					  		'(219 16 40)
            SF-VALUE    "Line width first color (pix)"    		"10"
            SF-VALUE	"Linewidth change per iteraton (pix)" 	"0"
		    SF-VALUE    "Feather first line (pix)"	      		"0"
            SF-TOGGLE	"Second color transparant"		   		FALSE
            SF-COLOR    "Second color"                    		'(61 219 16)
		    SF-VALUE	"Line width second color"         		"10"
            SF-VALUE	"Linewidth change per iteraton (pix)" 	"0"
		    SF-VALUE    "Feather second line (pix)"       		"0"
		    SF-VALUE	"Iterations"	  				  		"5"
            )


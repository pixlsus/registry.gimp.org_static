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
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
;   Plugin      : ff-semi-auto-bokeh 
;   Author      : Hans Schopmeijer
;   Date        : 13-4-2013
;   Version     : 1.0
;   Required    : Gimp 2.8
;   Location    : Filters/Bokeh/free-form-semi-auto-bokeh
;   
;	
;	This plugin creates a Bokeh-effect from points highlighted with the path-tool.
;	Only the position of the nodes making the path are important, not the path between points!
;	The path used should be at the top of the pathstack.
;	The path may contain multiple strokes.
;	The nodes in the path should be placed directly on the lightsources in the image.
;	The plugin will use the color of the image under these points to color the Bokeh-effect.
;	The size and the shape of the bokeh-effect will be taken from a selection the user must
;	make. This selection can have any form/size and can be placed anywhere on the image.
;   I left the shape and size of the bokeh-effect selectable to make it possible to 
;   simulate for instance a 8-side diaframa instead of the circular one.
;
;   The lightness and saturtion will be the same for all bokehs in the image, so if you
;   would like to produce different effects for different lightsources (for instance
;   lightsources in the distant giving blurrier bokehs), than use the plugin multiple
;   times, choosing different lightsources in separate layers. These can than be separately
;   manipulated.
;
;	Input : - an image on the active layer
;			- a path on top pathlayer
;			- a selection from which the size and shape are taken to make the bokeh-effect
 
;	Output: - a transparent layer with the bokeh-effect above the active layer in screen-mode
;			- beneath that the original image blurred by user defined amount (optional)
;			- beneath that the original layer
;			- the path on top off the pathstack
;           - the original selection
;
;	The Bokehlayer is not merged with the blurred image to allow further manipulation of the
;   bokeh-layer such as blurring, masking and changing opacity/color/brightness.
;	The path is kept in the pathstack and the selection is left intact to enable a repeat of
;   the plugin with different parameters or paths.
;
;   Finaly I would like to thank all those people who upload their scripts, freely for
;   everyone to read, for their contribution, without them I would be at a loss.
;
;	Enjoy!
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
	
(define (script-fu-ff-semi-auto-bokeh img drawa ringwidth lightness saturation blurlayer blur)  
  
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

   ; start of the main program

   (let* (
   		(data 0)
   		(sel-centre-x 0)
   		(sel-centre-y 0)
   		(dia 0)
		(item (car(gimp-image-get-active-layer img)))
		(pos 0)
		(width (car(gimp-image-width img)))
		(height (car(gimp-image-height img)))
		(bokeh-layer (car(gimp-layer-new img width height RGBA-IMAGE "bokeh" 100 NORMAL-MODE)))
		(blurred 0)
		(vectors (car(gimp-image-get-active-vectors img)))
		(scale (round (/ (sqrt (+ (* width width)(* height height))) 400)))
		(half-scale (round ( / scale 2)))
		(strokes 0)
		(stroke-id 0)
		(points 0)
		(strnr 0)
		(path-drawn FALSE)
		(color 0)
		(x 0 )
		(y 0 )
		(xcp 0)
		(ycp 0)
		(i 0)
		(channela 0)
		(channelb 0)
		(channelc 0)
		 )
	

	;check if there is a selection and if there is a path, if not, quit
	(if (equal? TRUE (car (gimp-selection-is-empty img)))
		(gimp-message-and-quit "There is no selection! \nPlease make a selection which size and shape will act as a template for the bokeh-effect."))
	(if (= vectors -1)
	    (gimp-message-and-quit "There are no paths in the image! \nPlease highlight the lightsources in the image with the nodes from the path-tool."))

	;start undo-group and save context
	(gimp-image-undo-group-start img)
	(gimp-context-push)

	;get position of the selection
	(set! data (cdr(gimp-selection-bounds img)))
	(set! sel-centre-x (round (/ (+ (caddr data)  (car data))  2)))
	(set! sel-centre-y (round (/ (+ (cadddr data) (cadr data)) 2)))

	;get largest dimension of the selection
	(set! dia (round (max (- (caddr data)  (car data)) (- (cadddr data) (cadr data)))))

	;save bokehshape
	(set! channelc (car(gimp-selection-save img)))
	(gimp-selection-none img)
 
	;create new transparent layer
    (set! pos (car(gimp-image-get-item-position img item)))
	(gimp-image-insert-layer img bokeh-layer 0 pos)
	(gimp-layer-set-name bokeh-layer (strcat "Bokeh : " (number->string (round ringwidth)) " "
														(number->string (round lightness)) " "
														(number->string (round saturation))
														))
	(gimp-layer-add-alpha bokeh-layer)
	(gimp-edit-clear bokeh-layer )

	;create a ringselection and save in a channel
	(gimp-image-select-item img CHANNEL-OP-REPLACE channelc)
  	(gimp-selection-feather img ringwidth)
	(set! channela (car(gimp-selection-save img)))
	(gimp-selection-shrink img ringwidth)
	(gimp-selection-feather img (* 2 ringwidth))
	(set! channelb (car(gimp-selection-save img)))
	(gimp-image-select-item img CHANNEL-OP-REPLACE channela)
	(gimp-image-remove-channel img channela)			 
	(gimp-selection-feather img ringwidth)
	(gimp-image-select-item img CHANNEL-OP-SUBTRACT channelb)
	(gimp-image-remove-channel img channelb)
	(set! channela (car(gimp-selection-save img)))	
	(gimp-selection-none img)

	;get the strokes
	(set! strokes (gimp-vectors-get-strokes vectors)) 
	;do for all strokes
	(set! strnr 0)
	(while (< strnr (car strokes))
		(set! stroke-id (aref (cadr strokes) strnr))
		(set! points (gimp-vectors-stroke-get-points vectors stroke-id))
		(if (< (cadr points) 3)
	    	()
	    	(begin
				(set! i 2)
				(while (< i (cadr points))
					(set! x ( aref (caddr points) i))
					(set! y ( aref (caddr points) (+ i 1)))
					;ensure node falls within the image
					(if (and (> x 0) (< x width) (> y 0) (< y height))
						(begin
							(set! path-drawn TRUE)
				            ;get color and ensure sample falls totaly within the image 
				            (set! xcp (max (- x half-scale) half-scale))
				            (set! xcp (min x (- width half-scale)))
                            (set! ycp (max (- y half-scale) half-scale))
				            (set! ycp (min y (- height half-scale)))
				            ;samle-area dependent of resolution	
				            (set! color (car (gimp-image-pick-color img drawa x y FALSE TRUE scale) )) 
  							;create white ring
  							(gimp-image-select-item img CHANNEL-OP-REPLACE channela)
  							(gimp-selection-translate img (- x sel-centre-x) (- y sel-centre-y))
			 				(gimp-context-set-foreground `(255 255 255))
			 				(gimp-edit-bucket-fill-full bokeh-layer 0 0 20.0 128.0 FALSE TRUE 0 0.0 0.0)
			 				;fill circel with sampled color 
							(gimp-context-set-foreground color)
  							(gimp-image-select-item img CHANNEL-OP-REPLACE channelc)
  							(gimp-selection-translate img (- x sel-centre-x) (- y sel-centre-y))
  							(gimp-edit-blend bokeh-layer 2 0 2 100 0 0 FALSE FALSE 1 0 TRUE x y (+ x dia) (+ y dia))
  							(gimp-selection-none img)
  						)
  						()
  					)
					(set! i (+ i 6))
				)
			)
		)
		(set! strnr (+ strnr 1))
	)

	(if (equal? path-drawn FALSE)
	    (gimp-message-and-quit "No bokeh-effect is produced!\nThere are no nodes within the image-boundaries"))
		
    ; Copy imagelayer and blur it, put it beneath the bokeh-layer 
	(if (equal? blurlayer TRUE)
		(begin
    		(set! blurred (car (gimp-layer-copy drawa 1)))
    		(gimp-image-insert-layer img blurred 0 (+ pos 1))
    		(gimp-layer-set-name blurred "Blurred Original")
			(plug-in-gauss-iir2 1 img blurred blur blur)
		)
		()
	)
	; set bokeh-layer mode to screen, increase saturation and remove channels 
	(gimp-layer-set-mode bokeh-layer 4)
	(gimp-hue-saturation bokeh-layer 0 0 lightness saturation)
	(gimp-image-select-item img CHANNEL-OP-REPLACE channelc)
	(gimp-image-remove-channel img channelc) ; first c!
	(gimp-image-remove-channel img channela)

	; flush the image, end undo-group and pop context and set original layer as active
   	(gimp-displays-flush)
   	(gimp-image-undo-group-end img)
   	(gimp-context-pop)
   	(gimp-item-set-visible vectors FALSE)
   	(gimp-image-set-active-layer img drawa)	
  )
 )

(script-fu-register "script-fu-ff-semi-auto-bokeh"
            _"<Image>/Filters/Bokeh/free-form-semi-auto-bokeh..."
            "Use the path tool to mark the lightsources in the image from which the bokeheffect will be made. The shape and the size of the bokeh-effect is taken from a selection the user must make before using this plugin."
            "Hans Schopmeijer"
            "GNU General Public License"
            "3-2013"
            "RGB* GRAY*"
            SF-IMAGE   		"Input Image"   							0
            SF-DRAWABLE 	"Drawable" 	   								0
            SF-ADJUSTMENT	"Bokeh-border thickness (pix)"				'(2 1 100 1 10 1)
            SF-ADJUSTMENT	"lightness correction bokehlayer"			'(50 -100 100 1 10 1)
			SF-ADJUSTMENT	"Saturation correction bokehlayer"			'(90 -100 100 1 10 1)
            SF-TOGGLE		"Insert a blurred copy from he image?" 		TRUE
            SF-ADJUSTMENT   "Blur-radius of the inserted layer"			'(100 1 200 1 10 1)
)
;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;
;   Plugin      : ff-auto-bokeh 
;   Author      : Hans Schopmeijer
;   Date        : 13-4-2013
;   Version     : 1.0
;   Required    : Gimp 2.8
;   Location    : Filters/Bokeh/free-form-auto-bokeh
;   
;
;	This plugin creates a Bokeh-effect from the brightest areas in the image.
;	These areas need to be approximately circular in shape.
;	The maximum size of the areas that will produce a Bokeh-effect is determind by the user.
;	The size and shape of the Bokeh-effect are determind by a selection the user must make 
;	before running this script. 
;	This selection can have any form/size and can be placed anywhere on the image.
;   I left the shape and size of the bokeh-effect selectable to make it possible to 
;   simulate for instance a 8-side diaframa instead of the circular one.
;
;	Input : an image on the active layer
;			a selection from which the size and shape are taken to make the bokeh-effect
 
;	Output: a transparent layer with the bokeh-effect above the active layer in screen-mode
;			beneath the that the original image blurred by user defined amount (optional)
;			beneath that the original layer
;
;	The Bokehlayer is not merged with the blurred image to allow further manipulation of the
;   bokeh-layer such as blurring, masking and changing opacity/color/brightness.
;
;   Finaly I would like to thank all those people who upload their scripts, freely for
;   everyone to read, for their contribution, without them I would be at a loss.
;
;   Enjoy!!
;			
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
	
(define (script-fu-ff-auto-bokeh img drawa sens sourcesize ringwidth lightness saturation blurlayer blur) 

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
    
	; Copy imagelayer and extract high intensity parts to a path.
	; First the contrast is stretched to the full width (0-255)
	; then the gamma is set to 0.3 to bring out the highlights
	; after threshold ,dimensionspecific blurring and threshold again
	; the white areas are turned into a path
	(define (brightest-to-path img layer lowthr1 lowthr2 scale)
    	(let* ( 
				(brightest (car (gimp-layer-copy layer 1)))
    		  )
    		(gimp-image-insert-layer img brightest 0 1)
    		(gimp-levels-stretch brightest)
    		(gimp-levels brightest 0 0 255 0.3 0 255)
   			(gimp-threshold brightest lowthr1 255)
			(plug-in-gauss-iir2 1 img brightest scale scale)  	
   			(gimp-threshold brightest lowthr2 255)
   			(gimp-image-select-color img 0 brightest `( 255 255 255)) 
   			(plug-in-sel2path 0 img brightest)
   			(gimp-image-remove-layer img brightest)
   			(gimp-selection-none img)
   		)	
	)
    
	; find threshold-level that results in (percentage) brightest pixels using binairy search
	(define (find-threshold layer percentage)
		(let* (
			 (thrhold 128)
			 (step 64)
			 (value 0)
			  )
			  (while (> step 1)
			  	;get percentage of pixels between thrhold and 255
				(set! value (car (cddr (cdddr (gimp-histogram layer 0 thrhold 255)))))
				(if (> value percentage)
					(set! thrhold (+ thrhold step))
					(set! thrhold (- thrhold step))
				)
				(set! step ( / step 2))
			  )
			  thrhold		  		
		)
	)
 
   ; start of the main program
   (let* (
   		(percentage 0.02)
   		(hist-low 0)
   		(data 0)
   		(sel-centre-x 0)
   		(sel-centre-y 0)
   		(dia 0)  
		(item (car(gimp-image-get-active-layer img)))
		(pos 0)
		(width (car(gimp-image-width img)))
		(height (car(gimp-image-height img)))
		(bokeh-layer (car(gimp-layer-new img width height RGBA-IMAGE "bokeh" 100 NORMAL-MODE)))
		(scale (round (/ (sqrt (+ (* width width)(* height height))) 400)))
		(half-scale (round (/ scale 2)))
		(brightest 0)
		(blurred 0)
		(pnts 0)
		(vectors 0)
		(strokes 0)
		(stroke-id 0)
		(points 0)
		(xcoor `())
		(ycoor `())
		(dx 0)
		(dy 0)
		(maxdim 0)
		(half-maxdim 0)
		(roundness 0)
		(strnr 0)
		(color 0)
		(op 0)
		(x 0 )
		(y 0 )
		(xcp 0)
		(ycp 0)
		(i 0)
		(channela 0)
		(channelb 0)
		(channelc 0)
	 )

	;check if there is a selection as blueprint for the bokeh-effect, if not quit
	(if (equal? TRUE (car (gimp-selection-is-empty img)))
		(gimp-message-and-quit "There is no selection! \nPlease make a selection which size and shape will act as a template for the bokeh-effect."))
		
	; save context, start undo-group
	(gimp-image-undo-group-start img)
	(gimp-context-push)

	;get position of the selection
	(set! data (cdr(gimp-selection-bounds img)))
	(set! sel-centre-x (round (/ (+ (caddr data)  (car data))  2)))
	(set! sel-centre-y (round (/ (+ (cadddr data) (cadr data)) 2)))
	;get largest dimension of the selection
	(set! dia (round (max (- (caddr data)  (car data)) (- (cadddr data) (cadr data)))))
	
	;save bokehshape
	(set! channelc (car(gimp-selection-save img)))
	(gimp-selection-none img)

	;Find histogram value so that (percentage) brightest pixels lie above that value
	(set! hist-low (find-threshold drawa percentage))
	
	;Extract high intensity parts to a path 
	(brightest-to-path img drawa hist-low (round (- 255 (* 2.54 sens))) scale)
	 
	; create new transparent layer for the bokeh-effect
    (set! pos (car(gimp-image-get-item-position img item)))
	(gimp-image-insert-layer img bokeh-layer 0 pos)
	(gimp-layer-set-name bokeh-layer (strcat "Bokeh : " 
											(number->string (round sens)) " "
											(number->string (round sourcesize)) " "
											(number->string (round ringwidth)) " "
											(number->string (round lightness)) " "
											(number->string (round saturation))
											))
	(gimp-layer-add-alpha bokeh-layer)
	(gimp-edit-clear bokeh-layer )

	;create a ringselection and save it in a channel
	(gimp-image-select-item img CHANNEL-OP-REPLACE channelc)
  	(gimp-selection-feather img ringwidth)
	(set! channela (car(gimp-selection-save img)))
	(gimp-selection-shrink img ringwidth)
	(gimp-selection-feather img (* 2 ringwidth))
	(set! channelb (car(gimp-selection-save img)))
	(gimp-image-select-item img CHANNEL-OP-REPLACE channela)
	(gimp-image-remove-channel img channela)			 
	(gimp-selection-feather img ringwidth)
	(gimp-image-select-item img CHANNEL-OP-SUBTRACT channelb)
	(gimp-image-remove-channel img channelb)
	(set! channela (car(gimp-selection-save img)))
	(gimp-selection-none img)

	; get the strokes
	(set! vectors (car(gimp-image-get-active-vectors img)))
	(if (= vectors -1)
	    (gimp-message-and-quit "There are no bright lightsources in the image! \nAre you shure you are on the correct layer?"))

	(set! strokes (gimp-vectors-get-strokes vectors)) 
	(set! strnr 0)
	(while (< strnr (car strokes))
		(set! stroke-id (aref (cadr strokes) strnr))
		(set! points (gimp-vectors-stroke-get-points vectors stroke-id))

		;points-vector to xcoor and ycoor list
		(set! xcoor `())
		(set! ycoor `())
		(set! i 2)
		(while (< i (cadr points))
			(set! xcoor (append xcoor (list( aref (caddr points) i))))
			(set! ycoor (append ycoor (list( aref (caddr points) (+ i 1)))))
			(set! i (+ i 6))
		)

		;determine the maximum size of the stroke
		(set! dx (-(apply max xcoor) (apply min xcoor) ))
		(set! dy (-(apply max ycoor) (apply min ycoor) ))
		(set! maxdim (max dx dy))
				
		;determine the roundness of the stroke
		(if (or (equal? dx 0.0) (equal? dy 0.0))
			(set! roundness 3)
			(begin
				(if (> dy dx)
			    	(set! roundness (/ dy dx))
					(set! roundness (/ dx dy))				
				)
			)
		)

		;make a bokeh if size of lightsource is smaller than sourcesize and the 
		;ratio between width and height of the lightsource is below 2.
		;These restrictions prefent big lighted areas and non-round lightsources
		;producing a bokeh.
		(if (and (< maxdim sourcesize) (< roundness 2))
	    	(begin
				;calculate centre of stroke
				(set! x (round (/ (apply + xcoor) (length xcoor))) )
				(set! y (round (/ (apply + ycoor) (length ycoor))) )
				(set! half-maxdim (round (/ maxdim 2)))
				;get color and ensure sample falls totaly within the image 
				(set! xcp (max (- x half-maxdim) half-maxdim))
				(set! xcp (min x (- width half-maxdim)))
                (set! ycp (max (- y half-maxdim) half-maxdim))
				(set! ycp (min y  (- height half-maxdim)))
				(set! color (car (gimp-image-pick-color img drawa xcp ycp FALSE TRUE maxdim ) ))
  				;create white ring
  				(gimp-image-select-item img CHANNEL-OP-REPLACE channela)
  				(gimp-selection-translate img (- x sel-centre-x) (- y sel-centre-y))
  				;make opacity of the ring depended on the size of the lightsource
  				(set! op (trunc (* maxdim (/ 10 sourcesize))))
			 	(gimp-context-set-foreground `(255 255 255))
			 	(gimp-edit-bucket-fill-full bokeh-layer 0 0 (+ 10 op) 128.0 FALSE TRUE 0 0.0 0.0)
			 	;fill circle with sampled color 
				(gimp-context-set-foreground color)
				(gimp-image-select-item img CHANNEL-OP-REPLACE channelc)
  				(gimp-selection-translate img (- x sel-centre-x) (- y sel-centre-y))
  				;make opacity of the bokeh depended on the size of the lightsource
 				(gimp-edit-blend bokeh-layer 2 0 2 (+ 50 (* op 5)) 0 0 FALSE FALSE 1 0 TRUE x y (+ x dia) (+ y dia))
  				(gimp-selection-none img)
			)
			()
		)
		(set! strnr (+ strnr 1))
	)

    ; Copy imagelayer and blur it, put it beneath the bokeh-layer 
    (if (equal? blurlayer TRUE)
		(begin
    		(set! blurred (car (gimp-layer-copy drawa 1)))
    		(gimp-image-insert-layer img blurred 0 (+ pos 1))
    		(gimp-layer-set-name blurred "Blurred Original")
			(plug-in-gauss-iir2 1 img blurred blur blur)
		)
		()
	)
    
	; set bokeh-layer mode to screen, modify bokeh-layer, delete channels and vectors 
	(gimp-layer-set-mode bokeh-layer 4)
	(gimp-hue-saturation bokeh-layer 0 0 lightness saturation)
	(gimp-image-remove-channel img channela)
	(gimp-image-remove-channel img channelc)
	(gimp-image-remove-vectors img vectors)

	; flush the image, end undo-group, pop context and set original layer as active
   	(gimp-displays-flush)
   	(gimp-image-undo-group-end img)
   	(gimp-context-pop)
    (gimp-image-set-active-layer img drawa)

  )
 )

(script-fu-register "script-fu-ff-auto-bokeh"
            _"<Image>/Filters/Bokeh/free-form-auto-bokeh..."
            "Creates a Bokeh-effect from available lightsources in the active layer. The shape and the size of the bokeh-effect is taken from a selection the user must make before using this plugin"
            "Hans Schopmeijer"
            "GNU General Public License"
            "3-2013"
            "RGB* GRAY*"
            SF-IMAGE    	"Input Image"   						0
            SF-DRAWABLE 	"Drawable" 	   							0
            SF-ADJUSTMENT	"Sensitivity (higher is more sensitive)"'(70 0 100 1 10 1)
            SF-ADJUSTMENT   "Maximum lightsource dimension (pix)"	'(80 3 1000 1 10 1)
            SF-ADJUSTMENT	"Bokeh-border thickness (pix)"			'(2 1 100 1 10 1)
            SF-ADJUSTMENT	"lightness correction bokehlayer"		'(50 -100 100 1 10 1)
			SF-ADJUSTMENT	"Saturation correction bokehlayer"		'(90 -100 100 1 10 1)
            SF-TOGGLE		"Insert a blurred copy from he image?" 	TRUE
            SF-ADJUSTMENT   "Blur-radius of the inserted layer"		'(50 1 200 1 10 1)
)



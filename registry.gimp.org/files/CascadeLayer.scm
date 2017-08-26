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
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Purpose:
; 1) Creates a border using the current layer.
; 2) Positions and cascades a layer relative to the image.
; (i.e. Top Left to Bottom Right, Left to Right, etc.)
; You can specify how many times to cascade the layer, with the default being 0, which
; means fill the image in the specified direction. If you use 1, it will position the
; layer to the selected starting position. You can also specify the x and y spacing for
; cascading, and the initial x and y position.
; 3) Create a checkerboard pattern from the layer.
; 4) Create a tessellation pattern from the layer.
; 5) Fill with the layer.
;
; Problems:
; 1) I have to hardcode the list values because I don't know
; how to make this language use the define values and not the define
; variable names themselves. The help I've found so far hasn't
; covered this topic. And the language doesn't pass values by reference,
; so I can't set variables inside functions.
;
; Notes:
;
; Use equal? instead of = for string compares.
;
; "All gimp-functions return a list, and even if the list contains
; only one element it must be accessed by car"
;
; "The car function is used to return the head of the list and the
; cdr (usually pronounced cudder) is used to get the tail of the list."
;
; if statement format:
;
;   (if condition
;      do_this_if_true
;      do_this_if_false
;   )
;
; example:
;
;   (if (< x 0)
;      (begin
;        (print "X is negative")
;        (set! sign -1)
;      ) ; end begin condition is true
;      (begin
;        (print "X is positive")
;        (set! sign 1)
;      ) ; end begin condition is false
;   ) ; end if
;
; SF-ADJUSTMENT "label" '(value lower upper step_inc page_inc digits type)
;	 digits: Digits after the point (decimal part).
;   type:   SF-SLIDER or 0, SF-SPINNER or 1
;
; Good tutorial info:
; http://www.seul.org/~grumbel/gimp/script-fu/script-fu-tut.html
; http://www.cs.indiana.edu/scheme-repository/imp/siod.html
; http://www.scheme.com/tspl2d/index.html
;
; Revision History:
; Rel 01.00 posted to http://registry.gimp.org on 2011 04 03
; Rel 01.01 Changed RGBA to RGB*
; Rel 01.02 Changed RGB* to RGB*,GRAY*
;
; ===========================================================================

; Constants:

; Math constants:
(define pi 3.1415926535897932384626433832795)

; Constants for layer position:
(define positionTopLeft 0)
(define positionTopRight 1)
(define positionCenter 2)
(define positionBottomLeft 3)
(define positionBottomRight 4)
(define positionCurrentPosition 5)

; Constants for offset direction:
(define offsetDirLtoR 0)   ; _"Left to Right" )
(define offsetDirTLtoBR 1) ; _"Top Left to Bottom Right"
(define offsetDirBLtoTR 2) ; _"Bottom Left to Top Right"
(define offsetDirTtoB 3)   ; _"Top to Bottom"
(define offsetDirRtoL 4)   ; _"Right to Left"
(define offsetDirTRtoBL 5) ; _"Top Right to Bottom Left"
(define offsetDirBRtoTL 6) ; _"Bottom Right to Top Left"
(define offsetDirBtoT 7)   ; _"Bottom to Top"
(define offsetDirChkr 8)   ; _"Checkerboard"
(define offsetDirTess 9)   ; _"Tessellation"
(define offsetDirFill 10)  ; _"Fill"


; ===========================================================================
; Function: getPosition
; Problem: The value is not passed by reference, so it's not returned.
; ===========================================================================
(define (getPosition
								valueIn
								valueOut
                    		)
   (let*(
  		)

		(if (equal? valueIn _"Top Left") 			 (set! valueOut positionTopLeft) )
   	(if (equal? valueIn _"Top Right")			 (set! valueOut positionTopRight) )
   	(if (equal? valueIn _"Center")				 (set! valueOut positionCenter) )
   	(if (equal? valueIn _"Bottom Left")			 (set! valueOut positionBottomLeft) )
   	(if (equal? valueIn _"Bottom Right") 		 (set! valueOut positionBottomRight) )
   	(if (equal? valueIn _"Current Position" )  (set! valueOut positionCurrentPosition) )

   ) ; end of let

) ; end define getPosition

; ===========================================================================
; Function: getDirection
; Problem: The value is not passed by reference, so it's not returned.
; ===========================================================================
(define (getDirection
								valueIn
								valueOut
                    		)
   (let*(
  		)

		(if (equal? valueIn _"Left to Right") 				  (set! valueOut offsetDirLtoR) )
   	(if (equal? valueIn _"Top Left to Bottom Right")  (set! valueOut offsetDirTLtoBR) )
   	(if (equal? valueIn _"Bottom Left to Top Right")  (set! valueOut offsetDirBLtoTR) )
   	(if (equal? valueIn _"Top to Bottom")				  (set! valueOut offsetDirTtoB) )
   	(if (equal? valueIn _"Right to Left") 				  (set! valueOut offsetDirRtoL) )
   	(if (equal? valueIn _"Top Right to Bottom Left")  (set! valueOut offsetDirTRtoBL) )
   	(if (equal? valueIn _"Bottom Right to Top Left")  (set! valueOut offsetDirBRtoTL) )
   	(if (equal? valueIn _"Bottom to Top")  			  (set! valueOut offsetDirBtoT) )
   	(if (equal? valueIn _"Checkerboard")  				  (set! valueOut offsetDirChkr) )
   	(if (equal? valueIn _"Tessellation")  				  (set! valueOut offsetDirTess) )
   	(if (equal? valueIn _"Fill")  						  (set! valueOut offsetDirFill) )

   ) ; end of let

) ; end define getDirection


; ===========================================================================
; Function: PatternFillTheLayer
; ===========================================================================
(define (PatternFillTheLayer
								theImage
								layerCopy
								height
								width
								heightY
								widthX
								startX
								startY
								cascadeDirection
                    		flipHorizontal
                    		flipVertical
                    		)
   (let*(
      (offset-x 0)
      (offset-y 0)
      (row 0)
      (col 0)
      (maxRow 0)
      (maxCol 0)
      (incrX 0)
      (allDone FALSE)
      (needToPosition FALSE)
  		(textMessage ())
  		(enableDebug 0)
  		(displayCount 0)
  		(flipLayer FALSE)
  		(flip-type ORIENTATION-HORIZONTAL)
  		)

			; Initialize loop variables.
   		(set! offset-x startX)
   		(set! offset-y startY)
   		(set! row 0)
   		(set! col 0)
     		(set! maxRow (+ (/ height heightY) 1) )				; row    = y direction

   		; Set the maxCol, and X increment.
      	; Checkerboard
         (if (= cascadeDirection offsetDirChkr)
      		(begin
         		(set! maxCol (+ (/ (/ width widthX) 2) 1) )	; column = x direction
		   		(set! incrX (* widthX 2))
      		) ; end begin
      	) ; end if
      	; Tessellation
         (if (= cascadeDirection offsetDirTess)
      		(begin
         		(set! maxCol (+ (/ width widthX) 1) )			; column = x direction
		   		(set! incrX widthX)
      		) ; end begin
      	) ; end if
      	; Fill
         (if (= cascadeDirection offsetDirFill)
      		(begin
         		(set! maxCol (+ (/ width widthX) 1) )			; column = x direction
		   		(set! incrX widthX)
      		) ; end begin
      	) ; end if

   		(while (< row maxRow)			; Row loop
   			(begin

					; For even rows, x has no initial offset.
					(if (even? row)
						(begin ; even rows
							(set! offset-x startX)
						) ; end begin
						(begin ; implied else - odd rows
                  	; Checkerboard
                     (if (= cascadeDirection offsetDirChkr)
                  		(begin
									(set! offset-x (+ startX widthX))
                  		) ; end begin
                  	) ; end if
                  	; Tessellation
                     (if (= cascadeDirection offsetDirTess)
                  		(begin
									(set! offset-x (- startX (/ widthX 2)))
                  		) ; end begin
                  	) ; end if
                  	; Fill
                     (if (= cascadeDirection offsetDirFill)
                  		(begin
									(set! offset-x startX)
                  		) ; end begin
                  	) ; end if
						) ; end begin
					) ; end if

   				(set! col 0) ; Initialize the column count.
		   		(while (< col maxCol)	; Column loop
   					(begin
		               ; Set the layer offsets to X,Y position
  				         (gimp-layer-set-offsets layerCopy offset-x offset-y)
  				         (set! needToPosition FALSE)
  				         ; Check if the layer needs to be flipped.
  				         (if (= flipLayer TRUE)
  				         	(begin
  				         		; Flip the layer.
  				         		; (gimp-drawable-transform-flip-simple drawable flip-type auto-center axis clip-result)
  				         		(set! layerCopy (car (gimp-drawable-transform-flip-simple layerCopy flip-type TRUE 0.0 TRUE)))

                           ; Vertical flipping gets disabled after the first flip.
            		         (if (= flipVertical TRUE)
            		         	(begin
            		         		(set! flipLayer FALSE)
            		         	) ; end begin
            		         ) ; end if
  				         	) ; end begin
  				         ) ; end if

      					; debug
      					(if (> enableDebug 0)
      						(begin
            					(if (< displayCount 5)
            						(begin
                  					(set! textMessage (string-append "offset-x offset-y is " (number->string offset-x)))
                  					(set! textMessage (string-append textMessage " "))
                  					(set! textMessage (string-append textMessage (number->string offset-y)))
                  					(gimp-message textMessage)
                  					(set! displayCount (+ displayCount 1))
                  				) ; end begin
                  			) ; end if
            				) ; end begin
            			) ; end if

		               ; Increment column counter
      		         (set! col (+ col 1))

		               ; Increment the X position.
      		         (set! offset-x (+ offset-x incrX))

                 		; Check if we're all done.
                 		(if (>= (+ row 1) maxRow)
                 			(begin
                 				(if (>= (+ col 1) maxCol)
                 					(begin
                 						(set! allDone TRUE)
	      	               	) ; end begin
   		                  ) ; end (if
   	               	) ; end begin
	                  ) ; end (if

                     ; Check if the layer needs to be copied and offset.
                     ;(if (< col maxCol)
                     (if (< offset-x (+ width incrX))
                     	(begin
                     		(if (= allDone FALSE)
                     			(begin

                           		; Copy the layer.
                           		(set! layerCopy (car (gimp-layer-copy layerCopy TRUE)))

      		               		; Add the layer to the image. (It will be positioned
            		         		; on the next iteration through the loop.)
                           		(gimp-image-add-layer theImage layerCopy -1)

                           		(set! needToPosition TRUE)

	      	               	) ; end begin
   		                  ) ; end (if

                     	) ; end begin
                     ) ; end (if (< col maxCol)

		            ) ; end begin
      		   ) ; end (while (< col maxCol)

               ; Increment row counter
               (set! row (+ row 1))

               ; Increment the Y position.
               (set! offset-y (+ offset-y heightY))

               ; Check if the layer needs to be flipped.
               ; Horizontal and Vertical flip occurs for each row.
               ; TODO allow flip on column too
		         (if (= flipVertical TRUE)
		         	(begin
		         		(set! flipLayer TRUE)
		         		(set! flip-type ORIENTATION-VERTICAL)
		         	) ; end begin
		         ) ; end if

		         (if (= flipHorizontal TRUE)
		         	(begin
		         		(set! flipLayer TRUE)
		         		(set! flip-type ORIENTATION-HORIZONTAL)
		         	) ; end begin
		         ) ; end if

            ) ; end begin
         ) ; end (while (< row maxRow)

         ; TODO/Problem For some reason, it adds one more layer than it should.
         ; Kluge fix: delete it
         (gimp-image-remove-layer theImage layerCopy)

         ; But there is still another bug where it can have one less than it
         ; should, but that isn't because of the above delete.

         (if (= needToPosition TRUE)
          	(begin
               ; Set the layer offsets to X,Y position
		         (gimp-layer-set-offsets layerCopy offset-x offset-y)
         	) ; end begin
         ) ; end if


   ) ; end of let

) ; end define PatternFillTheLayer

; ===========================================================================
; Function: MakeLayerBorder
; ===========================================================================
(define (MakeLayerBorder
								inimage
						  		indraw
                    		spacingX
                    		spacingY
                    		)

   (let*(
      (theImage inimage)
      (theDraw indraw)
      (height (car (gimp-image-height theImage)))
      (width (car (gimp-image-width theImage)))
      (heightDraw (car (gimp-drawable-height theDraw)))
      (widthDraw (car (gimp-drawable-width theDraw)))
  		(cascadeCount 0)
  		(positionLayer 0)
  		(cascadeDirection  0)
  		(initialAngle  0)
  		(initialOffsetX 0)
  		(initialOffsetY 0)
  		(flipHorizontal FALSE)
  		(flipVertical FALSE)
  		(listSize 0)
  		(ii 0)
  		(textMessage ())
      ; Constants for this function:
  		;(positionLayerList '(_"Top Left" _"Top Right" _"Bottom Right" _"Bottom Left")) ; '
  		;(positionLayerList '(positionTopLeft positionTopRight positionBottomRight positionBottomLeft)) ; '
  		;(cascadeDirectionList '(offsetDirLtoR offsetDirTtoB offsetDirRtoL offsetDirBtoT)) ; '
  		(positionLayerList    '(0  1   4   3)) ; '
  		(cascadeDirectionList '(0  3   4   7)) ; '
  		(initialAngleList     '(0 90 180 270) ) ; '
  		)

   ; Startup initialization.
   (gimp-image-undo-group-start theImage)

   ; TODO hardcode for now. Get list size by parsing list.
   (set! listSize 4)

   (set! ii 0)
   (while (< ii listSize)
   	(begin
   		; Get first element in list;
   		(set! positionLayer (car positionLayerList))
   		(set! cascadeDirection (car cascadeDirectionList))
   		(set! initialAngle (car initialAngleList))

   		; Set list to remainder of list.
   		(set! positionLayerList (cdr positionLayerList))
   		(set! cascadeDirectionList (cdr cascadeDirectionList))
   		(set! initialAngleList (cdr initialAngleList))

;			(set! textMessage "debug: calling CascadeLayer ")
;			(set! textMessage (string-append textMessage (string-append " " (number->string positionLayer))))
;			(set! textMessage (string-append textMessage (string-append " " (number->string cascadeDirection))))
;			(set! textMessage (string-append textMessage (string-append " " (number->string initialAngle))))
;			(gimp-message textMessage)

			; Make the border for this set of arguments.
			(CascadeLayer
								theImage
						  		theDraw
                    		positionLayer
                    		cascadeDirection
                    		cascadeCount
                    		spacingX
                    		spacingY
                    		initialAngle
                    		initialOffsetX
                    		initialOffsetY
                    		flipHorizontal
                    		flipVertical
                    		)

   		; Increment loop counter.
   		(set! ii (+ ii 1))

			; Make the original layer being used the active layer again.
			(gimp-image-set-active-layer theImage theDraw)

   	) ; end begin
   ) ; end while

   ; Shutdown cleanup.
   (gimp-image-undo-group-end theImage)
   (gimp-displays-flush)

   ) ; end of let

) ; end define MakeLayerBorder


; ===========================================================================
; Function: CascadeLayer
; ===========================================================================
(define (CascadeLayer
								inimage
						  		indraw
                    		positionLayer
                    		cascadeDirection
                    		cascadeCount
                    		spacingX
                    		spacingY
                    		initialAngle
                    		initialOffsetX
                    		initialOffsetY
                    		flipHorizontal
                    		flipVertical
                    		)

   (let*(
      (theImage inimage)
      (theDraw indraw)
      (height (car (gimp-image-height theImage)))
      (width (car (gimp-image-width theImage)))
      (heightDraw (car (gimp-drawable-height theDraw)))
      (widthDraw (car (gimp-drawable-width theDraw)))
      (heightLayer 0)
      (widthLayer 0)
      (wrap-around FALSE)
      (fill-type OFFSET-TRANSPARENT)
      (widthX 0)
      (heightY 0)
      (deltaWidth ())
      (deltaWidthOffset ())
      (deltaXOffset ())
      (deltaHeight ())
      (deltaHeightOffset ())
      (deltaYOffset ())
      (startX 0)
      (startY 0)
      (incrX 0)
      (incrY 0)
      (offset-x 0)
      (offset-y 0)
      (offsetValue 2)
      (copyLayer FALSE)
      (patternFillCase FALSE)
      (layerCopy ())
      (layerCopyCountFloat 0)
      (layerCopyCount 0)
      (ii 0)
      (degTorad 0)
      (textMessage ())
      (angleRadians 0)
      (auto-center TRUE)
      (center-x 0)
      (center-y 0)
      (interpolate TRUE)
      (clip-result TRANSFORM-RESIZE-ADJUST)
      (transform-direction TRANSFORM-FORWARD)
      (interpolation INTERPOLATION-CUBIC) ; INTERPOLATION-LINEAR)
      (supersample 0)
      (recursion-level 3)
      (run-mode RUN-NONINTERACTIVE)
      (everything FALSE)
      (angleSelection 0)
      )

   ; Startup initialization.
   (gimp-image-undo-group-start theImage)


;	(set! textMessage "debug: CascadeLayer ")
;	(set! textMessage (string-append textMessage (string-append " " (number->string positionLayer))))
;	(set! textMessage (string-append textMessage (string-append " " (number->string cascadeDirection))))
;	(set! textMessage (string-append textMessage (string-append " " (number->string initialAngle))))
;	(gimp-message textMessage)

	; *-*-*-*-*-*-*-*-*-*-*-*-*
	; Initialization:
	; *-*-*-*-*-*-*-*-*-*-*-*-*
	; Set the layerCopy variable to the current layer.
	(set! layerCopy theDraw)
	(set! widthLayer widthDraw)
	(set! heightLayer heightDraw)

	; Rotate the layer, if necessary, making a copy of it first.
	(if (not (= initialAngle 0))
		(begin
			; Make a copy of the layer.
     		(set! layerCopy (car (gimp-layer-copy layerCopy TRUE)))

   		; Add the layer to the image.
   		(gimp-image-add-layer theImage layerCopy -1)

			; Get the current position of the layer.
			(set! offset-x (car (gimp-drawable-offsets layerCopy)))
			(set! offset-y (cadr (gimp-drawable-offsets layerCopy)))

			; Rotate the layer.
			(set! degTorad (/ pi 180.0))
			(set! angleRadians (* initialAngle degTorad))
			(set! center-x (/ (+ offset-x widthLayer) 2))  ; center-x = (offset-x + widthLayer) / 2
			(set! center-y (/ (+ offset-y heightLayer) 2)) ; center-y = (offset-y + heightLayer) / 2

			; Notes: The first two rotation methods cause distortions in the
			; resulting layer image. Use the simple rotation function. But
			; that will only work when the angle is 90, 180, or 270.

			; Angle { 90 (1), 180 (2), 270 (3) } degrees
			(if (= initialAngle  90) (set! angleSelection 1) )
			(if (= initialAngle 180) (set! angleSelection 2) )
			(if (= initialAngle 270) (set! angleSelection 3) )

			; See if we can use the simple rotation method, which works
			; best for the 90, 180, 270 angles.
			(if (not (= angleSelection 0))
				(begin
					; Use the simple rotation function.
					(plug-in-rotate run-mode theImage layerCopy angleSelection everything)
				) ; end begin
				(begin ; implied else
					; Use the more complex rotation function.
      			(gimp-drawable-transform-rotate-default layerCopy
      				angleRadians auto-center center-x center-y interpolate clip-result)

      			;(gimp-drawable-transform-rotate layerCopy
      			;	angleRadians auto-center center-x center-y transform-direction
      			;	interpolation supersample recursion-level clip-result)

				) ; end begin
			) ; end if

         ; Set the layer offsets to X,Y position, to put the layer
         ; back into its original X,Y starting position.
         (gimp-layer-set-offsets layerCopy offset-x offset-y)

         ; Get the new width and height values for the layer.
         (set! heightLayer (car (gimp-drawable-height layerCopy)))
         (set! widthLayer (car (gimp-drawable-width layerCopy)))
		) ; end begin
	) ; end if

	; Get the X and Y widths including any spacing.
	(set! widthX  (+ widthLayer  spacingX))
	(set! heightY (+ heightLayer spacingY))


	; *-*-*-*-*-*-*-*-*-*-*-*-*
	; Calculate X deltas
	; *-*-*-*-*-*-*-*-*-*-*-*-*
	(set! deltaWidth (- width widthX))
	(set! deltaWidthOffset (/ deltaWidth offsetValue))
	(set! deltaXOffset (- (/ width offsetValue) (/ widthX 2)))

	; *-*-*-*-*-*-*-*-*-*-*-*-*
	; Calculate Y deltas
	; *-*-*-*-*-*-*-*-*-*-*-*-*
	(set! deltaHeight (- height heightY))
	(set! deltaHeightOffset (/ deltaHeight offsetValue))
	(set! deltaYOffset (- (/ height offsetValue) (/ heightY 2)))


	; *-*-*-*-*-*-*-*-*-*-*-*-*
   ; Calculate X and Y increment based on cascade direction.
	; *-*-*-*-*-*-*-*-*-*-*-*-*

   ; Top Left to Bottom Right
   (if (= cascadeDirection offsetDirTLtoBR)
		(begin
			(set! incrX widthX)
			(set! incrY heightY)
   		(set! layerCopyCountFloat (min (/ width widthX) (/ height heightY)))
		) ; end begin
	) ; end if

   ; Top to Bottom
   (if (= cascadeDirection offsetDirTtoB)
		(begin
			(set! incrX 0)
			(set! incrY heightY)
   		(set! layerCopyCountFloat (/ height heightY))
		) ; end begin
	) ; end if

   ; Left to Right
   (if (= cascadeDirection offsetDirLtoR)
		(begin
			(set! incrX widthX)
			(set! incrY 0)
   		(set! layerCopyCountFloat (/ width widthX))
		) ; end begin
	) ; end if

   ; Top Right to Bottom Left
   (if (= cascadeDirection offsetDirTRtoBL)
		(begin
			(set! incrX (* widthX -1))
			(set! incrY heightY)
   		(set! layerCopyCountFloat (min (/ width widthX) (/ height heightY)))
		) ; end begin
	) ; end if

   ; Bottom to Top
   (if (= cascadeDirection offsetDirBtoT)
		(begin
			(set! incrX 0)
			(set! incrY (* heightY -1))
   		(set! layerCopyCountFloat (/ height heightY))
		) ; end begin
	) ; end if

   ; Right to Left
   (if (= cascadeDirection offsetDirRtoL)
		(begin
			(set! incrX (* widthX -1))
			(set! incrY 0)
   		(set! layerCopyCountFloat (/ width widthX))
		) ; end begin
	) ; end if

   ; Bottom Left to Top Right
   (if (= cascadeDirection offsetDirBLtoTR)
		(begin
			(set! incrX widthX)
			(set! incrY (* heightY -1))
   		(set! layerCopyCountFloat (min (/ width widthX) (/ height heightY)))
		) ; end begin
	) ; end if

   ; Bottom Right to Top Left
   (if (= cascadeDirection offsetDirBRtoTL)
		(begin
			(set! incrX (* widthX -1))
			(set! incrY (* heightY -1))
   		(set! layerCopyCountFloat (min (/ width widthX) (/ height heightY)))
		) ; end begin
	) ; end if

   ; Checkerboard (special case)
   (if (= cascadeDirection offsetDirChkr)
		(begin
			(set! incrX widthX)
			(set! incrY heightY)
   		(set! layerCopyCountFloat (/ (* (/ width widthX) (/ height heightY))) 2)
		) ; end begin
	) ; end if

	; Tessellation  (special case)
   (if (= cascadeDirection offsetDirTess)
		(begin
			(set! incrX widthX)
			(set! incrY heightY)
   		(set! layerCopyCountFloat (/ (* (/ width widthX) (/ height heightY))) 2)
		) ; end begin
	) ; end if

	; Fill  (special case)
   (if (= cascadeDirection offsetDirFill)
		(begin
			(set! incrX widthX)
			(set! incrY heightY)
   		(set! layerCopyCountFloat (/ (* (/ width widthX) (/ height heightY))) 2)
		) ; end begin
	) ; end if

	; *-*-*-*-*-*-*-*-*-*-*-*-*
   ; Calculate X and Y start position
	; *-*-*-*-*-*-*-*-*-*-*-*-*

	; Top Left
	(if (= positionLayer positionTopLeft)
		(begin
			(set! startX 0)
			(set! startY 0)

			; Only certain cases are allowed.
		   (if (= cascadeDirection offsetDirTLtoBR) (set! copyLayer TRUE)) ; Top Left to Bottom Right
		   (if (= cascadeDirection offsetDirTtoB)   (set! copyLayer TRUE)) ; Top to Bottom
		   (if (= cascadeDirection offsetDirLtoR)   (set! copyLayer TRUE)) ; Left to Right

		   ; If the cascadeCount is set to 1, allow the layer to be moved into position.
		   (if (= cascadeCount 1) (set! copyLayer TRUE))

		) ; end begin
	) ; end if

	; Bottom Left
	(if (= positionLayer positionBottomLeft)
		(begin
			(set! startX 0)
			(set! startY deltaHeight)

			; Only certain cases are allowed.
		   (if (= cascadeDirection offsetDirBLtoTR) (set! copyLayer TRUE)) ; Bottom Left to Top Right
		   (if (= cascadeDirection offsetDirBtoT)   (set! copyLayer TRUE)) ; Bottom to Top
		   (if (= cascadeDirection offsetDirLtoR)   (set! copyLayer TRUE)) ; Left to Right

		   ; If the cascadeCount is set to 1, allow the layer to be moved into position.
		   (if (= cascadeCount 1) (set! copyLayer TRUE))

		) ; end begin
	) ; end if

	; Center
	(if (= positionLayer positionCenter)
		(begin
			(set! startX deltaXOffset)
			(set! startY deltaYOffset)

			; All directions are allowable.
			(set! copyLayer TRUE)

		) ; end begin
	) ; end if

	; Top Right
	(if (= positionLayer positionTopRight)
		(begin
			(set! startX deltaWidth)
			(set! startY 0)

			; Only certain cases are allowed.
		   (if (= cascadeDirection offsetDirTRtoBL) (set! copyLayer TRUE)) ; Top Right to Bottom Left
		   (if (= cascadeDirection offsetDirTtoB)   (set! copyLayer TRUE)) ; Top to Bottom
		   (if (= cascadeDirection offsetDirRtoL)   (set! copyLayer TRUE)) ; Right to Left

		   ; If the cascadeCount is set to 1, allow the layer to be moved into position.
		   (if (= cascadeCount 1) (set! copyLayer TRUE))

		) ; end begin
	) ; end if

	; Bottom Right
	(if (= positionLayer positionBottomRight)
		(begin
			(set! startX deltaWidth)
			(set! startY deltaHeight)

			; Only certain cases are allowed.
		   (if (= cascadeDirection offsetDirBRtoTL) (set! copyLayer TRUE)) ; Bottom Right to Top Left
		   (if (= cascadeDirection offsetDirBtoT)   (set! copyLayer TRUE)) ; Bottom to Top
		   (if (= cascadeDirection offsetDirRtoL)   (set! copyLayer TRUE)) ; Right to Left

		   ; If the cascadeCount is set to 1, allow the layer to be moved into position.
		   (if (= cascadeCount 1) (set! copyLayer TRUE))

		) ; end begin
	) ; end if

	; Current Position
	(if (= positionLayer positionCurrentPosition)
		(begin
			; Get the current position of the layer.
			(set! startX (car (gimp-drawable-offsets layerCopy)))
			(set! startY (cadr (gimp-drawable-offsets layerCopy)))

			; All directions are allowable.
			(set! copyLayer TRUE)

		) ; end begin
	) ; end if

   ; Checkerboard (special case) - Start at top left
   (if (= cascadeDirection offsetDirChkr)
		(begin
			(set! startX 0)
			(set! startY 0)
			(set! patternFillCase TRUE)
		) ; end begin
	) ; end if

   ; Tessellation (special case) - Start at top left
   (if (= cascadeDirection offsetDirTess)
		(begin
			(set! startX 0)
			(set! startY 0)
			(set! patternFillCase TRUE)
		) ; end begin
	) ; end if

   ; Fill (special case) - Start at top left
   (if (= cascadeDirection offsetDirFill)
		(begin
			(set! startX 0)
			(set! startY 0)
			(set! patternFillCase TRUE)
		) ; end begin
	) ; end if


	; *-*-*-*-*-*-*-*-*-*-*-*-*
   ; Add any initial offset to the calculated X and Y start positions.
	; *-*-*-*-*-*-*-*-*-*-*-*-*
	(set! startX (+ startX initialOffsetX))
	(set! startY (+ startY initialOffsetY))



	; *-*-*-*-*-*-*-*-*-*-*-*-*
	; Set the layerCopyCount value.
	; *-*-*-*-*-*-*-*-*-*-*-*-*

	; Truncate the layerCopyCountFloat so it's an integer.
	; Use ceiling function instead of trunc so that it
	; automatically rounds up to the next integer.
	;(set! layerCopyCount (trunc layerCopyCountFloat))
	(set! layerCopyCount (ceiling layerCopyCountFloat))

	; debug
	;(set! textMessage (string-append "layerCopyCountFloat is " (number->string layerCopyCountFloat)))
	;(set! textMessage (string-append textMessage (string-append "layerCopyCount is " (number->string layerCopyCount))))
	;(gimp-message textMessage)

	; See if a cascade count was specified.
	(if (> cascadeCount 0)
		(begin
			(if (< cascadeCount layerCopyCount) (set! layerCopyCount cascadeCount))
			(if (= layerCopyCount 0) (set! layerCopyCount cascadeCount))
		) ; end begin
	); end if

	; If this is from the center, the layerCopyCount needs to be
	; half of what it is.
	(if (= positionLayer positionCenter)
		(begin
			(set! layerCopyCount (ceiling (/ layerCopyCount 2)))
		) ; end begin
	); end if

	; *-*-*-*-*-*-*-*-*-*-*-*-*
	; TODO validate sizes
	; *-*-*-*-*-*-*-*-*-*-*-*-*

   ; Calculate X position and increment
   (if (> width widthX)
   	(begin

		) ; end begin
	) ; end (if (> width widthX)

   ; Calculate Y position and increment
   (if (> height heightY)
   	(begin

		) ; end begin
	) ; end (if (> height heightY)

	; *-*-*-*-*-*-*-*-*-*-*-*-*
	; Do the actual copying now, if enabled to.
	; *-*-*-*-*-*-*-*-*-*-*-*-*
   (if (= copyLayer TRUE)
   	(begin

			; Initialize loop variables.
   		(set! offset-x startX)
   		(set! offset-y startY)
   		(set! ii 0)

   		(while (< ii layerCopyCount)
   			(begin

               ; Set the layer offsets to X,Y position
               (gimp-layer-set-offsets layerCopy offset-x offset-y)

					; debug
					;(set! textMessage (string-append "offset-x offset-y is " (number->string offset-x)))
					;(set! textMessage (string-append textMessage " "))
					;(set! textMessage (string-append textMessage (number->string offset-y)))
					;(gimp-message textMessage)

               ; Increment loop counter
               (set! ii (+ ii 1))

               ; Check if the layer needs to be copied and offset.
               (if (< ii layerCopyCount)
               	(begin
               		; Copy the layer.
               		(set! layerCopy (car (gimp-layer-copy layerCopy TRUE)))

               		; Add the layer to the image. (It will be positioned
               		; on the next iteration through the loop.)
               		(gimp-image-add-layer theImage layerCopy -1)

               		; Adjust the offsets.
               		(set! offset-x (+ offset-x incrX))
               		(set! offset-y (+ offset-y incrY))

               	) ; end begin
               ) ; end (if (< ii layerCopyCount)

            ) ; end begin
         ) ; end (while (< ii layerCopyCount)

		) ; end begin
		(begin ; implied else
			; Only display the error message if it's not a special case.
			(if (= patternFillCase FALSE)
				(begin
            	(set! textMessage "CascadeLayer combination does not get done: ")
            	(set! textMessage (string-append textMessage (string-append " " (number->string positionLayer))))
            	(set! textMessage (string-append textMessage (string-append " " (number->string cascadeDirection))))
            	(set! textMessage (string-append textMessage (string-append " " (number->string initialAngle))))
            	(gimp-message textMessage)
				) ; end begin
			) ; end (if (= checkerboardLayer FALSE)
		) ; end begin
	) ; end (if (= patternFillCase FALSE)


	; *-*-*-*-*-*-*-*-*-*-*-*-*
	; Do the checkerboard, tessellation, or fill, if enabled to.
	; *-*-*-*-*-*-*-*-*-*-*-*-*
   (if (= patternFillCase TRUE)
   	(begin

   		(PatternFillTheLayer
   							theImage
   							layerCopy
								height
								width
								heightY
								widthX
								startX
								startY
								cascadeDirection
                    		flipHorizontal
                    		flipVertical
				)

		) ; end begin
	) ; end (if (= patternFillCase TRUE)


   ; Shutdown cleanup.
   (gimp-image-undo-group-end theImage)
   (gimp-displays-flush)

   ) ; end of let

) ; end define CascadeLayer

(script-fu-register 	"MakeLayerBorder"
   "<Image>/Script-Fu/Effects Layer/Make border using layer"
   "Positions and cascades a layer to create a border."
   "ACC"
   "ACC"
   "Apr 2011"
   "RGB*,GRAY*"
   SF-IMAGE      	"SF-IMAGE" 0
   SF-DRAWABLE   	"SF-DRAWABLE" 0
   SF-ADJUSTMENT "x spacing" '(0 -4096 4096 1 5 0 SF-SPINNER)
   SF-ADJUSTMENT "y spacing" '(0 -4096 4096 1 5 0 SF-SPINNER)
)

(script-fu-register 	"CascadeLayer"
   "<Image>/Script-Fu/Effects Layer/Position and Cascade layer"
   "Positions and cascades a layer relative to the image. (i.e. Top Left to Bottom Right, Left to Right, etc.) You can specify how many times to cascade the layer, with the default being 0, which means fill the image in the specified direction. If you use 1, it will position the layer to the selected starting position. You can also specify the x and y spacing for cascading, and the initial x and y position. Other choices: Checkerboard, Tessellate, or Fill the layer."
   "ACC"
   "ACC"
   "Apr 2011"
   "RGB*,GRAY*"
   SF-IMAGE      	"SF-IMAGE" 0
   SF-DRAWABLE   	"SF-DRAWABLE" 0
   SF-OPTION _"Start Position" '(
   				_"Top Left"
   				_"Top Right"
   				_"Center"
   				_"Bottom Left"
   				_"Bottom Right"
   				_"Current Position" )
   SF-OPTION _"Cascade Direction" '(
   				_"Left to Right"
   				_"Top Left to Bottom Right"
   				_"Bottom Left to Top Right"
   				_"Top to Bottom"
   				_"Right to Left"
   				_"Top Right to Bottom Left"
   				_"Bottom Right to Top Left"
   				_"Bottom to Top"
   				_"Checkerboard"
   				_"Tessellation"
   				_"Fill" )
   SF-ADJUSTMENT "Cascade Count (0 = fill in direction)" '(0 0 4096 1 5 0 SF-SPINNER)
   SF-ADJUSTMENT "x spacing" '(0 -4096 4096 1 5 0 SF-SPINNER)
   SF-ADJUSTMENT "y spacing" '(0 -4096 4096 1 5 0 SF-SPINNER)
   SF-ADJUSTMENT "Angle" '(0 -360 360 1 15 0 SF-SPINNER)
   SF-ADJUSTMENT "initial x offset" '(0 -4096 4096 1 5 0 SF-SPINNER)
   SF-ADJUSTMENT "initial y offset" '(0 -4096 4096 1 5 0 SF-SPINNER)
   SF-TOGGLE     "flip horizontal" FALSE
   SF-TOGGLE     "flip vertical" FALSE
)


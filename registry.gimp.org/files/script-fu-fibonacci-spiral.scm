; Fibonacci Spiral, V1.0
;
; AUTHOR: theilr (http://flickr.com/photos/theilr), (c) 2009
;
; This script was tested with GIMP 2.6.7
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License Version 3 as 
; published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
; GNU General Public License at  http://www.gnu.org/licenses for
; more details.
;
; DESCRIPTION:
;
; Converts a square or rectangular image into a Fibonacci spiral
; (similar to a "golden spiral" -- the fine distinctions are discussed
; under the wikipedia entry on Golden spiral
; [http://en.wikipedia.org/wiki/Golden_spiral]) by making smaller
; copies of the image, rotating them, and arranging them into a spiral
; tiling.  If the initial image is square, then the tiling has no
; overlap; for a rectangular image, the user can specify a blending mode
; for how the overlap is treated.
;
; Works best if the image has a Fibonacci number of pixels as width
; and height (and this is especially true if the original image is a
; square).  The user can toggle an automatic rescaling to the nearest
; Fibonacci length (smaller than or equal to the original size).
;
; Located in menu "<Image> / Filters / theilr / Fibonacci Spiral"
;
; USAGE NOTES:
;
;   The user can specify the angle (in 90 degree increments, measured
;   clockwise) that the square is turned with each reduction in size.
;   To get the spiral effect, you'll probably want to use the default
;   value of 90, or possibly 270.
;
;   The user can specify the quadrant where the center of the spiral
;   well end up.  Default is lower right.  
;
;   Much of the rescaling, etc, is based on image size, not layer size.
;   So if canvas and current layer are not the same size, there's no
;   telling what will actually happen.
;
;   If a there is a selection, it is ignored.
;
;   Each rescaled rectangle/square is in a different layer, so you'll
;   probably want to follow up by merging all layers.
;
; BUGS:
;   Although this permits various blending modes and opacities, I
;   find I almost always prefer normal mode and 100-percent opacity.
;   In other words, you might as well be working with square images.
;
;   There's no good way to put borders around each of the squares that
;   goes into the spiral; eg, if you give the original square a dark
;   border, it gets lost in the cubic rescaling after just a few
;   generations.
;
; SCRIPT SUMMARY:
;   Prepare Image by making it Golden Landscape Rectangle
;     Flip/flop image, if needed, so that spiral center 
;       ends up in desired quadrant
;     Rotate, if necessary, so image is landscape
;     Optionally rescale the image to have Fibonacci width and height
;     If square, make into a landscape-mode golden rectangle
;
;   Loop:
;      Copy layer, then:
;       reduce size by factor of golden ratio[*]
;       possibly rotate layer
;       move layer to appropriate position on the spiral
;   Repeat until there's no more room.
;
;   Un-rotate, and Un-flip/flop 
;
;   [*] note, this is not done by multiplying or dividing, but
;       by continually subtacting short from long to get shorter,
;       until the shorter is zero (or less).  then you stop.
; 
; Version 1.0
; ==========================================================================

;; this function is invoked if the user toggles "Rescale to Fibonacci"
(define (rescale-to-fibonacci inImage)
  (let* 
      ( ;define local variables
       (theWidth  (car (gimp-image-width  inImage)))
       (theHeight (car (gimp-image-height inImage)))

       (currentSize 1)
       (nextSize 1)
       (tmpSize)
       (theLongSize (max theWidth theHeight))
       )

    ;; start generating Fibonacci numbers 
    ;; until theLongSize is exceeded -- then scale back 
    ;; by one Fibonacci number
    (while (<= nextSize theLongSize)
     (set! tmpSize nextSize)
     (set! nextSize (+ currentSize nextSize))
     (set! currentSize tmpSize)
     )
    
    (if (= theWidth theHeight)
	(gimp-image-scale inImage currentSize currentSize)
	(if (> theWidth theHeight)
	    (gimp-image-scale inImage currentSize (- nextSize currentSize))
	    (gimp-image-scale inImage (- nextSize currentSize) currentSize)
	    )
	)

    );; end let*
  );; end define rescale-to-fibonacci

(define (flip-image-for-orientation inImage inLocateCenter)
    ;; Flip as appropriate to put spiral center in desired corner
    (if (or (= inLocateCenter 1) (= inLocateCenter 2))
	(gimp-image-flip inImage ORIENTATION-HORIZONTAL)
	)
    (if (or (= inLocateCenter 2) (= inLocateCenter 3))
	(gimp-image-flip inImage ORIENTATION-VERTICAL)
	)
    );; end define flip-image-for-orientation  


(define (script-fu-fibonacci-spiral inImage inLayer 
				    inRescaleFibonacci 
				    inLocateCenter
				    inRotate
				    inOpacity
				    inBlendMode)

  (gimp-image-undo-group-start inImage) 
  (let* 
      ( ;define local variables
       (theWidth  (car (gimp-image-width  inImage)))
       (theHeight (car (gimp-image-height inImage)))

       (g (/ (+ (sqrt 5) 1) 2)) ;; Golden Ratio: 1.6180339887498949
       (nextSize)
       (currentSize)
       (tmpSize)
       (x 0)
       (y 0)
       (count 0)
       
       (rotateAngle (+ 1 inRotate))

       (cpyLayer)
       (portraitMode FALSE)
       )

    ;; clear any selections
    (gimp-selection-none inImage)

    ;; determine if portrait or landscape
    (if (> theHeight theWidth)
	(set! portraitMode TRUE)
	)

    ;; in portrait mode, the notion of upperleft, etc, is altered
    ;; 0 <-> 3, 1<->2
    (if (= portraitMode TRUE)
	(if (= (modulo inLocateCenter 2) 0)
	    (set! inLocateCenter (- inLocateCenter 1));; -1 if even
	    (set! inLocateCenter (+ inLocateCenter 1));; +1 if odd
	    )
	)
    (set! inLocateCenter (modulo (+ inLocateCenter 4) 4))

    ;; Do appropriate flipping to get spiral center in
    ;; the right place.  We will unflip (which is the
    ;; same operation) at the end of the process.
    (flip-image-for-orientation inImage inLocateCenter)

    ;; if portraitMode, then rotate to landscape
    ;; Ensure theWidth >= theHeight
    (if (= portraitMode TRUE)
	(gimp-image-rotate inImage ROTATE-90)
	)

    ;; if toggle set, then reset image size so width and height
    ;; are Fibonacci numbers
    (if (= inRescaleFibonacci TRUE)
	(rescale-to-fibonacci inImage)
	)

    ;; Recompute width and height, since the image has possibly
    ;; been rescaled and/or rotated at this point
    (set! theWidth  (car (gimp-image-width  inImage)))
    (set! theHeight (car (gimp-image-height inImage)))

    ;; If it's square, then add some empty width (to layer AND canvas)
    ;; To make it a golden rectangle.
    (if (= theHeight theWidth)
	(begin 
	  (set! theWidth (round (* theHeight g)))
	  (gimp-layer-resize inLayer theWidth theHeight 0 0)
	  (gimp-image-resize-to-layers inImage)
	  )
	)
	
    ;; By now, theWidth > theHeight, so 
    (set! nextSize theHeight)    ;; shorter dimension
    (set! currentSize theWidth)  ;; longer dimension

    ;; Now start making scaled-down copies as new layers
    (while (> (- currentSize nextSize) 0)
	   ;; Compute next Fibonacci numbers ... backward
	   (set! tmpSize nextSize)
	   (set! nextSize (- currentSize nextSize))
	   (set! currentSize tmpSize)
	   ;; Count iterations
	   (set! count (+ count 1))

	   ;; Make new layer as copy of original layer
	   (set! cpyLayer (car (gimp-layer-copy inLayer TRUE))) ;copy of layer
	   (gimp-image-add-layer inImage cpyLayer -1)
	   (gimp-layer-set-mode cpyLayer inBlendMode)
	   (gimp-layer-set-opacity cpyLayer inOpacity)
	   ;; Rescale new layer to appropriate size
	   (if (= (modulo rotateAngle 2) 1)
	       (gimp-layer-scale-full cpyLayer currentSize nextSize
				      TRUE INTERPOLATION-CUBIC)
	       (if (= (modulo count 2) 1)
		     (gimp-layer-scale-full cpyLayer nextSize currentSize
					    TRUE INTERPOLATION-CUBIC)
		     (gimp-layer-scale-full cpyLayer currentSize nextSize
					    TRUE INTERPOLATION-CUBIC)
		     )
	       )
	   ;; Rotate new layer by the appropriate angle
	   (plug-in-rotate RUN-NONINTERACTIVE inImage cpyLayer 
			   (* rotateAngle count) FALSE)

	   ;; Compute and apply offsets
	   (if (= (modulo count 4) 1)
	       (set! x (+ x currentSize))
	       )
	   (if (= (modulo count 4) 2)
	       (set! y (+ y currentSize))
	       )
	   (gimp-layer-set-offsets cpyLayer x y)

	   );; endwhile

    ;; if we started out in portraitMode (theHeight > theWidth), 
    ;; then rotate the whole image back to portraitMode
    (if (= portraitMode TRUE)
	(gimp-image-rotate inImage ROTATE-270)
	)

    ;; Un-Flip as appropriate to put spiral center in desired corner
    (flip-image-for-orientation inImage inLocateCenter)

    );; end let*
  (gimp-displays-flush)
  (gimp-image-undo-group-end inImage)

  );; end define

(script-fu-register "script-fu-fibonacci-spiral"
		    "<Image>/Filters/_theilr/_Fibonacci Spiral"
		    "Multiple smaller copies of image arranged in a spiral"
		    "theilr"
		    "(c) theilr"
		    "Dec 2009"
		    "RGB*"
		    SF-IMAGE "Image"  0
		    SF-DRAWABLE "Drawable" 0
		    SF-TOGGLE "Rescale to Fibonacci" FALSE
		    SF-OPTION "Where to put spiral center"
		    '("Lower Right"
		      "Lower Left"
		      "Upper Left" 
		      "Upper Right")
		    SF-OPTION "Rotate with each smaller square" 
		    '("90" "180" "270" "360")
		    SF-ADJUSTMENT "Opacity" '(100 0 100 1 10 0 SF-SLIDER)
		    SF-OPTION "Blending mode:"
		    '("NORMAL" 
		      "DISSOLVE"
		      "BEHIND"
		      "MULTIPLY"
		      "SCREEN"
		      "OVERLAY"
		      "DIFFERENCE"
		      "ADDITION"
		      "SUBTRACT"
		      "DARKEN-ONLY"
		      "LIGHTEN-ONLY"
		      "HUE"
		      "SATURATION"
		      "COLOR"
		      "VALUE"
		      "DIVIDE"
		      "DODGE"
		      "BURN"
		      "HARDLIGHT"
		      "SOFTLIGHT"
		      "GRAIN-EXTRACT"
		      "GRAIN-MERGE"
		      "COLOR-ERASE"
		      "ERASE"
		      "REPLACE"
		      "ANTI-ERASE")
		    )


  
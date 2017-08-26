; HDR Fake Effect is a script for The GIMP
;
; This script produces a fake HDR effect + on an image
;
; Follow the tut of jneurock@gimpology.com
; here the link :
; http://gimpology.com/submission/view/fake_hdr_look_in_gimp/
;
; The script use some code from Dodge burn is a script for The GIMP
; by  Harry Phillips <script-fu@tux.com.au> release in GPL
;
; The script is located in "<Image> / Script-Fu / Enhance / HDR Fake Effect..."
;
; Last changed: 21/11/08
;
; Copyright (C) 2008 Bui The Thang <vincent.valentine71@gmail.com>
;
; --------------------------------------------------------------------
; 
; Changelog:
;  Version 0.1
;    - Initial version
;
; --------------------------------------------------------------------
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

(define (my-layer-stuff		myImage
				myLayer
				modeOp
				thinNum
				thickNum
	)

    ;Initiate some variables
    (let* (
	(firstTemp (car (gimp-layer-copy myLayer 1)))
	(thinTemp)
	(thickTemp)
	(merged)
    )


    ;Rename the layer
    (gimp-drawable-set-name firstTemp "First temp")

    ;Add the first layer to the image
    (gimp-image-add-layer myImage firstTemp 0)

    ;Desaturate the layer
    (gimp-desaturate firstTemp)

    ;Copy and add the dodge me layer as the thin layer
    (set! thinTemp (car (gimp-layer-copy firstTemp 1)))
    (gimp-image-add-layer myImage thinTemp 0)

    ;Blur the thin layer
    (plug-in-gauss 1 myImage thinTemp thinNum thinNum 0)

    (if (= modeOp 1)

	;Change the mode of the thin layer to lighten
    	(gimp-layer-set-mode thinTemp 10)

	;Change the mode of the thin layer to darken
    	(gimp-layer-set-mode thinTemp 9)
    )


    ;Blur the dodge me layer
    (plug-in-gauss 1 myImage firstTemp thickNum thickNum 0)

    ;Copy the dodge me layer as a new layer
    (set! thickTemp (car (gimp-layer-copy firstTemp 1)))

    ;Add the new layer to the image
    (gimp-image-add-layer myImage thickTemp 1)

    ;Merge the top layer down and keep track of the newly merged layer
    (set! merged (car (gimp-image-merge-down myImage thinTemp 0)))

    ;Change the mode of the dodge copy layer to difference mode
    (gimp-layer-set-mode merged 6)

    ;Merge the top layer down and keep track of the newly merged layer
    (set! merged (car (gimp-image-merge-down myImage merged 0)))

    (if (= modeOp 1)

	(begin
    		;Rename the layer
    		(gimp-drawable-set-name merged "Dodge channel")

    		;Change the mode of the dodge copy layer to dodge mode
    		(gimp-layer-set-mode merged 16)
	)

	(begin

    		;Rename the layer
    		(gimp-drawable-set-name merged "Burn channel")

    		;Change the mode of the dodge copy layer to dodge mode
    		(gimp-layer-set-mode merged 17)

		;Invert layer
		(gimp-invert merged)
	)
    )

    ;Return
    ))

(define (script-fu-fake-hdr-effect-enhance inImage inDrawable softvalue1 softvalue2 softvalue3  lev3min Opa-num thin thick lev5min lev5max)

	;Start an undo group so the process can be undone with one undo
	(gimp-image-undo-group-start inImage)

	 ;Select none
	(gimp-selection-none inImage)

	(let* (
		( theNewlayer (car (gimp-layer-copy inDrawable 1)))
		(theNewlayer1 0)
		(theNewlayer2 (car (gimp-layer-copy inDrawable 1)))
		(theNewlayer3 0)
		(subdra 0)
		(layerRGB 0)
		)
		
		(set! subdra (car (gimp-image-get-active-drawable inImage)))
		; Detect if it is RGB. Change the image RGB if it isn't already
		(set! layerRGB (car (gimp-drawable-is-rgb inDrawable)))
		(if (= layerRGB 0) (gimp-image-convert-rgb inDrawable))


		(gimp-image-add-layer inImage theNewlayer 0)
		(gimp-desaturate-full theNewlayer 2)
		(gimp-invert theNewlayer)
		(plug-in-softglow RUN-NONINTERACTIVE inImage theNewlayer softvalue1 softvalue2 softvalue3)
		(gimp-layer-set-mode theNewlayer SOFTLIGHT-MODE )
		(gimp-layer-set-opacity theNewlayer 50)

		(set!  theNewlayer1 (car (gimp-layer-copy theNewlayer 1)))
			(gimp-image-add-layer inImage theNewlayer1 0)
			(gimp-layer-set-opacity theNewlayer1 75)
		
		(gimp-image-add-layer inImage theNewlayer2 0)
		(gimp-image-set-active-layer inImage theNewlayer2)
		(set! layerRGB (car (gimp-levels theNewlayer2 HISTOGRAM-VALUE lev3min 255 1.0 0 255)))
		(gimp-layer-set-opacity theNewlayer2 Opa-num)
		
		(set! subdra (car (gimp-image-flatten inImage)))

		(set! theNewlayer3 (car (gimp-layer-copy subdra 1)))
		(gimp-image-add-layer inImage theNewlayer3 0)
		(gimp-image-set-active-layer inImage theNewlayer3)		
		;Do the dodge layer first
		(my-layer-stuff inImage theNewlayer3 1 thin thick)
		;Do the burn layer
		(my-layer-stuff inImage theNewlayer3 0 thin thick) 
		
		(gimp-image-set-active-layer inImage theNewlayer3)
		(set! subdra (plug-in-color-enhance RUN-NONINTERACTIVE inImage theNewlayer3))
		(set! subdra (gimp-levels theNewlayer3 HISTOGRAM-VALUE lev5min lev5max 1.0 0 255))

		(gimp-image-flatten inImage)

	)

	;Finish the undo group for the process
	(gimp-image-undo-group-end inImage)

	;Ensure the updated image is displayed now
	(gimp-displays-flush)
)

(script-fu-register
    "script-fu-fake-hdr-effect-enhance"
    "<Image>/Script-Fu/Enhance/Fake HDR Effect enhance..."
    "Make a photo to fake HDR with GIMP"
    "Martin Weber <martweb@gmx.net>"
    "Martin Weber"
    "Nov, 2008"
    "RGB*"
    SF-IMAGE    "Image"        0

    SF-DRAWABLE    "Drawable"    0
    SF-ADJUSTMENT	"Softglow Glow Radius"		'(10.0 1.0 50.0 1 0 2 0)
    SF-ADJUSTMENT	"Softglow Brightness"		'(0.75 0.0 1.0 0.5 0 2 0)
    SF-ADJUSTMENT	"Softglow Sharpness"		'(0.85 0.0 1.0 0.5 0 2 0)
    SF-ADJUSTMENT   _"Level In Minimum"     '(100 0 255 1 1 0 0)
    SF-ADJUSTMENT   _"Dark Layer Opacity"     '(35 30 50 1 1 0 0)
    SF-ADJUSTMENT   _"Dodge Burn Thin"     '(10 0 1000 1 1 0 0)
    SF-ADJUSTMENT   _"Dodge Burn Thick"      '(25 0 10000 1 1 0 0)
    SF-ADJUSTMENT   _"Level In Minimum"     '(25 0 255 1 1 0 0)
    SF-ADJUSTMENT   _"Level In Maximum"     '(225 0 255 1 1 0 0)
)

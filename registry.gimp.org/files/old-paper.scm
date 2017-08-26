;;;    This GIMP script adds an old paper effect to the image.
;;;    It is based off of the tutorial by RobA at
;;;    http://forum.cartographersguild.com/showthread.php?533-Tutorial-Creating-old-weathered-paper-using-the-Gimp
;;;
;;;    Copyright (C) 2010  Mike Hogan <themikehogan@gmail.com>
;;;
;;;    This program is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Copy this script to your ~/.gimp-2.6/scripts directory as old-paper.scm
;; restart gimp or click Filters->Script-Fu->Refresh Scripts
;; load/create an image
;; click Filters->Decor->Old Paper... to start the script
;; Add your own custom grunge touch-ups to the Blots layer (kinda hard to automate this
;;   as the script doesn't know what grungy looks would look good on your image) Duplicate
;;   Blots layer to make it look darker
;; Play with different layer modes on your image to find what looks good, Burn, Multiply,
;;   Overlay, Darken-Only and Grain-Merge modes typically are the ones that would look good
;; Make sure to Merge the layers before adding anything under the map layers or the map image
;;   will be overlayed ontop of those layers underneath as well around the distressed areas
;; Enjoy your brand new^H^H^H old paper/parchment look
;;
;; Feel free to let me know of any improvements/modifications you'd like and that you did
;
; Updates:
; 	2010-09-10
;		put the add layers inside the undo group
;		removed RGBA constraint and added alpha channel automatically


(define (script-fu-old-paper theImage inLayer inAddBlots inDistress inSaveDistress inDistressAmount inBorderSize inPaperColor inMergeLayers inDropShadow)
(let*
    (
        (theNewWidth (+ (car (gimp-drawable-width inLayer)) (* 2 inBorderSize)))
        (theNewHeight (+ (car (gimp-drawable-height inLayer)) (* 2 inBorderSize)))
        (theFuzzLayer 0)
        (theBlotLayer 0)
        (theTextureLayer 0)
        (thePaperLayer 0)
        (theSelection 0)
        (thePaperSelection 0)
        (theBrush 0)
    )
    ;init (save state/selections/undo)
    (gimp-context-push)
    (gimp-image-undo-group-start theImage)
    (set! theSelection (car (gimp-selection-save theImage)))
    (gimp-selection-none theImage)
	
	(gimp-layer-add-alpha inLayer)
	
	(set! theFuzzLayer (car (gimp-layer-new theImage theNewWidth theNewHeight RGBA-IMAGE "Old-Paper-Fuzz" 20 MULTIPLY-MODE)))
	(set! theBlotLayer (car (gimp-layer-new theImage theNewWidth theNewHeight RGBA-IMAGE "Old-Paper-Blots" 100 OVERLAY-MODE)))
	(set! theTextureLayer (car (gimp-layer-new theImage theNewWidth theNewHeight RGBA-IMAGE "Old-Paper-Texture" 100 OVERLAY-MODE)))
	(set! thePaperLayer (car (gimp-layer-new theImage theNewWidth theNewHeight RGBA-IMAGE "Old-Paper-Paper" 100 NORMAL-MODE)))

    (if (= inDistress FALSE)                ; no need to resize if we aren't distressing the border
        (gimp-image-resize theImage theNewWidth theNewHeight inBorderSize inBorderSize)
    )

    ;add the layers
    (gimp-image-add-layer theImage theFuzzLayer -1)
    (if (= inAddBlots TRUE)
        (gimp-image-add-layer theImage theBlotLayer 2)
    )
    (gimp-image-add-layer theImage theTextureLayer 3)
    (gimp-image-add-layer theImage thePaperLayer 4)

    ; BURN-MODE MULTIPLY-MODE OVERLAY-MODE DARKEN-ONLY-MODE GRAIN-MERGE-MODE all look nice on different type of maps
    (gimp-layer-set-mode inLayer GRAIN-MERGE-MODE)
    (gimp-selection-all theImage)
    (gimp-selection-shrink theImage inBorderSize)
    (if (= inDistress TRUE)
        (script-fu-distress-selection theImage inLayer 197 8 4 2 TRUE TRUE)
    )
    (if (= inSaveDistress TRUE)
        (begin
            (set! thePaperSelection (car (gimp-selection-save theImage)))
            (gimp-drawable-set-name thePaperSelection "Old Paper Selection")
        )
    )

    (gimp-context-set-foreground inPaperColor)
    (gimp-edit-bucket-fill thePaperLayer FG-BUCKET-FILL NORMAL-MODE 100 255 FALSE 0 0)
    (plug-in-plasma RUN-NONINTERACTIVE theImage theTextureLayer (srand (realtime)) 2.5)
    (gimp-desaturate theTextureLayer)
    (plug-in-bump-map RUN-NONINTERACTIVE theImage theTextureLayer theTextureLayer 135 45 5 0 0 0 0 TRUE FALSE LINEAR)
    (gimp-context-set-foreground '(0 0 0))
    
    (set! theBrush (car (gimp-brush-new "old-paper border")))
    (gimp-brush-set-radius theBrush (/ inBorderSize 2))
    (gimp-brush-set-shape theBrush BRUSH-GENERATED-CIRCLE)
    (gimp-brush-set-hardness theBrush 1)
    (gimp-context-set-brush theBrush)
    
    (if (= inAddBlots TRUE)
        (begin
            (gimp-edit-stroke theBlotLayer)
            (plug-in-gauss-rle RUN-NONINTERACTIVE theImage theBlotLayer inBorderSize TRUE TRUE)
        )
        (set! theBlotLayer -1)
    )
    (plug-in-plasma RUN-NONINTERACTIVE theImage theFuzzLayer (srand (realtime)) 2.5)
    (gimp-desaturate theFuzzLayer)
    (gimp-brush-set-hardness theBrush 0)
    (gimp-context-set-opacity 60)
    (gimp-edit-stroke theFuzzLayer)
    
    (if (= inDropShadow TRUE)
        (script-fu-drop-shadow theImage thePaperLayer 8 8 15 '(0 0 0) 80 TRUE)
    )
    
    (if (= inMergeLayers TRUE)
        (let*
            (
                (theLayerName 0)
            )
            (set! theLayerName (car (gimp-drawable-get-name inLayer)))
            (set! inLayer (car (gimp-image-merge-visible-layers theImage EXPAND-AS-NECESSARY)))
            (gimp-drawable-set-name inLayer theLayerName)
        )
    )
    
    ;cleanup (restore state/selection/undo/flush display)
    (gimp-brush-delete theBrush)
    (gimp-selection-load theSelection)
    (gimp-image-remove-channel theImage theSelection)
    (gimp-image-undo-group-end theImage)
    (gimp-displays-flush)
    (gimp-context-pop)
    
    ;return the image and the layers incase any other scripts want to use them too
    ;if merging layers, the fuzz/blot/texture/paper layers are now gone
    ;theBlotLayer will be -1 if it is turned off
    (list theImage inLayer theFuzzLayer theBlotLayer theTextureLayer thePaperLayer)
)
)

(script-fu-register
    "script-fu-old-paper"
    "<Image>/Filters/Decor/Old Paper..."
    "Creates an old paper effect"
    "Mike Hogan"
    "Copyright 2010, Mike Hogan"
    "August 26, 2010"
    ""
    SF-IMAGE      "image"      0
    SF-DRAWABLE   "drawable"   0
    SF-TOGGLE     "Add Blot/Grunge Layer" TRUE
    SF-TOGGLE     "Distress Paper Edge" TRUE
    SF-TOGGLE     "Save Distression to Channel" TRUE
    SF-ADJUSTMENT "Distress Amount" '(127 1 255 1 10 0 0)
    SF-ADJUSTMENT  "Distress/Border size"     '(40 1 1000 1 10 0 1)
    SF-COLOR       "Paper Colour"         '(208 193 162)
    SF-TOGGLE     "Merge Visible Layers" FALSE
    SF-TOGGLE     "Drop Shadow" TRUE
)

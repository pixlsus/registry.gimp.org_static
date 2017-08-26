
; Uncrop

; Increase image/canvas size and synthesize outer band from edge of original.
; Requires resynthesizer plug-in.
; Lloyd Konneker (bootch served by nc.rr.com)

; Versions
; lloyd konneker lkk 5/15/2009 Initial version

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

; The effect:  widens the field of view, maintaining perspective of original
; Should be undoable, except for loss of selection.
; Should work on any image type, any count of layers and channels (although only active layer is affected.)
;
; Devt. cycle: cp uncrop-gimp-plugin.scm ~/.gimp-2.6/scripts, ScriptFu>Refresh scripts

; IN: Nothing special.  The selection is immaterial but is not preserved.
; OUT larger layer and image.  All other layers not enlarged.

; resize and center image by percent (converted to pixel units)
(define (resizeImageCentered image percentEnlarge)
  (let*
    (
      (priorWidth (car (gimp-image-width image)))
      (priorHeight (car (gimp-image-height image)))
      (increase (+ 1.0 (/ percentEnlarge 100)))
      (newWidth (* priorWidth increase))
      (newHeight (* priorHeight increase))
      (centeredOffX (/ (- newWidth priorWidth) 2 ))
      (centeredOffY (/ (- newHeight priorHeight) 2 ))
    )
    (if (not (car (gimp-image-resize image newWidth newHeight centeredOffX centeredOffY)))
                  (gimp-message "Failed resize"))
  )
)


; shrink selection by percent (converted to pixel units)
(define (shrinkSelectionByPercent theImage percent)
  (let*
    (
      (deltaFraction (/ percent 100))
      ; convert to pixel dimensions
      (priorWidth (car (gimp-image-width theImage)))
      (priorHeight (car (gimp-image-height theImage)))
      (deltaWidth (* priorWidth deltaFraction))
      (deltaHeight (* priorHeight deltaFraction))
      ; !!! Note total shrink percentage is halved (width of band is percentage/2)
      (maxDelta (/ (max deltaWidth deltaHeight ) 2 ))
    )
    ; (gimp-message (string-append "maxDelta"(number->string maxDelta)))
    (if (not (car (gimp-selection-shrink theImage maxDelta))) (gimp-message "Failed shrink selection") )
  )
)


(define (script-fu-uncrop image layer percentEnlarge)
  (let*
    (
      (dupeImage (car (gimp-image-duplicate image)))
      (selectAllPrior -1)
      (workLayerID -1)
    )

    ; (gimp-message-set-handler 1)	; debug: messages to console
    
    (gimp-image-undo-group-start image)
    
    ; Enlarge canvas and select the new, blank outer ring
    ; Remember the size of original to use later to select outer band
    (if (not (car (gimp-selection-all image)))  (gimp-message "Failed select all") )
    (set! selectAllPrior (car (gimp-selection-save image)))
    ; Resize image alone doesn't resize layer, so resize layer also
    (resizeImageCentered image percentEnlarge)
    (if (not (car (gimp-layer-resize-to-image-size layer)))  (gimp-message "Failed resize layer") )
    ; select outer band, the new blank canvas.
    (if (not (car (gimp-selection-load selectAllPrior))) (gimp-message "Failed select original") )
    (if (not (car (gimp-selection-invert image))) (gimp-message "Failed invert selection"))
    ; Assert target image is ready.
                  
    ; Prepare source (corpus) layer, a band at edge of original, in a dupe.
    ; ! Working with the original size, all pixels visible (not transparent.)
    ; Flatten (source everything visible) and to activate a layer in dupe
    (gimp-image-flatten dupeImage)         ; lkk !!! flatten, activates layer, but deletes alpha
    (set! workLayerID (car (gimp-image-get-active-layer dupeImage)))
    (cond ((= -1 workLayerID) (gimp-message "Failed get active layer")))
    ; lkk Resynth requires equal count of channels, target and source.
    (cond ((= 1 (car (gimp-drawable-has-alpha layer)))
           (if (not (car (gimp-layer-add-alpha workLayerID)))  (gimp-message "Failed add alpha") )
    )     )
    ; Select outer band:  select all, shrink
    (if (not (car (gimp-selection-all dupeImage)))  (gimp-message "Failed select all") )
    (shrinkSelectionByPercent dupeImage percentEnlarge)
    ; !!! Resynth inverts selection in corpus: the selection is the outer band to resynth.
    
    ;(gimp-display-new dupeImage)   ; debug to see the stencil as passed to plugin
    ;(gimp-displays-flush)     ; debug
      
    (plug-in-resynthesizer 1 image layer 0 0 1 workLayerID -1 -1 0.0 0.117 16 500)

    ; cleanup.  Any errors now are moot.
    (gimp-selection-none image)
    (gimp-image-remove-channel image selectAllPrior)
    (gimp-image-delete dupeImage)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)
) )



(script-fu-register "script-fu-uncrop"
                    "<Image>/Filters/Enhance/Uncrop..."
		    "Enlarge by synthesizing a concentric band that matches the edge, maintaining perspective.  Works best for small enlargement with natural edges. Undo a Crop instead if possible! Requires separate resynthesizer plug-in."
		    "Lloyd Konneker bootch@nc.rr.com"
		    "Copyright 2009 Lloyd Konneker"
		    "5/8/2009"
		    "RGB* GRAY*"
		    SF-IMAGE "Input Image" 0
		    SF-DRAWABLE "Input Layer" 0
		    SF-ADJUSTMENT "Percent enlargement" '(20 1 1000 1.0 1.0 0 1)
)


; TileableFilters.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.2 (20100323)

; Changes:
; 1.1 - switched to using named buffers
; 1.2 - cleaned up undo code location

; Description
; Provides tileable versions to a number of Filters.  Requires the base layer to already be tilable:
; Current filters are:
; Filters >Distorts >Tileable Emboss
; Filters >Blur >Tileable Motion Blur
; Filters >Distorts >Tileable Wind
; Filters >Light and Shadow >Tileable Sparkle
; Filters >Noise >Tileable Slur
; Filters >Noise >Tileable Spread
; Filters >Artistic >Tileable Cubism
; Filters >Artistic >Tileable Oilify
; Filters >Artistic >Tileable Photocopy
; Filters >Artistic >Tileable Softglow

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

;-----------------------------------------------------------------
;This is the engine
(define (tileable-functions img inLayer inFunction)
  (let* 
    (
      (width (car (gimp-drawable-width inLayer)))
      (x_off (min (trunc (/ width 2)) 200))
      (height (car (gimp-drawable-height inLayer)))
      (y_off (min (trunc (/ height 2)) 200))
      (tileImage 0)
      (tileLayer 0)
      (layers 0)
      (layercount 0)
      (counter 0)
      (buffname "tilablebuf")
  )
    
    (gimp-image-undo-group-start img)
    (set! tileImage (car (gimp-image-duplicate img)))
    (gimp-image-undo-disable tileImage)
    (set! tileLayer (car (gimp-image-get-active-layer tileImage)))
    (set! layers (cadr (gimp-image-get-layers tileImage)))
    (set! layercount (car (gimp-image-get-layers tileImage)))
    
    ;get rid of all other layers
    (while (< counter layercount)
      (if (<> (vector-ref layers counter) tileLayer)
        (gimp-image-remove-layer tileImage (vector-ref layers counter))
      )
      (set! counter (+ 1 counter))
    )
    
    ;offfset before tiling
    (gimp-drawable-offset tileLayer TRUE OFFSET-BACKGROUND x_off y_off)
    
    ;tile enlarges the image
    (plug-in-tile RUN-NONINTERACTIVE tileImage tileLayer (+ width x_off x_off) (+ height y_off y_off) FALSE)
    
    ;functions here
    (cond 
      (( equal? inFunction 0 ) (plug-in-emboss RUN-INTERACTIVE tileImage tileLayer 0 0 0 0))
      (( equal? inFunction 1 ) (plug-in-mblur RUN-INTERACTIVE tileImage tileLayer 0 0 0 0 0))
      (( equal? inFunction 2 ) (plug-in-wind RUN-INTERACTIVE tileImage tileLayer 0 0 0 0 0))
      (( equal? inFunction 3 ) (plug-in-sparkle RUN-INTERACTIVE tileImage tileLayer 0 0 0 0 0 0 0 0 0 0 0 0 0))
      (( equal? inFunction 4 ) (plug-in-randomize-slur RUN-INTERACTIVE tileImage tileLayer 0 0 0 0))
      (( equal? inFunction 5 ) (plug-in-spread RUN-INTERACTIVE tileImage tileLayer 0 0))
      (( equal? inFunction 6 ) (plug-in-cubism RUN-INTERACTIVE tileImage tileLayer 0 0 0))
      (( equal? inFunction 7 ) (plug-in-oilify RUN-INTERACTIVE tileImage tileLayer 0 0))
      (( equal? inFunction 8 ) (plug-in-photocopy RUN-INTERACTIVE tileImage tileLayer 0 0 0 0))
      (( equal? inFunction 9 ) (plug-in-softglow RUN-INTERACTIVE tileImage tileLayer 0 0 0))
  )
    
    ;select the center area
    (gimp-rect-select tileImage x_off y_off width height CHANNEL-OP-REPLACE FALSE 0)
    (set! buffname (car (gimp-edit-named-copy tileLayer buffname)))
    (gimp-floating-sel-anchor (car (gimp-edit-named-paste inLayer buffname FALSE)))
    
    ;clean-up
    (gimp-image-delete tileImage)
    (gimp-image-undo-group-end img)
    (gimp-drawable-update inLayer 0 0 width height)
    (gimp-displays-flush)
  )
)



;-----------------------------------------------------------------
; Emboss
(define (script-fu-tileable-emboss img inLayer)
  (let* ()
    (tileable-functions img inLayer 0)
  )
)

(script-fu-register "script-fu-tileable-emboss"
                "<Image>/Filters/Distorts/Tileable Emboss..."
          "Wrapper to make Emboss seamless for a seamless source."
          "Rob Antonishen"
          "Rob Antonishen"
          "Feb 2009"
          "RGB*"
          SF-IMAGE      "image"      0
          SF-DRAWABLE   "drawable"   0
)       

;-----------------------------------------------------------------
; Motion Blur -only makes sense for linear
(define (script-fu-tileable-mblur img inLayer)
  (let* ()
    (tileable-functions img inLayer 1)
  )
)

(script-fu-register "script-fu-tileable-mblur"
                "<Image>/Filters/Blur/Tileable Motion Blur..."
          "Wrapper to make Linear Motion Blur seamless for a seamless source."
          "Rob Antonishen"
          "Rob Antonishen"
          "Feb 2009"
          "RGB* GRAY*"
          SF-IMAGE      "image"      0
          SF-DRAWABLE   "drawable"   0
)       

;-----------------------------------------------------------------
; Wind
(define (script-fu-tileable-wind img inLayer)
  (let* ()
    (tileable-functions img inLayer 2)
  )
)

(script-fu-register "script-fu-tileable-wind"
                "<Image>/Filters/Distorts/Tileable Wind..."
          "Wrapper to make Wind filter seamless for a seamless source."
          "Rob Antonishen"
          "Rob Antonishen"
          "Feb 2009"
          "RGB*"
          SF-IMAGE      "image"      0
          SF-DRAWABLE   "drawable"   0
)       

;-----------------------------------------------------------------
; Sparkle
(define (script-fu-tileable-sparkle img inLayer)
  (let* ()
    (tileable-functions img inLayer 3)
  )
)

(script-fu-register "script-fu-tileable-sparkle"
                "<Image>/Filters/Light and Shadow/Tileable Sparkle..."
          "Wrapper to make Sparkle filter seamless for a seamless source."
          "Rob Antonishen"
          "Rob Antonishen"
          "Feb 2009"
          "RGB* GRAY*"
          SF-IMAGE      "image"      0
          SF-DRAWABLE   "drawable"   0
)       

;-----------------------------------------------------------------
; Slur
(define (script-fu-tileable-slur img inLayer)
  (let* ()
    (tileable-functions img inLayer 4)
  )
)

(script-fu-register "script-fu-tileable-slur"
                "<Image>/Filters/Noise/Tileable Slur..."
          "Wrapper to make Slur seamless for a seamless source."
          "Rob Antonishen"
          "Rob Antonishen"
          "Feb 2009"
          "RGB* GRAY*"
          SF-IMAGE      "image"      0
          SF-DRAWABLE   "drawable"   0
)       

;-----------------------------------------------------------------
; Spread
(define (script-fu-tileable-spread img inLayer)
  (let* ()
    (tileable-functions img inLayer 5)
  )
)

(script-fu-register "script-fu-tileable-spread"
                "<Image>/Filters/Noise/Tileable Spread..."
          "Wrapper to make Spread seamless for a seamless source."
          "Rob Antonishen"
          "Rob Antonishen"
          "Feb 2009"
          "RGB* GRAY*"
          SF-IMAGE      "image"      0
          SF-DRAWABLE   "drawable"   0
)       

;-----------------------------------------------------------------
; Cubism
(define (script-fu-tileable-cubism img inLayer)
  (let* ()
    (tileable-functions img inLayer 6)
  )
)

(script-fu-register "script-fu-tileable-cubism"
                "<Image>/Filters/Artistic/Tileable Cubism..."
          "Wrapper to make Cubism filter seamless for a seamless source."
          "Rob Antonishen"
          "Rob Antonishen"
          "Feb 2009"
          "RGB* GRAY*"
          SF-IMAGE      "image"      0
          SF-DRAWABLE   "drawable"   0
)       

;-----------------------------------------------------------------
; Oilify
(define (script-fu-tileable-oilify img inLayer)
  (let* ()
    (tileable-functions img inLayer 7)
  )
)

(script-fu-register "script-fu-tileable-oilify"
                "<Image>/Filters/Artistic/Tileable Oilify..."
          "Wrapper to make Oilify filter seamless for a seamless source."
          "Rob Antonishen"
          "Rob Antonishen"
          "Feb 2009"
          "RGB* GRAY*"
          SF-IMAGE      "image"      0
          SF-DRAWABLE   "drawable"   0
)       

;-----------------------------------------------------------------
; Photocopy
(define (script-fu-tileable-photocopy img inLayer)
  (let* ()
    (tileable-functions img inLayer 8)
  )
)

(script-fu-register "script-fu-tileable-photocopy"
                "<Image>/Filters/Artistic/Tileable Photocopy..."
          "Wrapper to make Photocopy filter seamless for a seamless source."
          "Rob Antonishen"
          "Rob Antonishen"
          "Feb 2009"
          "RGB* GRAY*"
          SF-IMAGE      "image"      0
          SF-DRAWABLE   "drawable"   0
)       

;-----------------------------------------------------------------
; Softglow
(define (script-fu-tileable-softglow img inLayer)
  (let* ()
    (tileable-functions img inLayer 9)
  )
)

(script-fu-register "script-fu-tileable-softglow"
                "<Image>/Filters/Artistic/Tileable Softglow..."
          "Wrapper to make Softglow filter seamless for a seamless source."
          "Rob Antonishen"
          "Rob Antonishen"
          "Feb 2009"
          "RGB* GRAY*"
          SF-IMAGE      "image"      0
          SF-DRAWABLE   "drawable"   0
)       
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
; 
; Rays of Light Glow  for GIMP 2.4
; Copyright (c) 2008 Justin Barca
; justinbarca@gmail.com
;
; Author comment:
;  Creates a shining glow as demonstrated in the tutorial here:
;  http://www.gimpusers.com/tutorials/rays-of-light-behind-text.html
;  This script will only operate on the alpha-layer
;  It assumes that the layer size is the same size as the image size
;  It will not add color to the rays as in the tutorial
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
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (script-fu-rays-of-light image
                                 drawable
                               color
                               wind-thresh
                               wind-strength
                               zoom-length
                               sel-grow
                               sel-blur
                               
                               )
  (let* ((ray-layer)(overlay-layer) (orig-drawable)(orig-foreground)
       )

  (gimp-context-push)

  (gimp-image-set-active-layer image drawable)
  (gimp-image-undo-group-start image)
  
  (gimp-layer-add-alpha drawable)
  (gimp-selection-layer-alpha drawable)
  (set! orig-foreground (car (gimp-palette-get-foreground)))
  (gimp-palette-set-foreground color)
  ;(gimp-palette-set-background '(0 0 0))
  
  ;(gimp-image-set-active-layer image background-layer)
  ;(gimp-edit-fill drawable 1)
  (set! ray-layer (car (gimp-layer-new image (car (gimp-image-width image)) (car(gimp-image-height image)) 1 "Rays" 100 0)))
  (set! orig-drawable drawable)
  (gimp-image-add-layer image ray-layer (car (gimp-image-get-layer-position image orig-drawable )))
  (gimp-image-lower-layer  image ray-layer)  
  (plug-in-colortoalpha 1 image ray-layer (car (gimp-palette-get-background)))
  
  
  (gimp-image-set-active-layer image ray-layer)
  (gimp-edit-fill ray-layer 0)
  ;(gimp-selection-invert image)
  ;(gimp-edit-fill drawable 1)
  (gimp-selection-all image)  
  (plug-in-polar-coords 1 image ray-layer 100 0 0 1 0)
  (gimp-image-rotate image 0)
  (gimp-image-set-active-layer image ray-layer)
  (plug-in-wind 1 image ray-layer wind-thresh 1 wind-strength 0 0)
  (plug-in-wind 1 image ray-layer wind-thresh 0 wind-strength 0 0) 
   
  (gimp-image-rotate image 2)
  (plug-in-polar-coords 1 image ray-layer 100 0 0 1 1)
  ;(gimp-layer-flatten drawable)
  ;(gimp-layer-add-alpha drawable)
  
    ;  (gimp-edit-bucket-fill drawable 1 0 100 0 0 1 1)
;  (gimp-edit-bucket-fill drawable 1 0 100 0 0 1 (/ (car(gimp-image-width image)) 2))
;  (gimp-edit-bucket-fill drawable 1 0 100 0 0 (/ (car(gimp-image-width image)) 2) 1)
;  (gimp-edit-bucket-fill drawable 1 0 100 0 0 (/ (car(gimp-image-width image)) 2) (/ (car(gimp-image-width image)) 2))
  (plug-in-mblur 1 image ray-layer 2 zoom-length 0 (/ (car(gimp-image-width image)) 2) (/ (car(gimp-image-width image)) 2))
  
  
  (set! overlay-layer (car (gimp-layer-new image (car (gimp-image-width image)) (car(gimp-image-height image)) 1 "Outline" 100 0)))
  (gimp-layer-add-alpha overlay-layer)
  (gimp-image-add-layer image overlay-layer (car (gimp-image-get-layer-position image ray-layer )))   
  (gimp-selection-layer-alpha orig-drawable)
  (gimp-selection-grow image sel-grow )
  (gimp-image-set-active-layer image overlay-layer)
  (gimp-edit-fill overlay-layer 0)
  (gimp-selection-all image)
  (plug-in-gauss 1 image overlay-layer sel-blur sel-blur 1)
  
  (set! ray-layer (car (gimp-image-merge-down image overlay-layer 0)))
  ;(set! background-layer (car (gimp-layer-new image (car (gimp-image-width image)) (car(gimp-image-height image)) 0 "Background" 100 1)))
  ;(gimp-image-add-layer image background-layer ( + 1 (car (gimp-image-get-layer-position image ray-layer ))))
      
  
  (plug-in-colortoalpha 1 image ray-layer '(0 0 0))  
  
  (gimp-image-set-active-layer image orig-drawable)
   
  (gimp-palette-set-foreground orig-foreground)
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)

  (gimp-context-pop)
  )
)

(script-fu-register "script-fu-rays-of-light"
  _"_Rays of Light..."
  _"Add radiating rays of light to alpha"
  "Justin Barca <justinbarca@gmail.com>"
  "Justin Barca"
  "2008/03/13"
  "RGB* GRAY*"
  SF-IMAGE      "Image"           0
  SF-DRAWABLE   "Drawable"        0
  SF-COLOR "Glow Color" '(255 255 255)
  SF-ADJUSTMENT _"Wind Threshold"       '(40 0 50 1 10 1 1)
  SF-ADJUSTMENT _"Wind Strength"       '(50 1 100 1 10 1 1)
  SF-ADJUSTMENT _"Zoom Length"    '(40 1 500 1 10 1 1)
  SF-ADJUSTMENT _"Selection Grow"    '(2 0 10 1 10 1 1)
  SF-ADJUSTMENT _"Selection Blur"    '(10 0 100 1 10 1 1)
)

(script-fu-menu-register "script-fu-rays-of-light"
                         "<Image>/Filters/Alpha to Logo")

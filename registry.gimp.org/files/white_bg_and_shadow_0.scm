; blur_and_shadow.scm is a script for The GIMP
;
; The script is located in "<Image> / Script-Fu / Decor / White background & Shadow..."
;
; Last changed: 25 November 2009
;
; Copyright (C) 2009 Guillaume Duwelz-Rebert <gduwelzrebert@gmail.com>
;
; --------------------------------------------------------------------
;
; Changelog:
;
; Version 1.0
; - Initial release
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
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;

(define (script-fu-white_bg_and_shadow theImage theDrawable)

  ; Variable init
  (let* (
      (foregroundWidth  (car (gimp-image-width theImage)))
	  (foregroundHeight (car (gimp-image-height theImage)))
	  (backgroundWidth)
	  (backgroundHeight)
	  (smallBorder)
	  (shadowRatio 0.00833333)
	  (shadowBlur)
	  (shadowSize)
	  
        )

    ; Increase width size by 6%
    (set! backgroundWidth (* 1.06 foregroundWidth))
    
    ; Calculate the number of pixel of the border in the horizontal direction
    (set! smallBorder (/ (- backgroundWidth foregroundWidth) 2))
    
    ; Calculate the background height (with 1 time the small border at the top and 3 times the small boder at the bottom)
    (set! backgroundHeight (+ (* smallBorder 4) foregroundHeight))
    
    (set! shadowBlur (* (* shadowRatio 2) foregroundWidth))
    (set! shadowSize (* shadowRatio foregroundWidth))

;; Save Original picture to Original layer

    ; Rename current picture to "Original"
    (gimp-item-set-name theDrawable "Original")

;; Create a new background layer

    ; Duplicate selected layer
    (define theBackground (car (gimp-layer-new theImage backgroundWidth backgroundHeight RGB-IMAGE "Background" 100 NORMAL-MODE)))
    (gimp-image-insert-layer theImage theBackground 0 0)
    
    ; Increase image size to the background size
    (gimp-image-resize-to-layers theImage)

    ; Make a rectangular selection of the whole background
    (gimp-image-select-rectangle theImage CHANNEL-OP-ADD 0 0 backgroundWidth backgroundHeight)
    
    ; Select the background layer as the active layer
    (gimp-image-set-active-layer theImage theBackground)
    
    ; Choose the white color to be the background color
    (gimp-context-set-background '(255 255 255))
    
    ; fill the rectangular selection with the background color
    (gimp-edit-fill theBackground BACKGROUND-FILL)

    ; Remove any selection done to be able to blur the shadow
    (gimp-selection-none theImage)

    ; Flush the result to update the display
    (gimp-displays-flush)

;; Create a new foreground layer

    ; Duplicate selected layer
    (define theForeground (car (gimp-layer-copy theDrawable TRUE)))
    (gimp-image-insert-layer theImage theForeground 0 0)
    
    ; Rename duplicated picture to "Foreground"
    (gimp-item-set-name theForeground "Foreground")

    ; scale the border layer to image size
    (gimp-layer-resize-to-image-size theForeground)

    ; move the layer to offset smallBorder:smallBorder
    (gimp-layer-set-offsets theForeground smallBorder smallBorder)

    ; scale the border layer to image size
    (gimp-layer-resize-to-image-size theForeground)

    ; Flush the result to update the display
    (gimp-displays-flush)
    
;; Create the foreground shadow

    ; Execute the script-fu that generate a dropped shadow
    (script-fu-drop-shadow theImage theForeground shadowSize shadowSize shadowBlur '(0 0 0) 65 FALSE)

;; Finish the process by selecting the background layer to be the active layer 
    
    ; Select the background layer as the active layer
    (gimp-image-set-active-layer theImage theBackground)

  )
)

(script-fu-register
          "script-fu-white_bg_and_shadow"			;func name
          "White background & Shadow"				;menu label
          "Add a white background to the current picture,\
increase it, and add a shadow to the foreground."		;description
          "Guillaume Duwelz-Rebert"				;author
          "copyright 2009, Guillaume Duwelz-Rebert"		;copyright notice
          "November 25, 2009"					;date created
          ""							;image type that the script works on
	  SF-IMAGE		"Image"		0
          SF-DRAWABLE		"Drawable"	0
        )
        (script-fu-menu-register "script-fu-white_bg_and_shadow" _"<Image>/Filters/Decor")

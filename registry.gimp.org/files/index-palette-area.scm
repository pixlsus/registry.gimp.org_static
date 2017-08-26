; 
; index-palette-area.scm
;
; A trio of palette and colormap tools designed for pixelart use.
;   - Convert an indexed colormap to a GIMP palette
;   - Create a GIMP palette based on a selected area
;   - Convert to indexed colors using a selected area as the colormap

; Copyright (C) 2008 "Stratadrake", strata_ranger@hotmail.com
;
; Version 0.0   07-21-2008  Written in GIMP 2.4.4
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; First function: Exports an indexed image's colormap to a GIMP palette 
;
(define (script-fu-cmap-to-palette image drawable)
(let* (
       (temp-palette (car(gimp-palette-new (car(gimp-image-get-name image)))))
       (cmap (gimp-image-get-colormap image))
       (index 0)
       (color '(255 0 0)) 
       (count (car cmap))
       (colors (cadr cmap))
      )

  (while (< index count)

    ; Compute the color
    (set! color (list(vector-ref colors index)
                     (vector-ref colors (+ 1 index))
                     (vector-ref colors (+ 2 index))
                 ))
    ; Add to palette
    (gimp-palette-add-entry temp-palette (number->string (/ index 3)) color)

    (set! index (+ 3 index))
  ) ; Endwhile

  temp-palette ; Return to caller
)) ; End function
(script-fu-register "script-fu-cmap-to-palette"
		    _"Colormap to palette"
		    "Export an image's colormap to a GIMP palette"
		    "'Stratadrake' (strata_ranger@hotmail.com)"
		    "July 2008"
		    ""
		    "INDEXED*"
		    SF-IMAGE       "Image"               0
		    SF-DRAWABLE    "Drawable"            0
)
(script-fu-menu-register "script-fu-cmap-to-palette"
			 _"<Image>/Colors/Map"
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Second function: Creates a GIMP palette based on a selected image area
;  
(define (script-fu-selection-to-palette image drawable)
(let* ((temp-image -1) (pal-name ""))

  ; Copy to a new image
  (let*(
         (sel-layer (car(gimp-edit-named-copy drawable "script-fu-palette")))
       )
    (set! temp-image (car(gimp-edit-named-paste-as-new sel-layer)))
    (gimp-buffer-delete sel-layer)
    ;(gimp-display-new temp-image) ; For debugging purposes
  )
  
  ; Convert to custom indexed palette, no dithering
  (gimp-image-convert-indexed temp-image 0 0 255 FALSE FALSE "")

  ; Export to a palette (named after the image)
  (set! pal-name (car(gimp-palette-rename (script-fu-cmap-to-palette temp-image 0)
                                      (car(gimp-image-get-name image)) )))

  ; Cleanup
  (gimp-image-delete temp-image)
  
  pal-name ; Return to caller
)) ; End function
(script-fu-register "script-fu-selection-to-palette"
		    _"Save to palette"
		    "Create a palette based on the selection"
		    "'Stratadrake' (strata_ranger@hotmail.com)"
		    "July 2008"
		    ""
		    "RGB* "
		    SF-IMAGE       "Image"               0
		    SF-DRAWABLE    "Drawable"            0
)
(script-fu-menu-register "script-fu-selection-to-palette"
			 _"<Image>/Select"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Third tool:  Converts an entire image to indexed colors, using a selected area as the palette
;
(define (script-fu-image-convert-indexed-using-sel image drawable dither)
(let ((palette (script-fu-selection-to-palette image drawable) )) ; Create the palette first

  ; Convert to the new palette
  (gimp-image-convert-indexed image dither CUSTOM-PALETTE 255 FALSE FALSE palette)

  ; Cleanup
  (gimp-palette-delete palette)
  (gimp-displays-flush)

)) ; End function
(script-fu-register "script-fu-image-convert-indexed-using-sel"
		    _"I_ndexed from selection..."
		    "Convert to indexed colors using a selected area as the palette"
		    "'Stratadrake' (strata_ranger@hotmail.com)"
		    "July 2008"
		    ""
		    "RGB* "
		    SF-IMAGE       "Image"               0
		    SF-DRAWABLE    "Drawable"            0
		    SF-OPTION      "Dithering mode"      '("No dithering"  "Floyd-Steinberg (normal)"  "Floyd-Steinberg (reduced color bleeding)" "Positioned")
        ;SF-TOGGLE      "Allow dithering of transparency" 0
)
(script-fu-menu-register "script-fu-image-convert-indexed-using-sel"
			 _"<Image>/Image/Mode"
)

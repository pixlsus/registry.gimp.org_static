;
; tone-adjust
;
; Copyright (C) 2010 Howard Roberts(howardroberts@comcast.net)
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
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(define (tone-adjust img drawable tone method density flatten)

   (gimp-context-push)
   (gimp-image-undo-group-start img)
   (let* ( 
	   (width (car (gimp-image-width img)))
           (height (car (gimp-image-height img)))
           (old-color (car (gimp-context-get-foreground)))
           (fill-color '(255 255 255))
           (overlay-layer (car (gimp-layer-new img width height RGB-IMAGE "Tone" 100 OVERLAY-MODE)))
           (myChannel (car (gimp-channel-new-from-component img RED-CHANNEL "Value")))
         )
  
  (gimp-image-add-layer img overlay-layer 0)
  (gimp-image-raise-layer-to-top img overlay-layer)
  (if (or (= tone 6)(= method 1))
    (gimp-selection-combine myChannel CHANNEL-OP-REPLACE)
  )
  (cond ((= tone 0)
         (set! fill-color '(0 109 255))
        )
        ((= tone 1)
          (set! fill-color '(0 181 255))
        )
	((= tone 2)
          (set! fill-color '(235 177 19))
        )
	((= tone 3)
          (set! fill-color '(237 138 0))
        )
	((= tone 4)
          (gimp-image-remove-layer img overlay-layer)
          (gimp-color-balance drawable 1 TRUE 0 -5 -20)

        )
	((= tone 5)
          (gimp-image-remove-layer img overlay-layer)
          (let* (
                (warmed-layer (car (gimp-layer-copy drawable FALSE)))
                )
		(gimp-image-add-layer img warmed-layer -1)
		(gimp-colorize warmed-layer 40 50 0)
		(gimp-layer-set-opacity warmed-layer density)
          )
        )
	((= tone 6)
          (set! fill-color '(255 255 255))
        )
  )
  (unless (or (= tone 4)(= tone 5))
    (if (= tone 6)
      (gimp-layer-set-opacity overlay-layer 100)
      (gimp-layer-set-opacity overlay-layer density)
    )
  )
  (cond ((or (< tone 4)(= tone 6))
    (gimp-context-set-background fill-color)
    (gimp-edit-bucket-fill-full overlay-layer BG-BUCKET-FILL NORMAL-MODE 100 255 FALSE FALSE SELECT-CRITERION-COMPOSITE 0 0)
    (gimp-context-set-background old-color)
    )
  )
    (if (= flatten TRUE)
        (gimp-image-flatten img)
    )
(gimp-selection-none img)
 )
    (gimp-displays-flush)
    (gimp-image-undo-group-end img)
    (gimp-context-pop)
)

(script-fu-register "tone-adjust"
                    "Warming or Cooling Filter"
                    "Warm or cool an image using one of several methods"
		    "Howard Roberts <howardroberts@comcast.net>"
                    "(c) 2010 Howard D. Roberts"
                    "May 24,2010"
                    "RGB*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Layer"		0
		    SF-OPTION      "Tone"    '("Cooling - Wratten 80"
					       "Cooling - Wratten 82" 
					       "Warming - Wratten 81" 
					       "Warming - Wratten 85"
					       "Roy's Warm"
					       "Brauer's Warm"
					       "Pasty Cadaveric Look")
		    SF-OPTION	   _"Overlay Fill Method\nApplies only to Wratten filters" '("Fill Entire layer"
						"Fill Red Channel")
		    SF-ADJUSTMENT _"Opacity\nPasty Cadaveric defaults to 100%"	'(25 1 100 0 1 0 0 0)
		    SF-TOGGLE     _"\nFlatten image"		FALSE)
(script-fu-menu-register "tone-adjust"
                         "<Image>/Colors")

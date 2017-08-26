;;  ***************************************************************************
;;  *   Copyright (C) 2008 by James Sambrook                                  *
;;  *   sambrook@va.metrocast.net                                             *
;;  *                                                                         *
;;  *   This program is free software; you can redistribute it and/or modify  *
;;  *   it under the terms of the GNU General Public License as published by  *
;;  *   the Free Software Foundation; either version 2 of the License, or     *
;;  *   (at your option) any later version.                                   *
;;  *                                                                         *
;;  *   This program is distributed in the hope that it will be useful,       *
;;  *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
;;  *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
;;  *   GNU General Public License for more details.                          *
;;  *                                                                         *
;;  *   You should have received a copy of the GNU General Public License     *
;;  *   along with this program; if not, write to the                         *
;;  *   Free Software Foundation, Inc.,                                       *
;;  *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
;;  ***************************************************************************


(define (script-fu-taijitu inSize colorOne colorTwo)

  (let* (
        (theImage (car (gimp-image-new inSize inSize RGB)))
        (baseLayer (car (gimp-layer-new theImage inSize inSize RGB-IMAGE "Taijitu" 100 NORMAL-MODE)))

; Size of the white blur on the eye
	  (halfSize (/ inSize 2.0))
	  (quarterSize (/ inSize 4.0))
	  (smallSize (/ inSize 9.0))
        )

	(gimp-context-push)

; Add the three layers of the image in reverse order so they appear properly
	(gimp-image-add-layer theImage baseLayer 0)

; Set the foreground color to first color
	(gimp-context-set-foreground colorOne)

; Set the background color to random color that will get changed to alpha
	(gimp-context-set-background '(70 80 90))

; Fill the initial image with white
	(gimp-drawable-fill baseLayer 1)

	(gimp-context-set-background colorTwo)

; Top small circle
	(gimp-ellipse-select theImage quarterSize 0 halfSize halfSize 2 1 0 0 )
	(gimp-edit-bucket-fill baseLayer BG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Bottom Small Circle
	(gimp-ellipse-select theImage quarterSize halfSize halfSize halfSize 2 1 0 0 )
	(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Get the right half of the Taijitu
	(gimp-fuzzy-select baseLayer (- inSize 1) (- inSize 1) 0 0 0 0 0 0)
	(gimp-ellipse-select theImage 1 1 (- inSize 1) (- inSize 1) 3 1 0 0 )
	(gimp-selection-grow theImage 1)
	(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Get the left half of the Taijitu
	(gimp-fuzzy-select baseLayer 1 1 0 0 0 0 0 0)
	(gimp-ellipse-select theImage 1 1 (- inSize 1) (- inSize 1) 3 1 0 0 )
	(gimp-selection-grow theImage 1)
	(gimp-edit-bucket-fill baseLayer BG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Small circle in the upper half
	(gimp-ellipse-select theImage (- halfSize (/ smallSize 2.0)) (- quarterSize (/ smallSize 2.0))
		smallSize smallSize 2 1 0 0 )
	(gimp-edit-bucket-fill baseLayer FG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Small circle in the lower half
	(gimp-ellipse-select theImage (- halfSize (/ smallSize 2.0)) (+ halfSize (- quarterSize (/ smallSize 2.0)))
		smallSize smallSize 2 1 0 0 )
	(gimp-edit-bucket-fill baseLayer BG-BUCKET-FILL
		NORMAL-MODE 100 1 TRUE 1 1)
	(gimp-selection-clear theImage)

; Get the transparent background
	(gimp-fuzzy-select baseLayer (- inSize 1) (- inSize 1) 0 0 0 0 0 0)
	(gimp-fuzzy-select baseLayer 1 1 0 0 0 0 0 0)
	(gimp-fuzzy-select baseLayer (- inSize 1) 1 0 0 0 0 0 0)
	(gimp-fuzzy-select baseLayer 1 (- inSize 1) 0 0 0 0 0 0)
	(gimp-layer-add-alpha baseLayer)
	(plug-in-colortoalpha RUN-NONINTERACTIVE theImage baseLayer '(70 80 90))
	(gimp-selection-clear theImage)

; Show the new image.  TAH-DAH!
	(gimp-display-new theImage)
   )
)

; Register the function with GIMP:

(script-fu-register
  "script-fu-taijitu"
  _"_Taijitu..."
  _"Create a Yin-Yang diagram."
  "James Sambrook"
  "9 October 2008"
  "James Sambrook.  King George, VA, USA"
  ""
  SF-ADJUSTMENT _"Image size"   '(640 300 3000 1 50 0 0)
  SF-COLOR      _"Color 1"      '(0 0 0)
  SF-COLOR      _"Color 2"      '(255 255 255)

)

(script-fu-menu-register "script-fu-taijitu"
                         "<Image>/Filters/SambrookJM/")
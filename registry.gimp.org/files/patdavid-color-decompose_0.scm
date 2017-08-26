; Color Decompose v0.2
; Created by Patrick David <patdavid@gmail.com>
; http://blog.patdavid.net
;
; Decompose a color image to channels using various
; decomposition methods.  Will use the (possibly) most
; useful channels for a B&W conversion:
;
; RGB, Value, Lightness, CMY, (cmy)K, L(ab), Luma
;
; v0.2 CHANGES
; REVMOED CMY from output, as they are just inversions of RGB...
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

(define (script-fu-patdavid-color-decompose Image Drawable)

	(let*
	  (
        (tmp 0)
        (lyr 0)
	  )

	  (gimp-image-undo-group-start Image)

      ; RGB
      (define tmp (plug-in-decompose 1 Image Drawable "RGB" 0))

      (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car tmp) )) Image) )
      (gimp-item-set-name (car lyr) "RGB - Red")
      (gimp-image-add-layer Image (car lyr) -1)

      (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car (cdr tmp)) )) Image) )
      (gimp-item-set-name (car lyr) "RGB - Green")
      (gimp-image-add-layer Image (car lyr) -1)
      
      (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car (cdr (cdr tmp))) )) Image) )
      (gimp-item-set-name (car lyr) "RGB - Blue")
      (gimp-image-add-layer Image (car lyr) -1)

      ; HSV - Value
      (define tmp (plug-in-decompose 1 Image Drawable "Value" 0))
      (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car tmp) )) Image) )
      (gimp-item-set-name (car lyr) "HSV - Value")
      (gimp-image-add-layer Image (car lyr) -1)

      ; HSL - Lightness
      (define tmp (plug-in-decompose 1 Image Drawable "Lightness" 0))
      (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car tmp) )) Image) )
      (gimp-item-set-name (car lyr) "HSL - Lightness")
      (gimp-image-add-layer Image (car lyr) -1)

      ; CMY
      ;(define tmp (plug-in-decompose 1 Image Drawable "CMY" 0))

      ;(define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car tmp) )) Image) )
      ;(gimp-item-set-name (car lyr) "CMY - Cyan")
      ;(gimp-image-add-layer Image (car lyr) -1)
      ;(gimp-invert (car lyr) )

      ;(define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car (cdr tmp)) )) Image) )
      ;(gimp-item-set-name (car lyr) "CMY - Magenta")
      ;(gimp-image-add-layer Image (car lyr) -1)
      ;(gimp-invert (car lyr) )
      ;
      ;(define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car (cdr (cdr tmp))) )) Image) )
      ;(gimp-item-set-name (car lyr) "CMY - Yellow")
      ;(gimp-image-add-layer Image (car lyr) -1)
      ;(gimp-invert (car lyr) )

      ; CMYK - K
      (define tmp (plug-in-decompose 1 Image Drawable "CMYK" 0))

      (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car (cdr (cdr (cdr tmp)))) )) Image) )
      (gimp-item-set-name (car lyr) "CMYK - K")
      (gimp-image-add-layer Image (car lyr) -1)
      (gimp-invert (car lyr) )

      ; LAB - L
      (define tmp (plug-in-decompose 1 Image Drawable "LAB" 0))

      (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car tmp) )) Image) )
      (gimp-item-set-name (car lyr) "LAB - L")
      (gimp-image-add-layer Image (car lyr) -1)

      ; YCbCr ITU R470 256
      (define tmp (plug-in-decompose 1 Image Drawable "YCbCr ITU R470 256" 0))

      (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car tmp) )) Image) )
      (gimp-item-set-name (car lyr) "Luma - y470f")
      (gimp-image-add-layer Image (car lyr) -1)

      ; YCbCr ITU R709 256
      (define tmp (plug-in-decompose 1 Image Drawable "YCbCr ITU R709 256" 0))

      (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car tmp) )) Image) )
      (gimp-item-set-name (car lyr) "Luma - y709f")
      (gimp-image-add-layer Image (car lyr) -1)

	  (gimp-displays-flush)



	  (gimp-image-undo-group-end Image)


	  )

)

; Finally register our script with script-fu
(script-fu-register "script-fu-patdavid-color-decompose"
                    "Color Decompose..."
                    "Decompose to useful channels for B&W conversions"
                    "Patrick David <patdavid@patdavid.net>"
                    "Patrick David"
                    "2012-12-10"
                    "RGB*"
			SF-IMAGE	"Image"		0
			SF-DRAWABLE	"Drawable"	0
)

(script-fu-menu-register "script-fu-patdavid-color-decompose" "<Image>/Colors")

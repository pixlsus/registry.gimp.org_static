; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
; http://www.gnu.org/licenses/gpl-3.0.html
;
; Copyright (C) 2011 AdSR
;
; Version 1.2 - Split-toning effect
; Original author: AdSR
; (C) 2011
;
; Tags: photo, split-toning
;
; See: http://gimp-tutorials.net/GIMP-split-toning-tutorial

(define (adsr-split-toning image layer highlights hi-opacity shadows
                           desaturate-orig)
  (gimp-image-undo-group-start image)
 
  (define (add-masked-layer image layer name tint invert-mask)
    (let* ((tint-layer (car (gimp-layer-new image
                                            (car (gimp-image-width image))
                                            (car (gimp-image-height image))
                                            (car (gimp-drawable-type layer))
                                            "Tint" 100 OVERLAY-MODE))))

      (gimp-drawable-set-name layer name)
      (gimp-image-add-layer image layer -1)
      (gimp-desaturate-full layer DESATURATE-LIGHTNESS)

      (gimp-image-set-active-layer image layer)
      (gimp-image-add-layer image tint-layer -1)

      (gimp-context-set-foreground tint)
      (gimp-drawable-fill tint-layer FOREGROUND-FILL)
      (gimp-image-set-active-layer image tint-layer)
      (set! layer
        (car (gimp-image-merge-down image tint-layer CLIP-TO-IMAGE)))

      (let* ((mask (car (gimp-layer-create-mask layer ADD-COPY-MASK))))
        (gimp-layer-add-mask layer mask)
        (if (= invert-mask TRUE)
          (gimp-invert mask)))

      layer))

  (if (<> (car (gimp-image-base-type image)) RGB)
    (gimp-image-convert-rgb image))

  (if (= desaturate-orig TRUE)
    (gimp-desaturate-full layer DESATURATE-LIGHTNESS))

  (let* ((hi-layer (car (gimp-layer-copy layer TRUE)))
         (lo-layer (car (gimp-layer-copy layer TRUE)))
         (original-fg (car (gimp-context-get-foreground))))

    (add-masked-layer image lo-layer "Shadows" shadows TRUE)
    (gimp-layer-set-opacity
      (add-masked-layer image hi-layer "Highlights" highlights FALSE)
      hi-opacity)
    
    (gimp-context-set-foreground original-fg))

  (gimp-image-undo-group-end image)
  (gimp-displays-flush))

(script-fu-register "adsr-split-toning"
                    _"Split-Toning..."
                    _"Rore's split-toning effect."
                    "AdSR (adsr at poczta onet pl)"
                    "Copyright (C) 2011 AdSR"
                    "2011-07-31"
                    "*"
                    SF-IMAGE      "Input image"    0
                    SF-DRAWABLE   "Input drawable" 0
                    SF-COLOR      _"Highlights" '(255 198 00)
                    SF-ADJUSTMENT _"Highlights opacity" '(75 0 100 1 5 0
                                                          SF-SLIDER)
                    SF-COLOR      _"Shadows" '(43 198 255)
                    SF-TOGGLE     _"Desaturate original" FALSE)

(script-fu-menu-register "adsr-split-toning" _"<Image>/Filters/Artistic")

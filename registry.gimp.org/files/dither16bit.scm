; GIMP - The GNU Image Manipulation Program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
; Copyright (C) 2011 Xaffron Software skwong@consultant.com 
;
; This script 
; 1)adds alpha channel to active layer,   
; 2)splits active layer into RGBA,
; 3)converts each layer to 1-, 4-, 5- or 6- bit grayscale w/ FS dither, then
; 4)recomposes it.
; The end effect is that images are "Dithered" to either:
; 12-bit color + 4-bit transparency (ARGB4444/4096colors),  
; 16-bit color + no transparency (RGB565/4096colors), or  
; 15-bit color + 1-bit transparency (ARGB1555/4096colors).  
;
; The higher-depth dither is especially useful for mobile developers who need to pre-dither
; assets for 12- or 16-bit screens.
;
; Version 0.1 Initial Version - No frills, barely works.
; Version 0.2 Renamed to dither16bit.scm - Added RGB565 and ARGB1555.
;
; INSTALL:  
;  Copy the dither16bit.scm file to your scripts dir.
;  Extract the separate 1Bit, 4Bit, 5Bit and 6BitGrayscale.gpl files from the zipfile and copy to your palettes dir.
;  To run, start up GIMP, go to 
;Image->Mode->Dither to ARGB4444
;Image->Mode->Dither to RGB565
;Image->Mode->Dither to ARGB1555
;
; Please email me for any issues... I'm pretty swamped, but I'll try to address!

(define (script-fu-12bit-dither aimg adraw)

  (let* (
		
		(img (car (gimp-drawable-get-image adraw)))
		(draw (car (gimp-image-get-active-layer img)))
        (owidth (car (gimp-image-width img)))
        (oheight (car (gimp-image-height img)))
		)

    (gimp-context-push)

    (gimp-image-undo-group-start img)

	(gimp-layer-add-alpha draw)

	(define layers (plug-in-decompose 1 img draw "RGBA" 0))
	(define red (car layers))
	(define green (cadr layers))
	(define blue (caddr layers))
	(define alpha (cadddr layers))
	
	(gimp-image-convert-rgb red)
	(gimp-image-convert-indexed red 1 4 4096 FALSE FALSE "4-Bit Grayscale")
	(gimp-image-convert-grayscale red)
	
	(gimp-image-convert-rgb green)
	(gimp-image-convert-indexed green 1 4 4096 FALSE FALSE "4-Bit Grayscale")
	(gimp-image-convert-grayscale green)

	(gimp-image-convert-rgb blue)
	(gimp-image-convert-indexed blue 1 4 4096 FALSE FALSE "4-Bit Grayscale")
	(gimp-image-convert-grayscale blue)

	(gimp-image-convert-rgb alpha)
	(gimp-image-convert-indexed alpha 1 4 4096 FALSE FALSE "4-Bit Grayscale")
	(gimp-image-convert-grayscale alpha)

	(gimp-display-new (car (plug-in-compose 1 red 0 green blue alpha "RGBA")))
	(gimp-image-delete red)
	(gimp-image-delete green)
	(gimp-image-delete blue)
	(gimp-image-delete alpha)
	
    (gimp-selection-none img)
	
	
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)

    (gimp-context-pop)
    )
)

(define (script-fu-rgb565-dither aimg adraw)

  (let* (
		
		(img (car (gimp-drawable-get-image adraw)))
		(draw (car (gimp-image-get-active-layer img)))
        (owidth (car (gimp-image-width img)))
        (oheight (car (gimp-image-height img)))
		)

    (gimp-context-push)

    (gimp-image-undo-group-start img)

	(define layers (plug-in-decompose 1 img draw "RGB" 0))
	(define red (car layers))
	(define green (cadr layers))
	(define blue (caddr layers))
	
	(gimp-image-convert-rgb red)
	(gimp-image-convert-indexed red 1 4 4096 FALSE FALSE "5-Bit Grayscale")
	(gimp-image-convert-grayscale red)
	
	(gimp-image-convert-rgb green)
	(gimp-image-convert-indexed green 1 4 4096 FALSE FALSE "6-Bit Grayscale")
	(gimp-image-convert-grayscale green)

	(gimp-image-convert-rgb blue)
	(gimp-image-convert-indexed blue 1 4 4096 FALSE FALSE "5-Bit Grayscale")
	(gimp-image-convert-grayscale blue)

	(gimp-display-new (car (plug-in-compose 1 red 0 green blue 0 "RGB")))
	(gimp-image-delete red)
	(gimp-image-delete green)
	(gimp-image-delete blue)
	
    (gimp-selection-none img)
	
	
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)

    (gimp-context-pop)
    )
)

(define (script-fu-argb1555-dither aimg adraw)

  (let* (
		
		(img (car (gimp-drawable-get-image adraw)))
		(draw (car (gimp-image-get-active-layer img)))
        (owidth (car (gimp-image-width img)))
        (oheight (car (gimp-image-height img)))
		)

    (gimp-context-push)

    (gimp-image-undo-group-start img)

	(gimp-layer-add-alpha draw)

	(define layers (plug-in-decompose 1 img draw "RGBA" 0))
	(define red (car layers))
	(define green (cadr layers))
	(define blue (caddr layers))
	(define alpha (cadddr layers))
	
	(gimp-image-convert-rgb red)
	(gimp-image-convert-indexed red 1 4 4096 FALSE FALSE "5-Bit Grayscale")
	(gimp-image-convert-grayscale red)
	
	(gimp-image-convert-rgb green)
	(gimp-image-convert-indexed green 1 4 4096 FALSE FALSE "5-Bit Grayscale")
	(gimp-image-convert-grayscale green)

	(gimp-image-convert-rgb blue)
	(gimp-image-convert-indexed blue 1 4 4096 FALSE FALSE "5-Bit Grayscale")
	(gimp-image-convert-grayscale blue)

	(gimp-image-convert-rgb alpha)
	(gimp-image-convert-indexed alpha 1 4 4096 FALSE FALSE "1-Bit Grayscale")
	(gimp-image-convert-grayscale alpha)

	(gimp-display-new (car (plug-in-compose 1 red 0 green blue alpha "RGBA")))
	(gimp-image-delete red)
	(gimp-image-delete green)
	(gimp-image-delete blue)
	(gimp-image-delete alpha)
	
    (gimp-selection-none img)
	
	
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)

    (gimp-context-pop)
    )
)

(script-fu-register "script-fu-12bit-dither"
  _"Dither to ARGB_4444..."
  _"Dither to ARGB4444 (4096) colors.  Useful for mobile applications."
  "Sunny Kwong <skwong@consultant.com>"
  "Sunny Kwong"
  "4/27/2011"
  "*"
  SF-IMAGE       "Input image" 0
  SF-DRAWABLE    "Input drawable" 0
)
(script-fu-register "script-fu-rgb565-dither"
  _"Dither to RGB_565..."
  _"Dither to RGB565 (4096) colors.  Useful for mobile applications."
  "Sunny Kwong <skwong@consultant.com>"
  "Sunny Kwong"
  "10/23/2012"
  "*"
  SF-IMAGE       "Input image" 0
  SF-DRAWABLE    "Input drawable" 0
)(script-fu-register "script-fu-argb1555-dither"
  _"Dither to ARGB_1555..."
  _"Dither to ARGB1555 (4096) colors.  Useful for mobile applications."
  "Sunny Kwong <skwong@consultant.com>"
  "Sunny Kwong"
  "10/23/2012"
  "*"
  SF-IMAGE       "Input image" 0
  SF-DRAWABLE    "Input drawable" 0
)
(script-fu-menu-register "script-fu-12bit-dither"
                         "<Image>/Image/Mode")
(script-fu-menu-register "script-fu-rgb565-dither"
                         "<Image>/Image/Mode")
(script-fu-menu-register "script-fu-argb1555-dither"
                         "<Image>/Image/Mode")

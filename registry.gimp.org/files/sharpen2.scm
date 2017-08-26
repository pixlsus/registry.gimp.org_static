;Script for sharpening images based on very simpel layer manipulations
;created by Redphoenix 07.2009

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
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.

;----------------------------------
;changelog

; Version 1.1 added brightness correction to reduce highlighted edges
;----------------------------------

(define	(script-fu-sharp2	 theImage
					theDrawable
					theRadius
                              thePower
                              theBrightness
                              theAlpha

	)
    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    (let* (
	  (base-layer (car (gimp-layer-copy theDrawable TRUE)))
	  (blur-layer (car (gimp-layer-copy theDrawable TRUE)))
	)
      (gimp-image-add-layer theImage base-layer -1)
      (gimp-image-set-active-layer theImage base-layer)
      (gimp-image-add-layer theImage blur-layer -1)
      ;use several blur operations with growing radius
      (plug-in-gauss TRUE theImage blur-layer ( / theRadius 5) ( / theRadius 5) 1)  
      (plug-in-gauss TRUE theImage blur-layer ( / theRadius 3) ( / theRadius 3) 1)      
      (plug-in-gauss TRUE theImage blur-layer ( / theRadius 2) ( / theRadius 2) 1)      
      (plug-in-gauss TRUE theImage blur-layer theRadius theRadius 0)      
    
	(gimp-layer-set-mode blur-layer GRAIN-EXTRACT-MODE)
      (let ((merged(car(gimp-image-merge-down theImage blur-layer 2))))
      ;Contrast the layer to enlarge the effect and correct appearing highlights with a brightness correction
      (gimp-brightness-contrast merged ( - 20 theBrightness) ( - thePower 50))
      (gimp-layer-set-opacity merged theAlpha)
	(gimp-layer-set-mode merged HARDLIGHT-MODE)
      (gimp-image-merge-down theImage merged 2)
      )
   )
    (gimp-displays-flush)

    ;End the undo group
    (gimp-image-undo-group-end theImage)
)

(script-fu-register "script-fu-sharp2"
            _"<Image>/Filters/Enhance/Sharpen2..."
            "Sharpens an image based on simpel layer manipulations"
            "Redphoenix"
            "2009, Redphoenix"
            "July 2009"
            "*"
            SF-IMAGE		"Image"		0
            SF-DRAWABLE         "Drawable"      0
            SF-ADJUSTMENT	_"Radius"	'(10  1 20  1 2 0 0)
            SF-ADJUSTMENT	_"Strength"	'(50  1 150  1 2 0 0)
            SF-ADJUSTMENT	_"Correction"	'(20  0 100  1 2 0 0)            
            SF-ADJUSTMENT	_"Opacity"   '(80  0 100  1 2 0 0)

)
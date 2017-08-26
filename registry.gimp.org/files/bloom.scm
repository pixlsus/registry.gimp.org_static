;Script for blooming images 
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

;----------------------------------

(define	(script-fu-bloom	 theImage
					theDrawable
					theRadius
                              theBlackout
                              theBrightness
                             	)
    ;Start an undo group so the process can be undone with one undo
    (gimp-image-undo-group-start theImage)

    (let* (
	  (blur1-layer (car (gimp-layer-copy theDrawable TRUE)))
	  (blur2-layer (car (gimp-layer-copy theDrawable TRUE)))
	  (blur3-layer (car (gimp-layer-copy theDrawable TRUE)))	
      )
      (gimp-image-add-layer theImage blur1-layer -1)
      (gimp-image-set-active-layer theImage blur1-layer)
      (gimp-image-add-layer theImage blur2-layer -1)
      (gimp-image-set-active-layer theImage blur2-layer) 
      (gimp-image-add-layer theImage blur3-layer -1)     
      ;use several blur operations with growing radius
      (gimp-desaturate-full blur1-layer DESATURATE-LUMINOSITY) 
      (gimp-desaturate-full blur2-layer DESATURATE-LUMINOSITY) 
      (gimp-desaturate-full blur3-layer DESATURATE-LUMINOSITY) 
      (gimp-brightness-contrast blur3-layer ( - -10 theBlackout) ( + theBlackout 47))
      (gimp-brightness-contrast blur2-layer ( - -20 theBlackout) ( + theBlackout 20))
      (gimp-brightness-contrast blur1-layer ( - -47 theBlackout) theBlackout)
    
      (plug-in-gauss TRUE theImage blur3-layer theRadius theRadius 0)  
      (plug-in-gauss TRUE theImage blur2-layer ( * theRadius 3) ( * theRadius 3) 0)      
      (plug-in-gauss TRUE theImage blur1-layer ( * theRadius 7) ( * theRadius 7) 0)      
  
	(gimp-layer-set-mode blur3-layer SCREEN-MODE)
      (gimp-layer-set-opacity blur3-layer (- theBrightness 30))     
      (let ((merged(car(gimp-image-merge-down theImage blur3-layer 2))))
	 (gimp-layer-set-mode merged SCREEN-MODE)
       (gimp-layer-set-opacity merged (- theBrightness 10))     
       (let ((merged2(car(gimp-image-merge-down theImage merged 2))))
	  (gimp-layer-set-mode merged2 SCREEN-MODE)
       (gimp-layer-set-opacity merged2 theBrightness)     
        (gimp-image-merge-down theImage merged2 2)
        )
      )
   )
    (gimp-displays-flush)

    ;End the undo group
    (gimp-image-undo-group-end theImage)
)

(script-fu-register "script-fu-bloom"
            _"<Image>/Filters/Light and Shadow/Bloom..."
            "Adds lightblurring to the image"
            "Redphoenix"
            "2009, Redphoenix"
            "July 2009"
            "*"
            SF-IMAGE		"Image"		0
            SF-DRAWABLE         "Drawable"      0
            SF-ADJUSTMENT	_"Radius"	'(30  5 60  1 2 0 0)
            SF-ADJUSTMENT	_"Blackout"	'(80  1 80  1 2 0 0)
            SF-ADJUSTMENT	_"Brightness"	'(100  50 100  1 2 0 0)            

)
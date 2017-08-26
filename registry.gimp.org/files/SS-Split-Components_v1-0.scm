
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version. http://www.gnu.org/copyleft/gpl.html
;  
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;  

; There are several ways an image can be separated into different components so that selective
; alterations can be done to improve sharpening, eliminate blemishes, clean up scratches, etc.

; Frequency separation is where a low-pass and a high-pass filter is applied to the image
; The frequency-separated image is useful for portrait photography 

; Color/Luminance separation leaves the colors of the image on one layer with a 'flat' value
; and the other layer carries luminance values with 'flat' grayscale information
; This is useful for landscape photography.
; http://pixinsight.com/doc/legacy/LE/14_color_spaces/why_separate/why_separate.html

(define (script-fu-frequency-separation img layer)


; sequence of operation
; create a new-from-visible layer called "temporary" and drop it into a new layer group.
; Copy the new layer from visible, call it 'low pass', blur it by a reasonable amount, 
; and set the mode to 'Grain Extract'. The two layers will interact to produce a (mostly) greyscale 
; image with all the edges visible.
; Create another layer-from-visible and call it 'high pass'. set the mode to 'grain merge'
; Set the mode for layer 'low pass' back to normal and delete 'temporary'.
    
		(gimp-image-undo-group-start img)	; Always good practice to undo all the steps of a script at once
		
		(let* (
			(ss-layer-group (car (gimp-layer-group-new img)))
			(ss-layer-temp (car (gimp-layer-new-from-visible img img "Temporary")))
			(ss-layer-lowpass (car (gimp-layer-new-from-visible img img "Low Pass")))
			(ss-layer-highpass 0) ; created later...
			(blur-radius (/ (car (gimp-image-width img)) 160))
			)

			(gimp-item-set-name ss-layer-group "Frequency Separation")
			(gimp-image-insert-layer img ss-layer-group 0 -1)			
			(gimp-image-insert-layer img ss-layer-temp ss-layer-group -1)				
			(gimp-image-insert-layer img ss-layer-lowpass ss-layer-group -1)	
			(gimp-layer-set-mode ss-layer-lowpass GRAIN-EXTRACT-MODE)
			; in interactive mode, the script will wait for the user to adjust the blur radius to suit.
			; a reasonable radius to start with is about image-width/160
			(plug-in-gauss-iir2 RUN-INTERACTIVE img ss-layer-lowpass blur-radius blur-radius)  ;{ RUN-INTERACTIVE (0), RUN-NONINTERACTIVE (1) }
			(set! ss-layer-highpass (car (gimp-layer-new-from-visible img img "High Pass")))
			(gimp-image-insert-layer img ss-layer-highpass ss-layer-group -1)	
			(gimp-layer-set-mode ss-layer-highpass GRAIN-MERGE-MODE)
			(gimp-layer-set-mode ss-layer-lowpass NORMAL-MODE)
			
			(gimp-image-remove-layer img ss-layer-temp)		
			(gimp-image-set-active-layer img ss-layer-group)

			(gimp-displays-flush)
		   (gimp-image-undo-group-end img) 		

	)
)




(define (script-fu-col-lum-separation img layer)


; sequence of operation
; create a new-from-visible layer called "temporary" and drop it into a new layer group.
; Copy the new layer from visible, call it 'luminosity', desaturate with luminosity, 
; and set the mode to 'Grain Extract'. The two layers will interact to produce a ghostly image
; that carries the colour information from the image.
; Create another layer-from-visible and call it 'color'. set the mode to 'grain merge'
; Set the mode for layer 'luminosity' back to normal and delete 'temporary'.
    
		(gimp-image-undo-group-start img)	; Always good practice to undo all the steps of a script at once
 	
		(let* (
			(ss-layer-group (car (gimp-layer-group-new img)))
			(ss-layer-temp (car (gimp-layer-new-from-visible img img "Temporary")))
			(ss-layer-luminosity (car (gimp-layer-new-from-visible img img "Luminosity")))
			(ss-layer-color 0) ; created later...
			)

			(gimp-item-set-name ss-layer-group "Col/Lum Separation")
			(gimp-image-insert-layer img ss-layer-group 0 -1)			
			(gimp-image-insert-layer img ss-layer-temp ss-layer-group -1)				
			(gimp-image-insert-layer img ss-layer-luminosity ss-layer-group -1)	
			(gimp-layer-set-mode ss-layer-luminosity GRAIN-EXTRACT-MODE)
			(gimp-desaturate-full ss-layer-luminosity DESATURATE-LUMINOSITY) ; { DESATURATE-LIGHTNESS (0), DESATURATE-LUMINOSITY (1), DESATURATE-AVERAGE (2) }
			(set! ss-layer-color (car (gimp-layer-new-from-visible img img "Color")))
			(gimp-image-insert-layer img ss-layer-color ss-layer-group -1)	
			(gimp-layer-set-mode ss-layer-color GRAIN-MERGE-MODE)
			(gimp-layer-set-mode ss-layer-luminosity NORMAL-MODE)
			
			(gimp-image-remove-layer img ss-layer-temp)		
			(gimp-image-set-active-layer img ss-layer-group)

			(gimp-displays-flush)
		   (gimp-image-undo-group-end img) 

	)
)

;==========================================

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version. http://www.gnu.org/copyleft/gpl.html
;  
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;  


(script-fu-register 
			"script-fu-frequency-separation"
			"<Image>/Filters/Generic/Split/Frequency Separation"
			"Splits the visible image into a high-frequency and a low-frequency layer, and leaves them in a new layer group. The script will ask for your opinion about a gaussian blur. The preset value is usually close to optimal."
			"savvysaffer.deviantart.com"
			"Copyright Mike Ochtman"
			"Ver 1.0 Aug 2012"
			"*" ; image type "", "*", "RGB"...
			SF-IMAGE "Image" 0
			SF-DRAWABLE "Layer" 0
)

(script-fu-register 
			"script-fu-col-lum-separation"
			"<Image>/Filters/Generic/Split/Color and Luminosity Separation"
			"Splits the visible image into a luminosity and color layer, and leave them in a new layer group."
			"savvysaffer.deviantart.com"
			"Copyright Mike Ochtman"
			"Ver 1.0 Aug 2012"
			"*" ; image type "", "*", "RGB"...
			SF-IMAGE "Image" 0
			SF-DRAWABLE "Layer" 0
)
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version. http://www.gnu.org/copyleft/gpl.html
;  
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;  



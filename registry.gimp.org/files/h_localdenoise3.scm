; Script to apply denoising selectively, so that to preserve small details.
; Writen by Wilfrid TETARD 2011
;; ----------------------------------------------------------------
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
; along with this program; if not, you can view the GNU General Public
; License version 3 at the web site http://www.gnu.org/licenses/gpl-3.0.html
; Alternatively you can write to the Free Software Foundation, Inc., 675 Mass
; Ave, Cambridge, MA 02139, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-h_localdenoise3
				theOldImage
				theOldLayer
				strinutile
				Methode
				LNoise1
				strinutile2
				findet
				Chromi
				debR
				debB
				strinutile3
				aidecont
				flatim
				helphelp
				
				
	)

	; Initialize an undo, so the process can be undone with a single undo
    	(gimp-image-undo-group-start theOldImage)

	; Variable init
  (let* (
	(theImageYcbcr 0)
	(theLayer-R 0)
	(theLayer-B 0)
	(theLayer-Y 0)
	(dif-layer 0)
	(aidcont-layer)
	(denoised-layer)
	(contfin-layer)
	(contnet-layer)
	(numLayers)
	(layersYcbcr)
	(layerIds)
	(numLayers2)
	(layerIds2)
	(numLayers3)
	(layerIds3)
	(numLayers4)
	(layerIds4)
	(numLayers5)
	(layerIds5)
	(numLayers6)
	(layerIds6)
	(Masque1)
	(theImageB)
	(theImageR)
	(theImageYcbcr)
	(LNoise)
	)
	

	; Initialize some layers
	(set! denoised-layer (car (gimp-layer-copy theOldLayer 1)))
	(set! contfin-layer (car (gimp-layer-copy theOldLayer 1)))
	(gimp-drawable-set-name denoised-layer "Denoised")
	(gimp-image-add-layer theOldImage denoised-layer 0)

	; Reduce Chroma noise. From the script 'dechroma.scm' written by Jean-Pierre Bucciol
	(if (> Chromi 0)
	(begin
	(define imageLAB (car (plug-in-decompose 1 theOldImage denoised-layer "LAB" 1)))
	(define layersLAB (gimp-image-get-layers imageLAB))
	(define layerA (aref (cadr layersLAB) 1))
	(define layerB (aref (cadr layersLAB) 2))
	(plug-in-gauss 1 imageLAB layerA Chromi Chromi 0)		
	(plug-in-gauss 1 imageLAB layerB Chromi Chromi 0)	
	(plug-in-recompose 1 imageLAB denoised-layer)	
	(gimp-image-delete imageLAB)
	)
	)

	; Give explanations
	(if (= helphelp TRUE)
	(begin 
	(if (= Methode 0)
	(gimp-message "With Wavelet-Denoise Plugin, raise denoise level until there is no more visible noise (generally : 1.5 with old plugin, 1500 with the newest). Don't care about lost details. Select 'Grey' as preview channel, 'Y' channel and 'YCbCr'.  Wavelet Denoise plugin has to be installed first. You can download it at : http://registry.gimp.org/node/4235")
	)
	(if (= Methode 2)
	(gimp-message "In G'mic/Enhancement, select Patch-based smoothing or Anisotropic Smoothing. Very slow plugin. G'Mic plugin has to be installed first. You can download it at : http://gmic.sourceforge.net/gimp.shtml")
	)
	(if (= Methode 1)
	(gimp-message "Film Degrain plugin leaves more noise but preserves value channel better than Wavelet Denoise. You can download it at : http://registry.gimp.org/node/25432 for Windows or http://members.ozemail.com.au/~hodsond/degrain.html else")
	) 
	)
	)

	; Building denoised image

	; With Wavelet Denoise plugin :
	(if (= Methode 0)
	(plug-in-wavelet-denoise RUN-INTERACTIVE theOldImage denoised-layer)
	)

	; With Anisotropic smoothing or Patch-based smoothing
	(if (= Methode 2)
	(plug-in-gmic 0 theOldImage denoised-layer 1
	(string-append
						"-v - " 
						"-gimp_patch_smoothing 9,7,3,9,1,1,0,0"
					)
				)		
	)

	; With "L" channel blur
	(if (= Methode 3)
	(begin
	(define imageLAB2 (car (plug-in-decompose 1 theOldImage denoised-layer "LAB" 1)))
	(define layersLAB2 (gimp-image-get-layers imageLAB2))
	(define layerL (aref (cadr layersLAB2) 0))
	(plug-in-gauss 1 imageLAB2 layerL (* 3.5 LNoise1) (* 3.5 LNoise1) 0)			
	(plug-in-recompose 1 imageLAB2 denoised-layer)	
	(gimp-image-delete imageLAB2)
	)
	)

	; with film degrain plugin
	(if (= Methode 1)
	(begin
	(plug-in-filmdegrain RUN-NONINTERACTIVE theOldImage denoised-layer (* 5 LNoise1) (* 5 LNoise1) (* 5 LNoise1))
	(plug-in-filmdegrain RUN-NONINTERACTIVE theOldImage denoised-layer (* 5 LNoise1) (* 5 LNoise1) (* 5 LNoise1))
	(plug-in-filmdegrain RUN-NONINTERACTIVE theOldImage denoised-layer (* 5 LNoise1) (* 5 LNoise1) (* 5 LNoise1))
	)
	)
	
	(gimp-drawable-set-name contfin-layer "Edges")
	(gimp-image-add-layer theOldImage contfin-layer 0)
	(set! theImageYcbcr (car (plug-in-decompose 1 theOldImage contfin-layer "YCbCr_ITU_R470" 1)))
	

	; If required, help for edges detection
	(if (> aidecont 0) 
	(begin
	(set! aidcont-layer (car (gimp-layer-copy contfin-layer 1)))
	(gimp-drawable-set-name aidcont-layer "Detection help")
	(gimp-image-add-layer theOldImage aidcont-layer 0)
	;(plug-in-retinex 1 theOldImage aidcont-layer 240 3 0 1.2) 
	(gimp-equalize aidcont-layer FALSE)
	(gimp-layer-set-opacity aidcont-layer (/ aidecont 2))
	(gimp-image-merge-down theOldImage aidcont-layer 0)
	)
	)
	
	; Detect edges
	(set! numLayers (car (gimp-image-get-layers theOldImage)))
	(set! layerIds (cadr (gimp-image-get-layers theOldImage)))
	(plug-in-c-astretch 1 theOldImage (vector-ref layerIds 0))
	(define imgaide (car (gimp-layer-copy (vector-ref layerIds 1) 1)))
	(plug-in-gauss RUN-NONINTERACTIVE theOldImage (vector-ref layerIds 0) (+ 1 (/ LNoise1 4)) (+ 1 (/ LNoise1 4))  0)
	(gimp-drawable-set-name imgaide "Denoise edges")
	(gimp-image-add-layer theOldImage imgaide 1)
	;(gimp-curves-spline imgaide 0 10 #(0 0 70 70 104 160 170 170 255 255))
	(set! numLayers6 (car (gimp-image-get-layers theOldImage)))
	(set! layerIds6 (cadr (gimp-image-get-layers theOldImage)))
	(gimp-layer-set-mode (vector-ref layerIds6 0) 12)
	(gimp-image-merge-down theOldImage (vector-ref layerIds6 0) 0)
	(set! numLayers2 (car (gimp-image-get-layers theOldImage)))
	(set! layerIds2 (cadr (gimp-image-get-layers theOldImage)))
	(plug-in-edge RUN-NONINTERACTIVE theOldImage (vector-ref layerIds2 0) 3 1 0)
	(gimp-desaturate-full (vector-ref layerIds2 0) 0)
	(gimp-invert (vector-ref layerIds2 0))
	(gimp-levels (vector-ref layerIds2 0) 0 (+ 170 (* 5 findet)) 255 (- 0.8 (/ findet 20)) 0 255)
	
	; Enhance denoise on 'Blue' or 'Red' channel
	(set! layersYcbcr (gimp-image-get-layers theImageYcbcr))
	(set! theLayer-B (aref (cadr layersYcbcr) 1))
	(set! theLayer-R (aref (cadr layersYcbcr) 2))
	(set! theLayer-Y (aref (cadr layersYcbcr) 0))
	
	; Enhance denoise on 'Blue' channel
	(if(> debB 0)
	(begin
	(set! theImageB (car (gimp-layer-new-from-drawable theLayer-B theOldImage)))
	(gimp-image-add-layer theOldImage theImageB 0)
	(gimp-drawable-set-name theImageB "B channel")
	(gimp-levels-stretch theImageB)
	(gimp-layer-set-opacity theImageB (* 0.75 debB))
	(gimp-layer-set-mode theImageB 10)
	(gimp-image-merge-down theOldImage theImageB 0)
	)
	)
	
	; Enhance denoise on 'Red' channel
	(if(> debR 0)
	(begin
	(set! theImageR (car (gimp-layer-new-from-drawable theLayer-R theOldImage)))
	(gimp-image-add-layer theOldImage theImageR 0)
	(gimp-drawable-set-name theImageR "R channel")
	(gimp-levels-stretch theImageR)
	(gimp-layer-set-opacity theImageR (* 0.75 debR))
	(gimp-layer-set-mode theImageR 10)
	(gimp-image-merge-down theOldImage theImageR 0)
	)
	)
	
	
	; Building final layer's mask for denoised layer
	(set! numLayers3 (car (gimp-image-get-layers theOldImage)))
  	(set! layerIds3 (cadr (gimp-image-get-layers theOldImage)))
	(plug-in-gauss RUN-NONINTERACTIVE theOldImage (vector-ref layerIds3 0) (* 2 LNoise1) (* 2 LNoise1)  0)
	(set! Masque1 (car(gimp-layer-create-mask (vector-ref layerIds3 0) 5)))
	(gimp-image-remove-layer theOldImage (vector-ref layerIds3 0))
	(gimp-layer-add-mask denoised-layer Masque1)
	(if (= Methode 2) 
	(gimp-layer-set-opacity denoised-layer 65)
	)
	(gimp-image-delete theImageYcbcr)
	(gimp-image-undo-group-end theOldImage)

    	; Renew display
    	(gimp-displays-flush)

	; If required, flatten image
	(if (= flatim TRUE)
	(set! theOldLayer (gimp-image-flatten theOldImage))
	(if (= helphelp TRUE) (gimp-message "You may now paint locally the 'Denoised' layer's mask : in black to recover some details, in white to remove some remaining noise"))
	)
	
  )
)

(script-fu-register "script-fu-h_localdenoise3"
	     _"<Image>/Harry's plugins/Noise/Harry's denoising 1.08"
           "Script to apply denoising selectively, so that to preserve small details."
	    "To use this script, you may have to install first Wavelet Denoising plugin, downloadable at http://registry.gimp.org/node/4235, or G'Mic for Gimp, downloadable at http://gmic.sourceforge.net/gimp.shtml."
            "Wilfrid TETARD"
            "14.05.2011"
            "*"
            SF-IMAGE        "Image"     0
            SF-DRAWABLE     "Drawable"  0
	    SF-STRING	    "   : ----------------------------------"  "                                  Essential settings :"
	    SF-OPTION	    "Noise Reduction method"
	    '(
	    "Wavelet Denoise plugin (has to be installed first)"
	    "Film Degrain plugin (has to be installed first)"
	    "G'mic's Smoothing (G'mic has to be installed first)"
	    "'L' channel blurring. No plugin required. Less effective."
	    ) 
	    SF-ADJUSTMENT	"Size of the noise to remove"        '(5  1 20 1 1 0 0)
	    SF-STRING	    "   : ----------------------------------"  "                          'In case you need it' settings :"
	    SF-ADJUSTMENT   _"Preserve small sized details (more noise)"     '(5 0 10 1 1 0 0)
	    SF-ADJUSTMENT   _"Reduce chroma noise"	'(0 0 30 1 1 0 0)
	    
	    SF-ADJUSTMENT   _"Enhance denoise on 'Red' channel" '(0 0 100 1 1 0 0)
	    SF-ADJUSTMENT   _"Enhance denoise on 'Blue' channel" '(0 0 100 1 1 0 0)
	    SF-STRING	    "   : ----------------------------------"  "                          Emergency setting (uncertain) :"
	
	    SF-ADJUSTMENT   _"Edges detection issue" '(0 0 100 1 1 0 0)
	    SF-TOGGLE _"Flatten image" FALSE
	    SF-TOGGLE _"Display advice" TRUE
           
)



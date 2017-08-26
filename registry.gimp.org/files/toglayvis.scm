;; toglayvis.scm -*-scheme-
;; toggles the visibility of layers
;; version 0.6    2005-11-15
;; version 0.7    2007-12-31
;; changes made in 0.7:
;; replaced:
;; gimp-image-undo-disable and gimp-image-undo-enable
;; by:
;; gimp-image-undo-freeze and gimp-image-undo-thaw 
;; changed menu register from:
;; _"<Image>/Script-Fu/Test TogLayVis"
;; to:
;; _"<Image>/Layer/Stack/_Toggle Visibility"
;; 
;; purpose of the script:
;; sometimes i want to see the (often slight) difference
;; in the influence that one ore more layers have to the whole image.
;; while switching off the ones and then switching on the others
;; the impression gets lost of that was has been before switching. 
;; this script toggles the visibility of selected layers.
;; with each call the visibility toggles
;; i use a shortcut (shift-ctrl-a) to call the script.
;; the layers are selected by enabling the anchor (link) button in the
;; layers dialog.
;;
;; Copyright (C) 2005 by Michael Hoelzen <MichaelHoelzen@aol.com>
;; http://www.remoserv.de
;;
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
(define 
    (script-fu-toglayvis image drawable)
	(let* (
			(tlv-layers (gimp-image-get-layers image))
			(tlv-num-layers (car tlv-layers))
			(tlv-layer-array (cadr tlv-layers))
			(count 0)
		  )
		  (gimp-image-undo-freeze image)
		  (while (< count tlv-num-layers)
				(let* (
						(tlv-layerid (aref tlv-layer-array count))
						(tlv-linked (car (gimp-drawable-get-linked tlv-layerid)))
						(tlv-visible (car (gimp-drawable-get-visible tlv-layerid)))
				      )
					  (if (= tlv-linked TRUE)
						(if (= tlv-visible TRUE)
							(gimp-drawable-set-visible tlv-layerid FALSE)
							(gimp-drawable-set-visible tlv-layerid TRUE))
					  )
					(set! count (+ count 1))
				)
			)
			(gimp-displays-flush)
			(gimp-image-undo-thaw image)
	)
)
;
(script-fu-register "script-fu-toglayvis"
					_"_Toggle Visibility"
                    "Toggles layer visibility"
                    "Michael Hoelzen>"
                    "Michael Hoelzen"
                    "2005"
                    ""
                    SF-IMAGE		"Image"			0
                    SF-DRAWABLE		"Drawable"		0)
;
(script-fu-menu-register "script-fu-toglayvis"
                         _"<Image>/Layer/Stack")
;

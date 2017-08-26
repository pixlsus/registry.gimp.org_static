; The GIMP -- an image manipulation program
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
; AllLayersFitToImageSize.scm  version 0.1  08/12/2011
;
; Copyright (C) 2011 vanca
;  

(define (AllLayersFitToImageSize img drawable onlyVisible)

  (let* (   
            (nroflayers 0)
            (layernames '())
            
            (index1 0)
            (isCurrentLayerVisible #f)

        )

    (gimp-context-push)

    ; Start the undo group
    (gimp-undo-push-group-start img)

    ; get 
    ; 1) the nr of layers
    ; 2) their names (in an array)
    (set! nroflayers (car (gimp-image-get-layers img)))
    (set! layernames (cadr (gimp-image-get-layers img)))

    ; main process
    (while (< index1 nroflayers)

      ; TO GET EACH LAYER USE: (aref layernames index1)
      ; process them between --START-- and --END--

      ; --START--

      ; convert to boolean
      (set! isCurrentLayerVisible (= 1 (car (gimp-drawable-get-visible (aref layernames index1)))))
      
      (when (and (= onlyVisible 1) isCurrentLayerVisible)
            (gimp-layer-resize-to-image-size (aref layernames index1)))

      (when (= onlyVisible 0)
            (gimp-layer-resize-to-image-size (aref layernames index1)))

      ; --END--

    (set! index1 (+ 1 index1)))

    ; Handle undo
    (gimp-undo-push-group-end img)

    ; refresh display
    (gimp-displays-flush)

    (gimp-context-pop)))

(script-fu-register "AllLayersFitToImageSize"
                    _"_AllLayersFitToImageSize..."
                    "Fit all layers to image size"
                    "vanca"
                    "vanca"
                    "2011"
                    ""
                    SF-IMAGE        "Image"                 0
                    SF-DRAWABLE     "Drawable"              0
                    SF-TOGGLE       "Only Visible"          0
                    )


(script-fu-menu-register "AllLayersFitToImageSize"
			 _"<Toolbox>/Xtns/Vanca")

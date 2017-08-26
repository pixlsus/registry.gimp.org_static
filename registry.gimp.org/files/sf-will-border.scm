;;  Shadow Glow - This is a script for The GIMP that creates glowing borders around selections
;;  Copyright (C) 2010  William Morrison
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.


(define (script-fu-border-glow img layer width feather back?)
 (let* ((orig-select 0) (new-select 0) (new-layer 0) (noalpha? 0))
 (set! orig-select (car (gimp-selection-save img)))
 (gimp-image-undo-group-start img)
 ;If there is no selection, set selection to the active layer's alpha
 (cond
 ((= 1 (car (gimp-selection-is-empty img))) 
  (cond
   ((= 0 (car (gimp-drawable-has-alpha layer))) 
     (gimp-message "Active layer does not have an alpha channel. Select a region") (set! noalpha? 1) (gimp-selection-none img) (gimp-image-remove-channel img orig-select))
   (else 
    (gimp-image-remove-channel img orig-select)
    (gimp-selection-layer-alpha layer)
    (set! orig-select (car (gimp-selection-save img)))
   )
  )
 )
 )

 ;If there's still no selection, (layer is transparent) warn the user and quit
 (cond
 ((and (= 0 noalpha?) (= 1 (car (gimp-selection-is-empty img)))) (gimp-message "Active layer's alpha channel is transparent. Select a region") (gimp-image-remove-channel img orig-select))
 
 ;Otherwise, continue with the script.
 ((= 0 noalpha?)
  

  (gimp-selection-border img (* 2 width))
  (gimp-selection-feather img (* feather width))
  (set! new-select (car (gimp-selection-save img)))
  (gimp-channel-combine-masks new-select orig-select 1 0 0)
  (gimp-selection-load new-select)

  (set! new-layer (car (gimp-layer-new img (car (gimp-image-width img)) (car (gimp-image-height img)) 1 "Glow Layer" 100 0)))
  (gimp-image-add-layer img new-layer -1)
  
  (cond
  ((= back? 0) (gimp-drawable-fill new-layer 3))
  (else (gimp-drawable-fill new-layer 1))
  )

  (gimp-edit-fill new-layer 0)

  (gimp-selection-load orig-select)
  (gimp-image-remove-channel img orig-select)
  (gimp-image-remove-channel img new-select)
 )
 )
  (gimp-image-undo-group-end img)
  ; Update the display
  (gimp-displays-flush)
 
 
 )
)
(script-fu-register "script-fu-border-glow"
                    "Border Glow..."
                    "Adds a border in the foreground colour around the current selection on a new layer"
                    "Will Morrison"
                    "GNU General Public License"
                    "2010"
                    "RGB*"
                    SF-IMAGE    "Image"         0
		    SF-DRAWABLE "Layer"		0
		    SF-ADJUSTMENT "Width" 	'(25 1 250 1 10 0 0)
		    SF-ADJUSTMENT "Feathering Factor" '(2 0 10 0.1 1 2 0)
                    SF-TOGGLE	"Use Background?" 0
)
(script-fu-menu-register "script-fu-border-glow"
                         "<Image>/Filters/Will's Script Pack")

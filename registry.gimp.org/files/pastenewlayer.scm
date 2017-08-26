; GIMP Paste to new layer
; Copyright (c) 2005-2008 Jonathan Stipe
; JonStipe@prodigy.net

; ---------------------------------------------------------------------

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (script-fu-paste-as-new-layer img
			    drawable)
  (gimp-image-undo-group-start img)
  (let* ((draw (car (gimp-image-get-active-drawable img))))
    (cond 
      ((= (car (gimp-drawable-is-layer draw)) 1)
       (gimp-floating-sel-to-layer (car (gimp-edit-paste draw 1))))
      ((= (car (gimp-drawable-is-layer-mask draw)) 1)
       (gimp-floating-sel-to-layer (car (gimp-edit-paste (car (gimp-image-get-active-layer img)) 1))))
      ((= (car (gimp-drawable-is-channel draw)) 1)
       (gimp-floating-sel-to-layer (car (gimp-edit-paste (vector-ref (cadr (gimp-image-get-layers img)) 0) 1))))
    )
  )
  (gimp-displays-flush)
  (gimp-image-undo-group-end img)
)

(script-fu-register "script-fu-paste-as-new-layer"
		    _"<Image>/Edit/Paste as/New _Layer"
		    "Pastes into a new layer."
		    "Jonathan Stipe <JonStipe@prodigy.net>"
		    "Jonathan Stipe"
		    "February 2008"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0)
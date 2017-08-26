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
; ActivePathStrokeDelete.scm  version 0.1  16/04/2011
;
; Copyright (C) 2011 vanca
;  


(define (ActivePathStrokeDelete    imgOrig 
                                   imgDraw)

  (let ((active-vectors (car (gimp-image-get-active-vectors imgOrig))))

    (gimp-message-set-handler 0)

    (gimp-context-push)

    ; Start the undo group
    (gimp-undo-push-group-start imgOrig)
    
    ; main process
    
    (cond 
      ((= -1 active-vectors)

       (gimp-message "There are no active vectors!"))

      (else

        (gimp-edit-stroke-vectors
              imgDraw
              active-vectors)

        (gimp-image-remove-vectors 
              imgOrig
              active-vectors)))

    ; Handle undo
    (gimp-undo-push-group-end imgOrig)

    ; refresh display
    (gimp-displays-flush)

    (gimp-context-pop)))

    (script-fu-register "ActivePathStrokeDelete"
                        _"_ActivePathStrokeDelete..."
                        "Active Path Stroke Delete"
                        "vanca"
                        "vanca"
                        "2011"
                        ""
                        SF-IMAGE    "Image"                     0
                        SF-DRAWABLE "Input drawable"            0
                        )

    (script-fu-menu-register "ActivePathStrokeDelete"
                 _"<Toolbox>/Xtns/Vanca")

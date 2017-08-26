;
; subtle-vignette.scm
;
; Copyright 2013 Glenn Wadsworth
; gwads71@gmail.com
;
; *********************************************************************
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
; *********************************************************************
;
; This script places a subtle reduction in brightness at the periphery
; of an image using a radial gradient.  The method is non-destructive 
; and customizable in that it creates a new layer that can be deleted 
; or modified to suit personal preference.  The intent is to save a 
; few button clicks on a frequently repeated processing task.
;
; The method:
; (1) duplicate the active layer
; (2) set the new layer's mode to "multiply"
; (3) add a layer mask to the new layer
; (4) draw a radial gradient on the mask from the center
;     (using an offset value of 40)
; (5) set the new layer's opacity to 60
;
; Adjust the layer opacity in the layers dialog to suit your personal 
; preference (or change the opacity value below).
;
; After copying this file to ~/.gimp-2.x/scripts/, restart GIMP and
; look for Filters->Light and Shadow->Subtle Vignette.
;
; References:
; GIMP Guru: "Simple Vignetting Using the GIMP"
;    http://gimpguru.org/tutorials/vignetting/
; Luminous Landscape: "Digitally Simulating a Center ND Filter"
;    http://www.luminous-landscape.com/tutorials/panorami.shtml
;
; Glenn Wadsworth
; gwads71@gmail.com
; http://www.flickr.com/photos/glennwadsworth
; Jan. 2013
;

(define (subtle-vignette image drawable)

   ; Local variables

   (let* (
        (opacity 60)         ; layer opacity -- adjust as desired
        (active-layer  (car (gimp-image-get-active-drawable image)))
        (width         (car (gimp-drawable-width active-layer)))
        (height        (car (gimp-drawable-height active-layer)))
        (vignette      (car (gimp-layer-copy active-layer TRUE)))
        (vignette-mask (car (gimp-layer-create-mask vignette ADD-WHITE-MASK)))
        (x1 (/ width 2))     ; start in the center
        (y1 (/ height 2))
        (x2 x1)              ; draw to 5% beyond the height
        (y2 (* height 1.05))
        (offset 40)          ; offset for radial blend - adjust as desired
        )

        ; adjust ending blend points if landscape orientation
        (if (< height width)
           (begin
              (set! x2 (* width 1.05))
              (set! y2 y1)
           )
        )

        ; the usual admin stuff
        (gimp-context-push)
        (gimp-image-undo-group-start image)

        ; duplicate the active layer
        (gimp-image-add-layer image vignette -1)

        ; give our new layer a clever name
        (gimp-layer-set-name vignette "vignette")

        ; set the new layer to multiply mode
        (gimp-layer-set-mode vignette MULTIPLY-MODE)

        ; add a layer mask
        (gimp-layer-add-mask vignette vignette-mask)

        ; add the radial gradient
        ; see http://developer.gimp.org/api/2.0/libgimp/libgimp-gimpedit.html
        (gimp-edit-blend vignette-mask
                         0             ; blend mode (0 = RGB mode)
                         0             ; paint mode (0 = normal mode)
                         2             ; gradient type (2 = radial)
                         100           ; opacity
                         offset        ; offset 
                         0             ; repeat (0 = none)
                         FALSE         ; reverse the gradient
                         FALSE         ; do adaptive supersampling
                         0             ; maximum recursion levels for supersampling 
                         0             ; supersampling threshold
                         TRUE          ; use dithering to reduce banding 
                         x1            ; starting point x-coordinate
                         y1            ; starting point y-coordinate
                         x2            ; ending point x-coordinate
                         y2)           ; ending point y-coordinate

        ; reduce the opacity
        (gimp-layer-set-opacity vignette opacity)

        ; close up shop
        (gimp-image-undo-group-end image)
        (gimp-displays-flush)
        (gimp-context-pop)
    )
)

; register the script
(script-fu-register "subtle-vignette"
		    "Subtle Vignette"
		    "Adds a subtle vignette layer"
		    "Glenn Wadsworth <gwads71@gmail.com>"
		    "Copyright 2013 Glenn Wadsworth"
		    "2013-01-08"
		    "RGB* GRAY*"
		    SF-IMAGE "Input Image" 0
		    SF-DRAWABLE "Input Drawable" 0
)

; make it a meun item
(script-fu-menu-register "subtle-vignette" "<Image>/Filters/Light and Shadow")


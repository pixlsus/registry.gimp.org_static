; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; http://www.gnu.org/licenses/gpl-3.0.html
;
; Copyright (C) 2011 elsamuko <elsamuko@web.de>
;


(define (elsamuko-erosion-sharpen img draw op gauss_blur)
  (let*
      ((owidth (car (gimp-image-width img)))
       (oheight (car (gimp-image-height img)))
       (blurred-layer (car (gimp-layer-copy draw FALSE)))
       (erode-layer (car (gimp-layer-copy draw FALSE)))
       (dilate-layer (car (gimp-layer-copy draw FALSE)))
       (erode-layermask (car (gimp-layer-create-mask erode-layer ADD-WHITE-MASK)))
       (dilate-layermask (car (gimp-layer-create-mask dilate-layer ADD-WHITE-MASK)))
       (additive-layer 0)
       (subtractive-layer 0)
       )
    
    ;init
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    
    ; add and blur copy
    (gimp-image-add-layer img blurred-layer -1)
    (gimp-drawable-set-name blurred-layer "Blurred")
    (plug-in-gauss TRUE img blurred-layer gauss_blur gauss_blur TRUE)
    
    ; subtract first from second
    (gimp-layer-set-mode blurred-layer SUBTRACT-MODE)
    (gimp-edit-copy-visible img)
    (set! subtractive-layer (car (gimp-layer-new-from-visible img img "Subtractive") ))
    (gimp-image-add-layer img subtractive-layer 0)
    (gimp-drawable-set-visible subtractive-layer FALSE)
    
    ; subtract second from first
    (gimp-image-lower-layer img blurred-layer)
    (gimp-layer-set-mode blurred-layer NORMAL-MODE)
    (gimp-layer-set-mode draw SUBTRACT-MODE)
    (gimp-edit-copy-visible img)
    (set! additive-layer (car (gimp-layer-new-from-visible img img "Additive") ))
    (gimp-image-add-layer img additive-layer 0)    
    
    ; set modes back to normal
    (gimp-drawable-set-visible subtractive-layer TRUE)
    (gimp-layer-set-mode draw NORMAL-MODE)
    
    ; add and erode copy
    (gimp-image-add-layer img erode-layer -1)
    (gimp-drawable-set-name erode-layer "Erode")
    (plug-in-vpropagate TRUE img erode-layer 1 1 0.7 15 0 255)
    
    ; add and dilate copy
    (gimp-image-add-layer img dilate-layer -1)
    (gimp-drawable-set-name dilate-layer "Dilate")
    (plug-in-vpropagate TRUE img dilate-layer 0 1 0.7 15 0 255)
    
    ; add layer masks
    (gimp-layer-add-mask erode-layer erode-layermask)
    (gimp-selection-all img)
    (gimp-edit-copy additive-layer)
    (gimp-floating-sel-anchor (car (gimp-edit-paste erode-layermask TRUE)))
    
    (gimp-layer-add-mask dilate-layer dilate-layermask)
    (gimp-selection-all img)
    (gimp-edit-copy subtractive-layer)
    (gimp-floating-sel-anchor (car (gimp-edit-paste dilate-layermask TRUE)))
    
    ; adjust levels
    (gimp-levels erode-layermask HISTOGRAM-VALUE 0 30 2 0 255)
    (gimp-levels dilate-layermask HISTOGRAM-VALUE 0 30 2 0 255)
    
    ; anti aliasing of layer masks
    (plug-in-antialias TRUE img erode-layermask)
    (plug-in-antialias TRUE img dilate-layermask)
    
    ; adjust levels 2nd
    (gimp-levels erode-layermask HISTOGRAM-VALUE 0 128 2 0 255)
    (gimp-levels dilate-layermask HISTOGRAM-VALUE 0 128 2 0 255)
    
    ; anti aliasing of layer masks 2nd
    (plug-in-antialias TRUE img erode-layermask)
    (plug-in-antialias TRUE img dilate-layermask)
    
    ; remove unnecessary layers
    (gimp-image-remove-layer img subtractive-layer)
    (gimp-image-remove-layer img additive-layer)
    (gimp-image-remove-layer img blurred-layer)
    
    ; set opacities
    (gimp-layer-set-opacity erode-layer op)
    (gimp-layer-set-opacity dilate-layer op)
    
    ; tidy up
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    )
  )

(script-fu-register "elsamuko-erosion-sharpen"
                    _"_Erosion Sharpen"
                    "Sharpens the image with erosion and dilation"
                    "elsamuko <elsamuko@web.de>"
                    "elsamuko"
                    "10/10/11"
                    "*"
                    SF-IMAGE       "Input image"           0
                    SF-DRAWABLE    "Input drawable"        0
                    SF-ADJUSTMENT _"Strength"             '(60 0 100 1 20 0 0)
                    SF-ADJUSTMENT _"Radius"               '(2 1 20 1 5 0 0)
                    )

(script-fu-menu-register "elsamuko-erosion-sharpen" _"<Image>/Filters/Enhance")

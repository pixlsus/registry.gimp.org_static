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
;
; Copyright (C) 2008 Michael Maier info[at]mmip.net
;
; Version 0.1 - Simulating a vintage photo based on this tutorial:
;               http://crazymurdock1.deviantart.com/art/Vintage-look-in-Gimp-61841683
; Version 0.2 - Optional sharpness and contrast layer
;               by Samuel Albrecht (http://registry.gimp.org/node/8718)
;
;

(define
  (vintage-look   img	
                  drw	
                  VarCyan
                  VarMagenta
                  VarYellow
                  Sharpen
                  )
  
  (let* ((drawable-width (car (gimp-drawable-width drw)))
         (drawable-height (car (gimp-drawable-height drw)))
         (new-image (car (gimp-image-new drawable-width drawable-height RGB)))
         (original-layer (car (gimp-layer-new new-image
                                              drawable-width drawable-height
                                              RGB-IMAGE "Original"
                                              100 NORMAL-MODE)))
         (cyan-layer 0)
         (magenta-layer 0)
         (yellow-layer 0)
         (sharpen-layer (car (gimp-layer-copy drw FALSE)))
         )
    
    ;Begin
    (gimp-undo-push-group-start img)
    (gimp-image-add-layer new-image original-layer 0)
    
    ;Sharpness + Contrast Layer
    (if(= Sharpen TRUE)
       (begin
         (gimp-image-add-layer img sharpen-layer -1)
         (gimp-desaturate-full sharpen-layer DESATURATE-LIGHTNESS)
         (plug-in-unsharp-mask 1 img sharpen-layer 5 1 0)
         (gimp-layer-set-mode sharpen-layer OVERLAY-MODE)
         )
       )
    
    ;Yellow Layer
    (set! yellow-layer (car (gimp-layer-new img drawable-width drawable-height RGB "color layer" 100  NORMAL)))	
    (gimp-image-add-layer img yellow-layer -1)
    (gimp-drawable-set-name yellow-layer "yellow")
    (gimp-context-set-background '(251 242 163) )
    (gimp-drawable-fill yellow-layer BACKGROUND-FILL)
    (gimp-layer-set-opacity yellow-layer VarYellow)
    (gimp-layer-set-mode yellow-layer MULTIPLY-MODE)
    
    ;Magenta Layer
    (set! magenta-layer (car (gimp-layer-new img drawable-width drawable-height RGB "color layer" 100  NORMAL)))	
    (gimp-image-add-layer img magenta-layer -1)
    (gimp-drawable-set-name magenta-layer "magenta")
    (gimp-context-set-background '(232 101 179) )
    (gimp-drawable-fill magenta-layer BACKGROUND-FILL)
    (gimp-layer-set-opacity magenta-layer VarMagenta)
    (gimp-layer-set-mode magenta-layer SCREEN-MODE)
    
    ;Cyan Layer 
    (set! cyan-layer (car (gimp-layer-new img drawable-width drawable-height RGB "color layer" 100  NORMAL)))	
    (gimp-image-add-layer img cyan-layer -1)
    (gimp-drawable-set-name cyan-layer "cyan")
    (gimp-context-set-background '(9 73 233) )
    (gimp-drawable-fill cyan-layer BACKGROUND-FILL)
    (gimp-layer-set-opacity cyan-layer VarCyan)
    (gimp-layer-set-mode cyan-layer SCREEN-MODE)
    
    ;End
    ;(gimp-image-flatten img)
    (gimp-displays-flush)
    (gimp-undo-push-group-end img)
    )
  )

(script-fu-register "vintage-look"
                    "<Image>/Script-Fu/Vintage-Look"
                    
                    "
                    Simulation of a vintage look.
                    Last version can be found at:
                    http://registry.gimp.org/node/1348
                    "
                    
                    "Michael Maier info[at]mmip.net >" 
                    "(c) Michael Maier. This is GPL Free Software." 	
                    "March 3, 2008" 
                    ""	
                    
                    SF-IMAGE      "Image"    0
                    SF-DRAWABLE   "Drawable" 0
                    SF-ADJUSTMENT _"Cyan"    '(17 0 100 1 1 0 0)
                    SF-ADJUSTMENT _"Magenta" '(20 0 100 1 1 0 0)
                    SF-ADJUSTMENT _"Yellow"  '(59 0 100 1 1 0 0)
                    SF-TOGGLE     _"Sharpen"  TRUE
                    )

;
; Postcard
;
; Copyright (C) 2010 Howard Roberts(howardroberts@comcast.net)
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

;       A translation of the Photoshop(TM) technique found at http://www.photoshopcafe.com/tutorials/sketch/sketch.htm 
;       A huge debt of gratitude to Elsamuko for incorporating Shadow Recovery to make this script work exactly as I
;       envisioned!

(define (hdroberts-postcard img drawable
                            shadowOpacity
                            edgeLevel edgeOpacity
                            screenColor screenOpacity
                            borderSize borderColor)
  (gimp-undo-push-group-start img)
  (let* ((width (car (gimp-image-width img)))
         (height (car (gimp-image-height img)))
         
         (copy-layer (car (gimp-layer-copy drawable TRUE)))
         (shadow-layer (car (gimp-layer-copy drawable TRUE)))         
         
         (edge-layer 0)
         (screen-layer (car (gimp-layer-new img
                                            width height
                                            RGB-IMAGE
                                            "Screen"
                                            screenOpacity
                                            SCREEN-MODE)))
         )
    
    ;shadow recovery from here: http://registry.gimp.org/node/112
    (if(> shadowOpacity 0)
       (begin
         (gimp-image-add-layer img copy-layer -1)
         (gimp-layer-set-mode copy-layer ADDITION-MODE)
         (gimp-layer-set-opacity copy-layer shadowOpacity)
         (gimp-image-add-layer img shadow-layer -1)
         (gimp-desaturate shadow-layer)
         (gimp-invert shadow-layer)
         (let* ((CopyMask (car (gimp-layer-create-mask copy-layer ADD-WHITE-MASK)))
                (ShadowMask (car (gimp-layer-create-mask shadow-layer ADD-WHITE-MASK)))
                )
           (gimp-layer-add-mask copy-layer CopyMask)
           (gimp-layer-add-mask shadow-layer ShadowMask)
           (gimp-selection-all img)
           (gimp-edit-copy shadow-layer)
           (gimp-floating-sel-anchor (car (gimp-edit-paste CopyMask TRUE)))
           (gimp-floating-sel-anchor (car (gimp-edit-paste ShadowMask TRUE)))
           )
         (gimp-layer-set-mode shadow-layer OVERLAY-MODE)
         (gimp-layer-set-opacity shadow-layer shadowOpacity)
         (gimp-image-remove-layer img copy-layer)
         (gimp-drawable-set-name shadow-layer "Shadow Recovery")
         )
       )
    
    ; sobel
    (gimp-edit-copy-visible img)
    (set! edge-layer (car (gimp-layer-new-from-visible img img "Sobel")))
    (gimp-image-add-layer img edge-layer 0)
    (plug-in-edge 1 img edge-layer edgeLevel 1 0)
    (gimp-invert edge-layer)
    (gimp-layer-set-opacity edge-layer edgeOpacity)
    (gimp-layer-set-mode edge-layer VALUE-MODE)
    
    ; screen
    (gimp-image-add-layer img screen-layer 0)
    (gimp-context-set-foreground screenColor)
    (gimp-edit-fill screen-layer FOREGROUND-FILL)
    (gimp-image-raise-layer-to-top img screen-layer)
    
    ; border
    (script-fu-fuzzy-border img screen-layer
                            borderColor borderSize
                            TRUE 8 FALSE 100 FALSE FALSE )
    )
  (gimp-undo-push-group-end img)
  )

(script-fu-register "hdroberts-postcard"
                    "Postcard"
                    "Vintage postcard or greeting card effect"
                    "Howard Roberts <howardroberts@comcast.net>"
                    "(c) 2010 Howard D. Roberts"
                    "May 6, 2010"
                    "RGB"
                    SF-IMAGE       "Image"         0
                    SF-DRAWABLE    "Layer to blur" 0
                    SF-ADJUSTMENT _"Shadow Recover Opacity" '(80 0 100 1 5 0 0)
                    SF-ADJUSTMENT  "Edge Level"             '(2  0  10 1 1 1 0)
                    SF-ADJUSTMENT  "Edge Opacity"           '(25 0 100 1 10 0 0)
                    SF-COLOR       "Screen Color"           '(255 255 255)
                    SF-ADJUSTMENT  "Screen Opacity"         '(10 0 100 1 1 0 0)
                    SF-ADJUSTMENT  "Border Size"            '(20 0 100 1 1 0 0)
                    SF-COLOR       "Border Color"           '(255 255 255))

(script-fu-menu-register "hdroberts-postcard" "<Image>/Filters/Artistic")

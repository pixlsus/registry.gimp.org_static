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
; Copyright (C) 2010 elsamuko <elsamuko@web.de>
;


(define (elsamuko-photochrom aimg adraw
                             color1 color2
                             contrast bw-merge
                             num1 num2
                             dodge retro)
  (let* ((img (car (gimp-drawable-get-image adraw)))
         (owidth (car (gimp-image-width img)))
         (oheight (car (gimp-image-height img)))
         (offset1 (* oheight (/ num1 100)))
         (offset2 (* oheight (/ num2 100)))
         (dodge-layer (car (gimp-layer-copy adraw FALSE)))
         (contrast-layer1 (car (gimp-layer-copy adraw FALSE)))
         (contrast-layer2 (car (gimp-layer-copy adraw FALSE)))
         (bw-screen-layer (car (gimp-layer-copy adraw FALSE)))         
         (bw-merge-layer (car (gimp-layer-copy adraw FALSE)))         
         (lum-layer     (car (gimp-layer-copy adraw FALSE)))
         (extra-layer  0)
         (merge-layer (car (gimp-layer-new img
                                           owidth 
                                           oheight
                                           RGBA-IMAGE
                                           "Grain Merge" 
                                           50 
                                           GRAIN-MERGE-MODE)))
         (merge-mask (car (gimp-layer-create-mask merge-layer ADD-WHITE-MASK)))
         (screen-layer (car (gimp-layer-new img
                                            owidth 
                                            oheight
                                            RGBA-IMAGE
                                            "Screen" 
                                            10 
                                            SCREEN-MODE)))
         (screen-mask (car (gimp-layer-create-mask screen-layer ADD-WHITE-MASK)))
         (multiply-layer (car (gimp-layer-new img
                                              owidth 
                                              oheight
                                              RGBA-IMAGE
                                              "Multiply" 
                                              10 
                                              MULTIPLY-MODE)))
         (multiply-mask (car (gimp-layer-create-mask multiply-layer ADD-WHITE-MASK)))
         (retro-layer (car (gimp-layer-new img
                                           owidth 
                                           oheight
                                           RGBA-IMAGE
                                           "Retro 1" 
                                           60 
                                           MULTIPLY-MODE)))
         (floatingsel 0)
         (retro-mask (car (gimp-layer-create-mask retro-layer ADD-WHITE-MASK)))
         (retro-layer2 (car (gimp-layer-new img
                                            owidth 
                                            oheight
                                            RGBA-IMAGE
                                            "Retro 2" 
                                            20 
                                            SCREEN-MODE)))
         (gradient-layer (car (gimp-layer-new img
                                              owidth 
                                              oheight
                                              RGBA-IMAGE
                                              "Gradient Overlay" 
                                              100 
                                              OVERLAY-MODE)))
         )
    
    ; init
    (gimp-context-push)
    (gimp-image-undo-group-start img)
    (if (= (car (gimp-drawable-is-gray adraw )) TRUE)
        (gimp-image-convert-rgb img)
        )
    
    ;set extra color layer
    (gimp-image-add-layer img lum-layer 0)
    (gimp-drawable-set-name lum-layer "Luminosity")
    (gimp-desaturate-full lum-layer DESATURATE-LIGHTNESS)
    (gimp-layer-set-mode lum-layer GRAIN-EXTRACT-MODE)
    (gimp-edit-copy-visible img)
    (set! extra-layer (car (gimp-layer-new-from-visible img img "Extra Color")))
    (gimp-image-add-layer img extra-layer 0)
    (gimp-layer-set-mode extra-layer GRAIN-MERGE-MODE)
    (gimp-layer-set-opacity extra-layer 50)
    (gimp-drawable-set-visible lum-layer FALSE)
    
    ;set BW screen layer
    (gimp-image-add-layer img bw-screen-layer -1)
    (gimp-drawable-set-name bw-screen-layer "BW Screen")
    (gimp-layer-set-mode bw-screen-layer SCREEN-MODE)
    (gimp-layer-set-opacity bw-screen-layer 50)
    (gimp-desaturate-full bw-screen-layer DESATURATE-LUMINOSITY)
    
    ;set BW merge layer
    (gimp-image-add-layer img bw-merge-layer -1)
    (gimp-drawable-set-name bw-merge-layer "BW Merge")
    (gimp-layer-set-mode bw-merge-layer GRAIN-MERGE-MODE)
    (gimp-layer-set-opacity bw-merge-layer bw-merge)
    (gimp-desaturate-full bw-merge-layer DESATURATE-LUMINOSITY)
    (gimp-curves-spline bw-merge-layer HISTOGRAM-VALUE 6 #(0 144 88 42 255 255))
    
    ;set contrast layers
    (gimp-image-add-layer img contrast-layer1 -1)
    (gimp-drawable-set-name contrast-layer1 "Contrast1")
    (gimp-layer-set-mode contrast-layer1 OVERLAY-MODE)
    (gimp-layer-set-opacity contrast-layer1 contrast)
    (gimp-desaturate-full contrast-layer1 DESATURATE-LUMINOSITY)
    
    (gimp-image-add-layer img contrast-layer2 -1)
    (gimp-drawable-set-name contrast-layer2 "Contrast2")
    (gimp-layer-set-mode contrast-layer2 OVERLAY-MODE)
    (gimp-layer-set-opacity contrast-layer2 contrast)
    (gimp-desaturate-full contrast-layer2 DESATURATE-LUMINOSITY)
    
    ;set dodge layer
    (gimp-image-add-layer img dodge-layer -1)
    (gimp-drawable-set-name dodge-layer "Dodge")
    (gimp-layer-set-mode dodge-layer DODGE-MODE)
    (gimp-layer-set-opacity dodge-layer 50)
    
    ;set merge layer
    (gimp-image-add-layer img merge-layer -1)
    (gimp-selection-all aimg)
    (gimp-context-set-foreground color1)
    (gimp-edit-bucket-fill merge-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
    (gimp-layer-add-mask merge-layer merge-mask)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-context-set-background '(0 0 0))
    (gimp-edit-blend merge-mask FG-BG-RGB-MODE
                     NORMAL-MODE GRADIENT-LINEAR
                     100 0 REPEAT-NONE
                     TRUE FALSE 1 0
                     TRUE 0 offset1 0 offset2)
    
    ;set screen layer
    (gimp-image-add-layer img screen-layer -1)
    (gimp-selection-all aimg)
    (gimp-context-set-foreground color1)
    (gimp-edit-bucket-fill screen-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
    (gimp-layer-add-mask screen-layer screen-mask)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-context-set-background '(0 0 0))
    (gimp-edit-blend screen-mask FG-BG-RGB-MODE
                     NORMAL-MODE GRADIENT-LINEAR
                     100 0 REPEAT-NONE
                     TRUE FALSE 1 0
                     TRUE 0 offset1 0 offset2)
    
    ;set multiply layer
    (gimp-image-add-layer img multiply-layer -1)
    (gimp-selection-all aimg)
    (gimp-context-set-foreground color2)
    (gimp-edit-bucket-fill multiply-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
    (gimp-layer-add-mask multiply-layer multiply-mask)
    (gimp-context-set-foreground '(255 255 255))
    (gimp-context-set-background '(0 0 0))
    (gimp-edit-blend multiply-mask FG-BG-RGB-MODE
                     NORMAL-MODE GRADIENT-LINEAR
                     100 0 REPEAT-NONE
                     TRUE FALSE 1 0
                     TRUE 0 offset1 0 offset2)
    
    ;optional retro colors
    (if(= retro TRUE)(begin
                       ;yellow with mask
                       (gimp-image-add-layer img retro-layer -1)
                       (gimp-selection-all aimg)
                       (gimp-context-set-foreground '(251 242 163))
                       (gimp-edit-bucket-fill retro-layer FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
                       (gimp-layer-add-mask retro-layer retro-mask)
                       (gimp-edit-copy contrast-layer1)
                       (set! floatingsel (car (gimp-edit-paste retro-mask TRUE)))
                       (gimp-floating-sel-anchor floatingsel)
                       
                       ;rose
                       (gimp-image-add-layer img retro-layer2 -1)
                       (gimp-selection-all aimg)
                       (gimp-context-set-foreground '(232 101 179))
                       (gimp-edit-bucket-fill retro-layer2 FG-BUCKET-FILL NORMAL-MODE 100 0 FALSE 0 0)
                       
                       ;gradient overlay
                       (gimp-image-add-layer img gradient-layer -1)
                       (gimp-context-set-foreground '(255 255 255))
                       (gimp-context-set-background '(0 0 0))
                       (gimp-edit-blend gradient-layer FG-BG-RGB-MODE
                                        NORMAL-MODE GRADIENT-LINEAR
                                        100 0 REPEAT-NONE
                                        FALSE FALSE 1 0
                                        TRUE 0 offset1 0 offset2)
                       
                       ;deactivate orange layers
                       (gimp-drawable-set-visible merge-layer FALSE)
                       (gimp-drawable-set-visible screen-layer FALSE)
                       (gimp-drawable-set-visible multiply-layer FALSE)
                       )
       )
    
    ;dodge b/w
    (if(= dodge TRUE)(begin
                       (gimp-desaturate-full dodge-layer DESATURATE-LUMINOSITY)
                       (gimp-drawable-set-visible extra-layer FALSE)
                       )
       )
    
    ; tidy up
    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
    (gimp-context-pop)
    )
  )

(script-fu-register "elsamuko-photochrom"
                    "_Photochrom"
                    "Photochrom effect from 1890."
                    "elsamuko <elsamuko@web.de>"
                    "elsamuko"
                    "26/02/10"
                    "RGB*"
                    SF-IMAGE       "Input image"          0
                    SF-DRAWABLE    "Input drawable"       0
                    SF-COLOR       "Screen & Grain Merge"  '(255 128 0)
                    SF-COLOR       "Multiply"              '(255 68 112)
                    SF-ADJUSTMENT  "Contrast"              '(60  0 100 1 10 0 0)
                    SF-ADJUSTMENT  "B/W Merging"           '(60  0 100 1 10 0 0)
                    SF-ADJUSTMENT  "Gradient Begin Offset" '(0 -100 200 1 10 0 0)
                    SF-ADJUSTMENT  "Gradient End Offset"   '(100 -100 200 1 10 0 0)
                    SF-TOGGLE      "B/W Dodging"            FALSE
                    SF-TOGGLE      "Retro"                  FALSE
                    )

(script-fu-menu-register "elsamuko-photochrom" _"<Image>/Filters/Artistic")

;-------------------------------------------------------------------------------------------
; Rays v.1.0
;
; This script paints funny rays and rays animations
; It works with GIMP version 2.6
;
;   Changelog
; v.1.0
;   Initial release
; v.1.1
;   Fixed deprecated function calls
;   Moved to <Toolbox>/Filters/Werdn/
;   Added new effect: painting rays on existent animation
; 
;-------------------------------------------------------------------------------------------
; Copyright (C) 2009  Werdn <werdn@werdn.org.ua>
; 
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
;-------------------------------------------------------------------------------------------

;-------------------------------------------------------------------------------------------
;  Static rays
;
;-------------------------------------------------------------------------------------------
(define (script-fu-werdn-rays
         img 
         layer-current 
         count 
         proportion
         rotation
         color 
         use-gradient 
         gradient 
         reverse
         merge-layers
         mode
         opacity)
  ; Some functions
  (define pi 3.141592653589793)
  (define (deg-rad x) (* pi (/ x 180)))
  (define (sqr x) (* x x))
  ; Local variables
  (let* ((width (car (gimp-image-width img)))
         (height (car (gimp-image-height img)))
         (center-x (/ width 2))
         (center-y (/ height 2))
         (radius (+ (sqrt (+ (sqr height) (sqr width))) 10))
         (angle-sector (/ 360 count))
         (angle-ray (* angle-sector proportion))
         (gradient-orig (car (gimp-context-get-gradient)))
         (layer-bg 0)
         (layer-rays 0)
         (layer-union 0)
         (vectors-result 0)
         (vectors-selection 0)
         (stroke 0))
    
    ; The following is done for all scripts
    (gimp-image-undo-group-start img)
    
    ; Creating layers
    (set! layer-rays 
          (car (gimp-layer-new img width height RGBA-IMAGE "Rays" 100 NORMAL-MODE)))
    
    ; Adding layers to image
    (gimp-image-add-layer img layer-rays -1)   
        
    ; Making new vector object    
    (set! vectors-selection (car (gimp-vectors-new img "werdn-rays-temp")))
    (gimp-image-add-vectors img vectors-selection -1)
    
    ; Paint star
    (set! stroke (car (gimp-vectors-bezier-stroke-new-moveto vectors-selection center-x center-y)))
    (while (> count 0)
           (begin
             (set! count (- count 1))
             (let* (
                    ; Angles in radians
                    (angle-ray-rad (deg-rad (+ rotation (* count angle-sector))))
                    (angle-start-rad (- angle-ray-rad (deg-rad (/ angle-ray 2))))
                    (angle-end-rad (+ angle-ray-rad (deg-rad (/ angle-ray 2))))
                    ; Coordinates of points
                    (point-1 (list
                              (+ center-x (* radius (cos angle-start-rad)))
                              (+ center-y (* radius (sin angle-start-rad)))))
                    (point-4 (list
                              (+ center-x (* radius (cos angle-end-rad)))
                              (+ center-y (* radius (sin angle-end-rad)))))
                    (point-2 (list
                              (+ (car point-1) (* radius (cos angle-ray-rad)))
                              (+ (cadr point-1) (* radius (sin angle-ray-rad)))))
                    (point-3 (list
                              (+ (car point-4) (* radius (cos angle-ray-rad)))
                              (+ (cadr point-4) (* radius (sin angle-ray-rad)))))
                    ; All points list
                    (points (list point-1 point-2 point-3 point-4)))
               (begin
                 ; Painting 4 points
                 (while (not (null? points))
                        (begin
                          (gimp-vectors-bezier-stroke-lineto vectors-selection stroke
                                                             (car (car points))
                                                             (cadr (car points)))
                          (set! points (cdr points)))
                        )
                 ; Painting center point
                 (gimp-vectors-bezier-stroke-lineto vectors-selection stroke center-x center-y)))))
    
    ; Convert vector object to selection
    (gimp-vectors-to-selection vectors-selection CHANNEL-OP-REPLACE TRUE FALSE 0 0)
    
    ; Remove vector object
    (gimp-image-remove-vectors img vectors-selection)
    
    ; Paint gradient if need
    (if (= use-gradient TRUE)
        (begin
          (gimp-context-set-gradient gradient)
          (gimp-edit-blend 
           layer-rays                            ;drawable 
           CUSTOM-MODE                           ;blend-mode 
           NORMAL-MODE                           ;paint-mode 
           GRADIENT-RADIAL                       ;gradient-type 
           100                                   ;opacity 
           0                                     ;offset 
           REPEAT-NONE                           ;repeat 
           reverse                               ;reverse 
           FALSE                                 ;supersample 
           1                                     ;max-depth 
           0                                     ;threshold 
           FALSE                                 ;dither 
           (/ width 2)                           ;x1 
           (/ height 2)                          ;y1 
           0                                     ;x2 
           0))
        ; Paint with brush color
        (begin
          (let* 
              (
               ; Save current color
               (old-color (car (gimp-context-get-foreground))))
            (gimp-context-set-foreground color)
            (gimp-edit-bucket-fill 
             layer-rays 
             FG-BUCKET-FILL
             NORMAL-MODE
             100
             0
             FALSE
             0
             0
             )
            (gimp-context-set-foreground old-color)
            )))
    
    ; Remove selection
    (gimp-selection-none img)
    
    ; Set rays layer mode
    (gimp-layer-set-mode layer-rays mode)
    (gimp-layer-set-opacity layer-rays opacity)
    
    ; Merge layers
    (if (= merge-layers TRUE)
        (begin
          (let*
              ((name (car (gimp-drawable-get-name layer-current))))
            (set! layer-union (car (gimp-image-merge-down img layer-rays 0)))
            ; Rename merged layer to original name
            (gimp-drawable-set-name layer-union name))))
    
    ; The following is also done for all script
    ; (gimp-display-new img)
    (gimp-context-set-gradient gradient-orig)
    (gimp-displays-flush)
    (gimp-image-undo-group-end img)))

;--------------------------------------------------------------------------------------------
;  Animation based on previous function
;  
;--------------------------------------------------------------------------------------------
(define (script-fu-werdn-rays-animate
         img 
         layer-current 
         count
         proportion
         frames
         repeat
         direction
         color 
         use-gradient 
         gradient 
         reverse
         merge-layers             ; If 'merge-layers' is set then copy background for each frame
         mode
         opacity)
  (let* ((frames-counter 0)
         (angle-step (if (= direction TRUE) (/ (/ 360 count) frames) (- 0 (/ (/ 360 count) frames))))
         (layer-copy 0))
    ; Open Undo group
    (gimp-image-undo-group-start img)
    ; Repeat rotation 'repeat' times
    (while (> repeat 0)
           (set! repeat (- repeat 1))
           (set! frames-counter frames)
           (while (> frames-counter 0)
                  (set! frames-counter (- frames-counter 1))
                  ; copy current frame (if enabled merging)
                  (if (= merge-layers TRUE)
                      (begin
                        (set! layer-copy (car (gimp-layer-copy layer-current TRUE)))
                        (gimp-image-add-layer img layer-copy 0)
                        (gimp-image-set-active-layer img layer-copy)))
                  ; Painting one frame
                  (script-fu-werdn-rays 
                   img 
                   (if (= merge-layers TRUE) layer-copy layer-current)
                   count 
                   proportion 
                   (* frames-counter angle-step)
                   color 
                   use-gradient
                   gradient 
                   reverse 
                   merge-layers
                   mode
                   opacity)))
    ; Close Undo group
    (gimp-image-undo-group-end img)
  ))

;--------------------------------------------------------------------------------------------
;  Add rotated rays to animated picture (lower layer will be a background)
;  
;--------------------------------------------------------------------------------------------
(define (script-fu-werdn-animation-add-rays
         img 
         count
         proportion
         rotate-count ; Count of rays for rotate during animation (default 1)
         direction
         color 
         use-gradient 
         gradient 
         reverse
         position     ; 0 - Under each frame, 1 - Above each frame
         mode
         opacity)
  ; Reverse list (Scheme in gimp haven't this function)
  (define (get-all-layers img)
    (let* (
        (all-layers (gimp-image-get-layers img))
        (i (car all-layers))
        (bottom-to-top '())
        )
      (set! all-layers (cadr all-layers))
      (while (> i 0)
        (set! bottom-to-top (append bottom-to-top (cons (aref all-layers (- i 1)) '())))
        (set! i (- i 1))
        )
      bottom-to-top
      )
    )
  
  ;(lambda (x) (if (= x TRUE) + -)) ; Function not work
  (let* ((layers-all (get-all-layers img))
         (layer-bg (car layers-all))
         (counter (- (length layers-all) 1))
         (angle-step (((lambda (x) (if (= x TRUE) + -)) direction) 0 (* rotate-count (/ (/ 360 count) counter))))
         (layer-frame 0)
         (layer-frame-bg 0))
    (begin
      ; Remove background from layers list
      (set! layers-all (cdr layers-all))
      ; Reverse angle
      ;(if (= direction FALSE) (set! angle-step (- 0 angle-step)))
      (gimp-image-undo-group-start img)
      (while (> counter 0)
             ; Prepare layer for painting
             (set! layer-frame (car layers-all))
             (set! layers-all (cdr layers-all))
             (set! layer-frame-bg (car (gimp-layer-copy layer-bg TRUE)))
             (gimp-image-add-layer img layer-frame-bg counter)             
             (gimp-image-set-active-layer img layer-frame-bg)
             
             ; If position = above then merge animation frame with frame BG
             (if (= position 1)
                 (set! layer-frame-bg (car (gimp-image-merge-down img layer-frame 0))))
             
             ; Painting one frame
             (script-fu-werdn-rays 
              img 
              layer-frame-bg
              count 
              proportion 
              (* counter angle-step)
              color 
              use-gradient
              gradient 
              reverse 
              TRUE
              mode
              opacity)
             
             ; Merging layers if we paint rays under the animation
             (if (= position 0)
                 (gimp-image-merge-down img layer-frame 0))
             
             ; Decrement counter
             (set! counter (- counter 1)))
      (gimp-image-undo-group-end img)
      (gimp-displays-flush)
      )))

; Finally register our script with script-fu.
(script-fu-register "script-fu-werdn-rays"
                    _"<Toolbox>/Filters/Werdn/Rays..."
                    "Make rays effect"
                    "Werdn <werdn@werdn.org.ua>"
                    "Werdn"
                    "2009-06-20"
                    "RGB*"
                    SF-IMAGE      _"The image"               0
                    SF-DRAWABLE   _"The layer"               0
                    SF-VALUE      _"Count"                   "10"
                    SF-ADJUSTMENT _"Proportion"              '(0.5 0.0 1.0 0.01 10 2 0)
                    SF-ADJUSTMENT _"Rotation"                '(0.0 0.0 360 1.0 10 2 0)
                    SF-COLOR      _"Color"                   '(255 127 0)
                    SF-TOGGLE     _"Rays gradient"           FALSE
                    SF-GRADIENT   _"Gradient for rays"       "Yellow Orange"
                    SF-TOGGLE     _"Reverse"                 FALSE
                    SF-TOGGLE     _"Merge layers"            FALSE
                    SF-OPTION     _"Rays paint mode"         '("Normal mode" "Dissolve mode" "Behind mode" "Multiplay mode" "Screen mode" "Overlay mode" "Difference mode" "Addition mode" "Substract mode" "Darken only mode" "Lighten only mode" "Hue mode" "Saturation mode" "Color mode" "Value mode" "Divide mode" "Dodge mode" "Burn mode" "Hardlight mode" "Softlight mode" "Grain extract mode" "Grain merge mode" "Color erase mode" "Erase mode" "Replace mode" "Anti erase mode")
                    ;NORMAL-MODE (0), DISSOLVE-MODE (1), BEHIND-MODE (2), MULTIPLY-MODE (3), SCREEN-MODE (4), OVERLAY-MODE (5), DIFFERENCE-MODE (6), ADDITION-MODE (7), SUBTRACT-MODE (8), DARKEN-ONLY-MODE (9), LIGHTEN-ONLY-MODE (10), HUE-MODE (11), SATURATION-MODE (12), COLOR-MODE (13), VALUE-MODE (14), DIVIDE-MODE (15), DODGE-MODE (16), BURN-MODE (17), HARDLIGHT-MODE (18), SOFTLIGHT-MODE (19), GRAIN-EXTRACT-MODE (20), GRAIN-MERGE-MODE (21), COLOR-ERASE-MODE (22), ERASE-MODE (23), REPLACE-MODE (24), ANTI-ERASE-MODE (25)
                     SF-ADJUSTMENT _"Opacity"                '(100.0 0.0 100.0 0.1 10 2 0)
                    )

(script-fu-register "script-fu-werdn-rays-animate"
                    _"<Toolbox>/Filters/Werdn/Rays animation..."
                    "Make rays animation effect"
                    "Werdn <werdn@werdn.org.ua>"
                    "Werdn"
                    "2009-06-20"
                    "RGB*"
                    SF-IMAGE      _"The image"               0
                    SF-DRAWABLE   _"The layer"               0
                    SF-VALUE      _"Count"                   "10"
                    SF-ADJUSTMENT _"Proportion"              '(0.5 0.0 1.0 0.01 10 2 0)
                    SF-VALUE      _"Frames"                  "10"
                    SF-VALUE      _"Repeat"                  "1"
                    SF-TOGGLE     _"Rotate CCW"              FALSE
                    SF-COLOR      _"Color"                   '(255 127 0)
                    SF-TOGGLE     _"Rays gradient"           FALSE
                    SF-GRADIENT   _"Gradient for rays"       "Yellow Orange"
                    SF-TOGGLE     _"Reverse"                 FALSE
                    SF-TOGGLE     _"Merge layers"            TRUE  
                    SF-OPTION     _"Rays paint mode"         '("Normal mode" "Dissolve mode" "Behind mode" "Multiplay mode" "Screen mode" "Overlay mode" "Difference mode" "Addition mode" "Substract mode" "Darken only mode" "Lighten only mode" "Hue mode" "Saturation mode" "Color mode" "Value mode" "Divide mode" "Dodge mode" "Burn mode" "Hardlight mode" "Softlight mode" "Grain extract mode" "Grain merge mode" "Color erase mode" "Erase mode" "Replace mode" "Anti erase mode")
                     SF-ADJUSTMENT _"Opacity"                '(100.0 0.0 100.0 0.1 10 2 0)
                    )

(script-fu-register "script-fu-werdn-animation-add-rays"
                    _"<Toolbox>/Filters/Werdn/Add rays to animation..."
                    "Paint rotated rays on top of existent animation. Bottom layer used as background."
                    "Werdn <werdn@werdn.org.ua>"
                    "Werdn"
                    "2009-06-20"
                    "RGB*"
                    SF-IMAGE      _"The image"               0
                    SF-VALUE      _"Count"                   "10"
                    SF-ADJUSTMENT _"Proportion"              '(0.5 0.0 1.0 0.01 10 2 0)
                    SF-VALUE      _"Number of rotated rays"  "1"
                    SF-TOGGLE     _"Rotate CCW"              FALSE
                    SF-COLOR      _"Color"                   '(255 127 0)
                    SF-TOGGLE     _"Rays gradient"           FALSE
                    SF-GRADIENT   _"Gradient for rays"       "Yellow Orange"
                    SF-TOGGLE     _"Reverse"                 FALSE
                    SF-OPTION     _"Position"                '("Under each frame" "Above each frame")
                    SF-OPTION     _"Rays paint mode"         '("Normal mode" "Dissolve mode" "Behind mode" "Multiplay mode" "Screen mode" "Overlay mode" "Difference mode" "Addition mode" "Substract mode" "Darken only mode" "Lighten only mode" "Hue mode" "Saturation mode" "Color mode" "Value mode" "Divide mode" "Dodge mode" "Burn mode" "Hardlight mode" "Softlight mode" "Grain extract mode" "Grain merge mode" "Color erase mode" "Erase mode" "Replace mode" "Anti erase mode")
                     SF-ADJUSTMENT _"Opacity"                '(100.0 0.0 100.0 0.1 10 2 0)
                    )

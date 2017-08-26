; Wavelet Decompose Script-Fu
;
; Copyright (C) 2009 Christoph A. Traxler
; 
; This program is free software; you can redistribute it and/or modify it under
; the terms of the GNU General Public License as published by the Free Software
; Foundation; either version 3 of the License, or (at your option) any later
; version.
; 
; This program is distributed in the hope that it will be useful, but WITHOUT
; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
; 
; The GNU General Public License can be found at
;   http://www.gnu.org/copyleft/gpl.html


(define (script-fu-wavelet-decompose image
                                     start
                                     count
                                     factor
                                     increase
                                     make-copy)
  (let* (
      (work-image 0)          ; my working image
      (layer-orig 0)          ; original layer
      (layer-resi 0)          ; residual layer
      (layer-orig-copy 0)     ; copy of original layer
      (layer-resi-copy 0)     ; copy of residual layer
      (layer-scale 0)         ; layer with current scale
      (layer-scale-copy 0)    ; layer with copy of current scale
      (blur-size start)       ; current blur size
      (i 1)                   ; counter (Start with 1)
    )
    
    ;-- Preparations --
    
    ; check if I should work on a copy
    (if (= make-copy TRUE)
      (begin
        (set! work-image (car (gimp-image-duplicate image)))
        (gimp-display-new work-image)
        (gimp-image-undo-disable work-image)
      )
      (begin
        (set! work-image image)
        (gimp-image-undo-group-start work-image)
      )
    )
    
    ; create two layers for Original and Residual
    (set! layer-orig (car(gimp-image-flatten work-image)))
    (gimp-drawable-set-name layer-orig "Original")
    (gimp-image-set-active-layer work-image layer-orig)
    (set! layer-resi (car(gimp-layer-copy layer-orig FALSE)))
    (gimp-image-add-layer work-image layer-resi -1)
    (gimp-drawable-set-name layer-resi "Residual")
    
    ;-- Decomposing --
    
    (while (<= i count)
      ; create layer with current scale detail
      (set! layer-orig-copy (car (gimp-layer-copy layer-orig FALSE)))
      (gimp-image-set-active-layer work-image layer-resi)
      (gimp-image-add-layer work-image layer-orig-copy -1)
      (set! layer-resi-copy (car (gimp-layer-copy layer-resi FALSE)))
      (gimp-image-set-active-layer work-image layer-resi)
      (gimp-image-add-layer work-image layer-resi-copy -1)
      (plug-in-gauss-iir RUN-NONINTERACTIVE
                         work-image
                         layer-orig-copy
                         blur-size
                         TRUE TRUE)
      (gimp-layer-set-mode layer-orig-copy GRAIN-EXTRACT-MODE)
      (set! layer-scale (car (gimp-image-merge-down
        work-image layer-orig-copy CLIP-TO-IMAGE)))
      (gimp-layer-set-mode layer-scale GRAIN-MERGE-MODE)
      (gimp-drawable-set-name
        layer-scale
        (string-append "Scale " (number->string i)
                       " (" (number->string blur-size) " pix)")
      )
      
      ; update residual
      (set! layer-scale-copy (car (gimp-layer-copy layer-scale FALSE)))
      (gimp-image-set-active-layer work-image layer-resi)
      (gimp-image-add-layer work-image layer-scale-copy -1)
      (gimp-layer-set-mode layer-scale-copy GRAIN-EXTRACT-MODE)
      (set! layer-resi (car (gimp-image-merge-down
        work-image layer-scale-copy CLIP-TO-IMAGE)))
      
      ; for the loop
      (set! i (+ i 1))
      (set! blur-size (+ (* blur-size factor) increase))
    )
    
    ;-- Finishing --
    
    ; bring original layer to top and hide it
    (gimp-image-raise-layer-to-top work-image layer-orig)
    (gimp-drawable-set-visible layer-orig FALSE)
    
    ; cleanup
    (if (= make-copy TRUE)
      (begin
        (gimp-image-undo-enable work-image)
        (gimp-image-clean-all work-image)
      )
      (begin
        (gimp-image-undo-group-end work-image)
      )
    )
    (gimp-displays-flush)
    
    ; return
    '(work-image)
  )
)

; register this script in The GIMP
(script-fu-register
  _"script-fu-wavelet-decompose"                            ; func name
  _"Wavelet Decompose ..."                                  ; menu label
  "Decomposes an image into layers with different detail scales.\
This script flattens the original image first an then creates new layers."
                                                            ; description
  "Christoph A. Traxler"                                    ; author
  "(C) 2009, Christoph A. Traxler"                          ; copyright
  "January 2009"                                            ; date created
  "RGB* GRAY*"                     ; working image type (non indexed Image)
  SF-IMAGE          "Image"             0
  SF-ADJUSTMENT    _"Minimum Scale (pixels)"
                                        '(1 1 1000 1 10 0 1)
  SF-ADJUSTMENT    _"Scale Count (number of different scales)"
                                        '(6 1 100 1 10 0 1)
  SF-ADJUSTMENT    _"Scale Increase Factor"
                                        '(2.0 1.0 10 0.1 1 1 1)
  SF-ADJUSTMENT    _"Scale Constant Increase (pixels)"
                                        '(0 0 10000 1 10 0 1)
  SF-TOGGLE        _"Work on copy (creates a new image)"
                                        TRUE
)
(script-fu-menu-register "script-fu-wavelet-decompose"
                         "<Image>/Image"
)

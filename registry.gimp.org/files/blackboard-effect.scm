; * Copyright (c) 2007 Pucelo for www.gimp.org.es
; * All rights reserved.
; *
; * Redistribution and use in source and binary forms, with or without
; * modification, are permitted provided that the following conditions
; * are met:
; * 1. Redistributions of source code must retain the above copyright
; *    notice, this list of conditions and the following disclaimer.
; * 2. Redistributions in binary form must reproduce the above copyright
; *    notice, this list of conditions and the following disclaimer in the
; *    documentation and/or other materials provided with the distribution.
; * 3. Neither the name of copyright holders nor the names of its
; *    contributors may be used to endorse or promote products derived
; *    from this software without specific prior written permission.
; *
; * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
; * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL COPYRIGHT HOLDERS OR CONTRIBUTORS
; * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
; * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; * POSSIBILITY OF SUCH DAMAGE.


(define (script-fu-simple-tizas img drawable copy aplanar)
;  (let*
;    (image)
;  )
  (define image (if
    (= copy TRUE)
    (car (gimp-image-duplicate img))
    img
  ))
;  (if (= copy TRUE)
;    (set! image (car (gimp-image-duplicate img)))
;    (set! image img)
;  )
;  (gimp-image-undo-disable image)
  (gimp-undo-push-group-start image); Empezar un bloque de deshacer. / Start a undo group.
  (set! drawable (car (gimp-image-flatten image))); La imagen es aplanada / The image is flatened
  ;Se añade una nueva capa a la imagen. / Create new layer and add to the image
  (define shadow-layer (car (gimp-layer-copy drawable 1)))
  (gimp-image-add-layer image shadow-layer -1)
  (gimp-layer-set-name shadow-layer "Sat") ; Nombre de la capa / Layer's name
  (gimp-layer-set-mode shadow-layer 12) ; Modo saturación / Saturation mode
  ; Create new layer and add to the image
  (define shadow-layer2 (car (gimp-layer-copy drawable 1)))
  (gimp-image-add-layer image shadow-layer2 -1)
  (gimp-layer-set-name shadow-layer2 "Hue / Tono")
  (gimp-layer-set-mode shadow-layer2 11)
  ; Sobel a la capa base / Sobel to base layer
  (plug-in-sobel 1 image drawable 1 1 0)
  (gimp-equalize drawable 0)
  (if 
    (= aplanar TRUE)
    (set! drawable (car (gimp-image-flatten image)))
    ()
  )
  (gimp-image-set-active-layer image drawable)
  (if
    (= copy TRUE)
    (gimp-display-new image)
    ()
  )
;  (gimp-image-undo-enable image)
  (gimp-undo-push-group-end image)
  (gimp-displays-flush)
)

(script-fu-register "script-fu-simple-tizas"
   _"<Image>/Filters/Decor/Blackboard effect..."
   "Parece como si lo hubieran dibujado en una pizarra con tizas de colores. Este efecto funciona mal con fotos JPEG excesivamente comprimidas."
   "Is based in this script http://gimp.org/docs/scheme_plugin/scheme-sample.html by Simon Budig <simon@gimp.org> / Esta basado en ese guion de Simon Budig."
   "Pucelo (based on a Simon Budig sample script) for www.gimp.org.es"
   "2007/4/21"
   "RGB*"
   SF-IMAGE "Image" 0
   SF-DRAWABLE "Drawable" 0
   SF-TOGGLE "Trabajar sobre copia (Work on copy)" FALSE
   SF-TOGGLE "Aplanar la imagen al final (Flatten image at finish)" TRUE
)
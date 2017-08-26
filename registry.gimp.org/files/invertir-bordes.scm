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


(define (script-fu-invertir-bordes img drawable copy)
  (define image (if
    (= copy TRUE)
    (car (gimp-image-duplicate img))
    img
  ))
  (gimp-undo-push-group-start image); Empezar un bloque de deshacer. / Start a undo group.
  (define drawable (car (gimp-image-flatten image))); La imagen es aplanada / The image is flatened

  ;crear matriz
  (define matriz (cons-array 25 'double))
  ;cargar valores en matriz como si fuera un vector
  (aset matriz 0 1)  (aset matriz 1 1)  (aset matriz 2 1)    (aset matriz 3 1)  (aset matriz 4 1)  ;primera fila
  (aset matriz 5 1)  (aset matriz 6 0)  (aset matriz 7 0)    (aset matriz 8 0)  (aset matriz 9 1)  ;segunda fila
  (aset matriz 10 1) (aset matriz 11 0) (aset matriz 12 -12) (aset matriz 13 0) (aset matriz 14 1) ;tercera fila
  (aset matriz 15 1) (aset matriz 16 0) (aset matriz 17 0)   (aset matriz 18 0) (aset matriz 19 1) ;cuarta fila
  (aset matriz 20 1) (aset matriz 21 1) (aset matriz 22 1)   (aset matriz 23 1) (aset matriz 24 1) ;quinta fila
  ;vector de canales
  (define canales (cons-array 5 'long))
  (aset canales 0 1)
  (aset canales 1 1)
  (aset canales 2 1)
  (aset canales 3 1)
  (aset canales 4 0)
  ;aplicar el filtro, divisor=4 y desplazamiento=0.
  (plug-in-convmatrix 1 image drawable 25  matriz 0 4 0 5 canales 0)
  ;continuar
  (gimp-image-set-active-layer image drawable)
  (if
    (= copy TRUE)
    (gimp-display-new image)
    ()
  )
  (gimp-undo-push-group-end image)
  (gimp-displays-flush)
)

(script-fu-register "script-fu-invertir-bordes"
   _"<Image>/Filters/Decor/Border invert..."
   "Inverte los colores en los bordes de los elementos de una foto, aunque la emborrona un poco. Fue sacado de http://soleup.eup.uva.es/mediawiki/index.php/Documentaci%C3%B3n_del_curso_de_GIMP_y_otros"
   "Is based in this script http://gimp.org/docs/scheme_plugin/scheme-sample.html by Simon Budig <simon@gimp.org> / Esta basado en ese guion de Simon Budig."
   "Pucelo (based on a Simon Budig sample script) for www.gimp.org.es"
   "2007/8/22"
   "RGB*"
   SF-IMAGE "Image" 0
   SF-DRAWABLE "Drawable" 0
   SF-TOGGLE "Trabajar sobre copia (Work on copy)" FALSE
)
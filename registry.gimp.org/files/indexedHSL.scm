; indexedHSL.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.1 (20091222)

; Changes:
; v1.1 Clean up and speed improvements.

; Description
; shift the HSV of all palette entries for the current image.
; colour mapping formulae are from wikipedia:
; http://en.wikipedia.org/wiki/HSL_and_HSV#Formal_specifications
; It heeps the HSL values in floatingpoint to reduce conversion errors

; License:
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
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (script-fu-indexedHSL img inLayer inH inL inS)
 
  ; takes RGB as 0-255, returns '(H S L) as 0-360, 0-1, 0-1
  (define (RGB2HSL R G B) 
    (let* ((R (/ R 255)) (G (/ G 255)) (B (/ B 255)) (maxRGB (max R G B)) (minRGB (min R G B)) (H 0) (S 0) (L 0))
      (set! H 
        (cond 
          ((= maxRGB minRGB) 0)
          ((= maxRGB R) (+ (* 60 (/ (- G B) (- maxRGB minRGB))) 360))
          ((= maxRGB G) (+ (* 60 (/ (- B R) (- maxRGB minRGB))) 120))
          ((= maxRGB B) (+ (* 60 (/ (- R G) (- maxRGB minRGB))) 240))
        )
      )
      (when (< H 0) (set! H (+ H 360)))     ;modulo
      (when (>= H 360) (set! H (- H 360)))  ;for real numbers
      (set! L (/ (+ maxRGB minRGB) 2))
      (set! S 
        (cond
          ((= maxRGB minRGB) 0)
          ((<= L 0.5) (/ (- maxRGB minRGB) (+ maxRGB minRGB)))
          ((> L 0.5) (/ (- maxRGB minRGB) (- 2 (+ maxRGB minRGB))))
        )
      )
      (list H S L)
    )
  )
  
  ; takes HSL as 0-360, 0-1, 0-1, returns '(R G B) as 0-255 integers
  (define (HSL2RGB H S L) ; takes HSL as 0-360, 0-1, 0-1, returns '(R G B) as 0-255 integers
    (let* 
      (
        (H (/ H 360)) (R 0) (G 0) (B 0)
        (q (if (< L 0.5) (* L (+ 1 S)) (- (+ L S) (* L S))))
        (p (- (* 2 L) q)) (tR (+ H (/ 1 3))) (tG H) (tB (- H (/ 1 3)))
      )
      (when (< tR 0) (set! tR (+ tR 1)))
      (when (> tR 1) (set! tR (- tR 1)))
      (when (< tG 0) (set! tG (+ tG 1)))
      (when (> tG 1) (set! tG (- tG 1)))
      (when (< tB 0) (set! tB (+ tB 1)))
      (when (> tB 1) (set! tB (- tB 1)))
      (set! R 
        (cond
          ((< tR (/ 1 6)) (+ p (* (- q p) 6 tR)))
          ((< tR 0.5) q)
          ((< tR (/ 2 3)) (+ p (* (- q p) 6 (- (/ 2 3) tR))))
          (p)
        )
      )
      (set! G 
        (cond
          ((< tG (/ 1 6)) (+ p (* (- q p) 6 tG)))
          ((< tG 0.5) q)
          ((< tG (/ 2 3)) (+ p (* (- q p) 6 (- (/ 2 3) tG))))
          (p)
        )
      )
      (set! B 
        (cond
          ((< tB (/ 1 6)) (+ p (* (- q p) 6 tB)))
          ((< tB 0.5) q)
          ((< tB (/ 2 3)) (+ p (* (- q p) 6 (- (/ 2 3) tB))))
          (p)
        )
      )
      (list (trunc (* R 255)) (trunc (* G 255)) (trunc (* B 255)))
    )
  )       

  ;---------------------
  ;main code begins here
  ;---------------------
  (let* 
    (
      (numColours (/ (car (gimp-image-get-colormap img)) 3))
      (origCM (cadr (gimp-image-get-colormap img)))
      (newCM origCM)
      (inS (/ inS 100)) ; aligned to HSL tool
      (inL (/ inL 200)) ; aligned to HSL tool
      (RGBColour '(0 0 0))
      (HSLColour '(0 0 0))
      (newH 0)
      (newS 0)
      (newL 0)
      (i 0)
    )     

    (gimp-context-push)  
    (gimp-image-undo-group-start img)
    
    (while (< i numColours)
      (begin
        (gimp-progress-update (/ i numColours))
        (set! HSLColour (RGB2HSL (vector-ref origCM (* i 3)) (vector-ref origCM (+ (* i 3) 1)) (vector-ref origCM (+ (* i 3) 2))))
    
        ; Hue
        (set! newH (+ (car HSLColour) inH))
        (when (> newH 360) (set! newH (- newH 360))) ; mod 360
        
        ; Saturation (This is the algorithm gimp uses)
        (set! newS (max 0 (min 1 (+ (cadr HSLColour) (* (cadr HSLColour) inS))))        )
    
        ; Lightness (This is the algorithm gimp uses)
        (set! newL (caddr HSLColour))
        (set! newL 
          (cond 
            ((> inL 0) (+ newL (* (- 1 newL) inL)))
            ((< inL 0) (+ newL (* newL inL)))
            (newL)
          )
        )
    
        (set! RGBColour (HSL2RGB newH newS newL))
    
        (aset newCM (* i 3) (car RGBColour))
        (aset newCM (+ (* i 3) 1) (cadr RGBColour))
        (aset newCM (+ (* i 3) 2) (caddr RGBColour))
    
        (set! i (+ i 1))
      )
   )    
   
   (gimp-image-set-colormap img (* numColours 3) newCM)
   (gimp-image-undo-group-end img)
   (gimp-displays-flush)
   (gimp-context-pop)
  )
)

(script-fu-register "script-fu-indexedHSL"
                    _"_Indexed Hue-Saturation..."
                    _"Modify the HSL of an indexed image."
                   "Rob Antonishen"
                   "Rob Antonishen"
                   "Dec 2009"
                   "INDEXED*"
                    SF-IMAGE       "Image"                0
                    SF-DRAWABLE    "Drawable"             0
                    SF-ADJUSTMENT  "Hue Adjust"           '(0 0 359 1 90 0 SF-SLIDER)
                    SF-ADJUSTMENT  "Lightness Adjust"     '(0 -100 100 1 10 0 SF-SLIDER)
                    SF-ADJUSTMENT  "Saturation Adjust"    '(0 -100 100 1 10 0 SF-SLIDER)
)

(script-fu-menu-register "script-fu-indexedHSL"
                         "<Image>/Colors"
)
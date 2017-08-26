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
; Copyright (C) 2011 AdSR
;
; Version 1.1 - Simulate channel response of some b&w films
; Original author: AdSR
; (C) 2011
;
; Tags: photo, b&w, monochrome, film, desaturation
;
; See: http://www.prime-junta.net/pont/How_to/100_Curves_and_Films/_Curves_and_films.html

(define (adsr-bw-films image layer type on-copy)
  (gimp-image-undo-group-start image)
 
  (let* ((bw-layer layer)
         (films '(("Agfa 200X"        18 41 41)
                  ("Agfapan 25"       25 39 36)
                  ("Agfapan 100"      21 40 39)
                  ("Agfapan 400"      20 41 39)
                  ("Ilford Delta 100" 21 42 37)
                  ("Ilford Delta 400" 22 42 36)
                  ("Ilford Delta 400 Pro & 3200"
                   31 36 33)
                  ("Ilford FP4"       28 41 31)
                  ("Ilford HP5"       23 37 40)
                  ("Ilford Pan F"     33 36 31)
                  ("Ilford SFX"       36 31 33)
                  ("Ilford XP2 Super" 21 42 37)
                  ("Kodak Tmax 100"   24 37 39)
                  ("Kodak Tmax 400"   27 36 37)
                  ("Kodak Tri-X"      25 35 40)))
         (film (list-ref films type))
         (rgb-factors (map (lambda (x) (* 0.01 x)) (cdr film))))

    (if (= on-copy TRUE)
      (begin
        (set! bw-layer (car (gimp-layer-copy layer TRUE)))
        (gimp-image-add-layer image bw-layer -1)
        (gimp-drawable-set-name bw-layer (car film))))

    (plug-in-colors-channel-mixer TRUE image bw-layer TRUE
                                  (car rgb-factors)
                                  (cadr rgb-factors)
                                  (caddr rgb-factors)
                                  0 0 0 0 0 0))

  (gimp-image-undo-group-end image)
  (gimp-displays-flush))

(script-fu-register "adsr-bw-films"
                    _"Black-and-White Film"
                    _"Desaturates the way various B&W films do."
                    "AdSR (adsr at poczta onet pl)"
                    "Copyright (C) 2011 AdSR"
                    "2011-02-13"
                    "RGB*"
                    SF-IMAGE    "Input image"    0
                    SF-DRAWABLE "Input drawable" 0
                    SF-OPTION   _"Film" '("Agfa 200X"
                                          "Agfapan 25"
                                          "Agfapan 100"
                                          "Agfapan 400"
                                          "Ilford Delta 100"
                                          "Ilford Delta 400"
                                          "Ilford Delta 400 Pro & 3200"
                                          "Ilford FP4"
                                          "Ilford HP5"
                                          "Ilford Pan F"
                                          "Ilford SFX"
                                          "Ilford XP2 Super"
                                          "Kodak Tmax 100"
                                          "Kodak Tmax 400"
                                          "Kodak Tri-X")
                    SF-TOGGLE _"Work on Copy" TRUE)

(script-fu-menu-register "adsr-bw-films" _"<Image>/Colors")

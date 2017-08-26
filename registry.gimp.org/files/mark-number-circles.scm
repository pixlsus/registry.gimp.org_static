; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
;
; Mark Number Circles -- GIMP script to make sequencial number along a path
; Copyright (C) 2011 Silas Silva


; This program tries to solve a long-term problem for users of image processing
; programs.  Sometimes it is necessary to "label" (or "mark") parts of a picture
; to be referenced in a document.  It is hard and repetitive to draw a circle, a
; number within it, etc., so I developed this script.
;
; To work with it, it is first necessary to draw a path on the image, with the
; vector/path tool, then call the Mark Number Circles script that will create
; one mark for node on the path.


; TODO: this script does not work well with numbers > 9, because of the circle
; size.  It should be corrected in a future version.

; Main function.  Asks for two parameters:  the circle radius and the image to
; work with.
(define (mark-number-circles radius image)
  (let ((vec (car (drop (gimp-path-get-points image
                        ; TODO: it looks really ugly to use too many car and cdr
                        (car (car (cdr (gimp-path-list image))))) 3))))
    (make-circles (vector->list vec) radius image)))

; Quick (and dirty?) implementation of the drop function of SRFI 1 extensions to
; Scheme, not implemented in TinyScheme (the interpreter used by Gimp).  It
; drops n elements of the list and returns the rest.
(define (drop lis n)
  (cond ((= n 0) lis)
        ((not (pair? lis)) lis)
        (else (drop (cdr lis) (- n 1)))))

; Make all circles.  lis is the list of coordinates.  It is the vector returned
; by gimp-path-list, but transformed in a list with vector->list.  See
; mark-number-circles function for detais.  radius is the circle radius and
; image is the image to work with.
(define (make-circles lis radius image)
  ; The i variable will hold the number to be printed within the circle
  (let ((i 1))
    (while (pair? lis)
           ; Extract x and y information
           (let ((x (car lis))
                 (y (car (cdr lis))))
             (gimp-image-undo-group-start image)
             (make-circle x y radius (number->string i) image))
             (gimp-image-undo-group-end image)

           ; Other itens are repetitive for us, so drop it.
           (set! lis (drop lis 9))
           (set! i (+ i 1)))))

; Draw ONE circle.  It receives x and y coordinates for the center of the layer
; (and therefore, the circle), the radius of the circle, the text that will be
; printed inside the circle and the image to work with.
(define (make-circle x y radius text image)
  ; Create a new layer and define x0 and y0 the top-left coordinate as to make
  ; it be in the center of x y
  (let ((layer (car (gimp-layer-new image radius radius RGBA-IMAGE "Circle" 100 NORMAL-MODE)))
        (x0       (- x  (/ radius 2)))
        (y0       (- y  (/ radius 2)))
        (fontsize (/ radius 1.5)))

    ; Adjust layer position
    (gimp-layer-set-offsets layer x0 y0)
    (gimp-image-add-layer image layer 0)
    (gimp-image-set-active-layer image layer)

    ; Make a circular selection
    (gimp-ellipse-select image x0 y0 radius radius REPLACE TRUE FALSE 10)

    ; Paint selection
    (gimp-edit-fill layer BG-IMAGE-FILL)
    (gimp-layer-set-name layer text)

    ; Creates text
    (let* ((text-layer (car (gimp-text-fontname image layer 0 0
                              text 0 TRUE fontsize PIXELS "Sans Bold")))
           ; Get text width and height
           (width      (car (gimp-drawable-width text-layer)))
           (height     (car (gimp-drawable-height text-layer)))

           ; Discover xt and yt the toplevel coordinate to make text be in the
           ; center of the circle.
           (xt         (- x (/ width 2)))
           (yt         (- y (/ height 2))))

      ; Position text.
      (gimp-layer-set-offsets text-layer xt yt)
      (gimp-floating-sel-anchor text-layer)
      (gimp-selection-none image)

      ; Flush contents.
      (gimp-displays-flush))))


; Register the script for GIMP.
(script-fu-register "mark-number-circles"
                    "Mark Number Circles"
                    "Produce \"circles\" to be used as numeric marks"
                    "Silas Silva <silasdb@gmail.com>"
                    "Copyright (C) 2011 Silas Silva"
                    "2011-02-21"
                    ""
                    SF-VALUE    "Circle Radius"   "50"
                    SF-IMAGE    "Image"             0
                    )

; Register the script in the menu
(script-fu-menu-register "mark-number-circles" "<Image>/Script-Fu")

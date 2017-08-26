;;; animation-rotation.scm
;;; Date: <2012-06-18 14:50 roan.horning@gmail.com>
;;; Author: Roan Horning <roan.horning@gmail.com>
;;; Version 1.11

;;; Version 1.11 for Gimp 2.4 and later

;;; This helps create simple rotating animations.

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; returns a two item list containg the angle to rotate the original image
; and the direction to rotate the image. The rotation-angle argument is 
; given in degrees from 0 to 360, the first return value is in degrees 
; between 0 and 180, the second return value denotes the direction of 
; rotation: 0 for clockwise and 1 for counterclockwise
(define (get-negative-motion rotation-angle)
  (if (> rotation-angle 180)
    (append (list (- 360 rotation-angle)) '(1))
    (append (list rotation-angle) '(0))))

(define (degrees->radians ang)
  (let* ((pi (* 4 (atan 1.0))))
    (/ (* pi ang) 180)))

; create a new layer rotated by desired degrees from a given layer
(define (layer-rotate-new img background rotate-angle)
  (let* ((layer-rotate (car (gimp-layer-new-from-drawable background 
                                                          img)))
         (layer-name (string-append "Rotate" (number->string rotate-angle)))
         (rotate-motion (get-negative-motion rotate-angle))
         )
    (gimp-image-add-layer img layer-rotate -1)
    (gimp-layer-set-name layer-rotate layer-name)
    (gimp-drawable-transform-rotate layer-rotate ; drawable 
                                    (degrees->radians (car rotate-motion))
                                    ; angle 
                                    FALSE         ; auto-center 
                                    (+ (car (gimp-drawable-offsets layer-rotate)) 


                                       (/ (car (gimp-drawable-width layer-rotate)) 2)) ; center-x 
                                    (+ (cadr (gimp-drawable-offsets layer-rotate)) 
                                       (/ (car (gimp-drawable-height layer-rotate)) 2)) ; center-y 
                                    (cadr rotate-motion)
                                    ; transform-direction 
                                    TRUE        ; interpolation 
                                    TRUE        ; supersample 
                                    3            ; recursion-level 
                                    1            ; clip-result
                                    )
    (gimp-image-raise-layer-to-top img layer-rotate)
    (gimp-image-set-active-layer img background)
    ))

; create a list of angles between 0 and 360 based on the number of frames
; to be used in the animation
(define (get-angles rotate-angles num-frames)
  (if (= num-frames 1)
    rotate-angles 
    (get-angles (append rotate-angles (list (- 360 (* (car rotate-angles) 
                                                      (- num-frames 1))))) 
                        (- num-frames 1))))

; recusive iteration used to generate list of angles
(define (get-frames-angles num-frames)
  (cdr (get-angles (list (/ 360 num-frames)) num-frames)))

; pendulum effect angles are between 0 and 180
(define (get-pendulum-angle rotation-angle)
  (if (> rotation-angle 180)
    (set! rotation-angle (- 360 rotation-angle))
    (set! rotation-angle rotation-angle)))

; modify list of frames to generate pendulum effect
(define (get-pendulum-frames-angles frames-angles)
  (map get-pendulum-angle frames-angles))

(define (script-fu-rotation img drw num-frames motion)
  (let* ((layer-background (car (gimp-image-get-active-layer img)))
         (frame-angles (get-frames-angles num-frames))
         (pendulum 1)
         )
    (if (= motion pendulum)
      (set! frame-angles (get-pendulum-frames-angles frame-angles)))

    (let loop ((angles frame-angles ))
      (if (not (null? angles))
        (begin
          (layer-rotate-new img layer-background  (car angles))
          (loop (cdr angles)))))

    (gimp-displays-flush)))

(script-fu-register
  "script-fu-rotation"
  "_Rotation Animation"
  "Create a simple rotating animation."
  "Roan Horning <roan.horning@gmail.com>"
  "Roan Horning"
  "02. 07. 2012"
  "RGB RGBA GRAY GRAYA"
  SF-IMAGE "Image" 0
  SF-DRAWABLE "Drawable" 0
  SF-ADJUSTMENT "Number of times to rotate image" '(8 4 24 4 1 1 0)
  SF-OPTION "Animation motion" '("circular" "pendulum") )

; new in 2.2, for older versions give the full menu path in the
; second parameter of the script-fu-register call.
(script-fu-menu-register "script-fu-rotation"
                         "<Image>/Script-Fu/Animation")

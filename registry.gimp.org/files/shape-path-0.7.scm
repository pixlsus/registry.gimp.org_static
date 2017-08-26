; GIMP Shape Paths
; Copyright (c) 2004-2007 Jonathan Stipe and Pucelo
; JonStipe@prodigy.net

; ---------------------------------------------------------------------

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (get-kappa) (* (/ (- (sqrt 2) 1) 3) 4))

(define (deg->rad input)
  (* input (/ *pi* 180))
)

(define (translate-point x y angle distance)
  (let* ((ang (deg->rad angle))
         (nx (+ (* distance (cos ang)) x))
         (ny (+ (* distance (sin ang)) y)))
    (list nx ny)
  )
)

(define (choose-name img name)
  (let* ((pathdata (gimp-path-list img))
         (numpaths (car pathdata))
         (pathlist (cadr pathdata))
         (nname name)
         (i 0))
    (if (> numpaths 0)
      (if (= (search-pathlist name pathlist numpaths) 1)
        (begin
          (while (= (search-pathlist nname pathlist numpaths) 1)
            (set! i (+ i 1))
            (set! nname (string-append name "#" (number->string i 10)))
          )
          nname
        )
        name
      )
      name
    )
  )
)

(define (search-pathlist name pathlist length)
  (let* ((i 0)
         (keepgoing 1)
         (output 0))
    (while (= keepgoing 1)
      (if (equal? name (nth i pathlist))
        (begin
          (set! output 1)
          (set! keepgoing 0)
        )
        (begin
          (set! i (+ i 1))
          (if (= i length)
            (set! keepgoing 0)
          )
        )
      )
    )
    output
  )
)

(define (script-fu-rectangle-path img
				drawable
				name
				top
				left
				bottom
				right)
  (gimp-undo-push-group-start img)
  (let* ((pathpoints (cons-array 36 (quote double)))
        (pathname (choose-name img name)))
    (aset pathpoints 0 left)
    (aset pathpoints 1 top)
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 left)
    (aset pathpoints 4 top)
    (aset pathpoints 5 2.0)
    (aset pathpoints 6 right)
    (aset pathpoints 7 top)
    (aset pathpoints 8 2.0)
    (aset pathpoints 9 right)
    (aset pathpoints 10 top)
    (aset pathpoints 11 1.0)
    (aset pathpoints 12 right)
    (aset pathpoints 13 top)
    (aset pathpoints 14 2.0)
    (aset pathpoints 15 right)
    (aset pathpoints 16 bottom)
    (aset pathpoints 17 2.0)
    (aset pathpoints 18 right)
    (aset pathpoints 19 bottom)
    (aset pathpoints 20 1.0)
    (aset pathpoints 21 right)
    (aset pathpoints 22 bottom)
    (aset pathpoints 23 2.0)
    (aset pathpoints 24 left)
    (aset pathpoints 25 bottom)
    (aset pathpoints 26 2.0)
    (aset pathpoints 27 left)
    (aset pathpoints 28 bottom)
    (aset pathpoints 29 1.0)
    (aset pathpoints 30 left)
    (aset pathpoints 31 bottom)
    (aset pathpoints 32 2.0)
    (aset pathpoints 33 left)
    (aset pathpoints 34 top)
    (aset pathpoints 35 2.0)
    (gimp-path-set-points img pathname 1 36 pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-square-path img
				drawable
				name
				top
				left
				size)
  (gimp-undo-push-group-start img)
  (let* ((pathpoints (cons-array 36 (quote double)))
        (right (+ left size))
        (bottom (+ top size))
        (pathname (choose-name img name)))
    (aset pathpoints 0 left)
    (aset pathpoints 1 top)
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 left)
    (aset pathpoints 4 top)
    (aset pathpoints 5 2.0)
    (aset pathpoints 6 right)
    (aset pathpoints 7 top)
    (aset pathpoints 8 2.0)
    (aset pathpoints 9 right)
    (aset pathpoints 10 top)
    (aset pathpoints 11 1.0)
    (aset pathpoints 12 right)
    (aset pathpoints 13 top)
    (aset pathpoints 14 2.0)
    (aset pathpoints 15 right)
    (aset pathpoints 16 bottom)
    (aset pathpoints 17 2.0)
    (aset pathpoints 18 right)
    (aset pathpoints 19 bottom)
    (aset pathpoints 20 1.0)
    (aset pathpoints 21 right)
    (aset pathpoints 22 bottom)
    (aset pathpoints 23 2.0)
    (aset pathpoints 24 left)
    (aset pathpoints 25 bottom)
    (aset pathpoints 26 2.0)
    (aset pathpoints 27 left)
    (aset pathpoints 28 bottom)
    (aset pathpoints 29 1.0)
    (aset pathpoints 30 left)
    (aset pathpoints 31 bottom)
    (aset pathpoints 32 2.0)
    (aset pathpoints 33 left)
    (aset pathpoints 34 top)
    (aset pathpoints 35 2.0)
    (gimp-path-set-points img pathname 1 36 pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-circle-path img
				drawable
				name
				x
				y
				radius)
  (gimp-undo-push-group-start img)
  (let* ((pathpoints (cons-array 36 (quote double)))
        (top (- y radius))
        (right (+ x radius))
        (bottom (+ y radius))
        (left (- x radius))
        (clength (* radius (get-kappa)))
        (cup (- y clength))
        (cright (+ x clength))
        (cdown (+ y clength))
        (cleft (- x clength))
        (pathname (choose-name img name)))
    (aset pathpoints 0 x)
    (aset pathpoints 1 top)
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 cright)
    (aset pathpoints 4 top)
    (aset pathpoints 5 2.0)
    (aset pathpoints 6 right)
    (aset pathpoints 7 cup)
    (aset pathpoints 8 2.0)
    (aset pathpoints 9 right)
    (aset pathpoints 10 y)
    (aset pathpoints 11 1.0)
    (aset pathpoints 12 right)
    (aset pathpoints 13 cdown)
    (aset pathpoints 14 2.0)
    (aset pathpoints 15 cright)
    (aset pathpoints 16 bottom)
    (aset pathpoints 17 2.0)
    (aset pathpoints 18 x)
    (aset pathpoints 19 bottom)
    (aset pathpoints 20 1.0)
    (aset pathpoints 21 cleft)
    (aset pathpoints 22 bottom)
    (aset pathpoints 23 2.0)
    (aset pathpoints 24 left)
    (aset pathpoints 25 cdown)
    (aset pathpoints 26 2.0)
    (aset pathpoints 27 left)
    (aset pathpoints 28 y)
    (aset pathpoints 29 1.0)
    (aset pathpoints 30 left)
    (aset pathpoints 31 cup)
    (aset pathpoints 32 2.0)
    (aset pathpoints 33 cleft)
    (aset pathpoints 34 top)
    (aset pathpoints 35 2.0)
    (gimp-path-set-points img pathname 1 36 pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-ellipse-path img
				drawable
				name
				x
				y
				radiusx
				radiusy
				rotation)
  (gimp-undo-push-group-start img)
  (let* ((pathpoints (cons-array 36 (quote double)))
        (kappa (get-kappa))
        (clengthx (* radiusx kappa))
        (clengthy (* radiusy kappa))
        (aone (translate-point x y (- rotation 90) radiusy))
        (atwo (translate-point x y rotation radiusx))
        (athree (translate-point x y (+ rotation 90) radiusy))
        (afour (translate-point x y (+ rotation 180) radiusx))
        (conea (translate-point (car aone) (cadr aone) (+ rotation 180) clengthx))
        (coneb (translate-point (car aone) (cadr aone) rotation clengthx))
        (ctwoa (translate-point (car atwo) (cadr atwo) (- rotation 90) clengthy))
        (ctwob (translate-point (car atwo) (cadr atwo) (+ rotation 90) clengthy))
        (cthreea (translate-point (car athree) (cadr athree) rotation clengthx))
        (cthreeb (translate-point (car athree) (cadr athree) (+ rotation 180) clengthx))
        (cfoura (translate-point (car afour) (cadr afour) (+ rotation 90) clengthy))
        (cfourb (translate-point (car afour) (cadr afour) (- rotation 90) clengthy))
        (pathname (choose-name img name)))
    (aset pathpoints 0 (car aone))
    (aset pathpoints 1 (cadr aone))
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 (car coneb))
    (aset pathpoints 4 (cadr coneb))
    (aset pathpoints 5 2.0)
    (aset pathpoints 6 (car ctwoa))
    (aset pathpoints 7 (cadr ctwoa))
    (aset pathpoints 8 2.0)
    (aset pathpoints 9 (car atwo))
    (aset pathpoints 10 (cadr atwo))
    (aset pathpoints 11 1.0)
    (aset pathpoints 12 (car ctwob))
    (aset pathpoints 13 (cadr ctwob))
    (aset pathpoints 14 2.0)
    (aset pathpoints 15 (car cthreea))
    (aset pathpoints 16 (cadr cthreea))
    (aset pathpoints 17 2.0)
    (aset pathpoints 18 (car athree))
    (aset pathpoints 19 (cadr athree))
    (aset pathpoints 20 1.0)
    (aset pathpoints 21 (car cthreeb))
    (aset pathpoints 22 (cadr cthreeb))
    (aset pathpoints 23 2.0)
    (aset pathpoints 24 (car cfoura))
    (aset pathpoints 25 (cadr cfoura))
    (aset pathpoints 26 2.0)
    (aset pathpoints 27 (car afour))
    (aset pathpoints 28 (cadr afour))
    (aset pathpoints 29 1.0)
    (aset pathpoints 30 (car cfourb))
    (aset pathpoints 31 (cadr cfourb))
    (aset pathpoints 32 2.0)
    (aset pathpoints 33 (car conea))
    (aset pathpoints 34 (cadr conea))
    (aset pathpoints 35 2.0)
    (gimp-path-set-points img pathname 1 36 pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-oval-path img
				drawable
				name
				x
				y
				radiustop
				radiusleft
				radiusdown
				radiusright)
  (gimp-undo-push-group-start img)
  (let* ((pathpoints (cons-array 36 (quote double)))
        (top (- y radiustop))
        (right (+ x radiusright))
        (bottom (+ y radiusdown))
        (left (- x radiusleft))
        (kappa (get-kappa))
        (clength1 (* radiustop kappa))
        (clength2 (* radiusright kappa))
        (clength3 (* radiusdown kappa))
        (clength4 (* radiusleft kappa))
        (cup (- y clength1))
        (cright (+ x clength2))
        (cdown (+ y clength3))
        (cleft (- x clength4))
        (pathname (choose-name img name)))
    (aset pathpoints 0 x)
    (aset pathpoints 1 top)
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 cright)
    (aset pathpoints 4 top)
    (aset pathpoints 5 2.0)
    (aset pathpoints 6 right)
    (aset pathpoints 7 cup)
    (aset pathpoints 8 2.0)
    (aset pathpoints 9 right)
    (aset pathpoints 10 y)
    (aset pathpoints 11 1.0)
    (aset pathpoints 12 right)
    (aset pathpoints 13 cdown)
    (aset pathpoints 14 2.0)
    (aset pathpoints 15 cright)
    (aset pathpoints 16 bottom)
    (aset pathpoints 17 2.0)
    (aset pathpoints 18 x)
    (aset pathpoints 19 bottom)
    (aset pathpoints 20 1.0)
    (aset pathpoints 21 cleft)
    (aset pathpoints 22 bottom)
    (aset pathpoints 23 2.0)
    (aset pathpoints 24 left)
    (aset pathpoints 25 cdown)
    (aset pathpoints 26 2.0)
    (aset pathpoints 27 left)
    (aset pathpoints 28 y)
    (aset pathpoints 29 1.0)
    (aset pathpoints 30 left)
    (aset pathpoints 31 cup)
    (aset pathpoints 32 2.0)
    (aset pathpoints 33 cleft)
    (aset pathpoints 34 top)
    (aset pathpoints 35 2.0)
    (gimp-path-set-points img pathname 1 36 pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-rounded-rectangle-path img
				drawable
				name
				top
				left
				bottom
				right
				radiusx
				radiusy)
  (gimp-undo-push-group-start img)
  (let* ((pathpoints (cons-array 72 (quote double)))
        (atop (+ top radiusy))
        (aleft (+ left radiusx))
        (abottom (- bottom radiusy))
        (aright (- right radiusx))
        (kappa (get-kappa))
        (clengthx (* radiusx kappa))
        (clengthy (* radiusy kappa))
        (cup (- atop clengthy))
        (cright (+ aright clengthx))
        (cdown (+ abottom clengthy))
        (cleft (- aleft clengthx))
        (pathname (choose-name img name)))
    (aset pathpoints 0 aleft)
    (aset pathpoints 1 top)
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 aleft)
    (aset pathpoints 4 top)
    (aset pathpoints 5 2.0)
    (aset pathpoints 6 aright)
    (aset pathpoints 7 top)
    (aset pathpoints 8 2.0)
    (aset pathpoints 9 aright)
    (aset pathpoints 10 top)
    (aset pathpoints 11 1.0)
    (aset pathpoints 12 cright)
    (aset pathpoints 13 top)
    (aset pathpoints 14 2.0)
    (aset pathpoints 15 right)
    (aset pathpoints 16 cup)
    (aset pathpoints 17 2.0)
    (aset pathpoints 18 right)
    (aset pathpoints 19 atop)
    (aset pathpoints 20 1.0)
    (aset pathpoints 21 right)
    (aset pathpoints 22 atop)
    (aset pathpoints 23 2.0)
    (aset pathpoints 24 right)
    (aset pathpoints 25 abottom)
    (aset pathpoints 26 2.0)
    (aset pathpoints 27 right)
    (aset pathpoints 28 abottom)
    (aset pathpoints 29 1.0)
    (aset pathpoints 30 right)
    (aset pathpoints 31 cdown)
    (aset pathpoints 32 2.0)
    (aset pathpoints 33 cright)
    (aset pathpoints 34 bottom)
    (aset pathpoints 35 2.0)
    (aset pathpoints 36 aright)
    (aset pathpoints 37 bottom)
    (aset pathpoints 38 1.0)
    (aset pathpoints 39 aright)
    (aset pathpoints 40 bottom)
    (aset pathpoints 41 2.0)
    (aset pathpoints 42 aleft)
    (aset pathpoints 43 bottom)
    (aset pathpoints 44 2.0)
    (aset pathpoints 45 aleft)
    (aset pathpoints 46 bottom)
    (aset pathpoints 47 1.0)
    (aset pathpoints 48 cleft)
    (aset pathpoints 49 bottom)
    (aset pathpoints 50 2.0)
    (aset pathpoints 51 left)
    (aset pathpoints 52 cdown)
    (aset pathpoints 53 2.0)
    (aset pathpoints 54 left)
    (aset pathpoints 55 abottom)
    (aset pathpoints 56 1.0)
    (aset pathpoints 57 left)
    (aset pathpoints 58 abottom)
    (aset pathpoints 59 2.0)
    (aset pathpoints 60 left)
    (aset pathpoints 61 atop)
    (aset pathpoints 62 2.0)
    (aset pathpoints 63 left)
    (aset pathpoints 64 atop)
    (aset pathpoints 65 1.0)
    (aset pathpoints 66 left)
    (aset pathpoints 67 cup)
    (aset pathpoints 68 2.0)
    (aset pathpoints 69 cleft)
    (aset pathpoints 70 top)
    (aset pathpoints 71 2.0)
    (gimp-path-set-points img pathname 1 72 pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-advanced-rounded-rectangle-path img
				drawable
				name
				top
				left
				bottom
				right
				radiusx1
				radiusy1
				radiusx2
				radiusy2
				radiusx4
				radiusy4
				radiusx3
				radiusy3)
  (gimp-undo-push-group-start img)
  (let* ((pathpoints (cons-array 72 (quote double)))
        (atop (+ top radiusy1))
        (aleft (+ left radiusx1))
        (abottom (- bottom radiusy4))
        (aright (- right radiusx2))
        (atop2 (+ top radiusy2))
        (aleft2 (+ left radiusx4))
        (abottom2 (- bottom radiusy3))
        (aright2 (- right radiusx3))
        (kappa (get-kappa))
        (clengthx (* radiusx2 kappa))
        (clengthy (* radiusy1 kappa))
        (cup (- atop clengthy))
        (cright (+ aright clengthx))
        (clengthx (* radiusx1 kappa))
        (clengthy (* radiusy4 kappa))
        (cdown (+ abottom clengthy))
        (cleft (- aleft clengthx))
        (clengthx (* radiusx3 kappa))
        (clengthy (* radiusy2 kappa))
        (cup2 (- atop2 clengthy))
        (cright2 (+ aright2 clengthx))
        (clengthx (* radiusx4 kappa))
        (clengthy (* radiusy3 kappa))
        (cdown2 (+ abottom2 clengthy))
        (cleft2 (- aleft2 clengthx))
        (pathname (choose-name img name)))
    (aset pathpoints 0 aleft)
    (aset pathpoints 1 top)
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 aleft)
    (aset pathpoints 4 top)
    (aset pathpoints 5 2.0)
    (aset pathpoints 6 aright)
    (aset pathpoints 7 top)
    (aset pathpoints 8 2.0)
    (aset pathpoints 9 aright)
    (aset pathpoints 10 top)
    (aset pathpoints 11 1.0)
    (aset pathpoints 12 cright)
    (aset pathpoints 13 top)
    (aset pathpoints 14 2.0)
    (aset pathpoints 15 right)
    (aset pathpoints 16 cup2)
    (aset pathpoints 17 2.0)
    (aset pathpoints 18 right)
    (aset pathpoints 19 atop2)
    (aset pathpoints 20 1.0)
    (aset pathpoints 21 right)
    (aset pathpoints 22 atop2)
    (aset pathpoints 23 2.0)
    (aset pathpoints 24 right)
    (aset pathpoints 25 abottom2)
    (aset pathpoints 26 2.0)
    (aset pathpoints 27 right)
    (aset pathpoints 28 abottom2)
    (aset pathpoints 29 1.0)
    (aset pathpoints 30 right)
    (aset pathpoints 31 cdown2)
    (aset pathpoints 32 2.0)
    (aset pathpoints 33 cright2)
    (aset pathpoints 34 bottom)
    (aset pathpoints 35 2.0)
    (aset pathpoints 36 aright2)
    (aset pathpoints 37 bottom)
    (aset pathpoints 38 1.0)
    (aset pathpoints 39 aright2)
    (aset pathpoints 40 bottom)
    (aset pathpoints 41 2.0)
    (aset pathpoints 42 aleft2)
    (aset pathpoints 43 bottom)
    (aset pathpoints 44 2.0)
    (aset pathpoints 45 aleft2)
    (aset pathpoints 46 bottom)
    (aset pathpoints 47 1.0)
    (aset pathpoints 48 cleft2)
    (aset pathpoints 49 bottom)
    (aset pathpoints 50 2.0)
    (aset pathpoints 51 left)
    (aset pathpoints 52 cdown)
    (aset pathpoints 53 2.0)
    (aset pathpoints 54 left)
    (aset pathpoints 55 abottom)
    (aset pathpoints 56 1.0)
    (aset pathpoints 57 left)
    (aset pathpoints 58 abottom)
    (aset pathpoints 59 2.0)
    (aset pathpoints 60 left)
    (aset pathpoints 61 atop)
    (aset pathpoints 62 2.0)
    (aset pathpoints 63 left)
    (aset pathpoints 64 atop)
    (aset pathpoints 65 1.0)
    (aset pathpoints 66 left)
    (aset pathpoints 67 cup)
    (aset pathpoints 68 2.0)
    (aset pathpoints 69 cleft)
    (aset pathpoints 70 top)
    (aset pathpoints 71 2.0)
    (gimp-path-set-points img pathname 1 72 pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-polygon-path img
				drawable
				name
				x
				y
				radius
				sides
				rotation)
  (gimp-undo-push-group-start img)
  (let* ((numpoints (* sides 9))
        (pathpoints (cons-array numpoints (quote double)))
        (init (- rotation 90))
        (deg 0)
        (i 14)
        (j 1)
        (p (translate-point x y init radius))
        (pathname (choose-name img name)))
    (aset pathpoints 0 (car p))
    (aset pathpoints 1 (cadr p))
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 (car p))
    (aset pathpoints 4 (cadr p))
    (aset pathpoints 5 2.0)
    (aset pathpoints (- numpoints 3) (car p))
    (aset pathpoints (- numpoints 2) (cadr p))
    (aset pathpoints (- numpoints 1) 2.0)
    (while (< j sides)
      (set! deg (+ (* (/ j sides) 360) init))
      (set! p (translate-point x y deg radius))
      (aset pathpoints (- i 8) (car p))
      (aset pathpoints (- i 7) (cadr p))
      (aset pathpoints (- i 6) 2.0)
      (aset pathpoints (- i 5) (car p))
      (aset pathpoints (- i 4) (cadr p))
      (aset pathpoints (- i 3) 1.0)
      (aset pathpoints (- i 2) (car p))
      (aset pathpoints (- i 1) (cadr p))
      (aset pathpoints i 2.0)
      (set! i (+ i 9))
      (set! j (+ j 1))
    )
    (gimp-path-set-points img pathname 1 numpoints pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-star-path img
				drawable
				name
				x
				y
				oradius
				iradius
				points
				rotation)
  (gimp-undo-push-group-start img)
  (let* ((numpoints (* points 18))
        (pathpoints (cons-array numpoints (quote double)))
        (pointstwo (* points 2))
        (radius iradius)
        (init (- rotation 90))
        (deg 0)
        (i 14)
        (j 1)
        (p (translate-point x y init oradius))
        (pathname (choose-name img name)))
    (aset pathpoints 0 (car p))
    (aset pathpoints 1 (cadr p))
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 (car p))
    (aset pathpoints 4 (cadr p))
    (aset pathpoints 5 2.0)
    (aset pathpoints (- numpoints 3) (car p))
    (aset pathpoints (- numpoints 2) (cadr p))
    (aset pathpoints (- numpoints 1) 2.0)
    (while (< j pointstwo)
      (set! deg (+ (* (/ j pointstwo) 360) init))
      (set! p (translate-point x y deg radius))
      (aset pathpoints (- i 8) (car p))
      (aset pathpoints (- i 7) (cadr p))
      (aset pathpoints (- i 6) 2.0)
      (aset pathpoints (- i 5) (car p))
      (aset pathpoints (- i 4) (cadr p))
      (aset pathpoints (- i 3) 1.0)
      (aset pathpoints (- i 2) (car p))
      (aset pathpoints (- i 1) (cadr p))
      (aset pathpoints i 2.0)
      (set! i (+ i 9))
      (set! j (+ j 1))
      (if (= radius oradius)
        (set! radius iradius)
        (set! radius oradius)
      )
    )
    (gimp-path-set-points img pathname 1 numpoints pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-flowerspike-path img
				drawable
				name
				x
				y
				radius
				sides
				rotation
				factora
				anga
				factorb
				angb)
  (gimp-undo-push-group-start img)
  (let* ((numpoints (* sides 9))
        (pathpoints (cons-array numpoints (quote double)))
        (init (- rotation 90))
        (deg 0)
        (i 14)
        (j 1)
        (iradiusa (* radius factora))
        (iradiusb (* radius factorb))
        (p (translate-point x y init radius))
        (pathname (choose-name img name)))
    (aset pathpoints 0 (car p))
    (aset pathpoints 1 (cadr p))
    (aset pathpoints 2 1.0)
    (set! p (translate-point x y (+ init anga) iradiusa))
    (aset pathpoints 3 (car p))
    (aset pathpoints 4 (cadr p))
    (aset pathpoints 5 2.0)
    (set! p (translate-point x y (+ init angb) iradiusb))
    (aset pathpoints (- numpoints 3) (car p))
    (aset pathpoints (- numpoints 2) (cadr p))
    (aset pathpoints (- numpoints 1) 2.0)
    (while (< j sides)
      (set! deg (+ (* (/ j sides) 360) init))
      (set! p (translate-point x y (+ deg angb) iradiusb))
      (aset pathpoints (- i 8) (car p))
      (aset pathpoints (- i 7) (cadr p))
      (aset pathpoints (- i 6) 2.0)
      (set! p (translate-point x y deg radius))
      (aset pathpoints (- i 5) (car p))
      (aset pathpoints (- i 4) (cadr p))
      (aset pathpoints (- i 3) 1.0)
      (set! p (translate-point x y (+ deg anga) iradiusa))
      (aset pathpoints (- i 2) (car p))
      (aset pathpoints (- i 1) (cadr p))
      (aset pathpoints i 2.0)
      (set! i (+ i 9))
      (set! j (+ j 1))
    )
    (gimp-path-set-points img pathname 1 numpoints pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-gear-path img
				drawable
				name
				x
				y
				oradius
				iradius
				teeth
				rotation)
  (gimp-undo-push-group-start img)
  (let* ((numpoints (* teeth 36))
        (pathpoints (cons-array numpoints (quote double)))
        (init (- rotation 90))
        (deg 0)
        (i 68)
        (j 1)
        (p (translate-point x y init iradius))
        (pathname (choose-name img name)))
    (aset pathpoints 0 (car p))
    (aset pathpoints 1 (cadr p))
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 (car p))
    (aset pathpoints 4 (cadr p))
    (aset pathpoints 5 2.0)
    (aset pathpoints (- numpoints 3) (car p))
    (aset pathpoints (- numpoints 2) (cadr p))
    (aset pathpoints (- numpoints 1) 2.0)
    (set! p (translate-point x y init oradius))
    (aset pathpoints 6 (car p))
    (aset pathpoints 7 (cadr p))
    (aset pathpoints 8 2.0)
    (aset pathpoints 9 (car p))
    (aset pathpoints 10 (cadr p))
    (aset pathpoints 11 1.0)
    (aset pathpoints 12 (car p))
    (aset pathpoints 13 (cadr p))
    (aset pathpoints 14 2.0)
    (set! deg (+ (* (/ 0.5 teeth) 360) init))
    (set! p (translate-point x y deg oradius))
    (aset pathpoints 15 (car p))
    (aset pathpoints 16 (cadr p))
    (aset pathpoints 17 2.0)
    (aset pathpoints 18 (car p))
    (aset pathpoints 19 (cadr p))
    (aset pathpoints 20 1.0)
    (aset pathpoints 21 (car p))
    (aset pathpoints 22 (cadr p))
    (aset pathpoints 23 2.0)
    (set! p (translate-point x y deg iradius))
    (aset pathpoints 24 (car p))
    (aset pathpoints 25 (cadr p))
    (aset pathpoints 26 2.0)
    (aset pathpoints 27 (car p))
    (aset pathpoints 28 (cadr p))
    (aset pathpoints 29 1.0)
    (aset pathpoints 30 (car p))
    (aset pathpoints 31 (cadr p))
    (aset pathpoints 32 2.0)
    (while (< j teeth)
      (set! deg (+ (* (/ j teeth) 360) init))
      (set! p (translate-point x y deg iradius))
      (aset pathpoints (- i 35) (car p))
      (aset pathpoints (- i 34) (cadr p))
      (aset pathpoints (- i 33) 2.0)
      (aset pathpoints (- i 32) (car p))
      (aset pathpoints (- i 31) (cadr p))
      (aset pathpoints (- i 30) 1.0)
      (aset pathpoints (- i 29) (car p))
      (aset pathpoints (- i 28) (cadr p))
      (aset pathpoints (- i 27) 2.0)
      (set! p (translate-point x y deg oradius))
      (aset pathpoints (- i 26) (car p))
      (aset pathpoints (- i 25) (cadr p))
      (aset pathpoints (- i 24) 2.0)
      (aset pathpoints (- i 23) (car p))
      (aset pathpoints (- i 22) (cadr p))
      (aset pathpoints (- i 21) 1.0)
      (aset pathpoints (- i 20) (car p))
      (aset pathpoints (- i 19) (cadr p))
      (aset pathpoints (- i 18) 2.0)
      (set! deg (+ (* (/ (+ j 0.5) teeth) 360) init))
      (set! p (translate-point x y deg oradius))
      (aset pathpoints (- i 17) (car p))
      (aset pathpoints (- i 16) (cadr p))
      (aset pathpoints (- i 15) 2.0)
      (aset pathpoints (- i 14) (car p))
      (aset pathpoints (- i 13) (cadr p))
      (aset pathpoints (- i 12) 1.0)
      (aset pathpoints (- i 11) (car p))
      (aset pathpoints (- i 10) (cadr p))
      (aset pathpoints (- i 9) 2.0)
      (set! p (translate-point x y deg iradius))
      (aset pathpoints (- i 8) (car p))
      (aset pathpoints (- i 7) (cadr p))
      (aset pathpoints (- i 6) 2.0)
      (aset pathpoints (- i 5) (car p))
      (aset pathpoints (- i 4) (cadr p))
      (aset pathpoints (- i 3) 1.0)
      (aset pathpoints (- i 2) (car p))
      (aset pathpoints (- i 1) (cadr p))
      (aset pathpoints i 2.0)
      (set! i (+ i 36))
      (set! j (+ j 1))
    )
    (gimp-path-set-points img pathname 1 numpoints pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-triangle-wave-path img
				drawable
				name
				x
				y
				amplitude
				wavelength
				cycles
				shearing
				rounding)
  (gimp-undo-push-group-start img)
  (let* ((numpoints (+ (* 18 cycles) 15))
        (pathpoints (cons-array numpoints (quote double)))
        (halfamplitude (/ amplitude 2))
        (wavelength1 (/ wavelength 4))
        (wavelength3 (* wavelength1 3))
        (wavelength2 (* wavelength rounding))
        (shear (* wavelength shearing))
        (dist x)
        (i 0)
        (pathname (choose-name img name)))
    (aset pathpoints 0 x)
    (aset pathpoints 1 y)
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 x)
    (aset pathpoints 4 y)
    (aset pathpoints 5 2.0)
    (while (< i cycles)
      (set! idxoffset (* 18 i))
      (aset pathpoints (+ idxoffset 6) (- (+ (+ dist shear) wavelength1) wavelength2))
      (aset pathpoints (+ idxoffset 7) (- y halfamplitude))
      (aset pathpoints (+ idxoffset 8) 2.0)
      (aset pathpoints (+ idxoffset 9) (+ (+ dist shear) wavelength1))
      (aset pathpoints (+ idxoffset 10) (- y halfamplitude))
      (aset pathpoints (+ idxoffset 11) 1.0)
      (aset pathpoints (+ idxoffset 12) (+ (+ (+ dist shear) wavelength1) wavelength2))
      (aset pathpoints (+ idxoffset 13) (- y halfamplitude))
      (aset pathpoints (+ idxoffset 14) 2.0)
      (aset pathpoints (+ idxoffset 15) (- (+ (- dist shear) wavelength3) wavelength2))
      (aset pathpoints (+ idxoffset 16) (+ y halfamplitude))
      (aset pathpoints (+ idxoffset 17) 2.0)
      (aset pathpoints (+ idxoffset 18) (+ (- dist shear) wavelength3))
      (aset pathpoints (+ idxoffset 19) (+ y halfamplitude))
      (aset pathpoints (+ idxoffset 20) 1.0)
      (aset pathpoints (+ idxoffset 21) (+ (+ (- dist shear) wavelength3) wavelength2))
      (aset pathpoints (+ idxoffset 22) (+ y halfamplitude))
      (aset pathpoints (+ idxoffset 23) 2.0)
      (set! dist (+ dist wavelength))
      (set! i (+ i 1))
    )
    (set! idxoffset (* 18 i))
    (aset pathpoints (+ idxoffset 6) dist)
    (aset pathpoints (+ idxoffset 7) y)
    (aset pathpoints (+ idxoffset 8) 2.0)
    (aset pathpoints (+ idxoffset 9) dist)
    (aset pathpoints (+ idxoffset 10) y)
    (aset pathpoints (+ idxoffset 11) 1.0)
    (aset pathpoints (+ idxoffset 12) dist)
    (aset pathpoints (+ idxoffset 13) y)
    (aset pathpoints (+ idxoffset 14) 2.0)
    (gimp-path-set-points img pathname 1 numpoints pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-square-wave-path img
				drawable
				name
				x
				y
				amplitude
				wavelength
				cycles
				convergence
				shearing
				displacement)
  (gimp-undo-push-group-start img)
  (let* ((numpoints (+ (* 36 cycles) 15))
        (pathpoints (cons-array numpoints (quote double)))
        (halfamplitude (/ amplitude 2))
        (halfwavelength (/ wavelength 2))
        (dist x)
        (convergence2  (* halfwavelength convergence))
        (shearing2     (* halfwavelength shearing))
        (displacement2 (* halfwavelength displacement))
        (i 0)
        (pathname (choose-name img name)))
    (aset pathpoints 0 x)
    (aset pathpoints 1 y)
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 x)
    (aset pathpoints 4 y)
    (aset pathpoints 5 2.0)
    (while (< i cycles)
      (set! idxoffset (* 36 i))
      (aset pathpoints (+ idxoffset 6) (- (+ dist shearing2) convergence2))
      (aset pathpoints (+ idxoffset 7) (- y halfamplitude))
      (aset pathpoints (+ idxoffset 8) 2.0)
      (aset pathpoints (+ idxoffset 9) (- (+ dist shearing2) convergence2))
      (aset pathpoints (+ idxoffset 10) (- y halfamplitude))
      (aset pathpoints (+ idxoffset 11) 1.0)
      (aset pathpoints (+ idxoffset 12) (- (+ dist shearing2) convergence2))
      (aset pathpoints (+ idxoffset 13) (- y halfamplitude))
      (aset pathpoints (+ idxoffset 14) 2.0)
      (set! dist (+ dist halfwavelength))
      (aset pathpoints (+ idxoffset 15) (+ (+ (+ dist displacement2) convergence2) shearing2))
      (aset pathpoints (+ idxoffset 16) (- y halfamplitude))
      (aset pathpoints (+ idxoffset 17) 2.0)
      (aset pathpoints (+ idxoffset 18) (+ (+ (+ dist displacement2) convergence2) shearing2))
      (aset pathpoints (+ idxoffset 19) (- y halfamplitude))
      (aset pathpoints (+ idxoffset 20) 1.0)
      (aset pathpoints (+ idxoffset 21) (+ (+ (+ dist displacement2) convergence2) shearing2))
      (aset pathpoints (+ idxoffset 22) (- y halfamplitude))
      (aset pathpoints (+ idxoffset 23) 2.0)
      (aset pathpoints (+ idxoffset 24) (- (- (+ dist displacement2) convergence2) shearing2))
      (aset pathpoints (+ idxoffset 25) (+ y halfamplitude))
      (aset pathpoints (+ idxoffset 26) 2.0)
      (aset pathpoints (+ idxoffset 27) (- (- (+ dist displacement2) convergence2) shearing2))
      (aset pathpoints (+ idxoffset 28) (+ y halfamplitude))
      (aset pathpoints (+ idxoffset 29) 1.0)
      (aset pathpoints (+ idxoffset 30) (- (- (+ dist displacement2) convergence2) shearing2))
      (aset pathpoints (+ idxoffset 31) (+ y halfamplitude))
      (aset pathpoints (+ idxoffset 32) 2.0)
      (set! dist (+ dist halfwavelength))
      (aset pathpoints (+ idxoffset 33) (+ (- dist shearing2) convergence2))
      (aset pathpoints (+ idxoffset 34) (+ y halfamplitude))
      (aset pathpoints (+ idxoffset 35) 2.0)
      (aset pathpoints (+ idxoffset 36) (+ (- dist shearing2) convergence2))
      (aset pathpoints (+ idxoffset 37) (+ y halfamplitude))
      (aset pathpoints (+ idxoffset 38) 1.0)
      (aset pathpoints (+ idxoffset 39) (+ (- dist shearing2) convergence2))
      (aset pathpoints (+ idxoffset 40) (+ y halfamplitude))
      (aset pathpoints (+ idxoffset 41) 2.0)
      (set! i (+ i 1))
    )
    (set! idxoffset (* 36 i))
    (aset pathpoints (+ idxoffset 6) dist)
    (aset pathpoints (+ idxoffset 7) y)
    (aset pathpoints (+ idxoffset 8) 2.0)
    (aset pathpoints (+ idxoffset 9) dist)
    (aset pathpoints (+ idxoffset 10) y)
    (aset pathpoints (+ idxoffset 11) 1.0)
    (aset pathpoints (+ idxoffset 12) dist)
    (aset pathpoints (+ idxoffset 13) y)
    (aset pathpoints (+ idxoffset 14) 2.0)
    (gimp-path-set-points img pathname 1 numpoints pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(define (script-fu-sine-wave-path img
				drawable
				name
				x
				y
				amplitude
				wavelength
				cycles)
  (gimp-undo-push-group-start img)
  (let* ((numpoints (+ (* 72 cycles) 6))
        (pathpoints (cons-array numpoints (quote double)))
        (rightone (/ wavelength 24))
        (righttwo (/ wavelength 12))
        (rightthree (/ wavelength 8))
        (rightfour (/ wavelength 6))
        (rightfive (/ wavelength 4.8))
        (rightsix (/ wavelength 4))
        (rightseven (/ wavelength (/ 12 3.5)))
        (righteight (/ wavelength 3))
        (rightnine (/ (* 3 wavelength) 8))
        (rightten (/ wavelength (/ 12 5)))
        (righteleven (/ wavelength (/ 12 5.5)))
        (righttwelve (/ wavelength 2))
        (rightthirteen (+ righttwelve rightone))
        (rightfourteen (+ righttwelve righttwo))
        (rightfifteen (+ righttwelve rightthree))
        (rightsixteen (+ righttwelve rightfour))
        (rightseventeen (+ righttwelve rightfive))
        (righteighteen (+ righttwelve rightsix))
        (rightnineteen (+ righttwelve rightseven))
        (righttwenty (+ righttwelve righteight))
        (righttwentyone (+ righttwelve rightnine))
        (righttwentytwo (+ righttwelve rightten))
        (righttwentythree (+ righttwelve righteleven))
        (righttwentyfour (+ righttwelve righttwelve))
        (righttwentyfive (+ righttwelve righttwelve rightone))
        (upone (* (+ (- (/ y amplitude) (/ (* 2 (sqrt 2)) 7)) (/ 1 7)) amplitude))
        (uptwo (* (+ (- (/ y amplitude) (/ (* 4 (sqrt 2)) 7)) (/ 2 7)) amplitude))
        (upthree (+ (* (* (/ (sqrt 2) 2) -1) amplitude) y))
        (upfour (* (- (- (/ y amplitude) (/ (* 3 (sqrt 2)) 7)) (/ 2 7)) amplitude))
        (upfive (- y amplitude))
        (downone (* (- (+ (/ y amplitude) (/ (* 2 (sqrt 2)) 7)) (/ 1 7)) amplitude))
        (downtwo (* (- (+ (/ y amplitude) (/ (* 4 (sqrt 2)) 7)) (/ 2 7)) amplitude))
        (downthree (+ (* (/ (sqrt 2) 2) amplitude) y))
        (downfour (* (+ (+ (/ y amplitude) (/ (* 3 (sqrt 2)) 7)) (/ 2 7)) amplitude))
        (downfive (+ y amplitude))
        (i 0)
        (idxoffset 0)
        (distoffset 0)
        (pathname (choose-name img name)))
    (aset pathpoints 0 x)
    (aset pathpoints 1 y)
    (aset pathpoints 2 1.0)
    (aset pathpoints 3 (+ x rightone))
    (aset pathpoints 4 upone)
    (aset pathpoints 5 2.0)
    (while (< i cycles)
      (set! idxoffset (* 72 i))
      (set! distoffset (* righttwentyfour i))
      (aset pathpoints (+ idxoffset 6) (+ x distoffset righttwo))
      (aset pathpoints (+ idxoffset 7) uptwo)
      (aset pathpoints (+ idxoffset 8) 2.0)
      (aset pathpoints (+ idxoffset 9) (+ x distoffset rightthree))
      (aset pathpoints (+ idxoffset 10) upthree)
      (aset pathpoints (+ idxoffset 11) 1.0)
      (aset pathpoints (+ idxoffset 12) (+ x distoffset rightfour))
      (aset pathpoints (+ idxoffset 13) upfour)
      (aset pathpoints (+ idxoffset 14) 2.0)
      (aset pathpoints (+ idxoffset 15) (+ x distoffset rightfive))
      (aset pathpoints (+ idxoffset 16) upfive)
      (aset pathpoints (+ idxoffset 17) 2.0)
      (aset pathpoints (+ idxoffset 18) (+ x distoffset rightsix))
      (aset pathpoints (+ idxoffset 19) upfive)
      (aset pathpoints (+ idxoffset 20) 1.0)
      (aset pathpoints (+ idxoffset 21) (+ x distoffset rightseven))
      (aset pathpoints (+ idxoffset 22) upfive)
      (aset pathpoints (+ idxoffset 23) 2.0)
      (aset pathpoints (+ idxoffset 24) (+ x distoffset righteight))
      (aset pathpoints (+ idxoffset 25) upfour)
      (aset pathpoints (+ idxoffset 26) 2.0)
      (aset pathpoints (+ idxoffset 27) (+ x distoffset rightnine))
      (aset pathpoints (+ idxoffset 28) upthree)
      (aset pathpoints (+ idxoffset 29) 1.0)
      (aset pathpoints (+ idxoffset 30) (+ x distoffset rightten))
      (aset pathpoints (+ idxoffset 31) uptwo)
      (aset pathpoints (+ idxoffset 32) 2.0)
      (aset pathpoints (+ idxoffset 33) (+ x distoffset righteleven))
      (aset pathpoints (+ idxoffset 34) upone)
      (aset pathpoints (+ idxoffset 35) 2.0)
      (aset pathpoints (+ idxoffset 36) (+ x distoffset righttwelve))
      (aset pathpoints (+ idxoffset 37) y)
      (aset pathpoints (+ idxoffset 38) 1.0)
      (aset pathpoints (+ idxoffset 39) (+ x distoffset rightthirteen))
      (aset pathpoints (+ idxoffset 40) downone)
      (aset pathpoints (+ idxoffset 41) 2.0)
      (aset pathpoints (+ idxoffset 42) (+ x distoffset rightfourteen))
      (aset pathpoints (+ idxoffset 43) downtwo)
      (aset pathpoints (+ idxoffset 44) 2.0)
      (aset pathpoints (+ idxoffset 45) (+ x distoffset rightfifteen))
      (aset pathpoints (+ idxoffset 46) downthree)
      (aset pathpoints (+ idxoffset 47) 1.0)
      (aset pathpoints (+ idxoffset 48) (+ x distoffset rightsixteen))
      (aset pathpoints (+ idxoffset 49) downfour)
      (aset pathpoints (+ idxoffset 50) 2.0)
      (aset pathpoints (+ idxoffset 51) (+ x distoffset rightseventeen))
      (aset pathpoints (+ idxoffset 52) downfive)
      (aset pathpoints (+ idxoffset 53) 2.0)
      (aset pathpoints (+ idxoffset 54) (+ x distoffset righteighteen))
      (aset pathpoints (+ idxoffset 55) downfive)
      (aset pathpoints (+ idxoffset 56) 1.0)
      (aset pathpoints (+ idxoffset 57) (+ x distoffset rightnineteen))
      (aset pathpoints (+ idxoffset 58) downfive)
      (aset pathpoints (+ idxoffset 59) 2.0)
      (aset pathpoints (+ idxoffset 60) (+ x distoffset righttwenty))
      (aset pathpoints (+ idxoffset 61) downfour)
      (aset pathpoints (+ idxoffset 62) 2.0)
      (aset pathpoints (+ idxoffset 63) (+ x distoffset righttwentyone))
      (aset pathpoints (+ idxoffset 64) downthree)
      (aset pathpoints (+ idxoffset 65) 1.0)
      (aset pathpoints (+ idxoffset 66) (+ x distoffset righttwentytwo))
      (aset pathpoints (+ idxoffset 67) downtwo)
      (aset pathpoints (+ idxoffset 68) 2.0)
      (aset pathpoints (+ idxoffset 69) (+ x distoffset righttwentythree))
      (aset pathpoints (+ idxoffset 70) downone)
      (aset pathpoints (+ idxoffset 71) 2.0)
      (aset pathpoints (+ idxoffset 72) (+ x distoffset righttwentyfour))
      (aset pathpoints (+ idxoffset 73) y)
      (aset pathpoints (+ idxoffset 74) 1.0)
      (aset pathpoints (+ idxoffset 75) (+ x distoffset righttwentyfive))
      (aset pathpoints (+ idxoffset 76) upone)
      (aset pathpoints (+ idxoffset 77) 2.0)
      (set! i (+ i 1))
    )
    (gimp-path-set-points img pathname 1 numpoints pathpoints)
    (gimp-path-set-current img pathname)
    (gimp-displays-flush)
  )
  (gimp-undo-push-group-end img))

(script-fu-register "script-fu-rectangle-path"
		    _"<Image>/Script-Fu/Shape Paths/_Rectangle..."
		    "Creates a rectangular path."
		    "Jonathan Stipe <JonStipe@prodigy.net>"
		    "Jonathan Stipe"
		    "December 2004"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"Rectangle"
		    SF-ADJUSTMENT	"Top"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Left"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Bottom"	'(10 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Right"	'(10 0 4294967296 1 10 1 1))

(script-fu-register "script-fu-square-path"
		    _"<Image>/Script-Fu/Shape Paths/_Square..."
		    "Creates a square path."
		    "Jonathan Stipe <JonStipe@prodigy.net>"
		    "Jonathan Stipe"
		    "December 2004"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"Square"
		    SF-ADJUSTMENT	"Top"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Left"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Size"	'(10 0 4294967296 1 10 1 1))

(script-fu-register "script-fu-circle-path"
		    _"<Image>/Script-Fu/Shape Paths/_Circle..."
		    "Creates a circular path."
		    "Jonathan Stipe <JonStipe@prodigy.net>"
		    "Jonathan Stipe"
		    "December 2004"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"Circle"
		    SF-ADJUSTMENT	"Center X"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Center Y"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Radius"	'(10 0 4294967296 1 10 1 1))

(script-fu-register "script-fu-ellipse-path"
		    _"<Image>/Script-Fu/Shape Paths/_Ellipse..."
		    "Creates an elliptical path."
		    "Jonathan Stipe <JonStipe@prodigy.net>"
		    "Jonathan Stipe"
		    "December 2004"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"Ellipse"
		    SF-ADJUSTMENT	"Center X"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Center Y"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Horizontal Radius"	'(10 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Vertical Radius"	'(10 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0))

(script-fu-register "script-fu-oval-path"
		    _"<Image>/Script-Fu/Shape Paths/O_val..."
		    "Creates an ovoidal path with four radii."
		    "Jonathan Stipe <JonStipe@prodigy.net> and Pucelo"
		    "Jonathan Stipe and Pucelo"
		    "April 2007"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"Oval"
		    SF-ADJUSTMENT	"Center X"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Center Y"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Top radius"	'(10 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Left radius"	'(10 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Bottom radius"	'(10 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Right radius"	'(10 0 4294967296 1 10 1 1))

(script-fu-register "script-fu-rounded-rectangle-path"
		    _"<Image>/Script-Fu/Shape Paths/R_ounded Rectangle..."
		    "Creates a rounded rectangular path."
		    "Jonathan Stipe <JonStipe@prodigy.net>"
		    "Jonathan Stipe"
		    "December 2004"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"RoundRectangle"
		    SF-ADJUSTMENT	"Top"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Left"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Bottom"	'(10 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Right"	'(10 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Horizontal Radius"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Vertical Radius"	'(0 0 4294967296 1 10 1 1))

(script-fu-register "script-fu-advanced-rounded-rectangle-path"
		    _"<Image>/Script-Fu/Shape Paths/_Advanced Rounded Rectangle..."
		    "Creates a rounded rectangular path with different radii for each corner."
		    "Jonathan Stipe <JonStipe@prodigy.net> and Pucelo"
		    "Jonathan Stipe and Pucelo"
		    "April 2007"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"AdvRoundRectangle"
		    SF-ADJUSTMENT	"Top"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Left"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Bottom"	'(10 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Right"	'(10 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Top-Left Horizontal Radius"	'(0 -4294967296 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Top-Left Vertical Radius"	'(0 -4294967296 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Top-Right Horizontal Radius"	'(0 -4294967296 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Top-Right Vertical Radius"	'(0 -4294967296 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Bottom-Left Horizontal Radius"	'(0 -4294967296 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Bottom-Left Vertical Radius"	'(0 -4294967296 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Bottom-Right Horizontal Radius"	'(0 -4294967296 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Bottom-Right Vertical Radius"	'(0 -4294967296 4294967296 1 10 1 1))

(script-fu-register "script-fu-polygon-path"
		    _"<Image>/Script-Fu/Shape Paths/_Polygon..."
		    "Creates a polygonal path."
		    "Jonathan Stipe <JonStipe@prodigy.net>"
		    "Jonathan Stipe"
		    "December 2004"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"Polygon"
		    SF-ADJUSTMENT	"Center X"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Center Y"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Radius"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Number of sides"	'(3 3 360 1 5 0 1)
		    SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0))

(script-fu-register "script-fu-star-path"
		    _"<Image>/Script-Fu/Shape Paths/S_tar..."
		    "Creates a star-shaped path."
		    "Jonathan Stipe <JonStipe@prodigy.net>"
		    "Jonathan Stipe"
		    "December 2004"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"Star"
		    SF-ADJUSTMENT	"Center X"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Center Y"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Outer Radius"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Inner Radius"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Number of points"	'(5 3 360 1 5 0 1)
		    SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0))

(script-fu-register "script-fu-flowerspike-path"
		    _"<Image>/Script-Fu/Shape Paths/_Flowers & Spikes..."
		    "Creates a flower or spike shaped path, negative factor values create complex stars."
		    "Jonathan Stipe <JonStipe@prodigy.net> and Pucelo"
		    "Jonathan Stipe and Pucelo"
		    "December 2006"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"FlowerSpike"
		    SF-ADJUSTMENT	"Center X"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Center Y"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Radius"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Number of sides"	'(3 3 360 1 5 0 1)
		    SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0)
		    SF-ADJUSTMENT	"Factor A"	'(1 -4294967296 4294967296 0.1 1 1 1)
		    SF-ADJUSTMENT "Angular Displacement A" '(0 -180 180 1 10 1 0)
		    SF-ADJUSTMENT	"Factor B"	'(1 -4294967296 4294967296 0.1 1 1 1)
		    SF-ADJUSTMENT "Angular Displacement B" '(0 -180 180 1 10 1 0))

(script-fu-register "script-fu-gear-path"
		    _"<Image>/Script-Fu/Shape Paths/_Gear..."
		    "Creates a gear-shaped path."
		    "Jonathan Stipe <JonStipe@prodigy.net> and Pucelo"
		    "Jonathan Stipe and Pucelo"
		    "December 2006"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"Gear"
		    SF-ADJUSTMENT	"Center X"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Center Y"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Outer Radius"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Inner Radius"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Number of teeth"	'(3 3 360 1 5 0 1)
		    SF-ADJUSTMENT "Rotation" '(0 0 360 1 10 1 0))

(script-fu-register "script-fu-triangle-wave-path"
		    _"<Image>/Script-Fu/Shape Paths/_Triangle Wave..."
		    "Creates a triangle wave-shaped path."
		    "Jonathan Stipe <JonStipe@prodigy.net> and Pucelo"
		    "Jonathan Stipe and Pucelo"
		    "April 2007"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"TriangleWave"
		    SF-ADJUSTMENT	"Start X"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Start Y"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Amplitude"	'(1 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Wavelength"	'(1 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Number of cycles"	'(10 1 4294967296 1 5 0 1)
		    SF-ADJUSTMENT	"Shearing"	'(0 -4294967296 4294967296 0.05 0.2 3 1)
		    SF-ADJUSTMENT	"Rounding"	'(0 -4294967296 4294967296 0.05 0.2 3 1))

(script-fu-register "script-fu-square-wave-path"
		    _"<Image>/Script-Fu/Shape Paths/S_quare Wave..."
		    "Creates a square wave-shaped path."
		    "Jonathan Stipe <JonStipe@prodigy.net> and Pucelo"
		    "Jonathan Stipe and Pucelo"
		    "April 2007"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"SquareWave"
		    SF-ADJUSTMENT	"Start X"	'(0 -4294967296 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Start Y"	'(0 -4294967296 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Amplitude"	'(1 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Wavelength"	'(1 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Number of cycles"	'(10 1 4294967296 1 5 0 1)
		    SF-ADJUSTMENT	"Convergence"	'(0 -4294967296 4294967296 0.05 0.2 3 1)
		    SF-ADJUSTMENT	"Shearing"	'(0 -4294967296 4294967296 0.05 0.2 3 1)
		    SF-ADJUSTMENT	"Displacement"	'(0 -1 1 0.05 0.2 3 1))

(script-fu-register "script-fu-sine-wave-path"
		    _"<Image>/Script-Fu/Shape Paths/S_ine Wave..."
		    "Creates a sine wave-shaped path."
		    "Jonathan Stipe <JonStipe@prodigy.net>"
		    "Jonathan Stipe"
		    "January 2005"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-STRING	"Name"	"SineWave"
		    SF-ADJUSTMENT	"Start X"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Start Y"	'(0 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Amplitude"	'(1 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Wavelength"	'(1 0 4294967296 1 10 1 1)
		    SF-ADJUSTMENT	"Number of cycles"	'(1 1 4294967296 1 5 0 1))
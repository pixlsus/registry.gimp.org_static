; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; 'layout'
;   0 = Allow week 6
;   1 = Force week 6
;   2 = Wrap w6 to w1
;   3 = Wrap w6 to w5

(define sg-calendar-months '( ("January" "February" "March" "April" "May" "June" 
                               "July" "August" "September" "October" "November" "December")
                              ("Januar" "Februar " "März" "April" "Mai" "Juni" 
                               "Juli" "August" "September" "Oktober" "November" "Dezember") ; German   
                              ("Gennaio" "Febbraio" "Marzo" "Aprile" "Maggio" "Giugno" 
                               "Luglio" "Agosto" "Settembre" "Ottobre" "Novembre" "Dicembre" ) ; Italian
                              ("Enero" "Febrero" "Marzo" "Abril" "Mayo" "Junio" 
                                "Julio" "Agosto" "Septiembre" "Octubre" "Noviembre" "Diciembre") ; Spanish
                              ("Janvier" "Février" "Mars" "Avril" "Mai" "Juin" 
                                "Juillet" "Août" "Septembre" "Octobre" "Novembre" "Décembre") )); French 
                               
(define sg-calendar-weekdays '( ("Monday" "Tuesday" "Wednesday" "Thursday" 
                                 "Friday" "Saturday" "Sunday")
                                ("Montag" "Dienstag" "Mittwoch" "Donnerstag" 
                                 "Freitag" "Samstag" "Sonntag") ; German
                                ("Lunedi" "Martedi" "Mercoledi" "Giovedi" 
                                 "Venerdi" "Sabato" "Domenica") ; Italian
                                ("Lunes" "Martes" "Miercoles" "Jueves" 
                                 "Viernes" "Sabado" "Domingo") ; Spanish
                                ("Lundi" "Mardi" "Mercredi" "Jeudi" 
                                 "Vendredi" "Samedi" "Dimanche") )) ; French

;; Perform a crude search for the largest font that will fit within
;; the cell (this algorithm could be better!)
;
(define (sg-calendar-calc-fontsize text font fontsize% width height)
  (let* ((fontsize 6) ;; minimum possible fontsize
         (extents nil)
         (last-extents nil)
         (last-fontsize 3)
         (adjust 2) )
    (set! extents (gimp-text-get-extents-fontname text fontsize PIXELS font))
    (set! width (* width fontsize% 0.01))
    (set! height (* height fontsize% 0.01))
    (while (and (<> last-fontsize fontsize) (not (equal? extents last-extents)))
      (if (or (> (car extents) width) (> (cadr extents) height))
        (begin 
          (set! fontsize last-fontsize)
          (set! adjust (+ (* (- adjust 1) 0.5) 1)) )
        (begin
          (set! last-extents extents)
          (set! last-fontsize fontsize) ) )
      (set! fontsize (truncate (* fontsize adjust)))
      (set! extents (gimp-text-get-extents-fontname text fontsize PIXELS font)) )
    (max fontsize 6) ) )
  

(define (script-fu-sg-calendar image drawable lang month year sunday? letters-in-day layout text-font number-font fontsize% justify? border border-color gravity)
  ;; 'leap-year' returns one if given year is a leap year, else zero
  ;
  (define (leap-year yy) 
    (if (= (modulo yy 4) 0)
      (if  (or (> (modulo yy 100) 0) (= (modulo yy 400) 0))
        1
        0 )
      0 ) )

  ;; Given a Gregorian date, the following computes the number of days that have elapsed since
  ;; March 1st, 0000. This date is chosen as an absolute reference
  ;; so that leap days occur at the "end of the year" (simplifying 
  ;; calculations). For lack of a better name, I shall call this a
  ;; "martius date" after the Roman word for the month of March.
  ;
  (define (gregorian->martius yy mm dd)
    (set! mm (modulo (+ mm 9) 12))
    (set! yy (- yy (truncate (/ mm 10))))
    (inexact->exact (+ (trunc (* 365 yy)) 
                       (trunc (/ yy 4)) 
                       (- (trunc (/ yy 100))) 
                       (trunc (/ yy 400)) 
                       (round (* mm 30.6)) dd -1 )) )

  ;; Given a Gregorian date, return the day of the week (0=Sunday, 1=Monday,...)
  ;
  (define (day-of-week yy mm dd)
    (modulo (+ (gregorian->martius yy mm dd) 3) 7) )

  ;; The following converts from an absolute number of days since 
  ;; March 1st, 0000 (i.e., a "martius date") to a Gregorian date
  ;; A list is returned containing '(year month day)
  ;
  (define (martius->gregorian mdays)
    (let* ((yy 0)
           (mm 0)
           (mi 0)
           (dd 0) )
      (set! yy (truncate (+ (/ mdays 365.2425) (/ 1.4780 365.2425))))
      (set! dd (- mdays (truncate (* yy 365.2425))))
      (when (< dd 0)
        (set! yy (- yy 1))
        (set! dd (- mdays (truncate (* yy 365.2425)))) )
      (set! mi (inexact->exact (truncate (/ (+ 0.52 dd) 30.60))))
      (set! mm (+ (modulo (+ mi 2) 12) 1))
      (set! yy (+ yy (truncate (/ (+ mi 2) 12))))
      (set! dd (+ (- dd (round (* mi 30.6))) 1))
      (list yy mm dd) ) )

  ;; Create a list of floats evenly distributed between start and end
  ;
  (define (algebraic-prog start end elements)
    (let ((elements (inexact->exact elements))
          (incr (if (zero? start)
                  (/ end (- elements 1))
                  (/ (- (/ end start) 1) (- elements 1)) ) ) )
      (let 
        loop ((cnt (- elements 1))
              (lis (if (zero? start)
                     '(0)
                     '(1) ) ) )
        (if (zero? cnt)
          (if (zero? start)
            (reverse lis)
            (map * (reverse lis) (make-list elements start)) )
          (loop (- cnt 1) (cons (+ (car lis) incr) lis)) ) ) ) )

  ;; Create a frame layer for a cell
  ;
  (define (create-cell-frame x y w h)
    (let* ((frame-layer (car (gimp-layer-new image 
                                             w h 
                                             RGBA-IMAGE "Cell #1" 
                                             100 NORMAL-MODE ))) )
      (gimp-drawable-fill frame-layer TRANSPARENT-FILL)
      (gimp-image-add-layer image frame-layer -1)
      (gimp-layer-set-offsets frame-layer x y)
      (gimp-rect-select image (+ x border) (+ y border) (- w (* 2 border)) (- h (* 2 border)) CHANNEL-OP-REPLACE FALSE 0)
      (gimp-selection-invert image)
      (gimp-context-set-background border-color)
      (gimp-edit-fill frame-layer BACKGROUND-FILL)
      (gimp-selection-none image)
      frame-layer ) )
  
  ;; Create the text layer for a cell-frame
  ;
  (define (create-cell-text text font fontsize gravity frame-layer)
    (let* ((text-layer (car (gimp-text-fontname image -1 
                                                0 0 
                                                text (* 2 border) 
                                                TRUE fontsize 
                                                PIXELS font )))
           (x-align 0)
           (y-align 0) )
      (if (or (= gravity 0) (= gravity 1) (= gravity 2))
        (set! y-align -1)
        (when (or (= gravity 6) (= gravity 7) (= gravity 8))
          (set! y-align 1) ) )
      (if (or (= gravity 0) (= gravity 3) (= gravity 6))
        (set! x-align -1)
        (when (or (= gravity 2) (= gravity 5) (= gravity 8))
          (set! x-align 1) ) )
      (fu-align-layers frame-layer text-layer x-align y-align)
      text-layer
      )
    )

  ;; align layer(s) with a base-layer (layers can be either a single layer or a list of layers)
  ;; vert-align and horiz-align
  ;;  -1 = LEFT, 0 = CENTER, 1 = RIGHT
  ;
  (define (fu-align-layers base-layer layers vert-align horiz-align)
    (let* ((anchor-x (car (gimp-drawable-offsets base-layer)))
           (anchor-y (cadr (gimp-drawable-offsets base-layer)))
           (width (car (gimp-drawable-width base-layer)))
           (height (car (gimp-drawable-height base-layer))) )
      (unless (pair? layers)
        (set! layers (list layers)) )
      (if (>= vert-align 0)
        (if (= vert-align 0)
          (set! anchor-x (+ anchor-x (/ width 2)))
          (set! anchor-x (+ anchor-x width)) ) )
      (if (>= horiz-align 0)
        (if (= horiz-align 0)
          (set! anchor-y (+ anchor-y (/ height 2)))
          (set! anchor-y (+ anchor-y height)) ) )
      (while (pair? layers)
        (let* (
            (layer (car layers))
            (ref-x (car (gimp-drawable-offsets layer)))
            (ref-y (cadr (gimp-drawable-offsets layer)))
            (orig-x ref-x)
            (orig-y ref-y)
            (offset-x 0)
            (offset-y 0)
            )
          (set! width (car (gimp-drawable-width layer)))
          (set! height (car (gimp-drawable-height layer)))
          (if (>= vert-align 0)
            (if (= vert-align 0)
              (set! ref-x (+ ref-x (/ width 2)))
              (set! ref-x (+ ref-x width))
              )
            )
          (if (>= horiz-align 0)
            (if (= horiz-align 0)
              (set! ref-y (+ ref-y (/ height 2)))
              (set! ref-y (+ ref-y height))
              )
            )
          (set! offset-x (+ orig-x (- anchor-x ref-x)))
          (set! offset-y (+ orig-y (- anchor-y ref-y)))
          (gimp-layer-set-offsets layer offset-x offset-y)
          )
        (set! layers (cdr layers))
        )
      )
    )
  ;; return a list of visible layers
  ;
  (define (fu-get-visible-layers image)
    (let loop ((layers (vector->list (cadr (gimp-image-get-layers image))))
               (viewables nil) )
      (if (null? layers)
        (if (null? viewables)
          '()
          (reverse viewables) )
        (loop (cdr layers)
              (if (zero? (car (gimp-drawable-get-visible (car layers))))
                viewables
                (cons (car layers) viewables) ) ) ) ) )
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main definition starts here ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ((bg-layer 0)
         (x1 0)
         (x2 0)
         (y1 0)
         (y2 0)
         (X's '())
         (Y's '()) 
         (x's '())
         (y's '())
         (row 0)
         (total-rows 6)
         (col 0)
         (text-fontsize 6)
         (number-fontsize 6)
         (firstday (day-of-week year (+ month 1) 1))
         (days-in-month (vector 31 (+ 28 (leap-year year)) 31 30 31 30 31 31 30 31 30 31))
         (i 0)
         (frames nil)
         (frame-layer 0)
         (cal-days nil)
         (dates-layer 0)
         (visibles '())
         (buffer "")
         (layers nil)
         (orig-bg (car (gimp-context-get-background)))
         (orig-sel 0)
         (months (list->vector (list-ref sg-calendar-months lang)))
         (weekday-strings (list->vector (map (lambda (x) 
                                               (substring x 
                                                          0 
                                                          (min (+ letters-in-day 1)
                                                               (string-length x) ) ) )
                                             (list-ref sg-calendar-weekdays lang) )))
         )         
    (gimp-context-push)
    (gimp-image-undo-group-start image)
    (set! orig-sel (car (gimp-selection-save image)))
    (set! buffer (car (gimp-edit-named-copy drawable "buffer")))
    (set! bg-layer (car (gimp-edit-named-paste drawable buffer FALSE)))
    (gimp-floating-sel-to-layer bg-layer)
    (gimp-buffer-delete buffer)
    (let loop ((pos (- (car (gimp-image-get-layer-position image drawable)) 1)))
      (if (zero? pos)
        #t
        (begin 
          (gimp-image-lower-layer image bg-layer)
          (loop (- pos 1)) )))

    (set! x1 (+ border (car (gimp-drawable-offsets bg-layer))))
    (set! x2 (- (+ x1 (car (gimp-drawable-width bg-layer))) (* 2 border)))
    (set! y1 (+ border (cadr (gimp-drawable-offsets bg-layer))))
    (set! y2 (- (+ y1 (car (gimp-drawable-height bg-layer))) (* 2 border)))
    (set! X's (map round (algebraic-prog x1 x2 8)))
    (set! Y's (map round (algebraic-prog y1 y2 7))) ;; 1 row for header + five rows for weeks 
    (set! x's X's)
    (set! y's Y's)
    (set! visibles (fu-get-visible-layers image))
    (set! months (list->vector (list-ref sg-calendar-months lang)))
    (set! weekday-strings (list->vector (map (lambda (x) 
                                               (substring x 
                                                          0 
                                                          (min (+ letters-in-day 1)
                                                               (string-length x) ) ) )
                                             (list-ref sg-calendar-weekdays lang) )))
    (unless (= sunday? TRUE)
       (set! firstday (modulo (- firstday 1) 7))
      )
    (when (or (= layout 1) (and (= layout 0) (> (+ firstday (vector-ref days-in-month month)) 35)))
      (set! Y's (map round (algebraic-prog y1 y2 8))) ;; 1 row for header + six rows for weeks 
      (set! y's Y's)
      (set! total-rows 7)
      )
    ;; Add header row with day labels
    ;; Determine fontsize for days
    (set! text-fontsize (apply min (map (lambda (text)
                               (sg-calendar-calc-fontsize text 
                                              text-font 
                                              80 
                                              (- (cadr X's) (car X's) (* 2 border)) 
                                              (- (cadr Y's) (car Y's) (* 2 border)) ) )
                             (vector->list weekday-strings) ) ) )
    (gimp-progress-update (/ 1 (* total-rows 7)))
    (set! x's X's)
    (while (< i 7)
      (set! frames (cons (create-cell-frame (car x's) ; cell upper-left x
                                             (car y's) ; cell upper-left y
                                             (round (- (cadr x's) (car x's))) ; width
                                             (round (- (cadr y's) (car y's)))) ; height
                                             frames))
      (set! cal-days (cons (create-cell-text (vector-ref weekday-strings (modulo (- i sunday? ) 7))
                                             text-font
                                             text-fontsize 
                                             7 ;; center text in cell
                                             (car frames)) 
                                             cal-days ))
      (set! i (+ i 1))
      (gimp-progress-update (/ (+ i 1) (* total-rows 7)))
      (set! x's (cdr x's))
      )
    (set! row (+ row 1))
    (set! y's (cdr y's))
    (set! x's X's)
    (set! i 0)

    ;; Determine fontsize for numbers
    (if (< fontsize% 80)
      (set! number-fontsize (sg-calendar-calc-fontsize "30" number-font fontsize% (- (cadr X's) (car X's) (* 4 border))  (- (cadr Y's) (car Y's) (* 4 border))))
      (set! number-fontsize (sg-calendar-calc-fontsize "30" number-font fontsize% (- (cadr X's) (car X's) (* 2 border))  (- (cadr Y's) (car Y's) (* 2 border))))
      )
    ;; create grid of "cells"
    ;; grid contains cells for the days
    ;; Each cell has a transparent layer with optional border and
    ;; valid days have a text layer holding the day number. 
    ;; the day of the month is "0" if cell is not in month
    ;; 
    (if (= layout 2) 
      (while (> (- (+ firstday (vector-ref days-in-month month)) i) 35)
        (set! frames (cons (create-cell-frame (car x's) ; cell upper-left x
                                               (car y's) ; cell upper-left y
                                               (round (- (cadr x's) (car x's))) ; width
                                               (round (- (cadr y's) (car y's)))) ; height
                                               frames))
        (let* (
            (date (+ (- 36 firstday) i))
            (date-str (number->string date))
            )
          (set! cal-days (cons (create-cell-text date-str number-font number-fontsize gravity (car frames)) cal-days))
          )
        (set! i (+ i 1))
        (gimp-progress-update (/ (+ i 8) (* total-rows 7)))
        (set! x's (cdr x's))
        (set! col (+ col 1))
        )
      )
    (let* (
        (bindle-x (car x's))
        )
      (while (< i firstday)
        (set! i (+ i 1))
        (gimp-progress-update (/ (+ i 8) (* total-rows 7)))
        (set! x's (cdr x's))
        )
      (unless (= i col)
        (set! frames (cons (create-cell-frame bindle-x ; cell upper-left x
                                               (car y's) ; cell upper-left y
                                               (round (- (car x's) bindle-x)) ; width
                                               (round (- (cadr y's) (car y's)))) ; height
                                               frames))
        (set! col i)
        )
      )
    (while (< row total-rows)
      (while (and (< col 7) (< i (+ (vector-ref days-in-month month) firstday)))
        (set! frames (cons (create-cell-frame (car x's) ; cell upper-left x
                                               (car y's) ; cell upper-left y
                                               (round (- (cadr x's) (car x's))) ; width
                                               (round (- (cadr y's) (car y's)))) ; height
                                               frames))
        (let* (
            (date (+ (- i firstday) 1))
            (date-str (if (and (= justify? TRUE) (< date 10))
                          (string-append " " (number->string date))
                          (number->string date)))
            (double-day 0)
            (cal-day 0)
            )
          (set! cal-day (create-cell-text date-str number-font number-fontsize gravity (car frames)))
          (if (= layout 3) ;; if needed, squeeze two dates into text cell (e.g., 23/30, 24/31)
            (when (and (> (+ firstday (vector-ref days-in-month month)) 35) 
                       (= row 5)
                       (<= (+ date 7) (vector-ref days-in-month month)))

              (gimp-edit-clear cal-day)
              (set! double-day (create-cell-text date-str number-font (* number-fontsize 0.5) 0 cal-day))
              (set! cal-day (car (gimp-image-merge-down image double-day EXPAND-AS-NECESSARY)))
              (set! double-day (create-cell-text (number->string (+ date 7)) number-font (* number-fontsize 0.5) 8 cal-day))
              (set! cal-day (car (gimp-image-merge-down image double-day EXPAND-AS-NECESSARY)))
              )
            )
          (set! cal-days (cons cal-day cal-days))
          )
        (set! i (+ i 1))
        (gimp-progress-update (/ (+ i 8) (* total-rows 7)))
        (set! x's (cdr x's))
        (set! col (+ col 1))
        )
      (unless (or (< i (+ (vector-ref days-in-month month) firstday)) (= col 7))
        (let* (
            (bindle-x (car x's))
            )
          (while (< col 7)
            (set! i (+ i 1))
            (set! x's (cdr x's))
            (set! col (+ col 1))
            )
          (set! frames (cons (create-cell-frame bindle-x ; cell upper-left x
                                                 (car y's) ; cell upper-left y
                                                 (round (- (car x's) bindle-x)) ; width
                                                 (round (- (cadr y's) (car y's)))) ; height
                                                 frames))
          (set! i (+ i 1))
          (set! x's (cdr x's))
          (set! col (+ col 1))
          (if (= col 7)
            (set! col 0)
            )
          )
        )
      (set! row (+ row 1))
      (set! col 0)
      (set! x's X's)
      (set! y's (cdr y's))
      )
    (gimp-image-set-active-layer image bg-layer)
    (set! frame-layer (car (gimp-layer-new-from-drawable bg-layer image)))
    (gimp-image-add-layer image frame-layer -1)
    (gimp-layer-add-alpha frame-layer)
    (gimp-selection-none image)
    (gimp-edit-clear frame-layer)
    (gimp-context-set-background border-color)
    (gimp-rect-select image x1 y1 (round (- x2 x1)) (round (- y2 y1)) CHANNEL-OP-REPLACE FALSE 0)
    (gimp-selection-invert image)
    (unless (= border 0)
      (gimp-edit-fill  frame-layer BACKGROUND-FILL)
      )
    (map (lambda (x) (gimp-drawable-set-visible x FALSE)) visibles)
    (map (lambda (x) (gimp-drawable-set-visible x FALSE)) cal-days)
    (set! frame-layer (car (gimp-image-merge-visible-layers image EXPAND-AS-NECESSARY)))
    (gimp-drawable-set-visible frame-layer FALSE)
    (map (lambda (x) (gimp-drawable-set-visible x TRUE)) cal-days)
    (set! dates-layer (car (gimp-image-merge-visible-layers image EXPAND-AS-NECESSARY)))
    (gimp-layer-resize dates-layer
                       (car (gimp-drawable-width frame-layer))
                       (car (gimp-drawable-height frame-layer))  
                       (- (car (gimp-drawable-offsets dates-layer)) (car (gimp-drawable-offsets frame-layer)))
                       (- (cadr (gimp-drawable-offsets dates-layer)) (cadr (gimp-drawable-offsets frame-layer))))
    (map (lambda (x) (gimp-drawable-set-visible x TRUE)) visibles)
    (gimp-drawable-set-visible frame-layer TRUE)
    (gimp-drawable-set-name dates-layer (string-append (vector-ref months month) ", " (number->string year)))
    (gimp-drawable-set-name frame-layer (vector-ref months month))
    (gimp-context-set-background orig-bg)
    (gimp-progress-update 1)
    (gimp-displays-flush)
    (gimp-context-pop)
    (gimp-selection-load orig-sel)
    (gimp-image-remove-channel image orig-sel)
    (gimp-image-remove-layer image bg-layer)
    (gimp-image-set-active-layer image drawable)
    (gimp-image-undo-group-end image)
    (list dates-layer frame-layer)
    )
  )

(define (script-fu-sg-calendar-year orig-image orig-drawable 
                                    lang 
                                    year 
                                    num-cols 
                                    padding
                                    sunday? letters-in-day layout text-font number-font fontsize% justify? border border-color gravity)
  (define (algebraic-prog start end elements)
    (let ((elements (inexact->exact elements))
          (incr (if (zero? start)
                  (/ end (- elements 1))
                  (/ (- (/ end start) 1) (- elements 1)) ) ) )
      (let 
        loop ((cnt (- elements 1))
              (lis (if (zero? start)
                     '(0)
                     '(1) ) ) )
        (if (zero? cnt)
          (if (zero? start)
            (reverse lis)
            (map * (reverse lis) (make-list elements start)) )
          (loop (- cnt 1) (cons (+ (car lis) incr) lis)) ) ) ) )
                                              
  (let* ((image 0)
         (orig-x (car (gimp-drawable-offsets orig-drawable)))
         (orig-y (cadr (gimp-drawable-offsets orig-drawable)))
         (orig-sel 0)
         (bounds (gimp-drawable-mask-intersect orig-drawable))
         (buffer "")
         (layer 0)
         (display 0) )
    (gimp-image-undo-group-start orig-image)
    (set! orig-sel (car (gimp-selection-save orig-image)))
    (unless (zero? (car bounds))
      (set! bounds (cdr bounds))
      (set! orig-x (car bounds))
      (set! orig-y (cadr bounds))
      (gimp-rect-select orig-image orig-x orig-y (caddr bounds) (cadddr bounds) CHANNEL-OP-REPLACE FALSE 0) )
    (set! buffer (car (gimp-edit-named-copy orig-drawable "buffer")))
    (set! image (car (gimp-edit-named-paste-as-new buffer)))
    (gimp-image-undo-disable image)
    (set! display (car (gimp-display-new image)))
    (set! layer (car (gimp-image-get-active-layer image)))
    (gimp-buffer-delete buffer)
    (let* ((num-cols (truncate num-cols))
           (width (car (gimp-drawable-width layer)))
           (height (car (gimp-drawable-height layer)))
           (x 0)
           (y 0)
           (w (floor (/ width num-cols)))
           (num-rows (ceiling (/ 12 num-cols)))
           (h (floor (/ height num-rows)))
           (w-sel (/ (- width (* width padding 0.01)) num-cols))
           (h-sel w-sel) ; assume square month initially
           (x-offsets '())
           (y-offsets '())
           (temp-layer 0)
           (month 0)
           (month-fontsize 6)
           (extents '()) )
      (when (< h (+ h-sel (* h-sel padding 0.01))) ; shrink month height to make room for banner
        (set! h-sel (- h (* h padding 0.009))) ; increase vertical padding a bit
        (set! w-sel h-sel) ; re-calculate horizontal layout
        )
      (set! month-fontsize (apply min (map (lambda (text) 
                                               (sg-calendar-calc-fontsize text text-font 100 w-sel (- h h-sel)) )
                                           (list-ref sg-calendar-months lang) )))
      (set! y (+ (cadr (gimp-text-get-extents-fontname (car (list-ref sg-calendar-months lang)) 
                                                       month-fontsize 
                                                       PIXELS 
                                                       text-font ))
                 2) )
      (set! y-offsets (map truncate (algebraic-prog y (- height h-sel) num-rows)))
      (while (pair? y-offsets)
        (set! x-offsets (map truncate (algebraic-prog 0 (- width w-sel) num-cols)))
        (while (and (pair? x-offsets) (< month 12))
          (set! x (car x-offsets))
          (set! y (car y-offsets))
          (gimp-rect-select image x y w-sel h-sel CHANNEL-OP-REPLACE FALSE 0)
          (set! buffer (car (gimp-edit-named-copy layer "buffer")))
          (gimp-floating-sel-to-layer (car (gimp-edit-named-paste layer buffer FALSE)))
          (set! temp-layer (car (gimp-image-get-active-layer image)))
          (gimp-buffer-delete buffer)
          (script-fu-sg-calendar image temp-layer lang month year sunday? 
                                 letters-in-day layout text-font number-font 
                                 fontsize% justify? border border-color gravity)
          (gimp-image-remove-layer image temp-layer)
          (set! extents (gimp-text-get-extents-fontname (list-ref (list-ref sg-calendar-months lang) month) 
                                                        month-fontsize 
                                                        PIXELS 
                                                        text-font ))
          (set! temp-layer (car (gimp-text-fontname image -1 
                                                    (+ x (/ (- w-sel (car extents)) 2))
                                                    (- y (cadr extents) (/ (cadddr extents) -2))
                                                    (list-ref (list-ref sg-calendar-months lang) month) 
                                                    0 TRUE 
                                                    month-fontsize PIXELS text-font )))
          (set! x-offsets (cdr x-offsets))
          (set! month (+ month 1)) )
        (set! y-offsets (cdr y-offsets)) ) )
    ;; Now, transfer the rendered layers to original image
    (gimp-selection-none image)
    (let loop ((layers (cdr (reverse (vector->list (cadr (gimp-image-get-layers image))))))
               (target-layer orig-drawable) )
      (if (null? layers)
        #t
        (begin
          (let ((x (car (gimp-drawable-offsets (car layers))))
                (y (cadr (gimp-drawable-offsets (car layers))))
                (pos (car (gimp-image-get-layer-position orig-image target-layer))) )
            (set! target-layer (car (gimp-layer-new-from-drawable (car layers) orig-image)))
            (gimp-image-add-layer orig-image target-layer pos)
            (gimp-layer-set-offsets target-layer (+ orig-x x) (+ orig-y y)) )
          (loop (cdr layers) target-layer) ) ) )
          
    (gimp-selection-load orig-sel)
    (gimp-image-remove-channel orig-image orig-sel)
    (gimp-image-set-active-layer orig-image orig-drawable)
    (gimp-image-undo-group-end orig-image)
    (gimp-display-delete display)
    (gimp-displays-flush)
    )
  )

(script-fu-register "script-fu-sg-calendar"
  "Calendar..."
  "Generate a calendar overlay for current layer"
  "Saul Goode"
  "Saul Goode"
  "10/26/09, updated Jan 2011"
  "RGB*,GRAY*"
  SF-IMAGE    "Image"    0
  SF-DRAWABLE "Drawable"  0
  SF-OPTION "Language" '("English" "German" "Italian" "Spanish" "French")
  SF-OPTION "Month" (car sg-calendar-months)
  SF-ADJUSTMENT "Year" '( 2011 1753 2050 1 10 0 1 )
  SF-TOGGLE "Sunday first" TRUE
  SF-OPTION "Day format" '("S M T ..." "Su Mo Tu ..." "Sun Mon Tue ...")
  SF-OPTION "Layout" '( "Allow 6-week span" "Force 6-week span" "Wrap Week 6 to Week 1" "Wrap Week 6 to Week 5")
  SF-FONT "Text font" "Sans" 
  SF-FONT "Number font" "Sans" 
  SF-ADJUSTMENT "Font Size (% of maximum)" '( 100 0 100 1 10 0 1)
  SF-TOGGLE "Right Justify" TRUE
  SF-ADJUSTMENT "Border width" '( 1 0 5 1 1 0 1 )
  SF-COLOR "Border color" '(0 0 0)
  SF-OPTION "Date Position" '( "top-left" "top-center" "top-right" "left-center" "center" "right-center" "bottom-left" "bottom-center" "bottom-right")
  )
(script-fu-menu-register "script-fu-sg-calendar"
  "<Image>/Filters/Render"
  )


(script-fu-register "script-fu-sg-calendar-year"
  "Calendar year..."
  "Generate a calendar for current layer"
  "Saul Goode"
  "Saul Goode"
  "Dec 2010"
  "RGB*,GRAY*"
  SF-IMAGE    "Image"    0
  SF-DRAWABLE "Drawable"  0
  SF-OPTION "Language" '("English" "German" "Italian" "Spanish" "French")
  SF-ADJUSTMENT "Year" '( 2011 1753 2050 1 10 0 1 )
  SF-ADJUSTMENT "Columns" '( 4 1 12 1 10 0 1 )
  SF-ADJUSTMENT "Padding" '( 10 0 80 1 10 0 1 )
 
  SF-TOGGLE "Sunday first" TRUE
  SF-OPTION "Day format" '("S M T ..." "Su Mo Tu ..." "Sun Mon Tue ...")
  SF-OPTION "Layout" '( "Allow 6-week span" "Force 6-week span" "Wrap Week 6 to Week 1" "Wrap Week 6 to Week 5")
  SF-FONT "Text font" "Sans" 
  SF-FONT "Number font" "Sans" 
  SF-ADJUSTMENT "Font Size (% of maximum)" '( 100 0 100 1 10 0 1)
  SF-TOGGLE "Right Justify" TRUE
  SF-ADJUSTMENT "Border width" '( 1 0 5 1 1 0 1 )
  SF-COLOR "Border color" '(0 0 0)
  SF-OPTION "Date Position" '( "top-left" "top-center" "top-right" "left-center" "center" "right-center" "bottom-left" "bottom-center" "bottom-right")
  )
(script-fu-menu-register "script-fu-sg-calendar-year"
  "<Image>/Filters/Render"
  )


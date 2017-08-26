;;
;; Convert the paths to a SVG Representation.
;; This is a replacement for the <Path>/Export Path... which also generates a
;; SVG file but one that does not allow the specification of path attributes
;; such as stroke and fill colour, or specific shape types.
;;
;; This script allows for the encoding of these attributes into the path name
;; (which is the only attribute Gimp (version <= 2.1.3) allows for paths) and
;; it decodes these to separate attributes in the SVG File. As extension to
;; this, the path may also encode specific shape types which then interprets
;; the path points differently.  
;;
;; Author: Gerrit Riessen, gerrit.riessen@open-source-consultants.de
;; Copyright (C) 2004 Gerrit Riessen
;; Licensed under the LGPL.
;;
(define (script-fu-svg-export img
                              drawable
                              file-name
                              only-locked
                              load-file)

  (define (point->string pt)
    (string-append " " (number->string (car pt))
                   "," (number->string (cadr pt))))

  (define (point-list->string plst)
    (let* ((str ""))
      (while plst
             (set! pnt (car plst))
             (set! plst (cdr plst))
             (set! str (string-append "" str "Pt:" (point->string pnt) "\n")))
      str))

  ;; map the attribute names to real names
  (define (map-attr-name attr-name)
    (let ((attr-name-map '(("fc" "fill") ;; color value
                           ("fo" "fill-opacity") ;; percentage value
                           ("sc" "stroke") ;; color value
                           ("so" "stroke-opacity") ;; percentage value
                           ("sw" "stroke-width") ;; positive value
                           ;; sj values: (miter | round | bevel | inherit)
                           ("sj" "stroke-linejoin")
                           ;; sl values: (butt | round | square | inherit)
                           ("sl" "stroke-linecap")
                           ;; tp values: (circle | polyline | polygon | rect )
                           ("tp" "__type__")
                           ("nm" "id") ;; name -> id
                           )))
      (while attr-name-map
             (set! fubar (car attr-name-map))
             (set! attr-name-map (cdr attr-name-map))
             (if (equal? (car fubar) attr-name)
                 (set! attr-name (car (cdr fubar))))))
    attr-name)
  
  (define (get-shape-type name)
    (let* ((attrs (strbreakup name "_"))
           (type "path"))
      (while attrs
             (set! attr (car attrs))
             (set! attrs (cdr attrs))
             (if (not (equal? "" attr))
                 (let* ((attr-name (substring attr 0 2))
                        (attr-value (substring attr 2)))
                   (if (equal? "tp" attr-name)
                       (set! type attr-value)))))
      type))

  ;; convert the float-array representation of the paths to an list
  ;; representation where only the anchor and moveto points are stored.
  ;; This is used for specific types (i.e. circle,rect,polyline,...) which
  ;; don't use the Bezier control points.
  (define (float-array-to-point-list points
                                     num-points)
    (let* ((cnt 0)
           (rval '()))
      (while (< cnt num-points)
             (set! pnt (list (aref points cnt) (aref points (+ cnt 1))))
             (set! p-type (aref points (+ cnt 2)))
             (set! cnt (+ cnt 3))
             (cond ;; Anchor
              ((= p-type 1.0)
               (set! rval (cons pnt rval)))
              ;; move to
              ((= p-type 3.0)
               (set! rval (cons pnt rval)))
              ;; control points are ignored
              ((= p-type 2.0)
               '())
              ;; Unknown type
              ((= 1 1) 
               (gimp-message 
                (string-append "Unknown type: " (number->string p-type))))))
      rval))

  ;; compute the midpoint between two points and return the point
  (define (point-midpoint pt1 pt2)
    (list (/ (+ (car pt1) (car pt2)) 2) (/ (+ (cadr pt1) (cadr pt2)) 2)))

  ;; count the number of points in a point-list
  (define (count-points point-list)
    (length point-list))

  ;; return the subtraction of the point 2 from point 1
  (define (point-subtract pt1 pt2)
    (list (- (car pt1) (car pt2)) (- (cadr pt1) (cadr pt2))))

  ;; return the distance from the origin to the point
  (define (vector-length pnt)
    (sqrt (+ (pow (car pnt) 2) (pow (cadr pnt) 2))))
  
  ;; return the distance between two points
  (define (point-distance pt1 pt2)
    (vector-length (point-subtract pt1 pt2)))

  ;; sort a list of points according to their distances from the origin.
  ;; Sorted from closest to furthest away.
  (define (sort-point-list-distance-from-origin plst)
    (qsort plst (lambda(d1 d2) (< d1 d2)) vector-length))

  (define (sort-point-list-lowest-x plst)
    (qsort plst (lambda(pt1 pt2) (< (car pt1) (car pt2)))))

  (define (sort-point-list-lowest-y plst)
    (qsort plst (lambda(pt1 pt2) (< (cadr pt1) (cadr pt2)))))

  (define (sort-point-list-highest-x plst)
    (qsort plst (lambda(pt1 pt2) (> (car pt1) (car pt2)))))

  (define (sort-point-list-highest-y plst)
    (qsort plst (lambda(pt1 pt2) (> (cadr pt1) (cadr pt2)))))

  (define (points-equal pt1 pt2)
    (and (= (car pt1) (car pt2)) (= (cadr pt1) (cadr pt2))))

  (define (create-line pt1 pt2)
    (if (points-equal pt1 pt2)
        (begin
          (gimp-message "Create Line: Returning empty line")
          '()) ;; return null list if points are equal
      (if (= (car pt1) (car pt2))
          (list (car pt1)) ;; return x-axis value if points have same x values
        (let* ((slope (/ (- (cadr pt1) (cadr pt2)) (- (car pt1) (car pt2))))
               (yinsect (- (cadr pt1) (* slope (car pt1)))))
          (list slope yinsect))))) ;; otherwise return slope and y-intersect 

  (define (line-intersection l1 l2)
    (if (or (= (length l1) 0) (= (length l2) 0))
        (gimp-message "ERROR: Line intersection: Empty line"))
    (if (or (and (= (length l1) 1) (= (length l2) 1)) ;; vertical lines
            (and (= (length l1) 2)  ;; slope are equal
                 (= (length l2) 2) (= (car l1) (car l2))))
        (gimp-message "ERROR: Line intersection: lines are parallel"))
    (if (and (= (length l1) 1) (= (length l2) 2))
        (list (car l1) (+ (* (car l2) (car l1)) (cadr l2))))
    (if (and (= (length l1) 2) (= (length l2) 1))
        (list (car l2) (+ (* (car l1) (car l2)) (cadr l1)))
      (let* ((xval (/ (- (cadr l2) (cadr l1)) (- (car l1) (car l2))))
             (yval (+ (* (car l1) xval) (cadr l1))))
        (list xval yval))))

  ;; handle the id string for a path which contains the fill/stroke and shape
  ;; definitions.
  ;; this returns the type if a type was defined
  (define (parse-id-attribute out-file
                              name)
    (let* ((attrs (strbreakup name "_")))
      (while attrs
             (set! attr (car attrs))
             (set! attrs (cdr attrs))
             (if (not (equal? "" attr))
                 (let* ((attr-name (map-attr-name (substring attr 0 2)))
                        (attr-value (substring attr 2)))
                   (fwrite (string-append "    " attr-name "=\"" attr-value
                                          "\"\n") out-file))))))

  (define (handle-type-path path-data 
                            out-file) 
    (let* ((path-type   (car path-data))
           (path-closed (cadr path-data))
           (num-points  (caddr path-data))
           (points      (car (cdddr path-data)))
           (cp-cnt 0) ;; control point count
           (cnt 0)
           (last-mto '())
           (last-cpt '()))
      (fwrite "    d=\"" out-file)
      (while (<= (+ cnt 3) num-points)
             (set! pnt (list (aref points cnt) (aref points (+ cnt 1))))
             (set! p-type (aref points (+ cnt 2)))
             (set! cnt (+ cnt 3))
           
             (cond ;; Anchor
              ((= p-type 1.0) 
               (if (= cnt 3) ;; first anchor point is a moveto
                   (begin
                     (set! last-mto pnt)
                     (fwrite "M" out-file)))
               (fwrite (point->string pnt) out-file)
               (fwrite "\n       C" out-file)
               (set! cp-cnt 0))
              
              ;; Control point
              ((= p-type 2.0) 
               (fwrite (point->string pnt) out-file)
               (set! last-cpt pnt)
               (set! cp-cnt (+ cp-cnt 1)))
              
              ;; moveto point
              ((= p-type 3.0) 

               ;; have a moveto point and need to check whether there is
               ;; an open bezier curve requiring extra points
               (cond ((and (= cp-cnt 1) (= path-closed 1))
                      (fwrite (point->string last-cpt) out-file)
                      (fwrite (point->string last-mto) out-file))
                     ((and (= cp-cnt 2) (= path-closed 1))
                      (fwrite (point->string last-mto) out-file))
                     ((and (= cp-cnt 1) (not (= path-closed 1)))
                      (fwrite (point->string last-cpt) out-file)
                      (fwrite (point->string last-cpt) out-file))
                     ((and (= cp-cnt 2) (not (= path-closed 1)))
                      (fwrite (point->string last-cpt) out-file)))

               (if (= path-closed 1)
                   (fwrite " Z" out-file))

               (fwrite (string-append "\n       M" (point->string pnt))
                       out-file)
               (set! last-mto pnt)
               (set! cp-cnt 0)
               (fwrite "\n       C" out-file))
              ;; Unknown point type
              ((= 1 1) 
               (gimp-message 
                (string-append "Unknown type: " (number->string p-type))))))
      
      (if (not (= cnt num-points))
          (gimp-message (string-append "Not all points where used: "
                                       (number->string cnt) " < "
                                       (number->string num-points))))

      (cond ((and (= cp-cnt 1) (= path-closed 1))
             (fwrite (point->string last-cpt) out-file)
             (fwrite (point->string last-mto) out-file))
            ((and (= cp-cnt 2) (= path-closed 1))
             (fwrite (point->string last-mto) out-file))
            ((and (= cp-cnt 1) (not (= path-closed 1)))
             (fwrite (point->string last-cpt) out-file)
             (fwrite (point->string last-cpt) out-file))
            ((and (= cp-cnt 2) (not (= path-closed 1)))
             (fwrite (point->string last-cpt) out-file)))

      (if (= path-closed 1)
          (fwrite " Z" out-file))
      (fwrite "\"\n" out-file)))
  

  (define (handle-type-rect path-data 
                            out-file) 
    (let* ((path-type                  (car path-data))
           (path-closed               (cadr path-data))
           (p-list (float-array-to-point-list (car (cdddr path-data))
                                              (caddr path-data)))
           (p-list (sort-point-list-distance-from-origin p-list))
           (num-points (count-points p-list))
           )

      (cond 
       ;; 4 point rectangle where the points are ordered:
       ;;    pt1 .-------. pt2   .---> x-axis
       ;;        |       |       |
       ;;    pt4 .-------. pt3   V y-axis
       ;; Point 1 and Point 3 are respectively the first and last in the
       ;; the list. Point 2 and point 4 must first be computed
       ((= num-points 4) 
        (let ((pt1 (car p-list))
              (pt2 (cadr p-list))
              (pt4 (caddr p-list))
              (pt3 (car (cdddr p-list))))
          (if (< (car pt2) (car pt4)) ;; compare x-axis values
              (begin 
                (set! pt2 (caddr p-list))
                (set! pt4 (cadr p-list))))
          (set! width 
                (/ (+ (point-distance pt1 pt2) (point-distance pt4 pt3)) 2))
          (set! height 
                (/ (+ (point-distance pt1 pt4) (point-distance pt2 pt3)) 2))
          (fwrite (string-append "    x=\""     (number->string (car pt1)) 
                                 "\" y=\""      (number->string (cadr pt1)) 
                                 "\" width=\""  (number->string width) 
                                 "\" height=\"" (number->string height) "\"") 
                  out-file)))
       ;; 8 point rectangle
       ;; This can be used to define the rx and ry values. The eight pt{1-8}
       ;; points are assumed to be defined as follows:
       ;;    i1 pt1    pt2 i2       .----->x-axis
       ;;     .  .------.  .        |
       ;;         line12            |
       ;;  pt8.            .pt3     V 
       ;;     |line78      |        y-axis
       ;;     |      line34|
       ;;  pt7.            .pt4
       ;;         line56
       ;;   i4.  .------.  .i3
       ;;        pt6    pt5
       ;;
       ;; Points i{1-4} are intersection points that are computed and not 
       ;; given: 
       ;;    i1 = intersection of line12 and line78
       ;;    i2 = intersection of line12 and line34
       ;;    i3 = intersection of line34 and line56
       ;;    i4 = intersection of line56 and line78
       ;; The average of the distances i1->pt1, pt2->i2, i4->pt6 and
       ;; pt5->i3 defines rx and the average of the distances pt8->i1, i2->pt3,
       ;; pt4->i3 and pt7->i4 defines ry. 
       ;;
       ;; The points pt{1-8} are the eight points we have now. These need to
       ;; be ordered to match the above diagram.
       ((= num-points 8) 
        (let* ((lst-ly (sort-point-list-lowest-y  p-list))
               (lst-lx (sort-point-list-lowest-x  p-list))
               (lst-hy (sort-point-list-highest-y p-list))
               (lst-hx (sort-point-list-highest-x p-list))
               (pt1-2 (list (car lst-ly) (cadr lst-ly)))
               (pt3-4 (list (car lst-hx) (cadr lst-hx)))
               (pt5-6 (list (car lst-hy) (cadr lst-hy)))
               (pt7-8 (list (car lst-lx) (cadr lst-lx)))
               (lst-12-lx (sort-point-list-lowest-x  pt1-2))
               (lst-34-ly (sort-point-list-lowest-y  pt3-4))
               (lst-56-hx (sort-point-list-highest-x pt5-6))
               (lst-78-hy (sort-point-list-highest-y  pt7-8))
               (pt1 (car  lst-12-lx))
               (pt2 (cadr lst-12-lx))
               (pt3 (car  lst-34-ly))
               (pt4 (cadr lst-34-ly))
               (pt5 (car  lst-56-hx))
               (pt6 (cadr lst-56-hx))
               (pt7 (car  lst-78-hy))
               (pt8 (cadr lst-78-hy))
               (line12 (create-line pt1 pt2))
               (line34 (create-line pt3 pt4))
               (line56 (create-line pt5 pt6))
               (line78 (create-line pt7 pt8))
               (i1 (line-intersection line12 line78))
               (i2 (line-intersection line12 line34))
               (i3 (line-intersection line56 line34))
               (i4 (line-intersection line56 line78))
               (rx (/ (+ (point-distance i1 pt1) (point-distance pt2 i2)
                         (point-distance i4 pt6) (point-distance pt5 i3)) 4))
               (ry (/ (+ (point-distance i1 pt8) (point-distance i2 pt3)
                         (point-distance pt7 i4) (point-distance pt4 i3)) 4))
               (width (/ (+ (point-distance i1 i2) (point-distance i4 i3)) 2))
               (height (/ (+ (point-distance i1 i4) (point-distance i2 i3)) 2))
               )
          (fwrite (string-append "    x=\""     (number->string (car i1)) 
                                 "\" y=\""      (number->string (cadr i1)) 
                                 "\" width=\""  (number->string width) 
                                 "\" height=\"" (number->string height)
                                 "\" rx=\""     (number->string rx)
                                 "\" ry=\""     (number->string ry) "\"")
                  out-file)))

       ;; only 4 or 8 make sense
       ((= 1 1) (gimp-message (string-append "Rect type, point count bad: "
                                             (number->string num-points)))))))

  ;; Assume that successive points are opposite and that the average distance
  ;; between all the pairs of points represents the diameter and the average
  ;; midpoint of all the pairs is the center.
  (define (handle-type-circle path-data out-file) 
    (let* ((p-list (float-array-to-point-list (car (cdddr path-data))
                                              (caddr path-data)))
           (cpt '(-1 -1))
           (diameter -1))
      (while p-list
             (set! pt1 (car p-list))
             (set! pt2 (cadr p-list))
             (set! p-list (cddr p-list))
             (if (and (= (car cpt) -1) (= (cadr cpt) -1))
                 (set! cpt (point-midpoint pt1 pt2))
               (set! cpt (point-midpoint cpt (point-midpoint pt1 pt2))))
             (if (= diameter -1)
                 (set! diameter (point-distance pt1 pt2))
               (set! diameter (/ (+ diameter (point-distance pt1 pt2)) 2))))
      (fwrite (string-append "   cx=\"" (number->string (car cpt))
                             "\" cy=\"" (number->string (cadr cpt))
                             "\" r=\"" (number->string (/ diameter 2)) "\"")
                             out-file)))
  
  ;; Assume that the points are ordered and we just dump them out.
  (define (handle-type-polygon path-data out-file) 
    (let* ((p-list (float-array-to-point-list (car (cdddr path-data))
                                              (caddr path-data))))
      (fwrite "    points=\"" out-file)
      (while p-list
             (set! pnt (car p-list))
             (set! p-list (cdr p-list))
             (fwrite (point->string pnt) out-file))
      (fwrite "\"" out-file)))

  ;; Same as polygon, tag name has already been modified
  (define (handle-type-polyline path-data out-file) 
    (handle-type-polygon path-data out-file))
  
  (define (generate-data type
                         path-data
                         out-file)
    (cond ((equal? "path"     type) (handle-type-path     path-data out-file))
          ((equal? "rect"     type) (handle-type-rect     path-data out-file))
          ((equal? "circle"   type) (handle-type-circle   path-data out-file))
          ((equal? "polygon"  type) (handle-type-polygon  path-data out-file))
          ((equal? "polyline" type) (handle-type-polyline path-data out-file))
          ;; else does not work?!?
          ((= 1 1) (gimp-message 
                    (string-append "Unknown Shape Type: '" type "'")))))
  
  ;; ******** End of Helper functions
  ;; Main program code
  (let* ((paths (gimp-path-list img))
         (names (cadr paths))
         (img-width (car (gimp-image-width img)))
         (img-height (car (gimp-image-height img)))
         (out-file (fopen file-name "w"))
         (n "")
         (path-data '()))

    (fwrite (string-append
             "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
             "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 20010904//EN\"\n      "
             "\"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd\">\n"
             "\n<svg xmlns=\"http://www.w3.org/2000/svg\"\n"
             "     width=\"" (number->string img-width)
             "\" height=\"" (number->string img-height)
             "\"\n     viewbox=\"0 0 " (number->string img-width)
             " " (number->string img-height) "\">\n\n")
            out-file)
    (fflush out-file)

    (while names
           (set! n (car names))
           (set! names (cdr names))
           (if (and (= only-locked 1) 
                    (= (car (gimp-path-get-locked img n)) 0))
               '()
             (begin
               (set! shape-type (get-shape-type n))
               (fwrite (string-append "  <" shape-type " __id__=\"" n "\"\n") 
                       out-file)

               (parse-id-attribute out-file n)
               (set! path-data (gimp-path-get-points img n))
               (generate-data shape-type path-data out-file)
               
               (fwrite "/>\n" out-file)
               (fflush out-file))))

    (fwrite "\n</svg>\n" out-file)
    (fclose out-file))

  (if (= load-file 1)
      (let* ((new-image (car (gimp-file-load 0 file-name file-name))))
        (gimp-display-new new-image))))

(script-fu-register "script-fu-svg-export"
                    _"<Image>/Script-Fu/Utils/SVG Export..."
"Export path values as SVG file. Script assumes that the path 
name is an encoding of SVG attributes. Encodings are done using
2 letter prefixes followed by a value.

where 2 letter code is one of:
    fc => fill color (SVG attribute: fill)
    fo => fill opacity (SVG attribute: fill-opacity)
    nm => name of the path (SVG attribute: id)
    sc => stroke color (SVG attribute: stroke)
    sj => stroke line join (SVG attribute: stroke-linejoin)
    sl => stroke line cap (SVG attribute: stroke-linecap)
    so => stroke opacity (SVG attribute: stroke-opacity)
    sw => stroke width (SVG attribute: stroke-width)
    tp => type of shape can be: circle,rect,polyline,polygon

The value can not contain an underscore and exact values for
attributes can be obtained from the SVG Specifications:
   http://www.w3.org/TR/SVG

For example:
  _fcwhite_scblack_tppolyline_slbutt_sjmiter_nmexample_

Loading the resultant SVG file is only possible if you have
SVG support compiled in, but you can export SVG using this
script.
"
                    "Gerrit Riessen (http://gimp-registry.fargonauten.de/node/100)"
                    "Gerrit Riessen"
                    "27 August 2004"
                    "*"
                    SF-IMAGE    "Image" 0
                    SF-DRAWABLE "Drawable" 0
                    SF-FILENAME "Output file" "/tmp/output.svg"
                    SF-TOGGLE   "Export only locked paths?" FALSE
                    SF-TOGGLE   "Load SVG file?" TRUE)
                                    


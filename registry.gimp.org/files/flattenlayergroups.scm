;;; Flatten Layer Groups
;;; by Timofei Shatrov
;;; v. 0.1

(define (vector-for-each fn vec)
  "Run fn on each element of vec"
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len))
    (fn (vector-ref vec i))))

(define (is-true? fn item)
  ;; does fn return '(TRUE) ?
  (= (car (fn item)) TRUE))

(define (flatten-layer-group img layer)
  "Flatten a single layer group"
  ;; ok... there is no function for that... gotta do it the hard way...
  (let ((layers (cadr (gimp-image-get-layers img))))
    (vector-for-each
     (lambda (lr) (gimp-item-set-visible lr (if (= lr layer) TRUE FALSE)))
     layers)
    ;; do we need to make every sublayer of layer visible at this point?
    (gimp-image-merge-visible-layers img EXPAND-AS-NECESSARY)))

(define (flatten-layer-groups img)
  "Flatten all layer groups in an image"
  (gimp-image-undo-group-start img)
  (let* ((get-layers (gimp-image-get-layers img))
         (layers (cadr get-layers))
         (visi-status (make-vector (car get-layers)))
         (i 0)
         )
    ;; remember visibility status
    (vector-for-each
     (lambda (layer)
       (let ((visible (car (gimp-item-get-visible layer))))
         (vector-set! visi-status i visible)
         (set! i (+ i 1))))
     layers)
    ;; flatten each layer group   
    (vector-for-each
     (lambda (layer)
       (if (is-true? gimp-item-is-group layer)
           (flatten-layer-group img layer)))
     layers)
    ;; restore visibility status (note that old layers list is useless now)
    (set! i 0)
    (vector-for-each
     (lambda (layer)
       (gimp-item-set-visible layer (vector-ref visi-status i))
       (set! i (+ i 1)))
     (cadr (gimp-image-get-layers img))))
  (gimp-image-undo-group-end img)
  (gimp-displays-flush))

(define script-fu-flatten-layer-groups flatten-layer-groups)

(script-fu-register
 "script-fu-flatten-layer-groups"
 "Flatten Layer Groups"
 "Flattens all layer groups in an image"
 "Timofei Shatrov"
 "Copyright 2012"
 "June 15, 2012"
 ""
 SF-IMAGE     "Image to use"       0
 )

(script-fu-menu-register "script-fu-flatten-layer-groups" "<Image>/Image")

;;; Extract background/foreground from animation
;;; v. 0.3

(define (bgmask-diff-layers img layer1 layer2 threshold buffer)
  (let* ((layer1-copy (car (gimp-layer-copy layer1 FALSE)))
         (layer2-copy (car (gimp-layer-copy layer2 FALSE))))
    (gimp-image-insert-layer img layer1-copy 0 0)
    (gimp-image-insert-layer img layer2-copy 0 0)
    (gimp-layer-set-mode layer2-copy DIFFERENCE-MODE)
    (let ((layer (car (gimp-image-merge-down img layer2-copy 2))))
      (cond ((> buffer 0)
             (gimp-desaturate layer)
             (gimp-levels layer 0 threshold (min (+ threshold buffer) 255) 1 0 255)
             )
            (else (gimp-desaturate layer) (gimp-threshold layer threshold 255)))
      layer)))

(define (bgmask-remove-alpha-channel layer)
  (let ((mask (car (gimp-layer-create-mask layer ADD-ALPHA-TRANSFER-MASK))))
    (gimp-layer-add-mask layer mask)
    (gimp-layer-remove-mask layer MASK-DISCARD)))

;; Averaging color values with opacity, the reference guide:
;; Ln - opacity 100/n
;; ...
;; L3 - opacity 100/3
;; L2 - opacity 100/2
;; L1 - opacity 100/1

(define (bgmask-average-layers img lst get-layer no-alpha)
  (let ((i 1)
        (res #f))
    (for-each
     (lambda (x)
       (let* ((layer (get-layer x))
              (opacity (car (gimp-layer-get-opacity layer))))
         (cond ((> i 1)
                (gimp-layer-set-opacity layer (/ opacity i))
                (set! res (car (gimp-image-merge-down img layer 2))))
               (else (set! res layer)))
         (set! i (+ i 1))))
     lst)
    (if (and res no-alpha) (bgmask-remove-alpha-channel res))
    res))

(define (bgmask-diff-matrix img layer other-layers threshold)
  (bgmask-average-layers img other-layers
                         (lambda (other-layer)
                           (bgmask-diff-layers img layer other-layer threshold 0))
                         #f))

(define (bgmask-produce-premasks img prime-matrix matrices)
  (map
   (lambda (matrix)
     (let ((prime-copy (car (gimp-layer-copy prime-matrix FALSE)))
           (pos (car (gimp-image-get-item-position img matrix))))
       (gimp-image-insert-layer img prime-copy 0 pos)
       (gimp-layer-set-mode prime-copy DIFFERENCE-MODE)
       (let ((merged (car (gimp-image-merge-down img prime-copy 2))))
         (gimp-threshold merged 1 255)
         ;;(gimp-invert merged)
         merged)))
   matrices))

(define (bgmask-mask-common img mask-layer)
  "Make a selection from a layer that's supposed to be a mask but isn't"
  (let ((mask (car (gimp-layer-create-mask mask-layer ADD-COPY-MASK))))
    (gimp-layer-add-mask mask-layer mask)
    (gimp-image-select-item img CHANNEL-OP-REPLACE mask)
    (gimp-layer-remove-mask mask-layer MASK-DISCARD)))

(define (bgmask-create-layer-with-mask img source-layer mask-layer)
  (let ((layer (car (gimp-layer-copy source-layer TRUE))))
    (gimp-image-insert-layer img layer 0 0)
    (bgmask-mask-common img mask-layer)
    (if (bgmask-is-true? gimp-selection-bounds img)
        (gimp-edit-clear layer))
    (gimp-selection-none img)
    (gimp-image-remove-layer img mask-layer)
    layer))

(define (bgmask-diff-median img layers threshold)
  (for-each (lambda (layer) (gimp-item-set-visible layer TRUE)) layers)
  (let* ((nl (length layers)) (i 0)
         (matrices (map (lambda (layer)
                          (gimp-progress-update (/ i nl)) (set! i (+ i 1))
                          (bgmask-diff-matrix img layer layers threshold))
                        layers))
         (prime-matrix #f))
    (for-each
     (lambda (matrix)
       (let ((matrix-copy (car (gimp-layer-copy matrix FALSE))))
         (gimp-image-insert-layer img matrix-copy 0 0)
         (if prime-matrix
             (begin
               (gimp-layer-set-mode matrix-copy DARKEN-ONLY-MODE)
               (set! prime-matrix (car (gimp-image-merge-down img matrix-copy 2))))
             (set! prime-matrix matrix-copy))))
     matrices)
    (set! matrices (bgmask-produce-premasks img prime-matrix matrices))
    (gimp-image-remove-layer img prime-matrix)
    (gimp-progress-update 1)
    (let* ((components (let loop ((li layers)
                                  (mi matrices))
                         (cond ((null? li) '())
                               (else (cons (bgmask-create-layer-with-mask img (car li) (car mi))
                                           (loop (cdr li) (cdr mi)))))))
           (background (bgmask-average-layers img components (lambda (x) x) #t)))
      (gimp-item-set-name background "Background [bg]")
      (gimp-image-reorder-item img background 0 (car (gimp-image-get-layers img)))
      background)))

(define (bgmask-get-layers img)
  (reverse (vector->list (cadr (gimp-image-get-layers img)))))

(define (bgmask-image-copy img)
  (let ((newimg (car (gimp-image-new
                      (car (gimp-image-width img))
                      (car (gimp-image-height img))
                      0)))
        (layers (bgmask-get-layers img)))
    (for-each
     (lambda (layer)
       (let ((layer-copy (car (gimp-layer-new-from-drawable layer newimg))))
         (gimp-image-insert-layer newimg layer-copy 0 0)))
     layers)
    newimg))

(define (bgmask-all-layers img threshold)
  ;;(gimp-image-undo-group-start img)
  (let ((newimg (bgmask-image-copy img)))
    (gimp-image-undo-disable newimg)
    (bgmask-diff-median newimg (bgmask-get-layers newimg) threshold)
    (gimp-image-undo-enable newimg)
    (gimp-display-new newimg))
  ;;(gimp-image-undo-group-end img)
  (gimp-displays-flush))

(define (script-fu-bgmask-all-layers img threshold)
  (bgmask-all-layers img threshold))
         
(script-fu-register
 "script-fu-bgmask-all-layers"
 "Extract background..."
 "Extract background from animation frames"
 "Timofei Shatrov"
 "Copyright 2012"
 "October 27, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE     "Image to use"       0
 SF-ADJUSTMENT "Threshold" '(10 1 255 1 5 0 SF-SPINNER)
 )

(script-fu-menu-register "script-fu-bgmask-all-layers" "<Image>/Script-Fu/BgMask")

(define (bgmask-mask-against-background img bg layers threshold buffer)
  (gimp-image-undo-group-start img)
  (gimp-item-set-visible bg TRUE)
  (let ((nl (length layers)) (i 0))
    (for-each
     (lambda (layer)
       (gimp-progress-update (/ i nl)) (set! i (+ i 1))
       (let ((diff (bgmask-diff-layers img bg layer threshold buffer)))
         (bgmask-mask-common img diff)
         (let ((mask (car (gimp-layer-create-mask layer ADD-SELECTION-MASK))))
           (gimp-layer-add-mask layer mask))
         (gimp-selection-none img)
         (gimp-image-remove-layer img diff)))
     layers))
  (gimp-image-undo-group-end img)
  (gimp-displays-flush))

(define (script-fu-mask-against-background img threshold buffer)
  (let* ((layers (bgmask-get-layers img)))
    (bgmask-mask-against-background img (car layers) (cdr layers) threshold buffer)))
         
(script-fu-register
 "script-fu-mask-against-background"
 "Mask against background..."
 "Create difference masks for all layers compared to bottom layer"
 "Timofei Shatrov"
 "Copyright 2012"
 "October 27, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE     "Image to use"       0
 SF-ADJUSTMENT "Threshold" '(10 1 255 1 5 0 SF-SPINNER)
 SF-ADJUSTMENT "Buffer" '(0 0 50 1 5 0 SF-SPINNER)
 )

(script-fu-menu-register  "script-fu-mask-against-background" "<Image>/Script-Fu/BgMask")

(define (bgmask-vector-for-each fn vec)
  "Run fn on each element of vec"
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len))
    (fn (vector-ref vec i))))

(define (bgmask-walk-layers-recursive img test fn)
  (let loop ((layers (cadr (gimp-image-get-layers img))))
    (bgmask-vector-for-each
     (lambda (layer)
       (cond ((test layer) (fn layer))
             ((bgmask-is-true? gimp-item-is-group layer)
              (loop (cadr (gimp-item-get-children layer))))))
     layers)))

(define (bgmask-is-true? fn item)
  ;; does fn return '(TRUE) ?
  (= (car (fn item)) TRUE))

(define (script-fu-average-linked-layers img delete-originals no-alpha)
  (gimp-image-undo-group-start img)
  (let ((layers '()) (originals '())) 
    (bgmask-walk-layers-recursive 
     img (lambda (layer) (bgmask-is-true? gimp-item-get-linked layer))
     (lambda (layer)
       (gimp-item-set-linked layer FALSE)
       (let ((layer-copy (car (gimp-layer-copy layer FALSE))))
         (gimp-image-insert-layer img layer-copy 0 0)
         (gimp-item-set-visible layer-copy TRUE)
         (set! originals (cons layer originals))
         (set! layers (cons layer-copy layers)))))
    (let ((res (bgmask-average-layers img (reverse layers) (lambda (x) x) (= no-alpha TRUE))))
      (gimp-item-set-name res "Average"))
    (if (= delete-originals TRUE)
        (for-each (lambda (layer) (gimp-image-remove-layer img layer)) originals)))
  (gimp-image-undo-group-end img)
  (gimp-displays-flush))

(script-fu-register
 "script-fu-average-linked-layers"
 "Average linked layers..."
 "Create a layer from linked layers with averaged pixel values"
 "Timofei Shatrov"
 "Copyright 2012"
 "October 28, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE     "Image to use"       0
 SF-TOGGLE "Delete originals" 0
 SF-TOGGLE "Remove alpha from result" 0
 )

(script-fu-menu-register  "script-fu-average-linked-layers" "<Image>/Script-Fu/BgMask")

;; Manually extract background

(define (bgmask-make-image-sized-layer img name)
  (car (gimp-layer-new img
                       (car (gimp-image-width img))
                       (car (gimp-image-height img))
                       1 name 100 0)))

(define *bgmask-manual-bg-group-name* "BgMask: select background")
(define *bgmask-manual-bg-layer-name* "BgMask: Background buffer")
(define *bgmask-manual-alpha-layer-name* "BgMask: Background alpha")

(define (bgmask-manual-bg-init img)
  (let ((group (car (gimp-layer-group-new img)))
        (bg-layer (bgmask-make-image-sized-layer img *bgmask-manual-bg-layer-name*))
        (alpha-layer (bgmask-make-image-sized-layer img *bgmask-manual-alpha-layer-name*)))
    (gimp-item-set-name group *bgmask-manual-bg-group-name*)
    (gimp-drawable-fill alpha-layer BACKGROUND-FILL)
    (gimp-image-insert-layer img group 0 0)
    (gimp-image-insert-layer img bg-layer group 0)
    (gimp-image-insert-layer img alpha-layer group 0)
    (let ((mask (car (gimp-layer-create-mask alpha-layer ADD-BLACK-MASK))))
      (gimp-layer-add-mask alpha-layer mask))
    ))

(define (bgmask-manual-bg-check-consistency img group)
  "Check consistency of bgmask's layer group"
  (let ((n-children (gimp-item-get-children group))
        (raise-error (lambda () (error "BgMask's layer group is corrupt. Please fix or remove it"))))
    (if (< (car n-children) 2) (raise-error)
        (let ((first-layer (vector-ref (cadr n-children) 0))
              (second-layer (vector-ref (cadr n-children) 1)))
          (cond ((= (car (gimp-layer-get-mask first-layer)) -1) (raise-error))
                ((not (bgmask-is-true? gimp-drawable-has-alpha second-layer)) (raise-error))
                (#t #t))))))

(define (bgmask-manual-bg-check img)
  "Check whether bgmask's layer group exists"
  (let ((n-layers (gimp-image-get-layers img)))
    (if (= (car n-layers) 0) #f
        (let ((first-layer (vector-ref (cadr n-layers) 0)))
          (and (bgmask-is-true? gimp-item-is-group first-layer)
               (string=? (car (gimp-item-get-name first-layer))
                         *bgmask-manual-bg-group-name*)
               (bgmask-manual-bg-check-consistency img first-layer))))))
                
(define (bgmask-manual-update-alpha img)
  (let* ((group (vector-ref (cadr (gimp-image-get-layers img)) 0))
         (group-layers (cadr (gimp-item-get-children group)))
         (alpha-layer (vector-ref group-layers 0))
         (bg-layer (vector-ref group-layers 1)))
    (gimp-image-select-item img CHANNEL-OP-REPLACE bg-layer)
    (gimp-layer-remove-mask alpha-layer MASK-DISCARD)
    (let ((mask (car (gimp-layer-create-mask alpha-layer ADD-SELECTION-MASK))))
      (gimp-layer-add-mask alpha-layer mask))
    (gimp-selection-none img)
    (gimp-item-set-visible alpha-layer TRUE)
    (gimp-item-set-visible group TRUE)
    ))

(define (bgmask-manual-add-selection img drw)
  (and (= (car (gimp-image-get-floating-sel img)) -1)
       (let* ((group (vector-ref (cadr (gimp-image-get-layers img)) 0))
              (bg-layer (vector-ref (cadr (gimp-item-get-children group)) 1)))
         (if (not (= bg-layer drw))
             (let ((floating (begin (gimp-edit-copy drw)
                                    (gimp-edit-paste drw TRUE)
                                    (car (gimp-image-get-floating-sel img)))))
               (gimp-floating-sel-to-layer floating)
               (gimp-image-reorder-item img floating group 2)
               (gimp-item-set-visible floating TRUE)
               (let ((new-bg (car (gimp-image-merge-down img bg-layer CLIP-TO-IMAGE))))
                 (gimp-item-set-name new-bg *bgmask-manual-bg-layer-name*)))))))

(define (bgmask-find-visible-frame img drw)
  "If none are visible, use drw"
  (let loop ((layers (cdr (vector->list (cadr (gimp-image-get-layers img))))))
    (cond ((null? layers) drw)
          ((bgmask-is-true? gimp-item-get-visible (car layers)) (car layers))
          (#t (loop (cdr layers))))))

(define (script-fu-bgmask-manual-bg img drw)
  (gimp-image-undo-group-start img)
  (if (not (bgmask-manual-bg-check img)) (bgmask-manual-bg-init img))
  ;; if selection: add to background
  (if (bgmask-is-true? gimp-selection-bounds img)
        ;; select the first visible frame, this ensures that the user gets what they actually see
      (begin
        (set! drw (bgmask-find-visible-frame img drw))
        (bgmask-manual-add-selection img drw)))
  ;; update background mask
  (bgmask-manual-update-alpha img)
  (gimp-image-set-active-layer img drw)
  (gimp-image-undo-group-end img)
  (gimp-displays-flush))

(script-fu-register
 "script-fu-bgmask-manual-bg"
 "Extract background manually"
 "Adds current selection to a background buffer layer. There's another layer above it that serves as visual aid to easily see the parts that were already selected. Bind this function to a key."
 "Timofei Shatrov"
 "Copyright 2012"
 "December 15, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE     "Image to use"       0
 SF-DRAWABLE  "Current layer"      0
 )

(script-fu-menu-register  "script-fu-bgmask-manual-bg" "<Image>/Script-Fu/BgMask")

;; Mask adjustments

(define (bgmask-apply-masks img)
  (bgmask-vector-for-each 
   (lambda (layer)
     (let ((mask (car (gimp-layer-get-mask layer))))
       (if (not (= mask -1)) (gimp-layer-remove-mask layer MASK-APPLY))))
   (cadr (gimp-image-get-layers img))))

(define (bgmask-adjust-mask-blur img drw amount)
  (let ((mask (car (gimp-layer-get-mask drw))))
    (if (and (not (= mask -1)) (> amount 0))
        (plug-in-gauss 1 img mask amount amount 1))))

(define (bgmask-adjust-mask-grow img drw amount)
  "Shrink if amount < 0"
  (let ((mask (car (gimp-layer-get-mask drw))))
    (cond ((= mask -1))
          ((= amount 0))
          (#t 
           (gimp-image-select-item img 2 mask)
           (if (> amount 0)
               (gimp-selection-grow img amount)
               (gimp-selection-shrink img (- amount)))
           (gimp-layer-remove-mask drw 1)
           (let ((newmask (car (gimp-layer-create-mask drw ADD-SELECTION-MASK))))
             (gimp-layer-add-mask drw newmask))
           (gimp-selection-none img)))))

(define (bgmask-transfer-alpha img)
  (bgmask-vector-for-each
   (lambda (layer)
     (cond
      ((not (= (car (gimp-layer-get-mask layer)) -1)))
      ((begin 
         (gimp-selection-all img)
         (gimp-image-select-item img 1 layer)
         (not (bgmask-is-true? gimp-selection-bounds img))))
      (#t (gimp-selection-none img)
          (let ((newmask (car (gimp-layer-create-mask layer ADD-ALPHA-TRANSFER-MASK))))
            (gimp-layer-add-mask layer newmask)))))
   (cadr (gimp-image-get-layers img))))

(define (bgmask-adjust-mask-denoise img drw amount)
  (let ((mask (car (gimp-layer-get-mask drw))))
    (cond ((= mask -1))
          ((= amount 0))
          (#t
           (gimp-image-select-item img 2 mask)
           (gimp-selection-invert img)
           (let ((temp1 (bgmask-make-image-sized-layer img "Bgmask denoise: temp")))
             (gimp-drawable-fill temp1 WHITE-FILL)
             (gimp-image-insert-layer img temp1 0 0)
             (gimp-context-push)
             (gimp-context-set-foreground '(0 0 0))
             (if (bgmask-is-true? gimp-selection-bounds img)
                 (gimp-edit-fill temp1 0))
             (gimp-selection-none img)
             (let ((cmatrix '(1 1 1 1 1
                              1 1 1 1 1
                              1 1 0 1 1
                              1 1 1 1 1
                              1 1 1 1 1))
                   (channels '(1 1 1 1 0)))
               (plug-in-convmatrix 1 img temp1 25 (list->vector cmatrix)
                                   0 24 0 5 (list->vector channels) 0)
               (gimp-context-set-sample-threshold-int (* 12 amount))

               (gimp-image-select-color img CHANNEL-OP-REPLACE temp1 '(0 0 0))
               (gimp-context-set-foreground '(0 0 0))
               (gimp-edit-fill mask 0)

               (gimp-image-select-color img CHANNEL-OP-REPLACE temp1 '(255 255 255))
               (gimp-context-set-foreground '(255 255 255))
               (gimp-edit-fill mask 0)
               (gimp-image-remove-layer img temp1))
             (gimp-context-pop)
             (gimp-selection-none img))))))

(define (bgmask-adjust-mask-smoothen img drw amount)
  (let ((mask (car (gimp-layer-get-mask drw)))
        (threshold amount))
    (cond ((= mask -1))
          (#t
           (if (> amount 0) (gimp-threshold mask threshold 255))
           (gimp-image-select-item img 2 mask)
           (plug-in-sel2path 1 img drw)
           (let ((path (car (gimp-image-get-active-vectors img))))
             (gimp-selection-none img)
             (gimp-context-push)
             (gimp-context-set-foreground '(0 0 0))
             (gimp-edit-fill mask 0)
             (gimp-image-select-item img 2 path)
             (if (bgmask-is-true? gimp-selection-bounds img)
                 (gimp-edit-fill mask 2))
             (gimp-context-pop)
             (gimp-image-remove-vectors img path))
           (gimp-selection-none img)))))


(define (script-fu-bgmask-adjust-masks img transfer-alpha denoise blur grow smoothen apply-masks)
  (gimp-image-undo-group-start img)
  (if (= transfer-alpha TRUE) (bgmask-transfer-alpha img))
  (let ((nl (car (gimp-image-get-layers img))) (i 0))
    (bgmask-vector-for-each
     (lambda (layer)
       (gimp-progress-update (/ i nl)) (set! i (+ i 1))
       (bgmask-adjust-mask-denoise img layer denoise)
       (bgmask-adjust-mask-blur img layer blur)
       (bgmask-adjust-mask-grow img layer grow)
       (if (= smoothen TRUE) (bgmask-adjust-mask-smoothen img layer 128)))
     (cadr (gimp-image-get-layers img))))
  (if (= apply-masks TRUE) (bgmask-apply-masks img))
  (gimp-image-undo-group-end img)
  (gimp-displays-flush))

(script-fu-register
 "script-fu-bgmask-adjust-masks"
 "Mask adjustments..."
 "Grow/shrink/blur all masks in the image and/or mass apply them"
 "Timofei Shatrov"
 "2012-2013"
 "May 21, 2013"
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE      "Image to use"       0
 SF-TOGGLE     "1. Transfer alpha channel to masks" 0
 SF-ADJUSTMENT "2. Denoise amount (0-10)" '(0 0 10 1 5 0 SF-SPINNER)
 SF-ADJUSTMENT "3. Blur radius" '(0 0 255 1 5 1 SF-SPINNER)
 SF-ADJUSTMENT "4. Grow/shrink amount" '(0 -255 255 1 5 0 SF-SPINNER)
 SF-TOGGLE     "5. Smoothen via paths" 0
 SF-TOGGLE     "6. Apply masks" 0
 )

(script-fu-menu-register "script-fu-bgmask-adjust-masks" "<Image>/Script-Fu/BgMask")


(define (bgmask-calculate-covering-path img radius)
  (let* ((width (car (gimp-image-width img)))
         (height (car (gimp-image-height img)))
         (path '()))
    (let loop ((x 0) (y 0) (dir -1))
      (cond ((> x width) (map exact->inexact path))
            (else (set! path (append path (list x y)))
                  (cond ((= dir -1) (loop x height 0))
                        ((= dir 1) (loop x 0 0))
                        ((= dir 0) (loop (+ x radius) y (if (= y 0) -1 1)))))))))

(define (bgmask-clone-color-erase img source target)
  (gimp-context-push)
  (let* ((radius 100)
         (path (bgmask-calculate-covering-path img radius))
         (brush (car (gimp-brush-new "BgMask temp brush")))
         )
    (gimp-brush-set-shape brush 0)
    (gimp-brush-set-hardness brush 1)
    (gimp-brush-set-radius brush radius)
    (gimp-context-set-brush brush)
    (gimp-context-set-sample-merged FALSE)
    (gimp-context-set-paint-mode COLOR-ERASE-MODE)
    ;;(gimp-context-set-paint-mode 0)
    (gimp-context-set-opacity 100)
    (gimp-context-set-brush-aspect-ratio 0)
    (gimp-context-set-brush-angle 0)
    (gimp-context-set-brush-size radius)
    (gimp-clone target source 0 0 0 (length path) (list->vector path))
    (gimp-brush-delete brush))
  (gimp-context-pop))

(define (script-fu-bgmask-blend-edges img amount)
  (gimp-image-undo-group-start img)
  (let* ((layers (bgmask-get-layers img))
         (bg (car layers))
         (rest (cdr layers))
         (nl (length rest)) (i 0))
    (for-each
     (lambda (layer)
       (gimp-progress-update (/ i nl)) (set! i (+ i 1))
       (let ((mask (car (gimp-layer-get-mask layer))))
         (if (not (= mask -1)) (gimp-layer-remove-mask layer MASK-APPLY)))
       (gimp-image-select-item img CHANNEL-OP-REPLACE layer)
       (gimp-selection-invert img)
       (if (bgmask-is-true? gimp-selection-bounds img)
           (begin
             (gimp-selection-grow img amount)
             (bgmask-clone-color-erase img bg layer))))
     layers))
  (gimp-selection-none img)
  (gimp-image-undo-group-end img)
  (gimp-displays-flush))
  
(script-fu-register
 "script-fu-bgmask-blend-edges"
 "Blend edges..."
 "Blend edges using Color to Alpha algorithm using background as a reference"
 "Timofei Shatrov"
 "Copyright 2013"
 "May 19, 2013"
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE     "Image to use"       0
 SF-ADJUSTMENT "Radius" '(1 0 255 1 5 0 SF-SPINNER)
 )

(script-fu-menu-register "script-fu-bgmask-blend-edges" "<Image>/Script-Fu/BgMask")
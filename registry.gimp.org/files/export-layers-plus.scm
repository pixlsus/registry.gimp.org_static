;;; Export layers plus: export layers as separate images
;;; v. 0.1

(define *elp-default-frame-rate* 100)

(define (elp-is-true? fn item)
  (= (car (fn item)) TRUE))

(define (elp-sanitize-string s)
  "Remove characters illegal in Windows filenames"
  (let* ((bad-characters '(#\\ #\/ #\: #\* #\? #\" #\< #\> #\|))
         (reslist (let loop ((slist (string->list s)))
                    (cond ((null? slist) (list))
                          ((memv (car slist) bad-characters)
                           (loop (cdr slist)))
                          (else (cons (car slist) (loop (cdr slist))))))))
    (list->string reslist)))

(define (elp-replace-once string tokens)
  (if (null? tokens) (cons "%" string)
      (let* ((token-re (string-append "^" (caar tokens)))
             (token-val-fn (cdar tokens))
             (buffer (make-vector 1)))
        (if (re-match token-re string buffer)
            (let* ((boundaries (vector-ref buffer 0))
                   (token (substring string (car boundaries) (cdr boundaries)))
                   (rest (substring string (cdr boundaries) (string-length string))))
              (cons (token-val-fn token) rest))
            (elp-replace-once string (cdr tokens))))))

(define (elp-replace-all string tokens)
  (let loop ((slist (string->list string))
             (result ""))
    (cond ((null? slist) result)
          ((char=? (car slist) #\%)
           (let ((res (elp-replace-once (list->string (cdr slist)) tokens)))
             (loop (string->list (cdr res)) (string-append result (car res)))))
          (else 
           (loop (cdr slist) (string-append result (make-string 1 (car slist))))))))

(define (elp-generic-val-fn value)
  (lambda (token) (elp-sanitize-string value)))

(define (elp-format-percent-i value)
  (lambda (token)
    (let* ((token-len (string-length token))
           (len (and (> token-len 1)
                     (string->number (substring token 0 (- token-len 1)))))
           (ns (number->string value))
           (lns (string-length ns)))
      (cond
       ((not len) ns)
       ((> lns len) (substring ns (- lns len) lns))
       (else (string-append (make-string (- len lns) #\0) ns))))))

(define (elp-vector-for-each fn vec)
  "Run fn on each element of vec"
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len))
    (fn (vector-ref vec i))))

(define (elp-walk-layers walk-direction img test fn)
  (let ((layers (vector->list (cadr (gimp-image-get-layers img))))
        (count 0))
    (if (= walk-direction 0) (set! layers (reverse layers)))
    (for-each
     (lambda (layer) 
       (if (or (not test) (test layer)) (fn count layer))
       (set! count (+ count 1)))
     layers)))


(define (elp-image-copy img)
  (let ((newimg (car (gimp-image-new
                      (car (gimp-image-width img))
                      (car (gimp-image-height img))
                      0)))
        (layers (reverse (vector->list (cadr (gimp-image-get-layers img))))))
    (for-each
     (lambda (layer)
       (let ((layer-copy (car (gimp-layer-new-from-drawable layer newimg))))
         (gimp-image-insert-layer newimg layer-copy 0 0)))
     layers)
    newimg))


(define (elp-export-layer img img-name path filename-template count layer)
  (let* ((tokens `(("%" . ,(lambda (token) "%"))
                   ("n" . ,(elp-generic-val-fn img-name))
                   ("l" . ,(elp-generic-val-fn (car (gimp-item-get-name layer))))
                   ("\\d*i" . ,(elp-format-percent-i count))))
         (name (elp-replace-all filename-template tokens))
         (outpath (string-append path "/" name)))
    (gimp-file-save 1 img layer outpath name)))


(define (elp-walk-layers-filtered walk-direction img layer-filter fn)
  (elp-walk-layers
   walk-direction img 
   (cond ((= layer-filter 0) #f)
         ((= layer-filter 1) (lambda (layer) (elp-is-true? gimp-item-get-visible layer)))
         ((= layer-filter 2) (lambda (layer) (elp-is-true? gimp-item-get-linked layer))))
   fn))

(define (elp-simple-export img img-name path filename-template walk-direction count-offset layer-filter)
  ;;; DOESNT APPLY LAYER MASKS!
  (elp-walk-layers-filtered
   walk-direction img layer-filter
   (lambda (count layer) 
     (elp-export-layer img img-name path filename-template (+ count count-offset) layer))))

(define (elp-index-export img img-name path filename-template count-offset index)
  "index is just a list of layers that need to be exported"
  (let ((count 0))
    (for-each
     (lambda (layer)
       (elp-export-layer img img-name path filename-template (+ count count-offset) layer)
       (set! count (+ count 1)))
     index)))

(define (elp-has-layer-masks img)
  (let loop ((layers (vector->list (cadr (gimp-image-get-layers img)))))
    (if (null? layers) #f
        (let ((mask (car (gimp-layer-get-mask (car layers)))))
          (if (not (= mask -1)) #t
              (loop (cdr layers)))))))

(define (elp-apply-masks img)
  (elp-vector-for-each 
   (lambda (layer)
     (let ((mask (car (gimp-layer-get-mask layer))))
       (if (not (= mask -1)) (gimp-layer-remove-mask layer MASK-APPLY))))
   (cadr (gimp-image-get-layers img))))


(define (elp-get-delay layer)
  (let* ((buffer (make-vector 2))
         (layer-name (car (gimp-item-get-name layer))))
    (if (re-match "\\((\\d+)ms\\)" layer-name buffer)
        (let* ((boundaries (vector-ref buffer 1))
               (ldelay (string->number (substring layer-name (car boundaries) (cdr boundaries)))))
          (if (= ldelay 0) *elp-default-frame-rate* ldelay))
        *elp-default-frame-rate*)))

(define (elp-get-timeline img walk-direction layer-filter)
  (let ((timeline '()))
    (elp-walk-layers-filtered
     walk-direction img layer-filter
     (lambda (count layer)
       (set! timeline (cons (cons layer (elp-get-delay layer)) timeline))))
    (reverse timeline)))


(define (elp-process-timeline timeline frame-rate)
  (let* ((make-empty-frame 
          (lambda () (list frame-rate)))
         (fill-frame
          (lambda (frame tll)
            "return #f if frame is not filled, #t if filled"
            (let ((remaining (car frame))
                  (available (cdr tll)))
              (cond ((= remaining 0) #t)
                    ((>= available remaining)
                     (set-car! frame 0)
                     (set-cdr! frame (cons (cons (car tll) remaining) (cdr frame))) 
                     (set-cdr! tll (- available remaining))
                     #t)
                    (else
                     (set-car! frame (- remaining available))
                     (set-cdr! frame (cons (cons (car tll) available) (cdr frame)))
                     (set-cdr! tll 0)
                     #f)))))
         (reverse-frame
          (lambda (frame)
            (set-cdr! frame (reverse (cdr frame)))))
         )
    (let loop ((tl timeline)
               (ptl '())
               (curframe (make-empty-frame)))
      (if (pair? tl)
          (if (fill-frame curframe (car tl))
              (loop (if (= (cdar tl) 0) (cdr tl) tl) 
                    (cons (reverse-frame curframe) ptl)
                    (make-empty-frame))
              (loop (cdr tl) ptl curframe))
          (reverse 
           (if (pair? (cdr curframe))
               (cons curframe ptl)
               ptl))))))

(define (elp-select-best-layer frame)
  (let loop ((choices (cdr frame))
             (max-delay 0)
             (best '()))
    (if (pair? choices)
        (let ((cur-delay (cdar choices)))
          (cond ((> cur-delay max-delay)
                 (loop (cdr choices) cur-delay (list (car choices))))
                ((= cur-delay max-delay)
                 (loop (cdr choices) max-delay (cons (car choices) best)))
                (else
                 (loop (cdr choices) max-delay best))))
        (car (list-ref best (quotient (length best) 2))))))


(define (elp-interpolate-layers img layer1 layer2 w1 w2)
  "Uses a more precise algorithm than bgmask average layers to deal with transparency

If pair (x, a) represents a pixel with color x and opacity a, the correct interpolation would be:
 (x, a) ~ (y, b) = ((ax+by)/(a+b), (a+b)/2) 

However this algorithm (and bgmask's algorithm) do NOT perform correct interpolation because it seems
too hard to emulate the necessary arithmetic via layer modes.

bgmask's is pretty much only correct when a=b=1

This algorithm is correct when a=b and when a=0, b=1 and a=1, b=0. It always calculates opacity correctly,
and the colors are approximately correct, aside from the abovementioned cases when it's exactly correct.

The formula for this algorithm is:

 (x, a) ~ (y, b) = ( (a+(1-b))/2 * x + (b+(1-a))/2 * y, (a+b)/2)

Weights w1 and w2 are used to calculate wfactor = w1/(w1+w2) which is used instead of 50% opacity
to calculate averages.
"
  (gimp-item-set-visible layer1 TRUE)
  (gimp-item-set-visible layer2 TRUE)
  (let ((wfactor (if (= w1 w2) 50
                     (* (/ w1 (+ w1 w2)) 100)))
        (l1copy (car (gimp-layer-copy layer1 TRUE)))
        (l2copy (car (gimp-layer-copy layer2 TRUE)))
        (new-from-alpha 
         (lambda (layer)
           (let ((newmask (car (gimp-layer-create-mask layer ADD-ALPHA-TRANSFER-MASK)))
                 (masklayer (car (gimp-layer-copy layer TRUE))))
             (gimp-layer-add-mask layer newmask)
             (gimp-image-select-item img 2 newmask)
             (gimp-selection-invert img)
             (gimp-drawable-fill masklayer WHITE-FILL)
             (gimp-image-insert-layer img masklayer 0 0)
             (gimp-context-push)
             (gimp-context-set-foreground '(0 0 0))
             (if (elp-is-true? gimp-selection-bounds img)
                 (gimp-edit-fill masklayer 0))
             (gimp-context-pop)
             (gimp-selection-none img)
             (gimp-layer-remove-mask layer MASK-DISCARD)
             masklayer))))
             
    (gimp-image-insert-layer img l2copy 0 0)
    (gimp-image-insert-layer img l1copy 0 0)
    (let* ((ml2 (new-from-alpha l2copy))
           (ml1 (new-from-alpha l1copy))
           (ml2c (car (gimp-layer-copy ml2 TRUE)))
           (ml1c (car (gimp-layer-copy ml1 TRUE))))
      ;; ml1 > ml2 > l1 > l2
      (gimp-image-insert-layer img ml2c 0 0)
      (gimp-image-insert-layer img ml1c 0 0)
      ;; ml1c > ml2c > ml1 > ml2 > l1 > l2
      (gimp-invert ml2c)
      (gimp-layer-set-opacity ml1c wfactor)
      (gimp-layer-set-opacity ml1 wfactor)
      (let* ((colorlayer (car (gimp-image-merge-down img ml1c 0)))
             (colormask (car (gimp-layer-create-mask colorlayer ADD-COPY-MASK)))
             (opacitylayer (car (gimp-image-merge-down img ml1 0)))
             (opacitymask (car (gimp-layer-create-mask opacitylayer ADD-COPY-MASK))))
        (gimp-layer-add-mask colorlayer colormask)
        (gimp-layer-add-mask opacitylayer opacitymask)
        (gimp-image-select-item img 2 colormask)
        (let ((mask (car (gimp-layer-create-mask l1copy ADD-SELECTION-MASK))))
          (gimp-layer-add-mask l1copy mask))
        (gimp-image-select-item img 2 opacitymask)
        (let* ((final (car (gimp-image-merge-down img l1copy 0)))
               (finalmask (car (gimp-layer-create-mask final ADD-SELECTION-MASK))))
          (gimp-layer-add-mask final finalmask)
          (gimp-layer-remove-mask final MASK-APPLY)
          (gimp-image-remove-layer img colorlayer)
          (gimp-image-remove-layer img opacitylayer)
          (gimp-selection-none img)
          final)))))


(define (elp-resample-frame img frame resample-threshold)
  ;; only resample when 2 layers
  (if (or (not (= (length (cdr frame)) 2))
          (< (cdadr frame) resample-threshold)
          (< (cdaddr frame) resample-threshold))
      (elp-select-best-layer frame)
      (elp-interpolate-layers img (caadr frame) (caaddr frame) (cdadr frame) (cdaddr frame))))

(define (elp-resampling-index img timeline resample-mode frame-rate resample-threshold)
  (let ((ptl (elp-process-timeline timeline frame-rate)))
    (cond ((= resample-mode 1)
           (map elp-select-best-layer ptl))
          ((= resample-mode 2)
           (map (lambda (frame) (elp-resample-frame img frame resample-threshold)) ptl)))))

(define (script-fu-export-layers-plus img path filename-template
                                      walk-direction count-offset
                                      layer-filter 
                                      resample-mode frame-rate
                                      resample-threshold
                                      )
  (let* ((img-name (car (gimp-image-get-name img)))
         (do-simple-export 
          (lambda (img)
            (elp-simple-export img img-name path filename-template walk-direction count-offset layer-filter)))
         (simple (not (or (not (= resample-mode 0)) (elp-has-layer-masks img)))))
    (if simple
        (do-simple-export img)
        (let* ((timg (elp-image-copy img))
               (tempimgs (list timg)))
          (elp-apply-masks timg)
          (if (= resample-mode 0) 
              (do-simple-export timg)
              (let* ((timeline (elp-get-timeline timg walk-direction layer-filter))
                     (index (elp-resampling-index timg timeline resample-mode frame-rate resample-threshold)))
                (elp-index-export img img-name path filename-template count-offset index)))
          (for-each gimp-image-delete tempimgs)))))



(script-fu-register
  "script-fu-export-layers-plus"
  "Export Layers..."
  "Export layers as separate images"
  "Timofei Shatrov"
  "2013"
  "2013"
  "*"
  SF-IMAGE       "Image"         0
  SF-DIRNAME     "Output directory"  ""
  SF-STRING      "Filename format\
%n - image name\
%l - layer name\
%i - number of layer
%6i - padded to 6 digits
%% = %" "%n_%6i.png"
  SF-OPTION "Walk direction" '("Bottom to top" "Top to bottom")
  SF-ADJUSTMENT "Count offset" '(0 0 999999 1 10 0 1)
  SF-OPTION "Filter" '("All layers" "Visible layers" "Linked layers")
  SF-OPTION "Resample mode" '("Off" "No interpolation" "Use interpolation")
  SF-ADJUSTMENT "Frame rate (ms)" '(40 1 999999 1 10 0 1)
  SF-ADJUSTMENT "Interpolation threshold (ms)" '(0 0 999999 1 10 0 1)
)

(script-fu-menu-register "script-fu-export-layers-plus" "<Image>/File/E_xport")
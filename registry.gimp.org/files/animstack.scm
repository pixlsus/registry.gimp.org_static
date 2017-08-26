;;; GIMP Animation Tools
;;; by Timofei Shatrov
;;; v. 0.61

(define (display-to-string value)
  "Prints anything to string using display function"
  (let ((port (open-output-string)))
    (display value port)
    ;; Fun fact: get-output-string is not mentioned once in tinyscheme docs...
    (get-output-string port)))

(define (gimp-message* . args)
  (let ((port (open-output-string)))
    (for-each
     (lambda (arg)
       (display arg port)
       (display " " port))
     args)
    (gimp-message (get-output-string port))))

(define (vector-for-each fn vec)
  "Run fn on each element of vec"
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len))
    (fn (vector-ref vec i))))

(define (vector-for-each-i fn vec)
  "Run fn on each element of vec"
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len))
    (fn (vector-ref vec i) i)))

(define (is-true? fn item)
  ;; does fn return '(TRUE) ?
  (= (car (fn item)) TRUE))

(define (int-round x)
  (inexact->exact (round x)))

(define *default-animstack-hash-size* 200)

(define (animstack-hash size hashfn)
  ;;based on http://www.math.grin.edu/~stone/events/scheme-workshop/hash-tables.html
  (let* ((table (make-vector size '()))
         (add (lambda (key . value)
                (let ((index (hashfn key)))
                  (vector-set! table index
                               (cons (cons key value) (vector-ref table index))))))
         (init (lambda (assoc-list)
                 (for-each (lambda (pair) (apply add pair)) assoc-list)))
         (get-assoc (lambda (key)
                      (let ((index (hashfn key)))
                        (assoc key (vector-ref table index))))))
    (lambda (op . args)
      (case op
        ((add) (apply add args))
        ((init) (apply init args))
        ((assoc) (apply get-assoc args))
        ((contents) table)
        ((stat)
         (let ((collisions 0)
               (filled 0))
           (vector-for-each
            (lambda (cell)
              (if (pair? cell)
                  (begin
                    (set! filled (+ filled 1))
                    (if (> (length cell) 1) (set! collisions (+ collisions 1))))))
            table)
           `(("Size:" ,size) ("Filled:" ,filled) ("Collisions:" ,collisions))))
        (else (error (string-append "Unsupported hash operation" (symbol->string op))))))))

(define (animstack-hashfn size)
  (let* ((seed (random size))
         (sqsize (int-round (sqrt size)))
         (hashfn
          (lambda (obj)
            "It's terrible, but quick..."
            (cond
             ((integer? obj) obj)
             ((char? obj) (char->integer obj))
             ((string? obj)
              (let ((sl (string-length obj)))
                (+ sl
                   (* sqsize (if (> sl 0) (hashfn (string-ref obj 0)) seed))
                   (if (> sl 1) (hashfn (string-ref obj 1)) seed)
                   )))
             ((symbol? obj)
              (hashfn (symbol->string obj)))
             ((else (hashfn (display-to-string obj))))))))
    (lambda (obj) (modulo (hashfn obj) size))))
                 
(define (make-animstack-hash assoc-list)
  (let* ((size *default-animstack-hash-size*)
         (ah (animstack-hash size (animstack-hashfn size))))
    (ah 'init assoc-list)
    ah))

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
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE     "Image to use"       0
 )

(script-fu-menu-register "script-fu-flatten-layer-groups" "<Image>/Image")

;;; Animation stacker

;;; (gimp-image-reorder-item image item parent position)
;;; (gimp-image-insert-layer image layer parent position)

(define (reorder-or-insert-layer image item parent position)
  "because you never know..."
  (let ((args (list image item parent position)))
    (catch ;; actually GIMP still displays the error. it doesn't die, but might scare the user
     (catch #f (apply gimp-image-insert-layer args)) 
     (apply gimp-image-reorder-item args))))

(define (groupify-layer img layer)
  "If layer is not a layer group, make a layer group containing only layer"
  (if (not (is-true? gimp-item-is-group layer))
      (let ((group (car (gimp-layer-group-new img)))
            (layer-name (car (gimp-item-get-name layer)))
            (default-name-function ;; potentially customizable?
              (lambda (str) (string-append "+ " str))))
        (gimp-image-set-active-layer img layer)
        (gimp-item-set-name group (default-name-function layer-name))
        (gimp-image-insert-layer img group 0 -1)
        (gimp-image-reorder-item img layer group 0)
        (animstack-copy-layer-labels layer group)
        group)
      layer))

(define (put-layer-in-group img layer parent position . rest)
  "Position might be a positive (counted from the top) or negative (from the bottom)"
  (let* ((group (groupify-layer img parent))
         (group-length (car (gimp-item-get-children group)))
         (pos (if (< position 0)
                  (max 0 (+ group-length position 1))
                  (min group-length position)))
         (fn (if (null? rest) reorder-or-insert-layer (car rest))))
    (fn img layer group pos)
    group))


(define (get-layer-in-group group position)
  (if (is-true? gimp-item-is-group group)
      (let* ((group-children (gimp-item-get-children group))
             (group-length (car group-children))
             (pos (if (< position 0)
                      (max 0 (+ group-length position))
                      (min (- group-length 1) position))))
        (vector-ref (cadr group-children) pos))
      group))


(define (string2number str . opt)
  "Replacement for string->number, which throws an uncatchable
exception as of GIMP 2.8. Returns #f if not a number."
  (let ((s2a (string->atom str))
        (fn (if (pair? opt) (car opt) number?)))
    (and (fn s2a) s2a)))


(define animstack-save-selection #f)
(define animstack-restore-selection #f)

(let ((sel #f))
  (set! animstack-save-selection
        (lambda (img)
          (set! sel #f)
          (if (is-true? gimp-selection-bounds img)
              (set! sel (car (gimp-selection-save img))))))
  (set! animstack-restore-selection
        (lambda (img)
          (if sel
              (begin
                (gimp-selection-load sel)
                (gimp-image-remove-channel img sel))
              (gimp-selection-none img)))))

;; Tag syntax is [tag] or [tag:parameter] or [tag:param1:param2:...]
;; All parameters should be integers

(define (string-split string char)
  (let ((res (list)))
    (do ((i (string-length string) (- i 1))
         (chunk (list))
         (new (lambda (chunk) (cons (list->string chunk) res))))
        ((<= i 0) (new chunk))
      (let ((chr (string-ref string (- i 1))))
        (if (char=? chr char)
            (begin (set! res (new chunk)) (set! chunk (list)))
            (set! chunk (cons chr chunk)))))))
         
(define (parse-tag-param-simple str)
  (let ((valid-param (lambda (x) (or (number? x) (symbol? x)))))
    (string2number str valid-param)))

(define (parse-tag-param* str)
  (let ((valid-param (lambda (x) (or (number? x) (symbol? x)))))
    (if (= (string-length str) 0) 
        #f
        (or (string2number str valid-param) 'err))))

(define (parse-tag-param str)
  (let* ((ari (parse-generator-defn str))
         (rest (parse-tag-param* (caddr ari))))
    (cond ((or (eqv? rest 'err)
               (and (= (car ari) 0) (= (cadr ari) 1))) rest)
          (else (list (car ari) (cadr ari) rest)))))

(define (parse-animstack-tag string)
  "Parse string beginning with [tag]. Returns a pair ( tag . index )
where tag might be #f"
  (let* ((split1 (string-split string #\]))
         (tagstr (car split1)))
    (if (> (length split1) 1)
        ;;this might be legitimate tag
        (let* ((split2 (string-split tagstr #\:))
               (tagname (substring (car split2) 1 (string-length (car split2))))
               (params (map (if (= (string-length tagname) 0) 
                                parse-tag-param-simple ;;don't do ari stuff on label tags
                                parse-tag-param)
                            (cdr split2)))
               (pos (+ (string-length tagstr) 1)))
          (if (memv 'err params)
              (cons #f pos)
              (cons (cons tagname params) pos)))
        ;;cant find closing bracket...
        (cons #f (string-length string)))))

(define (extract-animstack-tags layer . params)
  "Returns list of animstack tags in the form (tag . parameters)"
  (let loop ((layer-name-list (string->list (car (gimp-item-get-name layer)))))
    (let ((tagtail (memv #\[ layer-name-list)))
      (if tagtail
          (let* ((parsed (parse-animstack-tag (list->string tagtail)))
                 (tag (car parsed))
                 (next (list-tail tagtail (cdr parsed))))
            (if (and tag (or (null? params) ((car params) tag)))
                (cons tag (loop next))
                (loop next)))
          (list)))))

(define (strip-tags string . params)
  "Removes all tags from string"
  (list->string
   (let loop ((str-list (string->list string)))
     (cond ((null? str-list) (list))
           ((char=? (car str-list) #\[)
            (let ((rest (memv #\] str-list)))
              (if (and rest
                       (let ((tag (car (parse-animstack-tag (list->string str-list)))))
                         (and tag (or (null? params) ((car params) tag)))))
                  (loop (cdr rest))
                  (cons (car str-list) (loop (cdr str-list))) 
                  )))
           (else (cons (car str-list) (loop (cdr str-list))))))))

(define (is-untagged? layer)
  (null? (extract-animstack-tags layer)))

(define (pop-params n params)
  (let ((pv (make-vector n #f)))
    (do ((i 0 (+ i 1))
         (pl params (cdr pl)))
        ((or (null? pl) (>= i n)) pv)
      (vector-set! pv i (car pl)))))

(define (process-param-list param-list)
  (let ((count 0))
    (map 
     (lambda (param-def) 
       (prog1 
        (if (pair? param-def)
            `(,(car param-def) (or (vector-ref pv ,count) (begin ,@(cdr param-def))))
            `(,param-def (vector-ref pv ,count)))
        (set! count (+ count 1))))
     param-list)))

;; (with-params (x y z) ....)
(macro (with-params form)
  (let ((param-list (cadr form))
        (body (cddr form)))
    `(let* ((pv (pop-params ,(length param-list) params))
            ,@(process-param-list param-list))
       ,@body)))

;; fun fact: this is the single most important function in AnimStack
(define (layer-walk get-layer start next test action limit ignore-first terminate-label)
  (let loop ((i start)
             (left limit)
             (terminate #f))
    (let ((layer (get-layer i)))
      (cond
       ((or (not layer) terminate (and left (<= left 0))) #f)
       (else
        (if (and terminate-label (animstack-layer-has-label layer terminate-label))
            (set! terminate #t))
        (cond ((and ignore-first (= i start)))
              ((not (test layer)))
              (else
               (action layer)
               (if left (set! left (- left 1)))))
        (loop (next i) left terminate))))))

(define (layer-getter layers)
  (let ((maxlen (vector-length layers)))
    (lambda (pos)
      (and (< -1 pos maxlen) (vector-ref layers pos)))))
 
(define (default-copy-name item . prefixargs)
  (let ((prefix (if (pair? prefixargs) (car prefixargs) "* ")))
    (string-append prefix (strip-tags (car (gimp-item-get-name item))))))

(define (get-bindings bindings)
  (map (lambda (g) (list (car g) ((cadr g)))) bindings))

(define (apply-effects-simple img effects . rest)
  (lambda (layer target)
    (for-each (lambda (effect) (apply effect img layer target rest)) effects)))

(define (copy-action img source pos opts)
  "Copy source layer and put it into target group at position pos"
  (let ((copy-name (default-copy-name source))) 
    (lambda (target)
      (let* ((bindings (get-bindings (cadr opts)))
             (apply-effects (apply-effects-simple img (list-ref opts 3)
                                                  bindings #f)))
        (if (caar opts) (apply-effects source target))
        (let ((new (car (gimp-layer-copy source FALSE))))
          (gimp-item-set-name new copy-name)
          (set! target (put-layer-in-group img new target pos gimp-image-insert-layer))
          (process-dup-options* opts img bindings target (dup-getter img new))
          (if (not (caar opts)) (apply-effects new target)))))))

(define (animstack-next-fn delta opts)
  (let* ((reverse (cadar opts))
         (delta (if reverse (- delta) delta)))
    (lambda (i) (+ i delta))))

(define (animstack-common delta pos actionfn)
  (lambda (img layer opts . params)
    (let* ((limit (if (null? params) #f (car params)))
           (start (get-start-position img layer opts))
           (layers (cadr (gimp-image-get-layers img)))
           (getter (layer-getter layers))
           (next (animstack-next-fn delta opts))
           (action (actionfn img layer pos opts))
           (tl (list-ref (car opts) 2)))
      (if (and limit (<= limit 0)) (set! limit #f))
      (for-each (lambda (effect) (effect img layer #f (list) #f)) (list-ref opts 2))
      (layer-walk getter start next is-untagged? action limit #t tl))
    (gimp-image-remove-layer img layer)
    ))

(define animstack-bg (animstack-common -1 -1 copy-action))

(define animstack-fg (animstack-common 1 0 copy-action))

(define (animstack-copy img layer opts . params)
  (with-params 
   ((pos -1) limit)
   (let* ((fn (animstack-common -1 pos copy-action)))
     (fn img layer opts limit))))

(define (noop-action img source pos opts)
  "do nothing with source layer, but do execute all the actions"
  (lambda (target)
    (let* ((bindings (get-bindings (cadr opts)))
           (apply-effects (apply-effects-simple img (list-ref opts 3)
                                                bindings #f)))
      (apply-effects source target)
      (process-dup-options* opts img bindings target (lambda (tgt) source)))))

(define animstack-noop (animstack-common -1 -1 noop-action))

(define (get-roll-layer-list layer)
  (if (is-true? gimp-item-is-group layer)
      (vector->list (cadr (gimp-item-get-children layer)))
      (list layer)))


(define animstack-reset-count #f)
(define animstack-init-count #f)
(define animstack-inc-count #f)
(define animstack-get-count #f)
(define animstack-set-repeat-value #f)

(let* ((count 0)
       (repeat-count 0)
       (repeat-value 1))
  (set! animstack-reset-count
        (lambda () (set! repeat-value 1) (set! repeat-count 0)))
  (set! animstack-init-count 
        (lambda (n) (set! count (int-round n)) (set! repeat-count 0)))
  (set! animstack-set-repeat-value
        (lambda (n) (if (= repeat-count 0) (set! repeat-value (int-round n)))))
  (set! animstack-inc-count
        (lambda ()
          (cond ((> repeat-value 0)
                 (set! repeat-count (+ repeat-count 1))
                 (if (>= repeat-count repeat-value)
                     (begin (animstack-reset-count) (set! count (+ count 1)))))
                ((= repeat-value 0) (animstack-reset-count))
                ((< repeat-value 0) (set! count (+ count repeat-value)) (animstack-reset-count)))
          count))
  (set! animstack-get-count (lambda () count)))


(define (roll-action img source pos opts roll-offset)
  (let* ((roll-list (get-roll-layer-list source))
         (roll-length (length roll-list))
         (count roll-offset))
    (if (= roll-length 0) (error "Empty roll"))
    (if (< count 0) (set! count (random roll-length)))
    (animstack-init-count count)
    (lambda (target)
      (let* ((pick (- roll-length 1 (modulo (animstack-get-count) roll-length)))
             (layer (list-ref roll-list pick))
             (bindings (get-bindings (cadr opts)))
             (apply-effects (lambda (layer src)
                              (for-each (lambda (effect)
                                          (effect img layer target bindings src))
                                        (list-ref opts 3)))))
        (if (caar opts) (apply-effects layer source))
        (let ((new (car (gimp-layer-copy layer FALSE))))
          (gimp-item-set-name new (default-copy-name new))
          (set! target (put-layer-in-group img new target pos gimp-image-insert-layer))
          (process-dup-options* opts img bindings target (dup-getter img new))
          (if (not (caar opts)) (apply-effects new #f))
          (animstack-inc-count))))))

(define (animstack-roll img layer opts . params)
  "Rolls a layer stack or a single layer up the layer list in a given position"
  (with-params 
   ((pos -1) limit (roll-offset 0))
   (let* ((start (get-start-position img layer opts))
          (layers (cadr (gimp-image-get-layers img)))
          (getter (layer-getter layers))
          (next (animstack-next-fn -1 opts))
          (action (roll-action img layer pos opts roll-offset))
          (tl (list-ref (car opts) 2)))
     (if (and limit (<= limit 0)) (set! limit #f))
     (animstack-reset-count)
     (for-each (lambda (effect) (effect img layer #f (list) layer)) (list-ref opts 2))
     (layer-walk getter start next is-untagged? action limit #t tl)))
  (gimp-image-remove-layer img layer))

(define (animstack-splice img layer opts . params)
  (with-params 
   ((pos -1) (roll-offset 0))
   (let* ((limit (length (get-roll-layer-list layer))))
     (animstack-roll img layer opts pos limit roll-offset))))

(define (check-all lst test)
  (let loop ((l lst))
    (cond ((null? l) #t)
          ((not (test (car l))) #f)
          (else (loop (cdr l))))))

;; matte action

(define (layer-matte-cutout img layer bg-layer threshold)
  (animstack-save-selection img)
  (if (is-true? gimp-selection-bounds img)
      (set! sel (car (gimp-selection-save img))))
  (gimp-item-set-visible bg-layer FALSE)
  (gimp-image-select-item img CHANNEL-OP-REPLACE layer) 
  (gimp-threshold (car (gimp-image-get-selection img)) 0 threshold)
  (gimp-layer-add-alpha bg-layer)
  (gimp-edit-clear bg-layer)
  (gimp-item-set-visible bg-layer TRUE)
  (animstack-restore-selection img))

;;TODO: fix terrible copy pasting of copy-action
(define (matte-action threshold img source pos opts)
  (let ((copy-name (default-copy-name source))) 
    (lambda (target)
      (let* ((bindings (get-bindings (cadr opts)))
             (apply-effects (apply-effects-simple img (list-ref opts 3)
                                                  bindings #f)))
        (if (caar opts) (apply-effects source target))
        (let ((new (car (gimp-layer-copy source FALSE))))
          (gimp-item-set-name new copy-name)
          (set! target (put-layer-in-group img new target pos gimp-image-insert-layer))
          (process-dup-options* opts img bindings target (dup-getter img new))
          (if (not (caar opts)) (apply-effects new target))
          (layer-matte-cutout img target new threshold))))))

(define (animstack-matte img layer opts . params)
  (with-params
   ((threshold 1) limit)
   (let* ((threshold (max threshold 0))
          (fn (animstack-common -1 -1 
                                (lambda args (apply matte-action threshold args)))))
     (fn img layer opts limit))))
 
;; delete 
(define (delete-action-factory step width)
  (lambda (img source pos opts)
    (let ((count 0))
      (lambda (target)
        (let* ((bindings (get-bindings (cadr opts)))
               (apply-effects (apply-effects-simple img (list-ref opts 3)
                                                    bindings #f)))
          (apply-effects source target)
          (process-dup-options* opts img bindings target (lambda (tgt) source))
          (if (< (modulo count step) width)
              (gimp-image-remove-layer img target))
          (set! count (+ count 1)))))))

(define (animstack-delete img layer opts . params)
  (let* ((pv (pop-params 3 params))
         (step (max (or (vector-ref pv 0) 2) 1))
         (width (max (or (vector-ref pv 1) (- step 1)) 0))
         (limit (vector-ref pv 2))
         (action (delete-action-factory step width))
         (fn (animstack-common -1 -1 action)))
    (fn img layer opts limit)))

(define (render-layer-group img group interval)
  "Like flatten-layer-group, but returns a new layer instead of merging"
  (let ((name (default-copy-name group "R "))
        (layers (cadr (gimp-image-get-layers img)))
        (result #f)
        (changed '()))
    (vector-for-each
     (lambda (lr) (gimp-item-set-visible lr (if (= lr group) TRUE FALSE)))
     layers)
    (if interval
        (vector-for-each-i
         (lambda (lr i)
           (if (not (<= (car interval) i (cdr interval)))
               (begin
                 (set! changed (cons (cons lr (car (gimp-item-get-visible lr))) changed))
                 (gimp-item-set-visible lr FALSE))))
         (cadr (gimp-item-get-children group))))
    (set! result (car (gimp-layer-new-from-visible img img name)))
    (if (pair? changed)
        (for-each (lambda (lv) (gimp-item-set-visible (car lv) (cdr lv))) changed))
    (vector-for-each
     (lambda (lr) (gimp-item-set-visible lr TRUE))
     layers)
    result))

(define (animstack-get-interval group only interval)
  (if (is-true? gimp-item-is-group group)
      (let* ((nlayers (car (gimp-item-get-children group)))
             (onlypos (if (< only 0)
                          (max 0 (+ nlayers only))
                          (min (- nlayers 1) only))))
        (if (< interval 0)
            (cons (max 0 (+ onlypos interval)) onlypos)
            (cons onlypos (min (- nlayers 1) (+ onlypos interval)))))
      #f))

(define (render-action replace under only interval img source pos opts)
  (lambda (target)
    (let* ((bindings (get-bindings (cadr opts)))
           (apply-effects (apply-effects-simple img (list-ref opts 3)
                                                bindings #f))
           (interval (if only (animstack-get-interval target only interval) #f))
           (realpos (if interval (car interval) 0))
           (shift (if under 0 1))
           (new (render-layer-group img target interval)))
      (if under
          (if interval
              (set! realpos (+ (cdr interval) 1))
              (set! realpos -1)))
      (set! target (put-layer-in-group img new target realpos gimp-image-insert-layer))
      (process-dup-options* opts img bindings target (dup-getter img new))
      (if replace
          (let ((test (if interval 
                          (lambda (i) (<= (+ (car interval) shift) i (+ (cdr interval) shift)))
                          (lambda (i) (not (= i 0))))))
            (vector-for-each-i
             (lambda (layer i) (if (test i) (gimp-image-remove-layer img layer)))
             (cadr (gimp-item-get-children target)))))
      (apply-effects new target) ;; render is always non-cumulative
      )))

;; [render:limit:replace:only:interval]
(define (animstack-render img layer opts . params)
  (with-params
   (limit replace only (interval 0))
   (let* ((rep (and replace (> replace 0)))
          (under (and replace (< replace 0)))
          (action (lambda args (apply render-action rep under only interval args)))
          (fn (animstack-common -1 0 action)))
     (fn img layer opts limit))))

(define (animstack-sample-to-target img source target x y width height)
  (animstack-save-selection img)
  (gimp-selection-none img)
  (gimp-edit-clear target)
  (gimp-image-select-rectangle img CHANNEL-OP-REPLACE x y width height)
  (if (is-true? gimp-edit-copy source)
      (let ((fl (car (gimp-edit-paste target FALSE)))
            (offsets (gimp-drawable-offsets target))
            (new-width (car (gimp-drawable-width target)))
            (new-height (car (gimp-drawable-height target))))
        ;;(gimp-context-set-interpolation INTERPOLATION-CUBIC)
        (gimp-layer-scale fl new-width new-height TRUE)
        (gimp-layer-set-offsets fl (car offsets) (cadr offsets))
        (gimp-floating-sel-anchor fl)))
  (animstack-restore-selection img))
        
(define (animstack-linear-transition count coords1 coords2)
  (if (> count 0)
      (let* ((avg (lambda (i n)
                   (let ((c1 (list-ref coords1 i))
                         (c2 (list-ref coords2 i)))
                     (+ c1 (* (/ n count) (- c2 c1))))))
             (di (lambda (i)
                   (let ((c1 (list-ref coords1 i))
                         (c2 (list-ref coords2 i)))
                     (/ (- c2 c1) count))))
             (dx (di 0))
             (dy (di 1))
             (dzx (di 2))
             (dzy (di 3)))
        (lambda (n)
          (let* ((newcoords (if (= n count) 
                                coords2
                                (let loop ((i 0))
                                  (if (< i 4) (cons (avg i n) (loop (+ i 1)))))))
                 (motion (if (= n count) 
                             (list 0 0 0 0 #t)
                             (let ((cw (list-ref newcoords 2))
                                   (ch (list-ref newcoords 3)))
                               (list (/ (+ dx (/ dzx 2)) cw) 
                                     (/ (+ dy (/ dzy 2)) ch) 
                                     (/ dzx (- cw dzx))
                                     (/ dzy (- ch dzy))
                                     #f)))))
            (list newcoords motion))))))

(define (animstack-linear-path count nodes)
  (let* ((segment-map (make-vector count))
         (m (length nodes))
         (node-map (make-vector m))
         (first-val (list (car nodes) (list 0 0 0 0 #t))))
    (cond
     ((< count m) (error (string-append "Too many nodes: "
                                        (number->string m) 
                                        ", no more than count=" 
                                        (number->string count)
                                        " allowed."))) 
     ((or (= m 1) (= count 1)) (lambda (n) first-val))
     (else
      (do ((i 0 (+ i 1))) ((>= i (- m 1)))
        (vector-set! node-map i (round (* (/ (+ i 1) (- m 1)) (- count 1)))))
      (vector-set! segment-map 0 first-val)
      (let ((prev 0))
        (vector-for-each-i
         (lambda (next i)
           (if (< i (- m 1))
               (let* ((curcoords (list-ref nodes i))
                      (nextcoords (list-ref nodes (+ i 1)))
                      (fn (animstack-linear-transition (- next prev) curcoords nextcoords)))
                 (do ((j (+ prev 1) (+ j 1))) ((> j next))
                   (vector-set! segment-map j (fn (- j prev))))))
           (set! prev next))
         node-map))
      (lambda (n) (vector-ref segment-map n))))))

(define (get-layer-coords layer)
  (let ((offsets (gimp-drawable-offsets layer))
        (width (car (gimp-drawable-width layer)))
        (height (car (gimp-drawable-height layer))))
    (list (car offsets) (cadr offsets) width height)))

(define (get-toplevel-parent layer)
  (let ((parent (car (gimp-item-get-parent layer))))
    (if (= parent -1)
        layer
        (get-toplevel-parent parent))))

(define (sampler-count-frames-above img layer opts)
  (let* ((reverse (cadar opts))
         (tl (list-ref (car opts) 2))
         (count 0)
         (toplevel (get-toplevel-parent layer))
         (pos (car (gimp-image-get-item-position img toplevel)))
         (layers (cadr (gimp-image-get-layers img)))
         (last (- (vector-length layers) 1))
         (terminate #f))
    (do ((i pos (+ i (if reverse 1 -1))))
        ((or terminate (< i 0) (> i last)) count)
      (let* ((layer (vector-ref layers i)))
        (if (is-untagged? layer)
            (set! count (+ count 1)))
        (if (and tl (animstack-layer-has-label layer tl))
            (set! terminate #t))))))

(define (make-temp-sampler-layer img group width height)
  (let ((layer (car (gimp-layer-new img width height RGBA-IMAGE
                                    "Sample layer"
                                    100 NORMAL-MODE))))
    (gimp-image-insert-layer img layer group 0)
    (gimp-layer-set-offsets layer 0 0)
    layer))

(define animstack-get-motion #f)
(define animstack-reset-motion #f)
(define animstack-set-motion #f)
(let ((motion (list 0 0 0 0 #f)))
  (set! animstack-get-motion (lambda () motion))
  (set! animstack-reset-motion (lambda () (set! motion (list 0 0 0 0 #f))))
  (set! animstack-set-motion (lambda (new-motion) (set! motion new-motion))))

(define (sampler-action img temp-layer source path 
                            pos opts roll-mode)
  (let* ((roll-list (if (>= roll-mode 0) 
                        (get-roll-layer-list source)
                        (list source)))
         (roll-length (length roll-list))
         (roll-count (if (>= roll-mode 0) roll-mode 0))
         (sampler-count 0))
    (if (= roll-length 0) (error "Empty roll"))
    (animstack-init-count roll-count)
    (lambda (target)
      (let* ((pick (- roll-length 1 (modulo (animstack-get-count) roll-length)))
             (src (list-ref roll-list pick))
             (pt (path sampler-count))
             (bindings (get-bindings (cadr opts)))
             (apply-effects (apply-effects-simple img (list-ref opts 3)
                                                  bindings #f)))
        (apply animstack-sample-to-target img src temp-layer (car pt))
        (animstack-set-motion (cadr pt))
        (if (caar opts) (apply-effects temp-layer target))
        (let ((new (car (gimp-layer-copy temp-layer FALSE))))
          (gimp-item-set-name new (default-copy-name new))
          (set! target (put-layer-in-group img new target pos gimp-image-insert-layer))
          (process-dup-options* opts img bindings target (dup-getter img new))
          (if (not (caar opts)) (apply-effects new target))
          (animstack-inc-count)
          (set! sampler-count (+ sampler-count 1)))))))

(define animstack-remember-image-size #f)
(define animstack-restore-image-size #f)

(let ((width #f)
      (height #f))
  (set! animstack-remember-image-size
        (lambda (img)
          (set! width (car (gimp-image-width img)))
          (set! height (car (gimp-image-height img)))))
  (set! animstack-restore-image-size
        (lambda (img)
          (gimp-image-resize img width height 0 0))))

;; [sampler:pos:count:limit:roll-mode:width:height]
(define (animstack-sampler img layer opts . params)
  (with-params
   ((pos -1) count limit (roll-mode -1)
    (width (car (gimp-image-width img)))
    (height (car (gimp-image-height img))))
   (let* ((start (get-start-position img layer opts))
          (layers (cadr (gimp-image-get-layers img)))
          (getter (layer-getter layers))
          (next (animstack-next-fn -1 opts))
          )
     (if (or (not count) (= count 0)) (set! count (max (sampler-count-frames-above img layer opts) 1)))
     (let* ((group (groupify-layer img layer))
            (temp-layer #f)
            (srcs (gimp-item-get-children group))
            (src-count (car srcs))
            (srcs (cadr srcs))
            (source #f)
            (node-layers #f))
       (if (= src-count 0) (error "Empty sampler"))
       (set! source (vector-ref srcs (- src-count 1)))
       
       (if (< count 0) (set! count (length (get-roll-layer-list source))))
       (if (or (not limit) (<= limit 0) (> limit count)) (set! limit count))

       (animstack-remember-image-size img)
       ;; need to resize the image so that it includes the whole source
       (let ((source-offsets (gimp-drawable-offsets source))
             (source-width (car (gimp-drawable-width source)))
             (source-height (car (gimp-drawable-height source))))
         (gimp-image-resize img (+ (car source-offsets) source-width) 
                            (+ (cadr source-offsets) source-height) 0 0))
       (set! node-layers 
             (if (= src-count 1) (list source)
                 (cdr (reverse (vector->list srcs)))))
       (set! temp-layer (make-temp-sampler-layer img group width height))
       (animstack-reset-count)
       ;; before effects are applied to temp layer
       (for-each 
        (lambda (effect) (effect img temp-layer #f (list) #f))
        (list-ref opts 2))
       (let* ((nodes (map get-layer-coords node-layers))
              (path (animstack-linear-path count nodes))
              (action (sampler-action img temp-layer source path 
                                      pos opts roll-mode))
              (tl (list-ref (car opts) 2)))
         (layer-walk getter start next is-untagged? action limit #t tl))
       (animstack-restore-image-size img)
       (animstack-reset-motion)
       (gimp-image-remove-layer img group)
       ))))

;; Duplicate trees

(define (dup-getter img layer)
  (let ((pos (car (gimp-image-get-item-position img layer))))
    (lambda (target) (get-layer-in-group target pos))))

(define (process-dup-options dup-options img bindings target getter)
  (let* ((opts1 (car dup-options))
         (cumulative (car opts1))
         (dir (list-ref opts1 3))
         ;; range set to always true because it gets confusing otherwise
         (apply-effects (lambda (layer tgt effects)
                          ((apply-effects-simple img effects bindings #f (lambda (n) #t)) layer tgt)))
         (pos (car (gimp-image-get-item-position img target)))
         )
    (letrec ((branch-out
              (lambda (from opts old-be old-de)
                (let* ((before-effects (append old-be (caar opts)))
                       (during-effects (append old-de (cadar opts)))
                       (branch-from from)
                       (cur-source from))
                  (for-each
                   (lambda (el)
                     (if (car el) 
                         (branch-out branch-from el
                                     (if (= branch-from from) before-effects '())
                                     during-effects)
                         (let* ((incpos (if (<= dir 0) (set! pos (+ pos 1))))
                                (new (duplicate-target-frame img cur-source dir pos))
                                (layer (getter new)))
                           (apply-effects layer new (cadr el)) ;; before-effects local
                           (if (= cur-source from) ;; before-effects branch
                               (apply-effects layer new before-effects))
                           (apply-effects layer new during-effects) ;; runtime-effects branch
                           (apply-effects layer new (cddr el)) ;; runtime-effects local
                           (set! branch-from new)
                           (if cumulative (set! cur-source new)))))
                   (cdr opts))))))
      (branch-out target (cdr dup-options) '() '()))))

(define (process-dup-options* opts . rest)
  (let ((dup-options (get-dup-opts opts)))
    (if dup-options (apply process-dup-options dup-options rest)))) 

(define (get-dup-opts opts)
  (and (> (length opts) 4) (list-ref opts 4)))

(define (get-start-position img layer opts)
  (let ((dup-opts (get-dup-opts opts)))
    (if dup-opts 
        (cadar dup-opts)
        (car (gimp-image-get-item-position img layer)))))

(define (build-dup-branch layers)
  (map (lambda (layer)
         (let* ((tags (sort-animstack-tags (extract-animstack-tags layer)))
                (before-effects (process-effect-tags (list-ref tags 2) #f))
                (during-effects (process-effect-tags (list-ref tags 3) #f)))
           (if (is-true? gimp-item-is-group layer)
               (cons (list before-effects during-effects)
                     (build-dup-branch (vector->list (cadr (gimp-item-get-children layer)))))
               (cons #f (cons before-effects during-effects)))))
       layers))

(define (build-dup-tree dupes opts position dir)
  (let ((cumulative (caar opts))
        (generator-alist (cadr opts))
        (before-effects (list-ref opts 2))
        (during-effects (list-ref opts 3)))
    (cons (list cumulative position generator-alist dir)
          (cons (list before-effects during-effects)
                (build-dup-branch dupes)))))

;; [dt:direction]
(define (animstack-dup-tree img layer opts . params)
  (with-params
   ((dir 1))
   (let* ((group (groupify-layer img layer))
          (contents* (gimp-item-get-children group))
          (contents (vector->list (cadr contents*)))
          (primary #f)
          (dupes #f)
          (position (car (gimp-image-get-item-position img group))))
     (if (= (car contents*) 0) (error "Empty duplicate tree"))
     (if (> dir 0) (set! contents (reverse contents)))
     (set! primary (car contents))
     (set! dupes (cdr contents))
     ;; nested dts don't do anything (it just hurts my brain thinking about it)
     (if (or (<= (length opts) 4) (not (list-ref opts 5)))
         (animstack-process-layer img primary (build-dup-tree dupes opts position dir)))
     (gimp-image-remove-layer img group))))

;; Action tag processing

(define (check-tag-params params test)
  (check-all params (lambda (x) (or (not x) (test x)))))

(define (animstack-parse-tagname str)
  ;; (name . (cumulative reverse terminate-label))
  (let* ((cumulative #t)
         (reverse #f)
         (terminate-label #f)
         (bake 
          (lambda (str) 
            (let* ((split (string-split str #\>)))
              (if (= (length split) 2)
                  (begin
                    (set! terminate-label (parse-tag-param-simple (cadr split)))
                    (set! str (car split)))))
            (list str cumulative reverse terminate-label))))
    (let loop ((str str))
      (cond ((= (string-length str) 0) (bake str))
            ((char=? (string-ref str 0) #\.)
             (set! cumulative #f)
             (loop (substring str 1 (string-length str))))
            ((char=? (string-ref str 0) #\~)
             (set! reverse #t)
             (loop (substring str 1 (string-length str))))
            (else (bake str))))))

(define *animstack-action-tag-assocs*
  (make-animstack-hash
   `(("bg" ,animstack-bg) 
     ("fg" ,animstack-fg)
     ("copy" ,animstack-copy)
     ("roll" ,animstack-roll)
     ("splice" ,animstack-splice)
     ("noop" ,animstack-noop)
     ("no" ,animstack-noop)
     ("matte" ,animstack-matte)
     ("delete" ,animstack-delete)
     ("render" ,animstack-render)
     ("sampler" ,animstack-sampler)
     ("dt" ,animstack-dup-tree)
     )))
      
(define (animstack-process-tag img layer tag generator-alist before-effects during-effects extra-opts)
  (let* ((tagpair (animstack-parse-tagname (car tag))) ;; (tagname . other opts)
         (tagname (car tagpair))
         (opts (apply list (cdr tagpair) generator-alist before-effects during-effects extra-opts))
         (tag-assoc (*animstack-action-tag-assocs* 'assoc tagname)))
    (and tag-assoc
         (or (check-tag-params (cdr tag) integer?)
             (error "Action tag parameters must be integer"))
         (apply (cadr tag-assoc) img layer opts (cdr tag)))))

;; generators

;; a generator processor must return a function with zero parameters
;; that returns numeric values when called repeatedly. It's called
;; once per frame to generate values for variables

(macro (generator form)
  (let ((res (gensym)))
    `(let ((x 0))
       (lambda ()
         (let ((,res (begin ,@(cdr form))))
           (set! x (+ x 1))
           ,res)))))

(define (animstack-const params)
  "const:value"
  (with-params ((value 0)) (lambda () value)))

(define (random-float range)
  (* (/ (random-next) 2147483647) range))

(define (animstack-rng params)
  "rng:range"
  (with-params ((range 10)) (lambda () (random-float range))))

(define (animstack-irng params)
  "irng:range"
  (with-params ((range 10)) (lambda () (random range))))

(define (float-remainder x period)
  (let ((div (floor (/ x (abs period)))))
    (- x (* (abs period) div))))

(define (sin-normalized x)
  "sine function with period 1"
  (sin (* (- x (floor x)) 2 *pi*)))

(define (animstack-osc params)
  "osc:amplitude:period:phase"
  (with-params
   ((amplitude 10) (period 10) (phase (random-float 1)))
   (if (= period 0) (error "Oscillator period cannot be 0"))
   (generator
    (* amplitude (sin-normalized (+ (/ x period) phase)))
    )))

(define (animstack-dosc params)
  "dosc:amplitude:period:phase"
  (with-params
   ((amplitude 10) (period 10) (phase (random-float 1)))
   (if (= period 0) (error "Oscillator period cannot be 0"))
   (generator
    (* amplitude (/ (* 2 *pi*) period) (sin-normalized (+ (/ x period) phase 0.25)))
    )))

(define (animstack-inc params)
  "inc:step:init"
  (with-params
   ((step 1) (init 0)) (generator (+ init (* x step)))))

(define (animstack-cycle params)
  (if (null? params) (set! params (list 0)))
  (let ((len (length params)))
    (generator (or (list-ref params (modulo x len)) 0))))

(define (animstack-poly params)
  (if (null? params) (set! params (list 1)))
  (set! params (reverse params))
  (if (= (or (car params) 0) 0) (set-car! params 1))
  (generator 
   (let ((result 0))
     (for-each
      (lambda (k) (set! result (+ (* result x) (or k 0))))
      params)
     (* result x))))

(define (animstack-dpoly params)
  (if (null? params) (set! params (list 1)))
  (set! params (reverse params))
  (if (= (or (car params) 0) 0) (set-car! params 1))
  (generator 
   (let ((result 0)
         (m (length params)))
     (for-each
      (lambda (k) 
        (set! result (+ (* result x) (* m (or k 0))))
        (set! m (- m 1)))
      params)
     result)))

(define (parse-generator-defn defn)
  (let* ((mul-split (string-split defn #\*))
         (add-split (string-split (car mul-split) #\+))
         (mul-found (= (length mul-split) 2))
         (add-found (= (length add-split) 2))
         (mul-str (if mul-found
                      (if add-found (cadr add-split) (car mul-split))
                      #f))
         (add-str (if add-found (car add-split) #f))
         (rest-str (if mul-found (cadr mul-split)
                       (if add-found (cadr add-split)
                           defn))))
    (list (or (and add-str (string2number add-str)) 0)
          (or (and mul-str (string2number mul-str)) 1)
          rest-str)))

(define (adjust-generator-fn coeffs fn)
  (let ((add (car coeffs))
        (mul (cadr coeffs)))
    (lambda () (+ add (* mul (fn))))))

(define *animstack-generator-tag-assocs*
  (make-animstack-hash
   `(("rng" ,animstack-rng)
     ("irng" ,animstack-irng)
     ("osc" ,animstack-osc)
     ("dosc" ,animstack-dosc)
     ("inc" ,animstack-inc)
     ("const" ,animstack-const)
     ("cycle" ,animstack-cycle)
     ("poly" ,animstack-poly)
     ("dpoly" ,animstack-dpoly)
     )))

(define (init-generators generator-tags)
  (let ((alist (list)))
    (do ((rest generator-tags (cdr rest)))
        ((null? rest) alist)
      (let* ((curtag (car rest))
             (namesplit (string-split (car curtag) #\=))
             (var (string->atom (car namesplit))))
        (if (not (and (= (length namesplit) 2) (symbol? var)))
            (error (string-append "syntax error: " (car curtag)))
            (let* ((defnlist (parse-generator-defn (cadr namesplit)))
                   (gen-assoc (*animstack-generator-tag-assocs* 'assoc (caddr defnlist))))
              (and gen-assoc
                   (or (check-tag-params (cdr curtag) number?)
                       (error "Generator tag parameters must be numbers"))
                   (set! alist 
                         (cons (list var (adjust-generator-fn defnlist ((cadr gen-assoc) (cdr curtag))))
                               alist)))))))))

;; Effect tags (before and during)

;; before [!<tagname>:p1:p2]
;; during affects 1 layer [-<range>;<tagname>:p1:p2]
;; during affects whole group [=<range>;<tagname>:p1:p2]

;; range syntax comma separated list of range designators
;; n - executed on nth step
;; n- executed after nth step (inclusive)
;; n-m - executed from nth to mth step (inclusive)
;; /n - executed every nth step
;; m/n - executed every nth step with offset m
;; rn - executed randomly 1 out of n times
;; mrn - executed randomly m out of n times

(define (get-range-fn range)
  (let* ((range-lst (string->list range))
         (paramtest (lambda (x) (and (integer? x) (>= x 0))))
         (pos-paramtest (lambda (x) (and (integer? x) (> x 0))))
         (res (cond ((null? range-lst) (lambda (n) #t))
                    ((memv #\- range-lst) ;; simple range
                     (let ((srange (string-split range #\-)))
                       (and (= (length srange) 2)
                            (let ((from (string2number (car srange) paramtest))
                                  (to (string2number (cadr srange) paramtest)))
                              (if (= (string-length (cadr srange)) 0)
                                  (and from (<= 0 from)
                                       (lambda (n) (<= from n)))
                                  (and from to (<= 0 from) (<= from to)
                                       (lambda (n) (<= from n to))))))))
                    ((memv #\/ range-lst) ;; period range
                     (let ((prange (string-split range #\/)))
                       (and (= (length prange) 2)
                            (let ((offset (string2number (car prange) paramtest))
                                  (period (string2number (cadr prange) paramtest)))
                              (if (= (string-length (car prange)) 0)
                                  (set! offset 0))
                              (and offset period (< 0 period)
                                   (lambda (n) (and (<= offset n)
                                                    (= (modulo (- n offset) period) 0))))))))
                    ((memv #\r range-lst) ;; random range
                     (let ((rrange (string-split range #\r)))
                       (and (= (length rrange) 2)
                            (let ((tries (string2number (car rrange) paramtest))
                                  (total (string2number (cadr rrange) pos-paramtest)))
                              (if (= (string-length (car rrange)) 0)
                                  (begin
                                    (set! tries 1)
                                    (if (= (string-length (cadr rrange)) 0)
                                        (set! total 2))))
                              (and tries total
                                   (lambda (n) (< (random-float 1) (/ tries total))))))))
                    (else ;; atom
                     (let ((nr (string2number range integer?)))
                       (and nr (lambda (n) (= n nr))))))))
    (if res res (error (string-append "Invalid range: " range)))))

(define (parse-range str)
  "Returns a function that given a nonnegative argument returns if it is in range"
  (if (= (string-length str) 0)
      (lambda (n) #t)
      (let ((collection (map get-range-fn (string-split str #\,))))
        (lambda (n)
          (let loop ((rlist collection))
            (cond ((null? rlist) #f)
                  (((car rlist) n) #t)
                  (else (loop (cdr rlist)))))))))


;; Actual effects procedures here

(define (animstack-move img params)
  (with-params
   ((x 0) (y 0))
   (cons
    (lambda (layer target)
      (let ((offsets (gimp-drawable-offsets layer)))
        (gimp-layer-set-offsets layer (+ (car offsets) x) (+ (cadr offsets) y))))
    #t)))

(define (animstack-offset img params)
  "if x or y param is omitted, offsets randomly by that param"
  ;; we need to support stackability for random case so that every
  ;; layer in a roll has the same random offset
  (with-params
   (x y wrap)
   (if (not (and wrap (= wrap 0)))
       (set! wrap TRUE))
   (cons (lambda (layer target)
           (let ((xx x)
                 (yy y)
                 (layers (get-roll-layer-list layer)))
             (if (not x)
                 (let ((width (car (gimp-drawable-width (car layers)))))
                   (set! xx (random-float width))))
             (if (not y)
                 (let ((height (car (gimp-drawable-height (car layers)))))
                   (set! yy (random-float height))))
             (for-each (lambda (layer)
                         (gimp-drawable-offset layer wrap 1 xx yy))
                       layers)))
         #t)))

(define (animstack-resize img params)
  "sets layer to image size"
  (cons (lambda (layer target)
          (gimp-layer-resize-to-image-size layer))
        #f))

(define (animstack-scatter img params)
  "scatter:mode - mode can be 
  <= 0 (default) - moves the layer randomly so it doesn't go outside the borders of the image
   > 0 - moves layer randomly so that part of it is still within image

   If there's a non-empty selection, use selection bounds instead of borders.
   "
  (with-params
   ((mode 0))
   (let* ((image-width (car (gimp-image-width img)))
          (image-height (car (gimp-image-height img)))
          (ox 0) (oy 0))
     (let ((selection-bounds (gimp-selection-bounds img)))
       (if (= (car selection-bounds) TRUE)
           (begin
             (set! ox (cadr selection-bounds))
             (set! oy (caddr selection-bounds))
             (set! image-width (- (cadddr selection-bounds) ox))
             (set! image-height (- (car (last selection-bounds)) oy)))))
     (cons (lambda (layer target)
             (let ((layer-width (car (gimp-drawable-width layer)))
                   (layer-height (car (gimp-drawable-height layer)))
                   (minx #f)
                   (maxx #f)
                   (miny #f)
                   (maxy #f))
               (cond ((<= mode 0)
                      (set! minx 0) (set! miny 0)
                      (set! maxx (- image-width layer-width))
                      (set! maxy (- image-height layer-height)))
                     (else
                      (set! minx (- layer-width))
                      (set! miny (- layer-height))
                      (set! maxx image-width)
                      (set! maxy image-height)))
               (set! minx (+ minx ox))
               (set! maxx (+ maxx ox))
               (set! miny (+ miny oy))
               (set! maxy (+ maxy oy))
               (if (and (< minx maxx) (< miny maxy))
                   (let ((ox (+ minx (random-float (- maxx minx))))
                         (oy (+ miny (random-float (- maxy miny)))))
                     (gimp-layer-set-offsets layer ox oy)))))
           #t))))


(define (animstack-get-disposal-mode str)
  "mode can be (replace) or (combine) at the end of the string"
  (let ((buffer (make-vector 2)))
    (and (re-match "\\((combine|replace)\\)\\s*$" str buffer)
         (let ((boundaries (vector-ref buffer 1)))
           (substring str (car boundaries) (cdr boundaries))))))

(define (add-frame-delay str delay)
  ;; we must find all substrings of the form (<number>ms) and remove them
  (let* ((disposal-mode (animstack-get-disposal-mode str))
         (add-disposal (lambda (str)
                         (if disposal-mode
                             (add-combine-replace str disposal-mode)
                             str))))
    (set! str (list->string
               (let loop ((sl (string->list str)))
                 (cond ((null? sl) (list))
                       ((char=? (car sl) #\()
                        (let* ((split (string-split (list->string (cdr sl)) #\)))
                               (inside (car split))
                               (len (string-length inside)))
                          (if (and (> (length split) 1)
                                   (> len 2)
                                   (equal? (substring inside (- len 2) len) "ms")
                                   (string2number (substring inside 0 (- len 2)) integer?))
                              (loop (cdr (memv #\) sl)))
                              (cons (car sl) (loop (cdr sl))))))
                       (else (cons (car sl) (loop (cdr sl))))))))
    (add-disposal
     (if delay (string-append str " (" (number->string delay) "ms)") str))))

(define (animstack-delay img params)
  "Sets frame delay for target"
  (with-params
   ((frame-delay 40) (corner-delay frame-delay))
   (if (<= frame-delay 0) (set! frame-delay #f))
   (cons (lambda (layer target)
           (if target
               (let* ((motion (animstack-get-motion))
                      (corner? (list-ref motion 4))
                      (oldname (car (gimp-item-get-name target)))
                      (newname (add-frame-delay oldname 
                                                (if corner? corner-delay frame-delay))))
                 (gimp-item-set-name target newname))))
         #t)))

(define (add-combine-replace str mode)
  (set! str (list->string
        (let loop ((sl (string->list str)))
          (cond ((null? sl) (list))
                ((char=? (car sl) #\()
                 (let* ((split (string-split (list->string (cdr sl)) #\)))
                        (inside (car split))
                        (len (string-length inside)))
                   (if (and (> (length split) 1)
                            (> len 2)
                            (or (equal? inside "combine")
                                (equal? inside "replace")))
                       (loop (cdr (memv #\) sl)))
                       (cons (car sl) (loop (cdr sl))))))
                (else (cons (car sl) (loop (cdr sl))))))))
  (if mode (string-append str " (" mode ")") str))

(define (animstack-replace img params)
  (with-params
   ((mode 1))
   (set! mode (cond ((< mode 0) "combine")
                    ((> mode 0) "replace")
                    (else #f)))
   (cons (lambda (layer target)
           (if target
               (let* ((oldname (car (gimp-item-get-name target)))
                      (newname (add-combine-replace oldname mode)))
                 (gimp-item-set-name target newname))))
         #t)))

(define (animstack-erase img params)
  "erase:n:direction - Cuts n letters from text layer, either from
   end (mode<=0) or from beginning (mode>0)

  Doesn't work for formatted text unfortunately (use shrink)"
  (with-params
   ((n 1) (mode 0))
   (if (< n 0) (set! n 0))
   (if (not (integer? n)) (set! n (int-round n)))
   (cons (lambda (layer target)
           (if (is-true? gimp-item-is-text-layer layer)
               (let* ((text (car (gimp-text-layer-get-text layer)))
                      (tlen (string-length text)))
                 (set! text (if (> tlen n)
                                (if (> mode 0)
                                    (substring text n tlen)
                                    (substring text 0 (- tlen n)))
                                ""))
                 (gimp-text-layer-set-text layer text))))
         #f)))

(define (animstack-shrink img params)
  (with-params
   ((dright 0) (dbottom 0) (dleft 0) (dtop 0))
   (cons (lambda (layer target)
           (let* ((width (car (gimp-drawable-width layer)))
                  (height (car (gimp-drawable-height layer)))
                  (offsets (gimp-drawable-offsets layer))
                  (ox (car offsets))
                  (oy (cadr offsets))
                  (new-ox ox)
                  (new-oy oy)
                  (new-width width)
                  (new-height height))
             ;; (if (= width 1) (begin (set! width 0) (gimp-item-set-visible layer TRUE)))
             ;; (if (= height 1) (begin (set! height 0) (gimp-item-set-visible layer TRUE)))
             (set! new-width (- new-width dright))
             (set! new-height (- new-height dbottom))
             (if (< dleft new-width)
                 (begin (set! new-ox (+ new-ox dleft))
                        (set! new-width (- new-width dleft)))
                 (begin (set! new-ox (+ new-width))
                        (set! new-width 0)))
             (if (< dtop new-height)
                 (begin (set! new-height (- new-height dtop))
                        (set! new-oy (+ new-oy dtop)))
                 (begin (set! new-oy (+ new-oy new-height))
                        (set! new-height 0)))
             ;; because GIMP won't allow 0 width/height layers for some reason...
             (if (= new-width 0)
                 (begin (set! new-width 1)
                        (gimp-item-set-visible layer FALSE)))
             (if (= new-height 0)
                 (begin (set! new-height 1)
                        (gimp-item-set-visible layer FALSE)))
             (gimp-layer-resize layer new-width new-height 
                                (- ox new-ox) (- oy new-oy))))
         #f)))

(define (animstack-scale img params)
  (with-params
   ((hscale 1) (vscale hscale))
   (cons (lambda (layer target)
           ;;(gimp-context-set-interpolation INTERPOLATION-CUBIC)
           (let* ((width (car (gimp-drawable-width layer)))
                  (height (car (gimp-drawable-height layer)))
                  (new-width (max 1 (* width hscale)))
                  (new-height (max 1 (* height vscale))))
             (gimp-layer-scale layer new-width new-height TRUE)))
         #f)))
    
(define (animstack-stretch img params)
  (with-params
   ((dwidth 0) (dheight 0))
   (cons (lambda (layer target)
           ;;(gimp-context-set-interpolation INTERPOLATION-CUBIC)
           (let* ((width (car (gimp-drawable-width layer)))
                  (height (car (gimp-drawable-height layer)))
                  (new-width (max 1 (+ width dwidth)))
                  (new-height (max 1 (+ height dheight))))
             (gimp-layer-scale layer new-width new-height TRUE)))
         #f)))

(define (animstack-add-margin layer margin)
  (cond ((is-true? gimp-item-is-group layer)
         (vector-for-each
          (lambda (child) (animstack-add-margin child margin))
          (cadr (gimp-item-get-children layer))))
        (else
         (let ((width (car (gimp-drawable-width layer)))
               (height (car (gimp-drawable-height layer)))
               (m2 (* 2 margin)))
           (gimp-layer-resize layer (+ m2 width) (+ m2 height) margin margin)))))

(define (autocrop-layer-transparent img layer margin)
  "Only remove transparent borders from layer"
  (gimp-image-set-active-layer img layer)
  (if (not (is-true? gimp-item-is-group layer))
      (gimp-layer-add-alpha layer))
  ;;add 1px transparent border
  (animstack-add-margin layer 1)
  (plug-in-autocrop-layer 1 img layer)
  (if (and margin (> margin 0))
      (animstack-add-margin layer margin)))

(define (animstack-rotate img params)
  (with-params
   ((angle 90) nocrop)
   (let* ((fn (lambda (rotator)
                (lambda (layer)
                  (animstack-save-selection img)
                  (gimp-selection-none img)
                  (gimp-context-set-transform-resize 0)
                  (rotator layer)
                  (animstack-restore-selection img)))))
     (set! angle (float-remainder angle 360))
     (cond ((= angle 0) (set! fn (lambda (layer))))
           ((or (= angle 90)
                (= angle 180)
                (= angle 270))
            (set! fn 
                  (fn (lambda (layer)
                        (gimp-item-transform-rotate-simple 
                         layer (- (/ angle 90) 1) TRUE 0 0)))))
           (else
            (set! angle (/ (* *pi* angle) 180))
            (set! fn
                  (fn (lambda (layer)
                        ;;(gimp-context-set-interpolation INTERPOLATION-CUBIC)
                        (gimp-context-set-transform-direction 0)
                        (gimp-item-transform-rotate layer angle TRUE 0 0))))))
     (cons (lambda (layer target)
             (if (not nocrop) (autocrop-layer-transparent img layer 0))
             (fn layer)) #t))))


(define (animstack-drotate img params)
  (with-params
   ((x 0) (y -1) (ix 0) (iy -1) nocrop)
   (let* ((angle (if (or (= ix iy 0) (= x y 0)) 0
                     (/ (* (- (atan y x) (atan iy ix)) 180) *pi*))))
     (animstack-rotate img (list angle nocrop)))))

(define (animstack-crop img params)
  (with-params
   ((margin 0))
   (cons (lambda (layer target)
           (autocrop-layer-transparent img layer margin))
         #t)))

(define (duplicate-target-frame img target dir ipos)
  (let* ((new (car (gimp-layer-copy target FALSE)))
         (pos (or ipos (car (gimp-image-get-item-position img target)))))
    (if (and (not ipos) (<= dir 0)) (set! pos (+ pos 1)))
    (gimp-image-insert-layer img new 0 pos)
    new))

(define (animstack-dup img params)
  "Duplicates the target frame. The result is very different in cumulative and non-cumulative modes.
   If parameter is <=0, put the duplicate before the target frame, otherwise put after."
  (with-params
   ((dir 0))
   (cons (lambda (layer target)
           (if target (duplicate-target-frame img target dir #f)))
         #t)))

(define (animstack-mask img params)
  "[mask:from] Adds mask from selection (or replaces existing one). If from is specified,
   applies a mask based on alpha of target layer at position *from*."
  (with-params
   (from)
   (let ((mask-from 
          (lambda (layer target from)
            (animstack-save-selection img)
            (let ((source (get-layer-in-group target (int-round from))))
              (gimp-image-select-item img CHANNEL-OP-REPLACE source)))))
     (cons (lambda (layer target)
             (cond ((is-true? gimp-item-is-group layer)) ;; Layer groups do not support masks
                   (else (if (>= (car (gimp-layer-get-mask layer)) 0)
                             (gimp-layer-remove-mask layer MASK-DISCARD)) ;; option to MASK-APPLY?
                         (if from (mask-from layer target from))
                         (let ((mask (car (gimp-layer-create-mask layer ADD-SELECTION-MASK))))
                           (gimp-layer-add-mask layer mask))
                         (if from (animstack-restore-selection img)))))
           #f))))

;; TODO a shrink-like effect, but with a rectangular mask

(define (animstack-opacity img params)
  (with-params
   ((opacity 100))
   (if (< opacity 0) (set! opacity 0))
   (if (> opacity 100) (set! opacity 100))
   (cons (lambda (layer target)
           (gimp-layer-set-opacity layer opacity))
         #f)))

(define (descartes-to-blur-params dx dy)
  (cons (sqrt (+ (* dx dx) (* dy dy)))
        (if (= dx dy 0) 0 (/ (* 180 (atan (- dy) (- dx))) *pi*))))

;; motion blur
(define (animstack-mb img params)
  (with-params
   ((dx 0) (dy 0))
   (let ((bp (descartes-to-blur-params dx dy)))
     (cons (lambda (layer target)
             (if (not (= dx dy 0))
                 (let ((len (car bp))
                       (angle (cdr bp)))
                   (plug-in-mblur 1 img layer 0 len angle 0 0))))
           #f))))

;; zoom blur (inward if r<0)
(define (animstack-zb img params)
  (with-params
   ((r 0) (cx 0.5) (cy 0.5))
   (let ((zoomfn (if (< r 0) plug-in-mblur-inward plug-in-mblur)))
     (cons (lambda (layer target)
             (if (not (= r 0))
                 (let* ((width (car (gimp-drawable-width layer)))
                        (height (car (gimp-drawable-height layer)))
                        (x (* cx width))
                        (y (* cy height)))
                   (zoomfn 1 img layer 2 (abs r) 0 x y))))
           #f))))

;; radial blur (seems to be symmetrical counter and clockwise...)
(define (animstack-rb img params)
  (with-params
   ((angle 0) (cx 0.5) (cy 0.5))
   (cons (lambda (layer target)
           (if (not (= angle 0))
               (let* ((width (car (gimp-drawable-width layer)))
                      (height (car (gimp-drawable-height layer)))
                      (x (* cx width))
                      (y (* cy height)))
                 (plug-in-mblur 1 img layer 1 0 angle x y))))
         #f)))

;; gaussian blur
(define (animstack-gb img params)
  (with-params
   ((rx 0) (ry rx))
   (cons (lambda (layer target)
           (if (not (= rx ry 0))
               (plug-in-gauss 1 img layer rx ry 1)))
         #f)))

;; sampler blur
(define (animstack-sb img params)
  (with-params
   ((kl 10) (kz 10))
   (let* ((kl (/ kl 30)) (kz (/ kz 100)))
     (cons (lambda (layer target)
             (let* ((motion (animstack-get-motion))
                    (dx (car motion))
                    (dy (cadr motion))
                    (width (car (gimp-drawable-width layer)))
                    (height (car (gimp-drawable-height layer)))
                    (mb (car (animstack-mb img (list (* kl dx width) (* kl dy height)))))
                    (zx (- (caddr motion)))
                    (zy (- (cadddr motion)))
                    (zv (if (< (abs zx) (abs zy)) zx zy))
                    (p-axis (if (< (abs zx) (abs zy)) width height))
                    ;; yeahhh idk there's no zoom blur in gimp that works when zx!=zy
                    (mz (car (animstack-zb img (list (* kz zv p-axis)))))
                    )
               (mb layer target)
               (mz layer target)))
           #f))))

;; invert colors
(define (animstack-invert img params)
  (cons (lambda (layer target)
          (gimp-invert layer))
        #f))

;; threshold:lower:upper
;; if no parameters, replace every visible pixel with black
(define (animstack-threshold img params)
  (with-params
   (lower upper)
   (if (or lower upper)
       (begin
         (set! lower (or lower 128))
         (set! upper (or upper 255))
         (set! upper (min (max upper 0) 255))
         (set! lower (min (max lower 0) upper))))
   (cons (lambda (layer target)
           (if lower
               (gimp-threshold layer lower upper)
               (begin 
                 (gimp-threshold layer 0 255)
                 (gimp-invert layer))))
         #f)))

;; desaturate:mode
(define (animstack-desaturate img params)
  (with-params
   ((mode 0))
   (set! mode (cond ((< mode 0) 0)   ;;lightness
                    ((= mode 0) 2)   ;;average
                    ((> mode 0) 1))) ;;luminosity
   (cons (lambda (layer target)
           (gimp-desaturate-full layer mode))
         #f)))

;; gradient map
;; uses current gradient or rgb -> white
(define (animstack-gradmap img params)
  (with-params
   (r g b)
   (if (or r g b)
       (begin
         (set! r (min (max (or r 0) 0) 255))
         (set! g (min (max (or g 0) 0) 255))
         (set! b (min (max (or b 0) 0) 255))
         (gimp-context-set-gradient "FG to BG (RGB)") ;; is this translated? i hope not!
         (gimp-context-set-foreground (list r g b))
         (gimp-context-set-background (list 255 255 255))))
   (cons (lambda (layer target)
           (plug-in-gradmap 1 img layer))
         #f)))


;; repeat:n repeat current layer in a roll n times (n=1 by default)
(define (animstack-repeat img params)
  (with-params
   ((n 2))
   (cons (lambda (layer target)
           (animstack-set-repeat-value n))
         #t)))

(define (animstack-seed img params)
  (with-params
   ((n 0))
   (cons (lambda (layer target)
           (srand n))
         #t)))

;; end

(define (resolve-bindings params bindings)
  (let ((resolve-symbol 
         (lambda (p)
           (if (symbol? p)
               (let ((binding (assoc p bindings)))
                 (if binding
                     (cadr binding)
                     (error (string-append "Unbound symbol: " (symbol->string p)))))
               p))))
    (map (lambda (p)
           (cond ((pair? p) (+ (car p) (* (cadr p) (resolve-symbol (caddr p)))))
                 (else (resolve-symbol p))))
         params)))

(define (animstack-effect fn mode default-range . params)
  (let ((count 0))
    (lambda (img layer target bindings group . extra)
      (let* ((effect/stackable (fn img (resolve-bindings params bindings)))
             (effect (car effect/stackable))
             (stackable (cdr effect/stackable))
             (range (if (and (pair? extra) (car extra)) (car extra) default-range))
             (count (if (and (>= (length extra) 2) (cadr extra)) (cadr extra) count))
             )
        (and (or (char=? mode #\!)
                 (and (not range) (> count 0)) ;; if no range set, skip the first layer
                 (and range (range count)))
             (if (and group (or (char=? mode #\!) (char=? mode #\+)))
                 (if stackable
                     (effect group target)
                     (for-each (lambda (layer) (effect layer target))
                               (get-roll-layer-list group)))
                 (effect layer target))))
      (set! count (+ count 1)))))

(define *animstack-effect-tag-assocs*
  (make-animstack-hash
   `(("scatter" ,animstack-scatter)
     ("move" ,animstack-move)
     ("offset" ,animstack-offset)
     ("resize" ,animstack-resize)
     ("delay" ,animstack-delay)
     ("replace" ,animstack-replace)
     ("erase" ,animstack-erase)
     ("shrink" ,animstack-shrink)
     ("scale" ,animstack-scale)
     ("stretch" ,animstack-stretch)
     ("rotate" ,animstack-rotate)
     ("drotate" ,animstack-drotate)
     ("crop" ,animstack-crop)
     ("dup" ,animstack-dup)
     ("mask" ,animstack-mask)
     ("opacity" ,animstack-opacity)
     ("mb" ,animstack-mb)
     ("zb" ,animstack-zb)
     ("rb" ,animstack-rb)
     ("gb" ,animstack-gb)
     ("sb" ,animstack-sb)
     ("thr" ,animstack-threshold)
     ("threshold" ,animstack-threshold)
     ("des" ,animstack-desaturate)
     ("desaturate" ,animstack-desaturate)
     ("gradmap" ,animstack-gradmap)
     ("invert" ,animstack-invert)
     ("repeat" ,animstack-repeat)
     )))

(define (process-effect-tag tag normal)
  (let* ((tag-defn (car tag))
         (tag-params (cdr tag))
         (tag-mode (string-ref tag-defn 0))
         (tag-rest (if (char=? tag-mode #\;) tag-defn (substring tag-defn 1 (string-length tag-defn))))
         (tag-defn-parsed (string-split tag-rest #\;))
         (len-tdp (length tag-defn-parsed))
         (tag-range #f)
         (tag-name #f))
    (cond ((and (char=? tag-mode #\!) normal)
           (if (not (= len-tdp 1))
               (error (string-append "Range not allowed in before effects: " tag-defn)))
           (if (not (check-tag-params tag-params number?))
               (error (string-append "Invalid parameter: " tag-defn)))
           (set! tag-name (car tag-defn-parsed)))
          (else
           (if (or (char=? tag-mode #\;) (and (not normal) (char=? tag-mode #\!)))
               (set! tag-mode #\-))
           (cond ((> len-tdp 2)
                  (error (string-append "Invalid tag syntax: " tag-defn)))
                 ((= len-tdp 2)
                  (set! tag-name (cadr tag-defn-parsed))
                  (set! tag-range (parse-range (car tag-defn-parsed))))
                 (else
                  (set! tag-name (car tag-defn-parsed))))))

    (let ((tag-assoc (*animstack-effect-tag-assocs* 'assoc tag-name)))
      (and tag-assoc
           (apply animstack-effect (cadr tag-assoc) tag-mode tag-range tag-params)))))

(define (map-filter fn lst . params)
  (let ((test (and (pair? params) (car params)))
        (res (map fn lst)))
    (let loop ((res res))
      (cond ((null? res) (list))
            ((if test (test (car res)) (car res)) (cons (car res) (loop (cdr res))))
            (else (loop (cdr res)))))))

(define (process-effect-tags tags normal)
  (map-filter (lambda (tag) (process-effect-tag tag normal)) tags))

;; Main processing
                   
(define (sort-animstack-tags tags)
  (let ((action-tags (list))
        (generator-tags (list))
        (before-tags (list))
        (during-tags (list)))
    (let loop ((tail tags))
      (if (pair? tail)
          (let* ((cur-tag (car tail))
                 (cur-tag-name (car cur-tag)))
            (cond ((= (string-length cur-tag-name) 0) (loop (cdr tail)))
                  ((memv #\= (string->list cur-tag-name))
                   (loop (cdr tail))
                   (set! generator-tags (cons cur-tag generator-tags)))
                  (else 
                   (let ((chr (string-ref cur-tag-name 0)))
                     (cond ((char=? chr #\!)
                            (loop (cdr tail))
                            (set! before-tags (cons cur-tag before-tags)))
                           ((or (char=? chr #\-)
                                (char=? chr #\+)
                                (char=? chr #\;))
                            (loop (cdr tail))
                            (set! during-tags (cons cur-tag during-tags)))
                           (else
                            (loop (cdr tail))
                            (set! action-tags (cons cur-tag action-tags))))))))))
    (list action-tags generator-tags before-tags during-tags)))

(define (animstack-process-layer img layer dup-options)
  (let* ((tags (sort-animstack-tags (extract-animstack-tags layer)))
         (action-tags (list-ref tags 0))
         ;; add default inc generator
         (generator-tags (cons '("i=inc") (list-ref tags 1)))
         (before-tags (list-ref tags 2))
         (during-tags (list-ref tags 3)))
    (if (or (pair? action-tags) (pair? during-tags) dup-options)
        (let ((generator-alist (init-generators generator-tags))
              (before-effects (process-effect-tags before-tags #t))
              (during-effects (process-effect-tags during-tags #t)))
          (if dup-options
              (set! generator-alist (append (caddar dup-options) generator-alist)))
          ;; if no action tag, but during tag present, add a simple noop action tag
          (if (null? action-tags) (set! action-tags (list (list "noop"))))
          (animstack-process-tag img layer (car action-tags)
                                 generator-alist before-effects during-effects
                                 (if dup-options (list dup-options) '()))))))

(define (is-multiply-tag? tag)
  (let ((name (car tag)))      
    (and (null? (cdr tag))
         (> (string-length name) 1)
         (char=? (string-ref name 0) #\*)
         (string2number (substring name 1 (string-length name))))))

(define (process-multiply-tag img layer tag)
  (let* ((tag-name (car tag))
         (layer-name (strip-tags (car (gimp-item-get-name layer)) is-multiply-tag?))
         (num (string2number (substring tag-name 1 (string-length tag-name))))
         (pos (car (gimp-image-get-item-position img layer)))
         (first-new-layer #f))
    (do ((i 0 (+ i 1)))
        ((>= i num))
      (let ((new (car (gimp-layer-copy layer FALSE))))
        (gimp-item-set-name new layer-name)
        (gimp-image-insert-layer img new 0 pos)
        (if (= i 0) (set! first-new-layer new))))
    (gimp-image-remove-layer img layer)
    first-new-layer))

(define (is-label-tag? tag)
  (and (= (string-length (car tag)) 0)))

(define animstack-reset-labels #f)
(define animstack-set-layer-labels #f)
(define animstack-layer-has-label #f)
(define animstack-copy-layer-labels #f)

(let ((label-hash (make-animstack-hash '()))
      (label-tag-symbol 
       (lambda (tag) 
         (if (null? (cdr tag))
             (string->symbol "")
             (cadr tag)))))
  (set! animstack-reset-labels
        (lambda () (set! label-hash (make-animstack-hash '()))))
  (set! animstack-set-layer-labels
        (lambda (layer tags)
          (apply label-hash 'add layer (map label-tag-symbol tags))))
  (set! animstack-copy-layer-labels
        (lambda (oldlayer newlayer)
          (let ((labels (cond ((label-hash 'assoc oldlayer) => cdr))))
            (if labels (apply label-hash 'add newlayer labels)))))
  (set! animstack-layer-has-label
        (lambda (layer label)
          (let ((lst (cond ((label-hash 'assoc layer) => cdr) (else #f))))
            (and lst (memv label lst))))))

(define (animstack-process-all-layers img)
  (srand (realtime))
  (gimp-image-undo-group-start img)
  (let ((layers (cadr (gimp-image-get-layers img)))) 
    ;; make everylayer visible. this is because it might be extremely
    ;; annoying to make them visible again after everything is jumbled up
    (vector-for-each
     (lambda (layer) (gimp-item-set-visible layer TRUE))
     layers)
    ;; preprocessing: find multiply tags and label tags and execute them
    (animstack-reset-labels)
    (vector-for-each
     (lambda (layer)
       (let ((tags (extract-animstack-tags layer is-multiply-tag?))
             (labeltags (extract-animstack-tags layer is-label-tag?)))
         (if (pair? labeltags)
             (gimp-item-set-name layer (strip-tags (car (gimp-item-get-name layer)) is-label-tag?)))
         (if (pair? tags) 
             (let ((newlayer (process-multiply-tag img layer (car tags))))
               (set! layer newlayer)))
         (if (and layer (pair? labeltags)) (animstack-set-layer-labels layer labeltags))))
     layers))
  ;; now the main part
  (gimp-context-push)
  (let ((layers (cadr (gimp-image-get-layers img))))
    (vector-for-each
     (lambda (layer) (animstack-process-layer img layer #f))
     layers))
  (gimp-context-pop)
  (gimp-image-undo-group-end img)
  (gimp-displays-flush))

(define script-fu-animstack-process-all animstack-process-all-layers)

(script-fu-register
 "script-fu-animstack-process-all"
 "Process AnimStack tags"
 "Process all AnimStack tags"
 "Timofei Shatrov"
 "Copyright 2012-2013"
 "May 19, 2013"
 "RGB RGBA GRAY GRAYA" ;; no layer groups in indexed :(
 SF-IMAGE     "Image to use"       0
 )

(script-fu-menu-register "script-fu-animstack-process-all" "<Image>/Filters/Animation")

;; Layer group helpers (release as a separate script maybe?)

(define (walk-layers-recursive img test fn)
  (let loop ((layers (cadr (gimp-image-get-layers img))))
    (vector-for-each
     (lambda (layer)
       (cond ((test layer) (fn layer))
             ((is-true? gimp-item-is-group layer)
              (loop (cadr (gimp-item-get-children layer))))))
     layers)))

(define (insert-layer-above-selected img layer)
  (let ((al (car (gimp-image-get-active-layer img))))
    (if (or (= al -1) (not (is-true? gimp-item-is-group al)))
        (gimp-image-insert-layer img layer 0 -1)
        (let ((pos (car (gimp-image-get-item-position img al)))
              (parent (car (gimp-item-get-parent al))))
          (gimp-image-insert-layer img layer parent pos)))))

(define (script-fu-pack-linked-layers img drw)
  (gimp-image-undo-group-start img)
  (let ((group (car (gimp-layer-group-new img)))
        (pos 0))
    (insert-layer-above-selected img group)
    (walk-layers-recursive 
     img (lambda (layer) (is-true? gimp-item-get-linked layer))
     (lambda (layer)
       (catch #f ;;prevent crash on reordering layer group into one of its children
              (begin
                (gimp-image-reorder-item img layer group pos)
                (set! pos (+ pos 1))
                (gimp-item-set-linked layer FALSE))))))
  (gimp-image-undo-group-end img)
  (gimp-displays-flush))

(define (script-fu-copy-linked-layers img drw)
  (gimp-image-undo-group-start img)
  (let ((group (car (gimp-layer-group-new img)))
        (pos 0))
    (insert-layer-above-selected img group)
    (walk-layers-recursive 
     img (lambda (layer) (is-true? gimp-item-get-linked layer))
     (lambda (layer)
       (gimp-item-set-linked layer FALSE)
       (let ((new (car (gimp-layer-copy layer FALSE))))
         (gimp-image-insert-layer img new group pos)
         (set! pos (+ pos 1))))))
  (gimp-image-undo-group-end img)
  (gimp-displays-flush))

(define (script-fu-unpack-layer-group img group)
  (if (is-true? gimp-item-is-group group)
      (begin 
        (gimp-image-undo-group-start img)
        (let ((layers (cadr (gimp-item-get-children group)))
              (parent (car (gimp-item-get-parent group)))
              (pos (car (gimp-image-get-item-position img group))))
          (if (< parent 0) (set! parent 0))
          (vector-for-each 
           (lambda (layer)
             (gimp-image-reorder-item img layer parent pos)
             (set! pos (+ pos 1)))
           layers))
        (gimp-image-remove-layer img group)
        (gimp-image-undo-group-end img)
        (gimp-displays-flush))))

(script-fu-register
 "script-fu-pack-linked-layers"
 "Pack Linked Layers"
 "Put all linked layers in a new layer group"
 "Timofei Shatrov"
 "Copyright 2012"
 "June 27, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE     "Image to use"       0
 SF-DRAWABLE  "Current layer"      0
 )

(script-fu-register
 "script-fu-copy-linked-layers"
 "Copy Linked Layers"
 "Put duplicates of all linked layers in a new layer group"
 "Timofei Shatrov"
 "Copyright 2012"
 "June 29, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE     "Image to use"       0
 SF-DRAWABLE  "Current layer"      0
 )

(script-fu-register
 "script-fu-unpack-layer-group"
 "Unpack Layer Group"
 "Put all layers in a layer group outside of that group"
 "Timofei Shatrov"
 "Copyright 2012"
 "June 27, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-IMAGE     "Image to use"       0
 SF-DRAWABLE  "Current layer"      0
 )

(script-fu-menu-register "script-fu-pack-linked-layers" "<Image>/Layer/Group")

(script-fu-menu-register "script-fu-copy-linked-layers" "<Image>/Layer/Group")

(script-fu-menu-register "script-fu-unpack-layer-group" "<Image>/Layer/Group")

;; Reverse/Mirror

(define (animstack-swap-layers img layer1 layer2 parent)
  (let* ((pos1 (car (gimp-image-get-item-position img layer1)))
         (pos2 (car (gimp-image-get-item-position img layer2))))
    (gimp-image-reorder-item img layer1 parent pos2)
    (gimp-image-reorder-item img layer2 parent pos1)))

(define (animstack-reverse-layers img parent layers copy?)
  (let ((len (vector-length layers)))
    (vector-for-each-i
     (if copy?
         (lambda (layer i)
           (let ((new (car (gimp-layer-copy layer FALSE))))
             (gimp-image-insert-layer img new parent 0)))
         (lambda (layer i)
           (if (< (* 2 i) (- len 1)) 
               (animstack-swap-layers img layer 
                                      (vector-ref layers (- len i 1)) parent))))
     layers)))

(define (animstack-mirror-layers img parent layers)
  (let ((len (vector-length layers)))
    (if (> len 2)
        (let ((middle (make-vector (- len 2))))
          (vector-for-each-i
           (lambda (layer i)
             (if (not (or (= i 0) (= i (- len 1))))
                 (vector-set! middle (- i 1) layer)))
           layers)
          (animstack-reverse-layers img parent middle #t)))))

(define (script-fu-reverse-mirror-layers img drw mode ignore-tagged)
  (let ((parent (car (gimp-item-get-parent drw)))
        (layers #f))
    (cond ((= parent -1) 
           (set! parent 0)
           (set! layers (cadr (gimp-image-get-layers img))))
          (else
           (set! layers (cadr (gimp-item-get-children parent)))))
    (if (= ignore-tagged TRUE)
        (set! layers (list->vector
                      (map-filter (lambda (x) x) (vector->list layers)
                                  is-untagged?))))
    (gimp-image-undo-group-start img)
    (cond ((= mode 0) (animstack-reverse-layers img parent layers #f))
          ((= mode 1) (animstack-mirror-layers img parent layers)))
    (gimp-image-undo-group-end img)))

(script-fu-register
 "script-fu-reverse-mirror-layers"
 "Reverse/Mirror layers..."
 "Reverse or mirror layers at the same level as selected layer"
 "Timofei Shatrov"
 "Copyright 2012"
 "October 11, 2012"
 ""
 SF-IMAGE     "Image to use"       0
 SF-DRAWABLE  "Current layer"      0
 SF-OPTION "Operation" '("Reverse" "Mirror")
 SF-TOGGLE "Ignore tagged layers" 0
 )
 
(script-fu-menu-register "script-fu-reverse-mirror-layers" "<Image>/Image")
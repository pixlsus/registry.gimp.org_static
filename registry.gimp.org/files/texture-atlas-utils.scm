;;; texture-atlas-utils.scm -*-scheme-*-
;;; Author: Edward Hutchins
;;; Version 0.1
;;;
;;; Utilities to create texture atlases from sets of images
;;; This is useful in 3D games to avoid excessive state changes
;;; In addition to the target image, a ".tat" file will be
;;; generated and updated to indicate which component images
;;; were incorporated into the atlas image. The format of this
;;; file is suitable for direct use in C/C++ development:
;;;
;;; --------8<----------------8<----------------8<--------
;;; // SRC <srcimage1-path> <border> <align-pow-2> <skip-pattern>
;;; // SRC <srcimage2-path> <border> <align-pow-2> <skip-pattern>
;;; [...]
;;; // SRC <srcimageN-path> <border> <align-pow-2> <skip-pattern>
;;; #define LIST_<destimagename>_ATLAS(_) \
;;; _(0,<srcimage1>,<srclayername1>,<x>,<y>,<width>,<height>) \
;;; _(1,<srcimage1>,<srclayername2>,<x>,<y>,<width>,<height>) \
;;; [...]
;;; _(<numimages-1>,<srcimageN>,<srclayernameN>,<x>,<y>,<width>,<height>)
;;; --------8<----------------8<----------------8<--------
;;;
;;; The generated list macro can be expanded via the C/C++
;;; preprocessor by passing macros as arguments, for example:
;;;
;;; #define MK_SPRITE(img,name,x,y,w,h) \
;;; { #img "_" #name, x, y, w, h },
;;; struct { char *name; int x, y, w, h; } sprites[] =
;;; { LIST_FOO_ATLAS(MK_SPRITE) { NULL } };
;;;
;;; Different macro forms can be used to generate enums, case
;;; statements etc.
;;;
;;; These utilities currently parse the comments in the .tat
;;; file and rebuild the atlas during each operation, so any
;;; hand-editing should be done after all source images have
;;; been combined.
;;;
;;; Use: The basic flow is to create a destination image as
;;; your atlas (usually but not necessarily 2^N,M on a side),
;;; then add source images one-by-one until the atlas is full.
;;; If a non-zero border is specified, each source layer will
;;; have border's worth of pixels padded on each side. If the
;;; Align 2^N box is checked, each source layer will be padded
;;; till the width and height are powers of two. Source images
;;; are remembered in the .tat file, use the Clear checkbox to
;;; remove all prior images. The skip pattern is a reg-ex used
;;; to ignore source layers by name, this can be useful when
;;; using cycle-layer-visibility.scm to include proxy images
;;; for animation testing (the default is to skip all layers
;;; matching " copy"). Each source image can only be added
;;; once; adding the same image again will remove the first
;;; addition and re-add at the end of the atlas list (border/
;;; alignment options may be changed in this way).
;;;
;;; Tip: Add all aligned images before any non-aligned ones
;;; to keep alignments on 2^N boundaries in mixed atlases.
;;; Alignment allows you to specify maximum LOD levels for
;;; mipmapped images to prevent bleed-through from adjacent
;;; sub-images in the atlas.
;;;
;;; TODO:
;;; add a "best fit" to scale source layers to a close 2^N
;;;
;;; the texture layout algorithm was inspired by:
;;; http://www.blackpawn.com/texts/lightmaps/default.html
;;;

;;
;; re-subs - regular expression substitution similar to perl's =~ s/<pattern>/<replacement>/
;;

(define (re-subs pat rep str)
    (let*
        (
            (at #((0 . 0)))
            (done "")
        )
        (if (re-match pat str at)
            (let*
                (
                    (pos (aref at 0))
                    (s (car pos))
                    (e (cdr pos))
                )
                (set! done (string-append done (substring str 0 s) rep))
                (set! str (substring str e (string-length str)))
            )
        )
        (string-append done str)
    )
)

;;
;; re-subsg - regular expression substitution similar to perl's =~ s/<pattern>/<replacement>/g
;;

(define (re-subsg pat rep str)
    (let*
        (
            (at #((0 . 0)))
            (done "")
            (shorter #t)
        )
        (while (and (re-match pat str at) shorter)
            (let*
                (
                    (pos (aref at 0))
                    (s (car pos))
                    (e (cdr pos))
                    (oldlen (string-length str))
                )
                (set! done (string-append done (substring str 0 s) rep))
                (set! str (substring str e (string-length str)))
                (set! shorter (> oldlen (string-length str)))
            )
        )
        (string-append done str)
    )
)

;;
;; re-split - split a string into a list by the given regular expression
;;

(define (re-split pat str)
    (let*
        (
            (at #((0 . 0)))
            (done nil)
            (shorter #t)
        )
        (while (and (re-match pat str at) shorter)
            (let*
                (
                    (pos (aref at 0))
                    (s (car pos))
                    (e (cdr pos))
                    (oldlen (string-length str))
                )
                (set! done (cons (substring str 0 s) done))
                (set! str (substring str e (string-length str)))
                (set! shorter (> oldlen (string-length str)))
            )
        )
        (reverse (cons str done))
    )
)

;;
;; x->string - convert things to strings
;;

(define (x->string x)
    (cond
        ((null? x) "")
        ((string? x) x)
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((boolean? x) (if x "#t" "#f"))
        ((vector? x) (string-append "#" (x->string (vector->list x))))
        ((list? x) (string-append "(" (join " " (map x->string x)) ")"))
        ((pair? x) (string-append "(" (x->string (car x)) " . " (x->string (cdr x)) ")"))
        (else "<unknown>")
    )
)

;;
;; x->boolean - convert things to booleans
;;

(define (x->boolean x)
    (cond
        ((boolean? x) x)
        ((null? x) #f)
        ((string? x) (not (string=? x "")))
        ((number? x) (not (= x 0)))
        ((list? x) (not (= (length x) 0)))
        ((vector? x) (not (= (vector-length x) 0)))
        (else #t)
    )
)

;;
;; string-remap - convert a string using the specified character modifying function
;;

(define (string-remap f str)
    (list->string (map f (string->list str)))
)

;;
;; join - glue a list of numbers and/or strings together with a seperator
;;

(define (join sep list)
    (if (pair? list)
        (string-append (x->string (car list)) (if (pair? (cdr list)) sep "") (join sep (cdr list)))
        (x->string list)
    )
)

;;
;; dbug - write a message to the starting console using the scheme equivalent of varargs
;;

(define (dbug . args)
    (let*
        (
            (old-handler (car (gimp-message-get-handler)))
        )
        (gimp-message-set-handler CONSOLE)
        (gimp-message (join "" (map x->string args)))
        (gimp-message-set-handler old-handler)
    )
)

;;
;; fail - pop a message box and invoke the exit continuation
;;

(define (fail exit . args)
    (let*
        (
            (old-handler (car (gimp-message-get-handler)))
            (msg (join "" (map x->string args)))
        )
        (gimp-message-set-handler ERROR-CONSOLE)
        (gimp-message msg)
        (gimp-message-set-handler old-handler)
        (exit msg)
        ; does not return
    )
)

;;
;; read-line - read a line from the port
;;

(define (read-line port)
    (let*
        (
            (line "")
            (c #\0)
        )
        (if (eof-object? (peek-char port))
            (read-char port)
            (begin
                (while (and (not (eof-object? (peek-char port))) (not (char=? c #\newline)))
                    (set! c (read-char port))
                    (if (not (char=? c #\newline)) (set! line (string-append line (make-string 1 c))))
                )
                line
            )
        )
    )
)

;;
;; texture-atlas-read - load a .tat file and parse the comments to recover source files and options
;;
;; format of lines read from the .tat file:
;; // SRC <filepath> <align-pow-2> <skip-pattern>
;;

(define (texture-atlas-read filename)
    ;(dbug (list 'texture-atlas-read filename))
    (if (file-exists? filename)
        (let* 
            (
                (f (open-input-file filename))
                (atlas-records nil)
                (line (read-line f))
            )
            (while (not (eof-object? line))
                ;(dbug "read " line ", match " (re-match "^//\\s+SRC\\s+" line))
                (if (re-match "^//\\s+SRC\\s+" line)
                    (begin
                        (set! line (re-subs "^//\\s+SRC\\s+" "" line))
                        (set! line (re-subs "\\s+$" "" line))
                        (set! atlas-records (append atlas-records (list (re-split "\\s+" line))))
                    )
                )
                (set! line (read-line f))
            )
            (close-input-port f)
            ;(dbug "got " (length atlas-records) " records: " atlas-records)
            atlas-records
        )
        nil
    )
)

;;
;; texture-atlas-write - generate a .tat file with comments describing both the source files and sub-image locations
;;

(define (texture-atlas-write exit atlas)
    (let* 
        (
            (atlas-list (cadr atlas))
            (src-file-list (caddr atlas))
            (img-id (list-ref atlas 3))
            (filename (list-ref atlas 4))
            (f (open-output-file filename))
            (src-files (join "\n// SRC " (map (lambda (x) (join " " x)) src-file-list)))
            (atlas-macro (join ") \\\n_(" (map (lambda (x) (join "," x)) atlas-list)))
        )

        ; simple header for backwards-compatibility
        (display (string-append "// TAT 1.0 " img-id) f)
        (newline f)
        (display (string-append "// this file generated by gimp texture-atlas-utils.scm") f)
        (newline f)

        ; write the list of files which contributed to this final image
        (if (> (length src-file-list) 0)
            (display (string-append "// SRC " src-files) f)
            (display "// no source images added" f)
        )
        (newline f)

        ; write the macro which maps each source image/layer to a position in the atlas
        (display (string-append "#define LIST_" img-id "_ATLAS(_)") f)
        (if (> (length atlas-list) 0)
            (display (string-append " \\\n_(" atlas-macro ")") f)
        )
        (newline f)

        (close-output-port f)
    )
)

;;
;; texture-atlas-insert! - attempt to insert the w,h rectangle into the free rect tree
;;
;; returns an offset pair or nil on failure
;; set-parent! is called to update the parent node in the event that a leaf rectangle is split
;;

(define (texture-atlas-insert! tree set-parent! w h)
    (let*
        (
            (offset nil)
        )

        (cond
            ((vector? tree) ; attempt to use this (x,y,w,h) rectangle leaf
                (let*
                    (
                        (fx (aref tree 0))
                        (fy (aref tree 1))
                        (fw (aref tree 2))
                        (fh (aref tree 3))
                    )

                    ; do we fit in this free node?
                    (if (and (>= fw w) (>= fh h))
                        (if (and (= fw w) (= fh h))
                            ; we fit exactly, delete the node and return the offset
                            (begin
                                ; mark the leaf empty (note that the empty node won't be deleted until the next insert)
                                (set-parent! nil)
                                ; return the offset
                                (set! offset (cons fx fy))
                            )
                            ; else split the free rectangle if it's bigger
                            (let*
                                (
                                    ; compute the remaining width and height (at least one of which should be > 0)
                                    (dw (- fw w))
                                    (dh (- fh h))
                                    ; create two new free rectangles
                                    (fits (vector fx fy fw fh))
                                    (left (vector fx fy fw fh))
                                    (node nil)
                                )

                                ; pick the best split direction
                                (if (> dw dh)
                                    ; remaining width is largest, split in x
                                    (begin
                                        (vector-set! fits 2 w) ; set free width to exactly w
                                        (vector-set! left 0 (+ fx w)) ; ajust x origin by w
                                        (vector-set! left 2 dw) ; set free width remaining
                                    )
                                    ; remaining height is largest, split in y
                                    (begin
                                        (vector-set! fits 3 h) ; set free height to exactly h
                                        (vector-set! left 1 (+ fy h)) ; ajust y origin by h
                                        (vector-set! left 3 dh) ; set free height remaining
                                    )
                                )

                                ; replace the parent with a new internal node composed of the two free rects
                                (set! node (cons fits left))
                                (set-parent! node)

                                ; insert into the rect which we know can contain the rectangle
                                (set! offset (texture-atlas-insert! fits (lambda (x) (set-car! node x)) w h))
                            )
                        )
                    )
                )
            )

            ((pair? tree) ; check both children of this internal node
                (cond
                    ; the first two null? conditions delete empty nodes from the tree
                    ((null? (car tree))
                        ; if the first child is an empty leaf, delete it and try the second one
                        (set-parent! (cdr tree))
                        ; attempt to fit into the other node using the grandparent's setter
                        (set! offset (texture-atlas-insert! (cdr tree) set-parent! w h))
                    )
                    ((null? (cdr tree))
                        ; if the second child is an empty leaf, delete it and try the first one
                        (set-parent! (car tree))
                        ; attempt to fit into the other node using the grandparent's setter
                        (set! offset (texture-atlas-insert! (car tree) set-parent! w h))
                    )
                    (else ; both children are valid
                        ; attempt to fit into the first node
                        (set! offset (texture-atlas-insert! (car tree) (lambda (x) (set-car! tree x)) w h))
                        ; if it didn't fit, attempt the second node
                        (if (null? offset)
                            (set! offset (texture-atlas-insert! (cdr tree) (lambda (x) (set-cdr! tree x)) w h))
                        )
                    )
                )
            )
        )

        ; return the position if we found it, or nil if not
        offset
    )
)

;;
;; texture-atlas-layout-rect! - return a pair x,y offset to the desired rectangle, updating the atlas, or exit on failure
;;

(define (texture-atlas-layout-rect! exit atlas w h)
    (let*
        (
            ; attempt to insert the w,h rectangle into the atlas tree
            (offset (texture-atlas-insert! (car atlas) (lambda (x) (set-car! atlas x)) w h))
        )

        (if (null? offset) (fail exit "(" w "x" h ") image too big for atlas!"))
        offset
    )
)

;;
;; make-pot - add double border and convert a number to the next >= power of 2 if align-pot is true
;;

(define (make-pot x border align-pot)
    (set! x (+ x border border))
    (if (x->boolean align-pot)
        (let ((pot 1))
            (while (< pot x) (set! pot (+ pot pot)))
            (set! x pot)
        )
    )
    x
)

;;
;; texture-atlas-layout-layers! - arrange the layers from a source image into the current drawable and update the atlas
;; 

(define (texture-atlas-layout-layers! exit atlas img from-file border align-pot skip-pat)
    ;(dbug (list 'texture-atlas-layout-layers! exit atlas img from-file border align-pot skip-pat))
    (let*
        (
            (drawable (car (gimp-image-get-active-drawable img)))
            (from-img (car (gimp-file-load RUN-NONINTERACTIVE from-file from-file)))
        )
        ;(dbug "layout loaded " from-file " as " from-img)
        (if (> from-img 0)
            (let*
                (
                    (from-img-id (make-image-id from-img))
                    (layers (gimp-image-get-layers from-img))
                    (num-layers (car layers))
                    (layer-array (cadr layers))
                    (i 0)
                )
                ;(dbug num-layers " layers: " layer-array)
                (while (< i num-layers)
                    (let*
                        (
                            (layer (aref layer-array i))
                            (n (car (gimp-drawable-get-name layer)))
                        )
                        (if (not (re-match skip-pat n))
                            (let*
                                (
                                    ; get the size of the source layer
                                    (w (car (gimp-drawable-width layer)))
                                    (h (car (gimp-drawable-height layer)))

                                    ; layout the properly aligned width and height
                                    (layout (texture-atlas-layout-rect! exit atlas (make-pot w border align-pot) (make-pot h border align-pot)))
                                    ; get the offset from the returned pair
                                    (x (+ (car layout) border))
                                    (y (+ (cdr layout) border))

                                    ; copy the source layer to the destination image
                                    (layer-copy (car (gimp-layer-new-from-drawable layer img)))

                                    ; generate an uppercase programmatic name for the sub image
                                    (N (string-remap char-upcase n))
                                )

                                ;(dbug "layer " n " (" x "," y ")-(" w "x" h ")")

                                ; merge the copied layer into the destination image at the x,y offset
                                (gimp-image-add-layer img layer-copy -1)
                                (gimp-layer-set-offsets layer-copy x y)
                                (gimp-drawable-set-visible layer-copy TRUE)
                                (gimp-image-merge-down img layer-copy EXPAND-AS-NECESSARY)

                                ; add a record of the placement to the list of component images in the atlas
                                (set-car! (cdr atlas) (append (cadr atlas) (list (list (length (cadr atlas)) from-img-id N x y w h))))
                            )
                            ; else
                            ;(dbug "skipping layer " n)
                        )
                        (set! i (+ i 1))
                    )
                )

                ; this cleans up gimp's records of the from-image we opened, it doesn't really delete the file (bad name!)
                (gimp-image-delete from-img)

                ; add a record of this image to the atlas
                (set-car! (cddr atlas) (append (caddr atlas) (list (list from-file border align-pot skip-pat))))
            )
            (fail exit "Failed to load \"" from-file "\"")
        )
    )
)

;;
;; texture-atlas-refresh! - read the .tat file and re-layout the images, skipping any filenames that match a pattern
;;

(define (texture-atlas-refresh! exit atlas img drawable skip-file-pat)
    (let*
        (
            (tat-file (list-ref atlas 4))
        )

        ;(dbug "refreshing from " tat-file)
        ; layout the image/layers for each from-file record returned by texture-atlas-read
        (for-each
            (lambda (rec)
                (let*
                    (
                        (from-file (car rec))
                        (border (string->number (cadr rec)))
                        (align-pot (string->number (caddr rec)))
                        ; stringify the layer skip pattern in case it had spaces
                        (skip-pat (join "\\s+" (cdddr rec)))
                    )
                    ;(dbug "refreshing " from-file " border=" border " align=" align-pot " skip=\"" skip-pat "\"")
                    (if (not (re-match from-file skip-file-pat))
                        (texture-atlas-layout-layers! exit atlas img from-file border align-pot skip-pat)
                    )
                )
            )
            (texture-atlas-read tat-file)
        )
    )
)

;;
;; make-image-id - generate an uppercase image identifier string from a gimp image
;;

(define (make-image-id img)
    ; strip any suffix off of the image name and convert to upper case
    (string-remap char-upcase (re-subs "\\.[^\\.]+$" "" (car (gimp-image-get-name img))))
)

;;
;; init-atlas - create an empty atlas for an image and clear the drawable
;;

(define (init-atlas exit img drawable)
    (let*
        (
            (tat-file (car (gimp-image-get-filename img)))
            (img-id (make-image-id img))
            (width (car (gimp-drawable-width drawable)))
            (height (car (gimp-drawable-height drawable)))
            ; the free-rectangle list starts as a simple rectangle
            ; as the rectangle splits the nodes become pairs of other pairs/rectangles
            (atlas-free (vector 0 0 width height))
        )

        ; make sure the drawable is visible so the merge down works as expected
        (gimp-drawable-set-visible drawable TRUE)
        ; since we re-run the layout proceedure for every source image, clear the drawable first
        (gimp-edit-clear drawable)

        ; create the .tat filename based on the destination image's name
        (if (< (string-length tat-file) 1) (fail exit "Destination image must be saved to disk first!"))
        ; always append the .tat even if there's no extension on the base filename
        (set! tat-file (string-append (re-subs "\\.[^\\.]+$" "" tat-file) ".tat"))

        ; the atlas consists of:
        ; ----------------------
        ; the free rectangle list, initially the spanning rectangle for the destination image
        ; the allocations list, initially empty
        ; the source files list, initially empty
        ; a target identifier string
        ; the tat filename

        (list atlas-free nil nil img-id tat-file)
    )
)

;;
;; texture-atlas-add-layers-from-image - main entrypoint to add the layers from a source image into the current image
;;

(define (texture-atlas-add-layers-from-image img drawable from-file border align-pot clear-atlas skip-pat)
    ;(dbug (list 'texture-atlas-add-layers-from-image img drawable from-file border align-pot clear-atlas skip-pat))
    (let*
        (
            (atlas nil)
        )

        (gimp-image-undo-group-start img)

        ; handle errors with scheme's try-catch mechanism
        (call-with-current-continuation
            (lambda (exit)

                ; setup an empty atlas and clear the image's drawable
                (set! atlas (init-atlas exit img drawable))

                ; load any existing .tat file unless the user elected to start over
                ;(dbug "clear-atlas " (x->boolean clear-atlas))
                (if (not (x->boolean clear-atlas))
                    (texture-atlas-refresh! exit atlas img drawable (string-append "^" from-file "$"))
                )

                ;(dbug "laying out user image " from-file)
                ; add in the new image/layers specified by the user
                (texture-atlas-layout-layers! exit atlas img from-file border align-pot skip-pat)

                ;(dbug "writing atlas")
                ; write out the new atlas
                (texture-atlas-write exit atlas)
            )
        )

        ;(dbug "done!")
        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

;;
;; texture-atlas-remove-layers-from-image - remove the specified file from the list of sources in the texture atlas
;;

(define (texture-atlas-remove-layers-from-image img drawable remove-file)
    ;(dbug (list 'texture-atlas-remove-layers-from-image img drawable remove-file))
    (let*
        (
            (atlas nil)
        )

        (gimp-image-undo-group-start img)

        ; handle errors with scheme's try-catch mechanism
        (call-with-current-continuation
            (lambda (exit)

                ; setup an empty atlas and clear the image's drawable
                (set! atlas (init-atlas exit img drawable))

                ; reload the existing .tat, skipping the user-specified source image
                (texture-atlas-refresh! exit atlas img drawable (string-append "^" remove-file "$"))

                ; write out the new atlas
                (texture-atlas-write exit atlas)
            )
        )

        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

;;
;; texture-atlas-rebuild-image - rebuild the texture atlas
;;

(define (texture-atlas-rebuild-image img drawable)
    ;(dbug (list 'texture-atlas-rebuild-image img drawable))
    (let*
        (
            (atlas nil)
        )

        (gimp-image-undo-group-start img)

        ; handle errors with scheme's try-catch mechanism
        (call-with-current-continuation
            (lambda (exit)

                ; setup an empty atlas and clear the image's drawable
                (set! atlas (init-atlas exit img drawable))

                ; reload the existing .tat
                (texture-atlas-refresh! exit atlas img drawable "^$")

                ; write out the new atlas
                (texture-atlas-write exit atlas)
            )
        )

        (gimp-image-undo-group-end img)
        (gimp-displays-flush)
    )
)

;;
;; register menu items
;;

(script-fu-register "texture-atlas-add-layers-from-image"
    "<Image>/Image/Texture Atlas/Add Image Layers to Atlas..."
    "Add an image's layers to the current image's texture atlas"
    "Edward Hutchins"
    "Edward Hutchins"
    "2009"
    "RGB RGBA GRAY GRAYA"
    SF-IMAGE        "Image to use"          0
    SF-DRAWABLE     "Layer to use"          0
    SF-FILENAME     "Image to add"          ""
    SF-ADJUSTMENT   "Border"                '(0 0 256 1 16 0 SF-SLIDER)
    SF-TOGGLE       "Align to 2^N"          FALSE
    SF-TOGGLE       "Clear atlas"           FALSE
    SF-STRING       "Skip layers matching"  "\\s+copy"
)

(script-fu-register "texture-atlas-remove-layers-from-image"
    "<Image>/Image/Texture Atlas/Remove Image Layers from Atlas..."
    "Remove an image's layers from the current image's texture atlas"
    "Edward Hutchins"
    "Edward Hutchins"
    "2009"
    "RGB RGBA GRAY GRAYA"
    SF-IMAGE        "Image to use"          0
    SF-DRAWABLE     "Layer to use"          0
    SF-FILENAME     "Image to remove"       ""
)

(script-fu-register "texture-atlas-rebuild-image"
    "<Image>/Image/Texture Atlas/Rebuild Atlas..."
    "Rebuild the current image's texture atlas"
    "Edward Hutchins"
    "Edward Hutchins"
    "2009"
    "RGB RGBA GRAY GRAYA"
    SF-IMAGE        "Image to use"          0
    SF-DRAWABLE     "Layer to use"          0
)

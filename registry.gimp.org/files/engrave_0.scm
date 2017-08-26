; Version 1.1, 2009-Apr-09
; Copyright: Pierre Lewis (leware at globetrotter dot net), GPLv3

; TODO: make sure full white remains full, idem full black
;       check hop behavior at 3

; From Script-Fu template, by  (C) 2004 Simon Budig <simon@gimp.org>
; Developed under GIMP 2.6.1, then 2.6.6 on Windows (TinyScheme),
; no idea what fun and games it might be to use on other versions

; Main help: http://pages.infinit.net/leware/engrave.html

; Note: the "lew" in function name is only to reduce the risk of collisions

(define (script-fu-lew-engrave image drawable pre-process lh gamma interpol option version2 blur early)

   (let* (
           (levels-white-thresh 1)   ; threshhold for final levels -- 1 is value used if not version 2
           ; (scale  (pow 255.001 gamma))  ; to scale to 0..1 range -- no longer used
           (width  (car  (gimp-drawable-width drawable)))
           (height (car  (gimp-drawable-height drawable)))
           (fwidth  1)    ; final image values (set later)
           (fheight 1)
           ; (x0     (car  (gimp-drawable-offsets drawable)))
           ; (y0     (cadr (gimp-drawable-offsets drawable)))
           ;        ^^^^ - here we pick the second element of the returned list...
           (mask-layer 0)
           (hack  (+ (trunc (/ lh 2)) 1))  ; need to rotate the mask because of a strange behavior of gimp-layer-scale-full with INTERPOLATION-NONE in 2.6.1, fixed in 2.6.6, so hack undone below
                                           ;              rows
                                           ; scale by 3   2 3 4     2 3 3 4
                                           ; scale by 4   3 4 5     3 4 4 5
                                           ;          5   3
                                           ;          6   4
                                           ;          7   4
                                           ;          8   5
                                           ; So:   +  trunc  / lh 2   1       --  for more info on bug: http://bugzilla.gnome.org/show_bug.cgi?id=576123
         )

       ; (gimp-message (strcat "width=" (number->string width 10) " height=" (number->string height 10) ))
       ; (gimp-message (strcat "scale=" (number->string scale 10) " gamma=" (number->string gamma 10) ))
       ; (gimp-message (strcat "option=" (number->string option 10) ))
       ; (gimp-message (strcat "version2=" (number->string version2 10) " early=" (number->string early 10) ))

       (set! interpol (- 2 interpol))   ; flip value: 0,1,2 --> 2,1,0, the idea being to put the usual "cubic" as default.

       (set! hack 0)            ; make into comment if 2.6.1 hack needed; this line disables hack for 2.6.6, no longer needed

       (gimp-image-undo-group-start image)

          ; if color, convert to grayscale
          (if (= (car (gimp-drawable-is-gray drawable)) 0)
             (gimp-image-convert-grayscale image)
          )

          (if (> version2 0)    ; I think need to treat bools as numbers -- "(if version2 ..." is perhaps just a presence check in TinyScheme?)
             (begin
                (gimp-levels drawable 0 0 255 (/ 1.0 gamma) 0 255)   ; apply reverse gamma on the initial image -- changing output levels here might allow to insure we always have a black or white line
                (set! gamma 1.0)  ; make it 1 so the rest of the code doesn't apply it again, and the mask gets equal-spaced levels
             )
          )

          (if (= pre-process 0)          ; scale down if not done prior to calling script (do after gamma adjustment)
             (begin
                (if (< width lh)        ; protection against silly uses
                   (set! width lh)
                )
                (if (< height lh)
                   (set! height lh)
                )
                (gimp-layer-scale drawable (/ width lh) (/ height lh) FALSE)
                (set! width  (car  (gimp-drawable-width drawable)))    ; get the new values for use below
                (set! height (car  (gimp-drawable-height drawable)))
             )
          )

          (if (= pre-process 1)          ; scale down if not done prior to calling script (do after gamma adjustment)
             (begin
                (if (< height lh)
                   (set! height lh)
                )
                (gimp-layer-scale drawable width (/ height lh) FALSE)
                (set! height (car  (gimp-drawable-height drawable)))
                (set! fwidth width) ; special case
             )
             (set! fwidth (* width lh))
          )

          (set! fheight (* height lh)) ; always

          (gimp-image-resize  image  fwidth fheight 0 0)  ; normal case

          (if (not (= pre-process 1))   ; don't do horizontal scale if pre-process = downsize vertical
             (gimp-layer-scale-full drawable fwidth  height  FALSE interpol)  ; was INTERPOLATION-CUBIC -- LANCZOS no good because it spills, altho if done first, perhaps not a big issue
          )

          (gimp-layer-scale-full drawable fwidth fheight FALSE INTERPOLATION-NONE)

          (set! mask-layer (car
            (gimp-layer-new  image  fwidth  fheight  GRAY-IMAGE "Engrave temporary" 100. SUBTRACT-MODE)
            ; no-better (gimp-layer-new-from-drawable drawable image)
          ) )

          (gimp-image-add-layer image mask-layer -1)

          (gimp-drawable-fill mask-layer WHITE-FILL)

          (gimp-image-set-active-layer image mask-layer)

          (gimp-rect-select image 0 0 1 lh CHANNEL-OP-REPLACE FALSE 0.0)

            (let* ( (y 0) (yp 0) (yprh 0) (ds -1) (di 1) (rgb (cons-array 1 'byte)) (offset 0.03) )

                ; offset is to adjust the blacks to get a better gamma (still work-in-progress)
                ; y goes from 0 to (lh-1), with increasing pixel values (in one case below, it goes backwards)
                ; yp is the desired pixel position; for the "center" options, it starts near center, then hops before and after until the edges are reached
                ;                                   for the other two, it tracks y (in one case, y==yp, so it's not even used)
                ; yprh is a rotation of above yp within the lh window, because of a bug in GIMP 2.6.1. In 2.6.6, yprh is in effect same as yp.
                ; di and ds are used to execute the hop (walk the code, no pun, to grok this)
                ; rgb is the greyscale pixel value (notwithstanding the "RBG" name)

                (if (= version2 0)
                   (set! offset 0.5) ; this value works better in version 1
                )

               (if (= option 0)  ; Center black    (brightest mask line in middle)
                  (begin
                     (set! y (- lh 1))            ; y decreasing for this one (grey scale)
                     (set! yp (trunc (/ lh 2)))
                     (while (>= y 0)
                        (aset rgb 0 (trunc (* 255  (pow (/ (+ offset y) lh) (/ 1.0 gamma)) ) ) )  ; with gamma (the "pow" is NOP in version 2
                        (set! yprh (modulo (+ yp hack) lh))  ; rotate hack - was yp
                        (gimp-drawable-set-pixel mask-layer 0 yprh (car (gimp-drawable-bpp mask-layer)) rgb)
                        (set! y (- y 1))
                        (set! yp (+ yp (* ds di)))  ; hop in growing zig-zag
                        (set! di (+ di 1))
                        (set! ds (- 0 ds))
                     )
                  )
               )
               (if (= option 1)  ; Center white    (brightest mask line on one edge)
                  (begin
                     (set! yp (trunc (/ lh 2)))
                     (while (< y lh)
                        (aset rgb 0 (trunc (* 255  (pow (/ (+ offset y) lh) (/ 1.0 gamma)) ) ) )  ; with gamma
                        (set! yprh (modulo (+ yp hack) lh))  ; rotate hack - was yp
                        (gimp-drawable-set-pixel mask-layer 0 yprh (car (gimp-drawable-bpp mask-layer)) rgb)
                        (set! y (+ y 1))
                        (set! yp (+ yp (* ds di)))  ; hop in growing zig-zag
                        (set! di (+ di 1))
                        (set! ds (- 0 ds))
                     )
                  )
               )
               (if (= option 2)  ; Black on bottom (brightest mask line at bottom)
                  (begin
                     (while (< y lh)              ; no yp needed here
                        ; (aset rgb 0 (trunc (* 255 (/ (+ offset y) lh))) )         ; linear
                        (aset rgb 0 (trunc (* 255  (pow (/ (+ offset y) lh) (/ 1.0 gamma)) ) ) )  ; with gamma
                        (set! yprh (modulo (+ y hack) lh))  ; rotate hack - was y
                        (gimp-drawable-set-pixel mask-layer 0 yprh (car (gimp-drawable-bpp mask-layer)) rgb)
                        (set! y (+ y 1))
                     )
                  )
               )
               (if (= option 3)  ; Black on top    (brightest mask line at top)
                  (begin
                     (set! yp (- lh 1))
                     (while (< y lh)
                        (aset rgb 0 (trunc (* 255  (pow (/ (+ offset y) lh) (/ 1.0 gamma)) ) ) )  ; with gamma
                        (set! yprh (modulo (+ yp hack) lh))  ; rotate hack - was yp
                        (gimp-drawable-set-pixel mask-layer 0 yprh (car (gimp-drawable-bpp mask-layer)) rgb)
                        (set! yp (- yp 1))
                        (set! y (+ y 1))
                     )
                  )
               )
               ; may want symetrical versions of line in center?

            )

            (gimp-edit-copy mask-layer)

          (gimp-selection-all image)  ; needed for bucket fill it seems

            (gimp-context-set-pattern (list-ref (cadr (gimp-patterns-get-list "")) 0))  ; multi-lingual equivalent of (gimp-context-set-pattern "Clipboard") : "clipboard" is always first

            (gimp-edit-bucket-fill mask-layer PATTERN-BUCKET-FILL NORMAL-MODE 100. 0. FALSE 0. 0.)

          (gimp-selection-none image)

          (if (< early 1)     ; skip these last two steps if early exit asked for
             (begin
                (set! mask-layer (car
                   (gimp-image-merge-down image mask-layer CLIP-TO-IMAGE)   ; returns a new layer!
                ) )

                (if (> version2 0)
                   (set! levels-white-thresh (trunc (/ 255 lh) ) )    ; assumes gamma now 1, ie. spacing is regular in the mask values
                )

                (gimp-levels mask-layer 0 0 levels-white-thresh 1.0 0 255)
                ; (gimp-levels mask-layer 0 0 (trunc (/ 100 lh)) 1.0 0 255)

                (if (> blur 1)
                   (plug-in-gauss 1 image mask-layer blur 1.0 1)  ; only horiz
                )
             )
             (gimp-message "Early exit: merge down & levels left to do")
          )

          (gimp-image-set-active-layer image mask-layer)  ; for engraveDoc -- not sure needed

       (gimp-image-undo-group-end image)

       ; finally we notify the UI that something has changed.

       (gimp-displays-flush)
   )
)


; Each pixel row of original (once scaled down if "pre-scaled" not selected) becomes a line (group of rows), so start with a rather small image (eg. 300x200)

(script-fu-register "script-fu-lew-engrave"
                    "<Image>/Filters/Artistic/Engrave..."
                    "Transform small grey image into horiz lines of varying widths"
                    "Pierre Lewis  <leware@globetrotter.net>"
                    "Pierre Lewis"
                    "2009-03-03"
                    ""  ; was: "GRAY*, RGB*"
                    SF-IMAGE "Input Image" 0
                    SF-DRAWABLE "Input Drawable" 0
                    SF-OPTION     "Pre-processing"  '("Downscale both" "Downscale vertical" "Pre-scaled")    ; default prevents users from accidently creating huge images
                    SF-ADJUSTMENT "Line width"   '(25 3 127 1 5 0 1)     ; List (start-value min-value max-value small-step large-step [int=0 or float=1] [slider=0 or roll-box=1]) -- IMPORTANT: don't make the minimum less than 3, the hop code will probably fail
                    SF-ADJUSTMENT "Gamma"        '(1.9 0.5 3.5 0.1 1.0 1 1)
                    SF-OPTION     "Horizontal interpolation" '("Cubic" "Linear" "None")   ; lanczos bleeds on rows above and below
                    SF-OPTION     "Line type"    '("Center black" "Center white" "Black on bottom" "Black on top")
                    SF-TOGGLE     "Version 2"    TRUE
                    SF-ADJUSTMENT "Blur radius"  '(3 1 9 1 2 0 1)     ; List (start-value min-value max-value small-step large-step [int=0 or float=1] [slider=0 or roll-box=1])
                    SF-TOGGLE     "Early exit"   FALSE
)

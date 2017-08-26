; -------------------------------------------
; Variable initialization
; -------------------------------------------
(define int_ImageHeight)
(define int_ImageWitdth) 
(define img_PatternImage)
(define drw_PatternImage)
(define Color)
(define TextColor)
(define Percentage)

; ----------------------------
; my-truncate float to integer and convert to string
; ----------------------------
(define (my-truncate inOrigValue inDividedValue inSymbCnt)
 (if (> 1 inDividedValue)
  (substring (number->string inOrigValue) 0 (+ inSymbCnt 1))
  (my-truncate inOrigValue (/ inDividedValue 10) (+ inSymbCnt 1))
 )
)

; ----------------------------
; loopY executes loopX, loopX draw the pattern
; ----------------------------
(define (loopY inX inY inImage inDraw inThumbSize)
 (if (< inY 0)
  inY
  (begin
   ; Executing loopX
   (loopX inX inY inImage inDraw inThumbSize)
   ; Iteration step
   (loopY inX (- inY 1) inImage inDraw inThumbSize)
  )
 )
)

; ----------------------------
; loopX for drawing one box line
; ----------------------------
(define (loopX inX inY inImage inDraw inThumbSize)
 (if (< inX 0)
  inX
  (begin
   ; Calculating color for the box
   (set! Color (- 255 (* (/ 255 47) (+ inX (* inY 6)))))
   ; Drawing the box
   (gimp-palette-set-foreground (list Color Color Color) )
   (gimp-selection-none inImage)
   (gimp-rect-select inImage (* inX inThumbSize) (* inY inThumbSize) inThumbSize inThumbSize CHANNEL-OP-ADD 0 0)
   (gimp-edit-fill inDraw FOREGROUND-FILL)
   (gimp-selection-none inImage)

   ; Drawing text "% of BLACK"
   (set! TextColor 0)
   (if (< Color 128)
     (set! TextColor 255)
   )
   (gimp-palette-set-foreground (list TextColor TextColor TextColor) )
   (set! Percentage (/ (* Color 100) 255))
   (gimp-text-fontname inImage inDraw (+ (* inX inThumbSize) (/ inThumbSize 8)) (- (+ (* inY inThumbSize) inThumbSize) (/ inThumbSize 5) (/ inThumbSize 8)) (my-truncate Percentage (/ Percentage 10) 0) -1 TRUE (/ inThumbSize 5) 0 "Courier New Bold")
   (gimp-selection-none inImage)

   ; Iteration step
   (loopX (- inX 1) inY inImage inDraw inThumbSize)
  )
 )
)

; ----------------------------
; Main function
; ----------------------------
(define (bw-dnegative-calibration-pattern in_thumb_size)

 ; Calculate size of the image
 (set! int_ImageHeight (* in_thumb_size 8) )
 (set! int_ImageWitdth (* in_thumb_size 6) )

 ; Create image and background layer
 (set! img_PatternImage (car (gimp-image-new int_ImageWitdth int_ImageHeight 0) ) )
 (set! drw_PatternImage (car (gimp-layer-new img_PatternImage int_ImageWitdth int_ImageHeight 0 "Pattern" 100 0) ) )

 ; Add created layer to the image
 (gimp-image-undo-disable img_PatternImage)
 (gimp-image-add-layer img_PatternImage drw_PatternImage 0)

 ; Fill layer with WHITE
 (set! drw_PatternImage (car (gimp-image-get-active-layer img_PatternImage) ) )
 (gimp-selection-all img_PatternImage)
 (gimp-palette-set-foreground '(0 0 0) )
 (gimp-palette-set-background '(255 255 255) )
 (gimp-edit-fill drw_PatternImage BACKGROUND-FILL)
 (gimp-selection-none img_PatternImage)

 ; Drawing the pattern
 (loopY 5 7 img_PatternImage drw_PatternImage in_thumb_size)
 (gimp-image-flatten img_PatternImage)
 
 ; Terminate
 (gimp-image-undo-enable img_PatternImage)
 (gimp-display-new img_PatternImage)

 ;(gimp-context-pop)
)

; ----------------------------
; Regestering Script
; ----------------------------
(script-fu-register
 "bw-dnegative-calibration-pattern"			;func name
 "Digital Negative Calibration Pattern"			;menu label
 "Digital Negative Calibration Pattern"			;description
 "Eriks Zelenka <isindir@users.sourceforge.net>"	;author
 "copyright 2006, Eriks Zelenka"			;copyright notice
 "November 17, 2006"					;date created
 "RGB*"							;image type that the script works on
 SF-VALUE	"Thumbnail Size"	"50"		;parameter
)

; ----------------------------
; Regestering Main window menu item
; ----------------------------
(script-fu-menu-register "bw-dnegative-calibration-pattern" "<Toolbox>/Xtns/Script-Fu/Utils")

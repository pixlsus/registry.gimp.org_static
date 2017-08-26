; Spaced V-Guides
; This function creates a series of v-guides in the image at the user specified
; spacing
(define (script-fu-spaced-vguides image spacing origin)

   (let*
      (
         (x 0) ; will be computed v-guide position.
	      (width (car (gimp-image-width image)))
         (center (/ width 2))
      )

      (gimp-context-push) ; Initiates the temporary state.

      ; Calculate starting point
      (set! x (calc-offset origin center width spacing))

      ; Iterate and create h-guides until y > height
      (while (< x width)
         (gimp-image-add-vguide image x)
         (set! x (+ x spacing))
      )

      ; DEBUG
      ;(gimp-message 
      ;   (string-append "Origin: " (number->string origin) " \nSpacing: " (number->string spacing)
      ;   "\nCenter: " (number->string center) "\nMod: " y
      ;   )
      ;)
      ; DEBUG

   	; Deactivates the temporary state and resets the previous user defaults.
      (gimp-context-pop) 
      (gimp-displays-flush)
   ) ; close let*
) ; close define


(define (calc-offset origin center width spacing)
   (cond 
      ((<= origin 0)  spacing) ; Left 
      ((<= origin 1)  (modulo center spacing)) ; Center
      ((<= origin 2)  (modulo width spacing)) ; Right
   )
)


; Finally register our script with script-fu.
(script-fu-register
   "script-fu-spaced-vguides"                   ;func name
   "Spaced V-Guides"                            ;menu label
   ;description
   "Create a series of vertical guides at even spacing"
   "Jeffrey Boulais"                           ;author
   "copyright 2009, Jeffrey Boulais"           ;copyright notice
   "February 2008"                             ;date created
   "RGB* GRAY* INDEXED*"                       ;image type that the script works on
   SF-IMAGE "Image" 0
   SF-VALUE "Vertical Guide Spacing (px)" "100"
   SF-OPTION "Start from" '("Left" "Center" "Right")
)
(script-fu-menu-register "script-fu-spaced-vguides" "<Image>/Image/Guides")
; Spaced H-Guides
; This function creates a series of h-guides in the image at the user specified
; spacing
(define (script-fu-spaced-hguides image spacing origin)

   (let*
      (
         (y 0) ; will be computed h-guide position.
	      (height (car (gimp-image-height image)))
         (center (/ height 2))
      )

      (gimp-context-push) ; Initiates the temporary state.

      ; Calculate starting point
      (set! y (calc-offset origin center height spacing))

      ; Iterate and create h-guides until y > height
      (while (< y height)
         (gimp-image-add-hguide image y)
         (set! y (+ y spacing))
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


(define (calc-offset origin center height spacing)
   (cond 
      ((<= origin 0)  spacing) ; Top 
      ((<= origin 1)  (modulo center spacing)) ; Center
      ((<= origin 2)  (modulo height spacing)) ; Bottom
   )
)


; Finally register our script with script-fu.
(script-fu-register
   "script-fu-spaced-hguides"                   ;func name
   "Spaced H-Guides"                            ;menu label
   ;description
   "Create a series of horizontal guides at even spacing."
   "Jeffrey Boulais"                           ;author
   "copyright 2009, Jeffrey Boulais"           ;copyright notice
   "February 2008"                             ;date created
   "RGB* GRAY* INDEXED*"                       ;image type that the script works on
   SF-IMAGE "Image" 0
   SF-VALUE "Horizontal Guide Spacing (px)" "100"
   SF-OPTION "Start from" '("Top" "Center" "Bottom")
)
(script-fu-menu-register "script-fu-spaced-hguides" "<Image>/Image/Guides")
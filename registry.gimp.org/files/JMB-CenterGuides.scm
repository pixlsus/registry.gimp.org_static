; Center Guides
; This function puts a horizontal and vertical guide at the center of the 
; canvas.
(define (script-fu-center-guides image)

   (let*
      (
	      (height (car (gimp-image-height image)))
	      (width (car (gimp-image-width image)))
         (ypos (/ height 2))
	      (xpos (/ width 2))
      )

      (gimp-context-push) ; Initiates the temporary state.

      (gimp-image-add-vguide image xpos)
      (gimp-image-add-hguide image ypos)

      ; Cleanup
      (gimp-context-pop) ; Deactivates the temporary state and resets the previous user defaults.
      (gimp-displays-flush)
   )

)


; Finally register our script with script-fu.
(script-fu-register
   "script-fu-center-guides"                   ;func name
   "Center Guides"                             ;menu label
   ;description
   "Create guides at the center of the canvas" 
   "Jeffrey Boulais"                           ;author
   "copyright 2009, Jeffrey Boulais"           ;copyright notice
   "October 13, 2009"                          ;date created
   "RGB* GRAY* INDEXED*"                       ;image type that the script works on
   SF-IMAGE "Image" 0
)
(script-fu-menu-register "script-fu-center-guides" "<Image>/Image/Guides")
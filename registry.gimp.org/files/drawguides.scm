; Draw guides v1.0
; Tested on Gimp v2.6.8
(define (script-fu-draw-guides image drawable)
  ; Start an undo group
  (gimp-image-undo-group-start image)

  (let* ((guide 0)
          (width (car (gimp-image-width image)))
          (height (car (gimp-image-height image)))
         )
    (while (<> 0 (set! guide (car (gimp-image-find-next-guide image guide))))
      (let* ((orientation (car (gimp-image-get-guide-orientation image guide)))
            (pos (car (gimp-image-get-guide-position image guide))))
        (if (= orientation 0)
          (gimp-pencil drawable 4 (vector 0 pos width pos))
          (gimp-pencil drawable 4 (vector pos 0 pos height))
        )
      )
    )
  )

  ; End the undo group
  (gimp-image-undo-group-end image)

  ; Flush displays
  (gimp-displays-flush)
)

(script-fu-register "script-fu-draw-guides"
  "<Image>/Filters/Render/Pattern/Draw guides"
  "Draw guides"
  "Frédéric BISSON"
  "Frédéric BISSON"
  "18/05/2010"
  "*"
  SF-IMAGE "Image"  0
  SF-DRAWABLE "Drawable" 0
)

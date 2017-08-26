; Descreen v1.0
; Tested on Gimp v2.6.8
(define (script-fu-descreen image drawable sensitivity selectiongrowth despeckle middle-ratio)
  ; This script requires the FFT plug-in which provides FFT forward and FFT inverse filter
  (if (not (defined? 'plug-in-fft-dir))
    (begin
      ; Display an error message
      (gimp-message "This script requires the FFT plug-in")

      ; Abort the script
      (quit)
    )
  )

  ; Start an undo group
  (gimp-image-undo-group-start image)

  ; Save the context (FFT plug-in change the foreground color)
  (gimp-context-push)

  ; Ensure the layer has no alpha channel (FFT plug-in does not work well with them)
  (gimp-layer-flatten drawable)

  ; Apply the FFT forward filter
  (plug-in-fft-dir RUN-NONINTERACTIVE image drawable)

  (let* (
          ; Duplicate the layer containing the image FFTed
          (detection (car (gimp-layer-copy drawable FALSE)))

          ; Add the new layer to the image
          (exec (gimp-image-add-layer image detection -1))

          ; Calculate the low threshold given the sensitivity (low-threshold=256-sensitivity)
          (low-threshold (- 256 sensitivity))

          ; Determine the dimensions of the middle zone that should remain untouched
          (middle-width  (/ (car (gimp-image-width  image)) middle-ratio))
          (middle-height (/ (car (gimp-image-height image)) middle-ratio))

          ; Calculate the middle of the picture
          (middle-x (/ (car (gimp-image-width  image)) 2))
          (middle-y (/ (car (gimp-image-height image)) 2))

          ; gimp-ellipse-select needs the upper left coordinates of the ellipse
          (middle-x (- middle-x (/ middle-width  2)))
          (middle-y (- middle-y (/ middle-height 2))))

    ; Keep only strong frequencies
    (gimp-threshold detection low-threshold 255)

    ; Select the points
    (gimp-by-color-select detection '(255 255 255) 0 0 0 FALSE 0 FALSE)

    ; Grow the selection
    (gimp-selection-grow image selectiongrowth)

    ; Remove the middle of the picture from the selection
    (gimp-ellipse-select image middle-x middle-y middle-width middle-height 1 FALSE FALSE 0)

    ; Fill the picture according to the selection.
    ; Foreground color has been set by the FFT plugin to #808080
    (gimp-edit-fill drawable 0)

    ; Delete the detection layer
    (gimp-image-remove-layer image detection)

    ; Unselect
    (gimp-selection-none image)
  )

  ; Do an FFT inverse on the picture
  (plug-in-fft-inv RUN-NONINTERACTIVE image drawable)

  ; Check if despeckle has been selected
  (if despeckle
    ; Do a last despeckle to remove small points
    (plug-in-despeckle RUN-NONINTERACTIVE image drawable 1 0 -1 256)
  )

  ; Restore the original context
  (gimp-context-pop)

  ; End the undo group
  (gimp-image-undo-group-end image)
)

(script-fu-register "script-fu-descreen"
  "<Image>/Filters/Enhance/Descreen"
  "Descreen filter"
  "Frédéric BISSON"
  "Frédéric BISSON"
  "01/05/2010"
  "RGB*"
  SF-IMAGE "Image"  0
  SF-DRAWABLE "Drawable" 0

  ; Sensitivity to frequency level :
  ; 1=nearly insensitive, 128=sensitive to any frequency level
  SF-ADJUSTMENT "Sensitivity" '(72 1 128 1 16 0 SF-ROLLBOX)

  ; After the frequency level detection, the filter will affect
  ; everything within the range of the growth
  SF-VALUE  "Selection growth" "16"

  ; Do a final despeckle before ending the filter
  SF-TOGGLE "Despeckle" TRUE

  ; Consider the middle of the picture to be a ratio of the
  ; image dimensions.
  SF-VALUE  "Ratio for middle preservation" "5"
)

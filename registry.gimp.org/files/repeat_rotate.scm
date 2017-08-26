(script-fu-register
 "script-fu-repeat-rotate"
 "<Image>/Layer/Transform/repeated rotation"
 "Repeat layer rotation"
 "Annamaria Trincardi"
 "copyright 2004, Annamaria Trincardi"
 "2004-11-06"
 "*"
 SF-IMAGE 	"image"		0
 SF-DRAWABLE 	"drawable"	0
 SF-OPTION	"Frame" '("Image" "Layer" "Mask")
 SF-ADJUSTMENT 	"Angle" '(15 15 180 15 30 0 0)
 )

(define (script-fu-repeat-rotate 
         inImage 
         inDrawable
         inFrame
         inAngle
         )
  ;; init
  ; create image  
  ; (turn undo off)
  (gimp-image-undo-group-start inImage)
  ;create, add and clear layer 
  ;;main 
  ( let* (
          (theAngle inAngle)
          (theActiveLayer (car(gimp-image-get-active-layer inImage)) )
          (theLayerName (car(gimp-drawable-get-name theActiveLayer)) )
          (theLayer (car(gimp-layer-copy theActiveLayer TRUE)) )
          )
     (while (> 360 theAngle) 
            (gimp-image-add-layer inImage theLayer -1)
            (gimp-rotate theLayer FALSE (/ *pi* (/ 180 theAngle)) )
            (gimp-drawable-set-name theLayer
                                    (string-append theLayerName " " (number->string theAngle)) )
            (gimp-drawable-set-visible theLayer TRUE)
            (set! theAngle (+ inAngle  theAngle) )
            (set! theLayer (car(gimp-layer-copy theActiveLayer TRUE)) )
                     )
     )
  ;; exit
  ; (turn dirty off)
  ; display inImage
  (gimp-displays-flush)
  ; (turn undo on)
  (gimp-image-undo-group-end inImage)
  )

(define (script-fu-dodgeburnlayer image)
  (gimp-context-push);
  (gimp-image-undo-group-start image);
  (let* (
      (width (car (gimp-image-width image)))
      (height (car (gimp-image-height image)))
      (dblayer (car (gimp-layer-new image width height 0 "Dodge & Burn" 100 5)))
    )
    (gimp-context-set-foreground '(192 192 192))
    (gimp-image-add-layer image dblayer 0)
    (gimp-edit-bucket-fill dblayer 0 0 100 0 FALSE 0 0)
  );
  (gimp-image-undo-group-end image);
  (gimp-context-pop);
  (gimp-displays-flush);
)

(script-fu-register
  "script-fu-dodgeburnlayer"
  "<Image>/Filters/Light and Shadow/Dodge & Burn layer"
  "Add a dodge & burn layer"
  "Cyril Bosselut"
  "GNU GPL V3"
  "October 19, 2012"
  ""
  SF-IMAGE "Image" 0
)
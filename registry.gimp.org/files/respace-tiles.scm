
(define (script-fu-respace-tiles-removeSpacingMargin inImage inLayer tileSize margin spacing)

  (let* (
        (theWidth (car (gimp-drawable-width inLayer)))
        (theHeight (car (gimp-drawable-height inLayer)))
        (theImage (car(gimp-image-new theWidth theHeight RGB)))
        (theLayer)
        )

  (gimp-context-push)
  (gimp-image-undo-disable inImage)

  (set! theLayer (car (gimp-layer-new theImage theWidth theHeight
                                      RGBA-IMAGE
                                      "Tiles"
                                      100 NORMAL-MODE)))
  (gimp-image-add-layer theImage theLayer 0) 
  (gimp-edit-clear theLayer)
  (gimp-display-new theImage)  
  (gimp-image-undo-disable theImage) 
  (letrec (
    (loopTiles 
     (lambda(tx ty sx sy)
       (cond 
         ((> (+ tileSize sy) theHeight) #t) ;No more lines
         ((> (+ tileSize sx) theWidth) (loopTiles 0 (round (+ ty tileSize)) margin (round (+ sy tileSize spacing)) )) ;New line
         ((begin 
           (gimp-selection-clear inImage)
           (gimp-rect-select inImage (round sx) (round sy) tileSize tileSize 0 FALSE 0)

           (gimp-edit-copy-visible inImage)  
             (let (
               (newTile (car (gimp-edit-paste theLayer FALSE)))
               )  
              (gimp-layer-set-offsets newTile (round tx) (round ty))
             )
           (loopTiles (round (+ tx tileSize)) ty (round (+ sx tileSize spacing)) sy)         
          ))
      ))
   )

   
  )
  (loopTiles 0 0 margin margin)
  )
  (gimp-selection-clear inImage)   
  (gimp-floating-sel-anchor (car (gimp-image-get-floating-sel theImage)))
  (gimp-image-undo-enable theImage)
  (gimp-displays-flush) 
  (gimp-image-undo-enable inImage)
  (gimp-context-pop)
  )
)

(script-fu-register "script-fu-respace-tiles-removeSpacingMargin"
  _"_Remove spacing and margin"
  _"Remove spacing and margin from a tileset. "
  "Premik"
  "LGPL"
  "2010"
  ""
 SF-IMAGE    "Image"    0 
 SF-DRAWABLE "Drawable" 0
 SF-ADJUSTMENT _"TileSize"   '(32 2 1600 1 8 0 0)
 SF-ADJUSTMENT _"Margin current"   '(1 0 256 1 1 0 0)
 SF-ADJUSTMENT _"Spacing current"   '(2 0 256 1 1 0 0)
)

(script-fu-menu-register "script-fu-respace-tiles-removeSpacingMargin"
                         "<Image>/Filters/Map/TileSet")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-respace-tiles-addSpacingMargin inImage inLayer tileSize margin spacing)

  (let* (
        (theWidth (car (gimp-drawable-width inLayer)))
        (theHeight (car (gimp-drawable-height inLayer)))
        (theImage (car(gimp-image-new theWidth theHeight RGB)))
        (theLayer)
        )

  (gimp-context-push)
   (gimp-image-undo-disable inImage)
  (set! theLayer (car (gimp-layer-new theImage theWidth theHeight
                                      RGBA-IMAGE
                                      "Tiles"
                                      100 NORMAL-MODE)))
  (gimp-image-add-layer theImage theLayer 0) 
  (gimp-image-undo-enable theImage)
  (gimp-edit-clear theLayer)
  (gimp-display-new theImage)  

  (letrec (
    (loopTiles 
     (lambda(sx sy tx ty)
       (cond 
         ((> (+ tileSize sy) theHeight) #t) ;No more lines
         ((> (+ tileSize sx) theWidth) (loopTiles 0 (round (+ sy tileSize)) margin (round (+ ty tileSize spacing)) )) ;New line
         ((begin 
           (gimp-selection-clear inImage)
           (gimp-rect-select inImage (round sx) (round sy) tileSize tileSize 0 FALSE 0)

           (gimp-edit-copy-visible inImage)  
             (let (
               (newTile (car (gimp-edit-paste theLayer FALSE)))
               )  
              (gimp-layer-set-offsets newTile (round tx) (round ty))
             )
           (loopTiles (round (+ sx tileSize)) sy (round (+ tx tileSize spacing)) ty)         
          ))
      ))
   )   
  )
  (loopTiles 0 0 margin margin)
  )
  (gimp-selection-clear inImage)
  (gimp-floating-sel-anchor (car (gimp-image-get-floating-sel theImage)))
  (gimp-image-undo-enable theImage)
  (gimp-displays-flush) 
  (gimp-image-undo-enable inImage)
  (gimp-context-pop)
  )
)


(script-fu-register "script-fu-respace-tiles-addSpacingMargin"
  _"Add spacing and margin"
  _"Add spacing and margin to existing tileset which has zero margin and spacing. "
  "Premik"
  "LGPL"
  "2010"
  ""
 SF-IMAGE    "Image"    0 
 SF-DRAWABLE "Drawable" 0
 SF-ADJUSTMENT _"TileSize"   '(32 2 1600 1 8 0 0)
 SF-ADJUSTMENT _"New margin"   '(1 0 256 1 1 0 0)
 SF-ADJUSTMENT _"New spacing"   '(2 0 256 1 1 0 0)
)

(script-fu-menu-register "script-fu-respace-tiles-addSpacingMargin"
                         "<Image>/Filters/Map/TileSet")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-fu-respace-tiles-fillSpaces inImage inLayer tileSize margin spacing deep top right bottom left)

  (let* (
        (theWidth (car (gimp-drawable-width inLayer)))
        (theHeight (car (gimp-drawable-height inLayer)))
        )

  (gimp-context-push)
  (gimp-image-undo-group-start inImage)

  (letrec (

    (copyColumn (lambda(fromX toX)       
           (gimp-selection-clear inImage)
           (gimp-rect-select inImage (round fromX) (- margin 1) 1 (- theHeight margin) 0 FALSE 0)
           (gimp-edit-copy-visible inImage)  
             (let (
               (newStrip (car (gimp-edit-paste inLayer FALSE)))
               )  
              (gimp-layer-set-offsets newStrip (round toX) (- margin 1))
	      (gimp-floating-sel-anchor (car (gimp-image-get-floating-sel inImage)))
             )
      ))

    (copyRow (lambda(fromY toY)       
           (gimp-selection-clear inImage)
           (gimp-rect-select inImage (- margin 1) (round fromY) (round (- theWidth margin)) 1 0 FALSE 0)
           (gimp-edit-copy-visible inImage)  
             (let (
               (newStrip (car (gimp-edit-paste inLayer FALSE)))
               )  
              (gimp-layer-set-offsets newStrip (- margin 1) (round toY))
	      (gimp-floating-sel-anchor (car (gimp-image-get-floating-sel inImage)))
             )
      ))

    (loopCols (lambda(x)
       (cond 
         ((> (+ tileSize x) theWidth) #t) ;No more
         ((begin 
           (if (= left TRUE) (copyColumn (+ x deep -1) (- x 1)))
	   (if (= right TRUE) (copyColumn (+ x tileSize (- deep)) (+ x tileSize)))
           (loopCols (round (+ x tileSize spacing)))   
          ))
      ))
    )
    (loopRows (lambda(y)
       (cond 
         ((> (+ tileSize y) theHeight) #t) ;No more
         ((begin 
           (if (= top TRUE) (copyRow (+ y deep -1) (- y 1)))
	   (if (= bottom TRUE) (copyRow (+ y tileSize (- deep)) (+ y tileSize)))
           (loopRows (round (+ y tileSize spacing)))   
          ))
      ))
    )   
  )
  (if (or (= left TRUE) (= right TRUE)) (loopCols margin))
  (gimp-displays-flush) 
  (gimp-selection-clear inImage)

  (if (or (= top TRUE) (= bottom TRUE)) (loopRows margin))
  )




  (gimp-image-undo-group-end inImage)
  (gimp-context-pop)
  )
)

(script-fu-register "script-fu-respace-tiles-fillSpaces"
  _"Fill spaces"
  _"Fill spaces with tile borders"
  "Premik"
  "LGPL"
  "2010"
  ""
 SF-IMAGE    "Image"    0 
 SF-DRAWABLE "Drawable" 0
 SF-ADJUSTMENT _"TileSize"   '(32 2 1600 1 8 0 0)
 SF-ADJUSTMENT _"Margin"   '(1 0 256 1 1 0 0)
 SF-ADJUSTMENT _"Spacing"   '(2 0 256 1 1 0 0)
 SF-ADJUSTMENT _"Copy distance"   '(1 1 128 1 1 0 0)
 SF-TOGGLE     "Top" TRUE
 SF-TOGGLE     "Right" TRUE
 SF-TOGGLE     "Bottom" TRUE
 SF-TOGGLE     "Left" TRUE

)


(script-fu-menu-register "script-fu-respace-tiles-fillSpaces"
                         "<Image>/Filters/Map/TileSet")


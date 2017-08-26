(define (leon-morph-edges img drw cnt)
  (let* (
        (i 0)
        (nl 0)
        ) 
   (gimp-context-push) 
   (gimp-image-undo-group-start img)
   
   (set! nl (car (gimp-layer-copy (car (gimp-image-get-active-layer img)) FALSE)))
   (gimp-image-add-layer img nl -1)
   (gimp-displays-flush) 
   
   (while (< i cnt) 
     (plug-in-dilate 1 img nl 0 FALSE 0.2 0 0 255)
     (set! i (+ i 1))
   )
   
   (gimp-layer-set-mode nl 6)
   (gimp-image-merge-down img nl 0)
   
   (gimp-displays-flush) 
   (gimp-image-undo-group-end img)
   (gimp-context-pop)
  )
)

(script-fu-register "leon-morph-edges"
                    "Morphology edges detection"
                    "Detect edges with dilate operation"
                    "Leonid Koninin"
                    "Leonid Koninin"
                    "2011"
                    "RGB*, GRAY*"
                    SF-IMAGE    "Image"         0
		    SF-DRAWABLE "Layer to process" 0
		    SF-VALUE    "Power" "2"
		    )
(script-fu-menu-register "leon-morph-edges"
                         "<Image>/Filters/Leon")

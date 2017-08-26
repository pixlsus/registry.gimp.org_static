
(script-fu-register "AdjustGifMovie"                    "AdjustGifMovie"                    "Adjust Size and Speed of Gif Movie"                    "Don Sauer dsauersanjose@aol.com"                    "public domain"                    "3.8.12_11.12AM"                    ""                    SF-FILENAME   "SF-FILENAME"            "/"                    SF-ADJUSTMENT "Timing-ms"                 '(100 10 2000 50 10 1 0)                    SF-ADJUSTMENT "PercentScale-%"            '(100 30 600  10 1  1 0))

(script-fu-menu-register "AdjustGifMovie" "<Image>/Filters/Animation")

(define         (AdjustGifMovie fpath timing scale)
(let*           ( 
(image          (car (file-gif-load 1 fpath "giffile")))
(backlay        0)
(null           0)
(fpath2         0)
(image2         0) 
(backlay2       0)
(NumbLay        0)
(tempLay        0)
(w              0)
(h              0)
(i              0)
                )  
;;=======       first set up new image
(set! null      (gimp-display-new    image ))
(set! backlay   (car (gimp-image-get-active-layer  image)))
(set! null      (gimp-drawable-set-name backlay  "backlay"))
;;=======       Clean up a second image using animationunoptimize
(set! image2    (car (plug-in-animationunoptimize 1 image backlay)))
(set! backlay2  (car (gimp-image-get-active-layer  image2)))
(set! null      (gimp-display-new    image2 ))
;;=======       find out how many layers
(set! NumbLay   (car (gimp-image-get-layers image2)))
;;=======       all the layers need to be renamed adding (replace) 
(set! i         0 )
(while          (< i NumbLay)
(set! null      (gimp-drawable-set-name (vector-ref(car(cdr(gimp-image-get-layers image2)))i) "lay (replace)"))
(set! i         (+ i 1))
)
;;=======       get dimension of the image
(set! w         (car (gimp-image-width  image) ))
(set! h         (car (gimp-image-height image) ))
;;=======       scale second image
(set! null      (gimp-image-scale image2 (* w ( / scale 100))   (* h ( / scale 100)) ))
;;=======       change the file name
(set! fpath2    (string-append  (substring fpath 0 (-(string-length fpath) 4) ) "_adj.gif" ))
;;=======       save gif movie
(set! null      (file-gif-save 1 image2 backlay2 fpath2 "giffile"  0 1 timing 0)     )))
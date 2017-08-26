;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export Layers to Anime Studio Project (ANME)
;; desc: This script lets you convert gimp images
;;       into Anime Studio 5 project files.  You 
;;       need to set apart a folder for images and
;;       the anme file, but the folder can be the 
;;       same for both.
;; Version: 2
;; Compatible with: Anime Studio 5, GIMP 2.6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: export-layers-to-anme
;; desc: main function
;; in: anme directory, layer png image directory, 
;;     anme file name, image, drawable
;; out: -
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define(export-layers-to-anme anme-dir lyr-dir anme-nm img drw)

(let*
((flnm 0)(p 0)(w 0)(h 0)(s 0)(img-name "")(img-name-raw "")(ln ""))

(set! img-name (car(gimp-image-get-name img)))
(set! img-name-raw (get-raw-file-name-no-ext img-name))

(if(string=? anme-nm "")(set! anme-nm img-name-raw))
(set! flnm (string-append anme-dir "\\" anme-nm ".anme"))
(set! p(open-output-file flnm))

(set! w(car(gimp-image-width img)))
(set! h(car(gimp-image-height img)))

;;Header
(dl "application/x-vnd.lm_mohodoc" p)
(dl "version 14" p)
(dl "### Created by the Export Layers To Anime Studio script for the GIMP." p)
(dl "" p)
(dl "" p)
(dl "### static values" p)
(set! ln (string-append "dimensions " (n2s w) " " (n2s h)))

(dl ln p)
(dl "frame_range 1 64" p)
(dl "fps 16" p)
(dl "back_color 234 234 234 255" p)
(dl "" p)
(dl "### layers" p)
(dl "layer_type 4" p)
(dl "{" p)
(dl "\t### generic layer values" p)
(display "\tname \"" p)
(display img-name p)
(display "\"" p)
(newline-ext p)
(newline-ext p)
(dl "\t### bone layer values" p)
(dl "\texpanded true" p)
(dl "" p)
(dl "\t### sub-layers" p)

(let*
( (dup-img(car(gimp-image-duplicate img))) )
(let*
( 
(lyrs(gimp-image-get-layers img))
(lc(car lyrs)) (clyr )
;(lyr-dir (string-append anme-dir "\\" anme-nm "_imgs"))
)
(set! lyrs (car(cdr lyrs)))
;(set! clyr (aref lyrs 0))
;(gimp-image-set-active-layer dup-img clyr)
;Export files 
(export-anme-layers lyr-dir dup-img lc lyrs w h p)
(gimp-image-delete dup-img)
)
)

(dl "} " p)
(close-output-port p)
)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: get-raw-file-name-no-ext
;; Desc: Gets a file name without the directory-path 
;;       and extension
;; in: file name string 
;; out: raw file name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-raw-file-name-no-ext s)

(let*((p1 0)(p2 0)(ss 0)(sl 0)(ch 0)(ns 0))

(set! sl (string-length s))
(set! p1 0)
(do((i p1 (+ i 1)))((>= i sl) i)
(set! ch (string-ref s i))
(if(char=? ch #\\)(set! p2 i))
) ;do

(set! p2 sl)
(do((i (- sl 1) (- i 1)))((<= i 0)i)
(set! ch (string-ref s i))
(if(char=? ch #\.)(set! p2 i))
) ;do

(if(>= p2 p1)
(set! ss (substring s p1 p2))
(set! ss "")
) ;if

ss ;return value
) ;let 
) ;define

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: newline-ext
;; desc: writes carriage return + new line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (newline-ext port-id) (display "\x0d\x0a" port-id))

;dl=display-line
(define (dl text port) 
(display  text port)(newline-ext port)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: n2s
;; desc: shorter version of number->string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define(n2s n)(atom->string n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: export-anme-layers
;; desc: export each layer to an anme 
;;       file
;; input: layer image directory, dup-img, layer count,
;;        current layer, layer list
;; return: <none>, documents are saved accordingly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (export-anme-layers lyr-dir img lc lyrs iw ih p)

(let*
(
(cl 0)(img-nm-base 0)(img-nm 0)
(cur-ln "")(x 0)(y 0)(lw 0)(lh 0)
(xform-x 0)(xform-y 0)
(xs "")(ys "")(buls 0)
(show 0)
)

(dl "\t### sub-layers" p)


(do 
(
(i (- lc 1) (- i 1))
)
(
(<= i 0) ;until
i ;return
)

(set! cl (aref lyrs i))
(set! img-nm-base (car(gimp-drawable-get-name cl)))
(set! img-nm (string-append lyr-dir "\\" img-nm-base ".png"))

(dl "\tlayer_type 2" p)
(dl "\t{" p)
(dl "\t\t### generic layer values" p)
;Write image
;{

(display "\t\tname \"" p)
(display img-nm-base p)
(display "\"\r\n" p)
;}

(display "\t\tvisible " p)
(set! show (car(gimp-drawable-get-visible cl)))
(if (= show 1) (display "true" p) (display "false" p))
(display "\r\n" p)

(dl "" p)
(dl "\t\t### transforms" p)
(dl "\t\ttranslation" p)
(dl "\t\t[" p)
(dl "\t\t\tkeys 1" p)

(set! lw (car(gimp-drawable-width cl)))
(set! lh (car(gimp-drawable-height cl)))
(set! x (car(gimp-drawable-offsets cl)))
(set! y (car(cdr(gimp-drawable-offsets cl))))

;xformX = -docWidth / docHeight + layerWidth.value / docHeight + 2.0 * layerX.value / docHeight;
(set! xform-x 
(+ (/ (- 0 iw) ih) (/ lw ih) (/ (* 2.0 x) ih ) )
)
;xformY = 1.0 - layerHeight.value / docHeight - 2.0 * layerY.value / docHeight;
(set! xform-y 
(- 1.0 (/ lh ih) (/ (* 2.0 y) ih) )
)

(set! xs (n2s xform-x))
(set! ys (n2s xform-y))
(set! cur-ln (string-append "\t\t\t\t0 1 0.1 0.5 " xs " " ys " 0"))
(dl cur-ln p)

(dl "\t\t]" p)
(dl "\t\t### image layer values" p)

;;save image here
(save-layer-to-png img cl img-nm)

(display "\t\timage \"" p)
(display img-nm p)
(display "\"\r\n" p)

;file.writeln("\tshape " + 
;2.0 * layerWidth.value / docHeight
(set! xform-x 
(/ (* 2.0 lw) ih)
)
; + " " + 
;2.0 * layerHeight.value / docHeight);
;xformY = 1.0 - layerHeight.value / docHeight - 2.0 * layerY.value / docHeight;
(set! xform-y 
(/ (* 2.0 lh) ih)
)
;(dl "\t\tshape 2.0 2.0" p)
(set! xs (n2s xform-x))
(set! ys (n2s xform-y))
(set! cur-ln (string-append "\t\tshape " xs " " ys))
(dl cur-ln p)
(dl "\t}" p)

) ; do
) ; let*
) ;define

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: save-layer-to-png
;; Desc: Saves a layer to a PNG file
;; in: original image, layer to save, file name
;; out: -
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (save-layer-to-png orig-image layer fname)
(let* 
(
(img 0)(al 0)(bfr "")
)
(set! bfr (car(gimp-edit-named-copy layer "temp-copy")))
(set! img (car(gimp-edit-named-paste-as-new bfr)))
(set! al (car(gimp-image-get-active-layer img)))
(file-png-save-defaults 1 img al fname "")
(gimp-image-delete img)
);let*
);define

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Registration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(script-fu-register
 "export-layers-to-anme"
 "Export Layers to Anime Studio (ANME)"
 "Exports layers to Anime Studio project (ANME)"
 "ja227"
 "ja227"
 "12/9/2008"
 ""
 SF-DIRNAME "ANME Dir" ""; Output ANME Directory; 
 SF-DIRNAME "Layer Img Dir" ""; layer images director
 SF-STRING "ANME Name" ""; Target ANME File name without extension.  If Null, image name is used.
 SF-IMAGE    "Image"    0 ; The image
 SF-DRAWABLE "Drawable" 0 ; The drawable
 )

(script-fu-menu-register "export-layers-to-anme"
                         "<Image>/File/")
;;(export-layers-to-anme "E:\\apps\\GIMP\\script projects\\anme exporter\\anme" "a" 4 105)
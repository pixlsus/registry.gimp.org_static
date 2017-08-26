; Show Yellow Tracking Dots 
; cf. http://www.eff.org/issues/printers
;     http://www.eff.org/Privacy/printers

; Installing this script:
; 1. (Gimp 2.2 Linux) just copy it into ~/.gimp-2.2/scripts/
;    (Windows XP) something like C:\Program Files\GIMP-2.2\share\GIMP\2.2\scripts 
;                  should be the right place
; 2. If gimp is already running call Xtns -> Script-Fu -> Refresh Scripts.

; How to discover yellow tracking dots printed by a laser color printer:
; 1. Create a text document with just a few words; at least one of them has to be colored
; 2. print it on the suspected laser color printer
; 3. scan an empty area (e.g. postcard size) of the printed page (600dpi or 1200 dpi)
; 4. run this script on the scan (<image context menu> -> Filters -> Politics -> ...)
; 5. Play with zoom (start with 100%)

(define (script-fu-yellowtrackingdots img drawable)

  (let* (
      ; Decompose the drawable
      (new-image (car (plug-in-decompose 1 img drawable "RGB" 1)))
      (layers '())
      (layersarray '())
      (lyr 0)
      (lyg 0)
      (lyb 0)
      (numlayers 0)
    )

    ; get handles of layers of new image
    (set! layers (gimp-image-get-layers new-image))
    (set! numlayers (car layers))
    (set! layersarray (cadr layers))
    (set! lyb (aref layersarray 0))
    (set! lyg (aref layersarray 1))
    (set! lyr (aref layersarray 2))

    ; blue component to buttom
    (gimp-image-lower-layer-to-bottom new-image lyb)
    ; add inverted average of both other components
    (gimp-layer-set-opacity lyg 50)
    (gimp-layer-set-opacity lyr 50)
    (gimp-layer-set-mode lyg 7)
    (gimp-layer-set-mode lyr 7)
    (gimp-invert lyg)
    (gimp-invert lyr)
    ; merge it and stretch contrast
    (set! lyb (car (gimp-image-merge-visible-layers new-image 2)))
    (plug-in-c-astretch 1 new-image lyb)

    ; Show image
    (gimp-display-new new-image)
    ; (gimp-message "Now you may zoom to 100%!")

    ; Flush the display (here not necessary)
    ; (gimp-displays-flush)

  )
)

(script-fu-register "script-fu-yellowtrackingdots"
                    "show yellow tracking dots"
                    "Shows yellow tracking dots"
                    "Hatto von Hatzfeld"
                    "Hatto von Hatzfeld"
                    "2008"
                    "RGB*"
                    SF-IMAGE    "Image"         0
                    SF-DRAWABLE "Scan to be analyzed" 0)
(script-fu-menu-register "script-fu-yellowtrackingdots"
                         "<Image>/Filters/Politics")




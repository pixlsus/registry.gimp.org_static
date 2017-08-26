; Sepia toning script for GIMP
; version 0.3 - GIMP 2.4.x
; just to learn how all it works ;-)
; by Jakub Klawiter 05.2007 - 11.2007
; 
; this is a copy of Sepia Toning tutorial 
; http://www.gimp.org/tutorials/Sepia_Toning/
; by Eric R. Jeschke
;
; copyleft GNU GPL v3 etc. etc.
;
;


(define 
    (script-fu-Sepia_Toning
        img 
        drawable
        desaturate
        merge-layers
        color 
    )


; Start an undo group. Everything between the start and the end will
; be carried out if an undo command is issued.
    (gimp-image-undo-group-start img)
    (gimp-displays-flush)

    (let* 
        (  ; variables definition
            (sepia-layer 0)
            (mask-layer 0)
            (mask 0) 
        )

; STEP 2 - copy and desaturate (optional) source layer

    (set! sepia-layer
        (car
            (gimp-layer-copy 
                drawable 
                TRUE
            )
        )
    )
    (gimp-layer-set-name sepia-layer "Sepia")
    (gimp-image-add-layer img sepia-layer -1)



    (if (equal? desaturate TRUE) 
        (gimp-desaturate sepia-layer)
        ()
    )



; STEP 3 Set foreground color
    (gimp-context-set-foreground color)

; STEP 4
; Create a new layer
    (set! mask-layer
        (car 
            (gimp-layer-new 
                img                             ; image handle
                (car (gimp-image-width img))    ; width of layer
                (car (gimp-image-height img))   ; height
                1                               ; type (RGB, RGBA, etc.)
                "Sepia Mask"                    ; name of layer
                100                             ; opacity
                COLOR-MODE                      ; mode
            )
        )
    )


; Add the new layer to the image 
    (gimp-image-add-layer img mask-layer -1) 

    (gimp-drawable-fill mask-layer 0)

; STEP 5
    (set! mask 
        (car
            (gimp-layer-create-mask mask-layer 0)
        )
    )
    (gimp-layer-add-mask mask-layer mask)

; STEP 6, 7 Copy image into Sepia Layer mask, and than invert it
    (gimp-layer-resize-to-image-size sepia-layer) ; workaround because i cannot 'paste in place' into mask
    (gimp-edit-copy sepia-layer)

    (let ((selection (car (gimp-edit-paste mask 0))))
        (gimp-floating-sel-anchor selection) 
    )
    (gimp-invert mask)

; merge layer down
    (if (equal? merge-layers TRUE) 
        (gimp-image-merge-down 
            img             ; img
            mask-layer      ; upper layer
            0               ; merge type [0,1,2]
        )
        ()
    )



    ) ; let* variables definition

; Complete the undo group
    (gimp-image-undo-group-end img)

)



(script-fu-register "script-fu-Sepia_Toning"
	"Sepia Toning"
	"Automatic version of great
Sepia Toning tutorial 
by Eric R. Jeschke (redskiesatnight.com/)

www.gimp.org/tutorials/Sepia_Toning/"
	"Jakub Klawiter"
	""
    "03.2007"
    "RGB RGBA"
	SF-IMAGE      "img"                 0
	SF-DRAWABLE   "drawable"            0
	SF-TOGGLE     "Desaturate source"   FALSE
    SF-TOGGLE     "Merge layers"        FALSE
    SF-COLOR      "color"               '(162 138 101))

(script-fu-menu-register
    "script-fu-Sepia_Toning" 
    "<Image>/Script-Fu/Photo"
)

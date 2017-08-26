; DRI aus 3 Bildern
; es muß drei Ebenen geben mit den Rohbildern in folgender Reihenfolge:
; 1. hell
; 2. dunkel
; 3. mittel

(define
  (script-fu-DRI-from-3
    Image
    Layer
  )
   ;Start an undo group so the process can be undone with one undo
  (gimp-image-undo-group-start Image)
   (let
    (
      (layers (vector->list (cadr (gimp-image-get-layers Image)))) ;die Handles der drei Ebenen als Liste holen
    )

    (let ;muß nochmal neu, weil die Variable layers verwendet werden soll
      (
        (HElayermask (car (gimp-layer-create-mask (car layers) 5))) ;die Ebenenmaske für die helle Ebene erstellen (macht sie noch nicht sichtbar)
                                                                    ;5 bedeutet Graustufenmaske
        (DElayermask (car (gimp-layer-create-mask (cadr layers) 5))) ;die Ebenenmaske für die dunkle Ebene erstellen (macht sie noch nicht sichtbar)
                                                                     ;5 bedeutet Graustufenmaske
      )

      (gimp-layer-add-mask (car layers) HElayermask) ;Ebenenmaske der hellen Ebene hinzufügen (macht sie sichtbar)
      (gimp-layer-add-mask (cadr layers) DElayermask) ;Ebenenmaske der dunklen Ebene hinzufügen (macht sie sichtbar)
      (gimp-invert HElayermask) ;die Ebenenmaske der hellen Ebene invertieren

    )

    ;Finish the undo group for the process
    (gimp-image-undo-group-end Image)

    (gimp-displays-flush)
         ;Rückgabe:
    ;nichts
  )

)

(script-fu-register
  "script-fu-DRI-from-3" ;func name
  ;_"<Image>/Filters/Enhance/DRI fom 3 images..."
  "[JR] DRI from 3 images..." ;menu label
  "Creates a DynamicRangeIncreased image from 3 source images that were \
taken with 3 different exposure settings. They must be stacked as three layers as follows: 1. bright 2. dark 3. middle exposure; then \
call the script." ;description
  "Jens-Arne Reumschuessel" ;author
  "copyright 2010, Jens-Arne Reumschuessel" ;copyright notice
  "June 7,2010" ;date created
  "RGB" ;image type that the script works on
  SF-IMAGE    "Image"     0
  SF-DRAWABLE "Drawable"  0
)
(script-fu-menu-register "script-fu-DRI-from-3" "<Image>/Filters/Enhance")

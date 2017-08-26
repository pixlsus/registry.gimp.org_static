(define (saturation-layer-extract image layer)
  (gimp-image-undo-group-start image)

  (let* ((saturation-image 0)
         (saturation-layer 0)
        )
    ; Applique un tatouage au calque
    (gimp-drawable-set-tattoo layer 1234)

    ; Récupère le canal saturation du calque courant dans une image
    (set! saturation-image (car (plug-in-decompose RUN-NONINTERACTIVE image layer "Saturation" -1)))

    ; Duplique le calque actif de l'image contenant le canal de saturation
    (set! saturation-layer (car (gimp-layer-new-from-drawable (car (gimp-image-get-active-layer saturation-image)) image)))

    ; Ajoute le calque à l'image de base
    (gimp-image-add-layer image saturation-layer -1)

    (gimp-drawable-set-name saturation-layer "Saturation")

    ; L'image contenant le canal de saturation est temporaire, on la supprime maintenant
    (gimp-image-delete saturation-image)
  )

  (gimp-image-undo-group-end image)

  ; Force la mise à jour de l'affichage
  (gimp-displays-flush)
)

(define (saturation-layer-apply image layer)
  (gimp-image-undo-group-start image)

  (let* ((saturation-image 0)
         (saturation-layer 0)
         (teinte-image 0)
         (teinte-layer 0)
         (valeur-image 0)
         (valeur-layer 0)
         (hsv-image 0)
         (hsv-layer 0)
         (base-layer (car (gimp-image-get-layer-by-tattoo image 1234)))
        )
    ; calque courant CS =calque de saturation à appliquer
    ; calque du dessous CD=calque auquel appliquer le calque de saturation
    ; création d'une image ID de CD avec plug-in-decompose

    ; Récupère une image contenant la teinte
    (set! teinte-image (car (plug-in-decompose RUN-NONINTERACTIVE image base-layer "Hue" -1)))

    ; Récupère une image contenant la saturation
    (set! saturation-layer layer)
    (set! saturation-image (car (gimp-image-new (car (gimp-drawable-width saturation-layer)) (car (gimp-drawable-height saturation-layer)) GRAY)))
    (let* ((saturation-copie (car (gimp-layer-new-from-drawable saturation-layer saturation-image))))
      (gimp-image-add-layer saturation-image saturation-copie -1)
    )

    ; Récupère une image contenant les valeurs
    (set! valeur-image (car (plug-in-decompose RUN-NONINTERACTIVE image base-layer "Value" -1)))

    ; Compose l'image à partir des calques récupérés
    (set! hsv-image (car (plug-in-compose RUN-NONINTERACTIVE teinte-image -1 saturation-image valeur-image -1 "HSV")))
    (set! hsv-layer (car (gimp-layer-new-from-drawable (car (gimp-image-get-active-layer hsv-image)) image)))
    (gimp-image-delete hsv-image)

    ; Ajoute le calque à l'image de base
    (gimp-image-add-layer image hsv-layer -1)
    (gimp-drawable-set-name hsv-layer "Saturation applied")
    (gimp-image-delete teinte-image)
    (gimp-image-delete valeur-image)
    (gimp-image-delete saturation-image)
    (gimp-image-remove-layer image saturation-layer)
  )

  (gimp-image-undo-group-end image)

  ; Force la mise à jour de l'affichage
  (gimp-displays-flush)
)

(script-fu-register "saturation-layer-extract"
		    "<Image>/Script-Fu/Saturation/Extract saturation"
		    "Extract saturation of the current layer into a new layer"
		    "Frédéric BISSON"
		    "Frédéric BISSON"
		    "2007-02-14"
		    "RGB*"
		    SF-IMAGE      "Image" 0
		    SF-DRAWABLE   "Calque" 0
)

(script-fu-register "saturation-layer-apply"
		    "<Image>/Script-Fu/Saturation/Apply saturation"
		    "Take the current layer as the saturation of the layer from which it was previously extracted"
		    "Frédéric BISSON"
		    "Frédéric BISSON"
		    "2007-02-14"
		    "RGB*"
		    SF-IMAGE      "Image" 0
		    SF-DRAWABLE   "Calque" 0
)

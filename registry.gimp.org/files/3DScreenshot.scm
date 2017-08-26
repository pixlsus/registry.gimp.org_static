(define (script-fu-3d-screenshot inLayer inShinyness inColorTop inColorBottom inShadowBlur inShadowOpacity inShadowColor inLumIntensity inFinalDimension)

    (let*
    (
    (theLayer 0)
    (theShinyness 0)
    (theColorTop 0)
    (theColorBottom 0)
    (theShadowBlur 0)
    (theShadowOpacity 0)
    (theShadowColor 0)
    (theLumIntensity 0)
    (theFinalDimension 0)
    (theImage 0)
    (theHeight 0)
    (theWidth 0)
    (theImageHeight 0)
    (theImageWidth 0)
    (theBackground 0)
    (theBackgroundGradient 0)
    (theReflection 0)
    (theShadow 0)
    (theLuminosityOne 0)
    (theLuminosityTwo 0)
    )

    (set! theLayer       inLayer)
    (set! theShinyness   inShinyness)
    (set! theColorTop    inColorTop)
    (set! theColorBottom inColorBottom)
    (set! theShadowBlur  inShadowBlur)
    (set! theShadowOpacity  inShadowOpacity)
    (set! theShadowColor  inShadowColor)
    (set! theLumIntensity  inLumIntensity)
    (set! theFinalDimension  inFinalDimension)

    ;On définit une variable theImage qui contient l'image
    (set! theImage (car (gimp-drawable-get-image theLayer)))

    ;On définit deux variables contenant les dimensions du screenshot
    (set! theHeight (car (gimp-image-height theImage)))
    (set! theWidth  (car (gimp-image-width  theImage)))

    ;;;;; Redimensionnement de l'image
    (gimp-image-resize theImage
                       (* 2 theWidth)
                       (* 2 theWidth)
                       (/ theWidth 2)
                       (/ theHeight 2))
    ;Dimensions de l'image
    (set! theImageHeight (car (gimp-image-height theImage)))
    (set! theImageWidth  (car (gimp-image-width  theImage)))

    ;;;;; Ajout du fond
    ;On crée un nouveau calque
    (set! theBackground (car (gimp-layer-new theImage theImageWidth theImageHeight RGB-IMAGE "Fond" 100 0)))
    ;On ajoute le nouveau calque à l'image
    (gimp-image-add-layer theImage theBackground 1)
    ;On crée un dégradé
    (set! theBackgroundGradient (gimp-gradient-new "Fond-screenshot-with-style"))
    ;On le met en tant que dégradé actif
    (gimp-context-set-gradient "Fond-screenshot-with-style")
    ;On définit sa première couleur
    (gimp-gradient-segment-set-left-color "Fond-screenshot-with-style" 0 theColorTop 100)
    ;On définit sa deuxième couleur
    (gimp-gradient-segment-set-right-color "Fond-screenshot-with-style" 0 theColorBottom 100)
    ;On peint avec le dégradé
    (gimp-edit-blend theBackground             ;drawable
                     CUSTOM-MODE               ;blend_mode (normal)
                     NORMAL-MODE               ;paint_mode (normal)
                     GRADIENT-LINEAR           ;gradient_type (linéaire)
                     100                       ;opacity
                     0                         ;offset (?)
                     REPEAT-NONE               ;repeat (none)
                     FALSE                     ;reverse
                     FALSE                     ;supersample (?)
                     0                         ;max_depth
                     0                         ;threshold
                     TRUE                      ;dither (?)
                     0 0 0 theImageHeight)     ;coordonnées

    ;;;;; Ajout du reflet
    ;On crée un nouveau calque
    (set! theReflection
      (car
        (gimp-layer-copy
         theLayer
         1)
      )
    )
    ;On nomme le calque
    (gimp-layer-set-name theReflection "Reflet")
    ;On ajoute le nouveau calque à l'image
    (gimp-image-add-layer theImage theReflection -1)
    ;On le retourne
    (gimp-drawable-transform-flip-simple theReflection 1 1 0 1)
    ;On le descend pour que son bord supérieur corresponde
    ;au bord inférieur de l'autre calque
    (gimp-layer-translate theReflection 0 theHeight)
    ;On diminue son opacité
    (gimp-layer-set-opacity theReflection theShinyness)

    ;;;;; Fusion des deux calques
    (gimp-drawable-set-visible theBackground FALSE)
    (set! theLayer (car (gimp-image-merge-visible-layers theImage 1)))
    (gimp-drawable-set-visible theBackground TRUE)

    ;;;;; Perspective
    ;D'abord on agrandit le calque à la taille de l'image
    (gimp-layer-resize-to-image-size theLayer)
    ; Et on applique la transformation
    (set! theLayer (car (gimp-drawable-transform-perspective-default theLayer
                                                 (- (/ theWidth 5.3))
                                                 (- (/ theHeight 4))
                                                 (* theWidth 3.7)
                                                 (- (* theHeight 1.7))
                                                 0
                                                 (* theHeight 3.5)
                                                 (* theWidth 2.1)
                                                 (* theHeight 5.3)
                                                 TRUE TRUE)))

    ;;;;; Ajout de l'ombre
    ;On crée un nouveau calque
    (set! theShadow
      (car
        (gimp-layer-new
         theImage
         theImageWidth
         theImageHeight
         0  ;RGB
         "Ombre"
         theShadowOpacity
         9) ;Assombrir
      )
    )
    ;On ajoute le nouveau calque à l'image
    (gimp-image-add-layer theImage theShadow 1)
    ;On sélectionne tout
    (gimp-selection-all theImage)
    ;pour pouvoir peindre en blanc
    (gimp-context-set-foreground '(255 255 255))
    (gimp-edit-bucket-fill theShadow 0 0 100 0 0 0 0)
    ;On sélectionne un rectangle
    (gimp-rect-select theImage (/ theWidth 2) (/ theHeight 2) theWidth theHeight 2 0 0)
    ;On crée un dégradé
    (set! theBackgroundGradient (gimp-gradient-new "Ombre-screenshot-with-style"))
    ;On le met en tant que dégradé actif
    (gimp-context-set-gradient "Ombre-screenshot-with-style")
    ;On définit sa première couleur
    (gimp-gradient-segment-set-left-color "Ombre-screenshot-with-style" 0 '(255 255 255) 100)
    ;On définit sa deuxième couleur
    (gimp-gradient-segment-set-right-color "Ombre-screenshot-with-style" 0 theShadowColor 100)
    ;On peint avec le dégradé (dans la sélection)
    (gimp-edit-blend theShadow                 ;drawable
                     CUSTOM-MODE               ;blend_mode (normal)
                     NORMAL-MODE               ;paint_mode (normal)
                     GRADIENT-LINEAR           ;gradient_type (linéaire)
                     100                       ;opacity
                     0                         ;offset (?)
                     REPEAT-NONE               ;repeat (none)
                     FALSE                     ;reverse
                     FALSE                     ;supersample (?)
                     0                         ;max_depth
                     0                         ;threshold
                     TRUE                      ;dither (?)
                     0 (/ theHeight 2) 0 (* theHeight 1.5))     ;coordonnées
    ;On vire la sélection
    (gimp-selection-none theImage)
    ;On floute l'ombre
    (plug-in-gauss 1 theImage theShadow theShadowBlur theShadowBlur 0)
    ;On la déforme
    (set! theShadow (car (gimp-drawable-transform-perspective-default theShadow
                                                 (/ theWidth 1.6)
                                                 (* theHeight 1.44)
                                                 (* theWidth 3)
                                                 (* theHeight 2)
                                                 (- (* theWidth 1.375))
                                                 (* theHeight 2.45)
                                                 (* theWidth 1.375)
                                                 (* theHeight 4)
                                                 TRUE TRUE)))

    ;;;;; Ajout de l'effet de luminosité
    ;On crée deux nouveaux calques
    (set! theLuminosityOne
      (car
        (gimp-layer-copy
         theLayer
         1)
      )
    )
    (set! theLuminosityTwo
      (car
        (gimp-layer-copy
         theLayer
         1)
      )
    )
    ;On nomme les calques
    (gimp-layer-set-name theLuminosityOne "Effet de lumière (détails)")
    (gimp-layer-set-name theLuminosityTwo "Effet de lumière (diffuse)")
    ;On ajoute les nouveaux calques à l'image
    (gimp-image-add-layer theImage theLuminosityOne 0)
    (gimp-image-add-layer theImage theLuminosityTwo 0)
    ;On ajoute un max de contraste
    (gimp-brightness-contrast theLuminosityOne 0 100)
    (gimp-brightness-contrast theLuminosityTwo 0 100)
    ;On le floute
    (plug-in-gauss 1 theImage theLuminosityOne 5 5 0)
    (plug-in-gauss 1 theImage theLuminosityTwo 20 20 0)
    (gimp-layer-set-opacity theLuminosityOne theLumIntensity)
    (gimp-layer-set-opacity theLuminosityTwo theLumIntensity)
    (gimp-layer-set-mode theLuminosityOne 4)
    (gimp-layer-set-mode theLuminosityTwo 4)

    ;On met le calque et l'image aux bonnes dimensions/proportions
    (gimp-image-scale theImage
                       theFinalDimension
                       theFinalDimension)

    ;Suppression des dégradés
    (gimp-gradient-delete "Fond-screenshot-with-style")
    (gimp-gradient-delete "Ombre-screenshot-with-style")

    ;Rafraichissement de l'image
    (gimp-displays-flush)
    )
)

(script-fu-register "script-fu-3d-screenshot"
                    _"3D Screenshot"
                    "Creates a stylish decoration for a screenshot"
                    "Mathieu Piette (mathieu.piette@gmail.com)"
                    "Mathieu Piette"
                    "08 June, 2006"
                    ""
                    SF-LAYER "Layer" 0
                    SF-ADJUSTMENT "Brillance" '(30 0 100 1 10 1 0)
                    SF-COLOR "Gradient (Top)"  '(255 255 255)
                    SF-COLOR "Gradient (Bottom)" '(230 230 230)
                    SF-ADJUSTMENT "Shadow Blur" '(40 0 100 1 10 1 0)
                    SF-ADJUSTMENT "Shadow Opacity" '(20 0 100 1 10 1 0)
                    SF-COLOR "Shadow Colour"  '(0 0 0)
                    SF-ADJUSTMENT "Light Effect" '(20 0 100 1 10 1 0)
                    SF-ADJUSTMENT "Image Size" '(800 0 10000 1 10 1 0)
)

(script-fu-menu-register "script-fu-3d-screenshot"
                        _"<Toolbox>/Xtns/Script-Fu")


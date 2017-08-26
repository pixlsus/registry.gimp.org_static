; This script automates most of Marvin X's fantastic planet making tutorial.
; This script does not do drop shadows or the iWarp filter, but everything
; else is here.    It also adds in an atomspheric glow around the planet,
; and it can be set up to create an earth-like colored landscape.
; I hope you enjoy it.
;
; James Sambrook


(define (script-fu-planet-setup EarthLike SunShadow inSize sTurb cTurb bDetail bSize sunAngle SunScale hold hue saturation value)

  (let* (
        (theWidth inSize)
        (theHeight inSize)
        (theImage (car (gimp-image-new theWidth theHeight RGB)))
        (baseLayer (car (gimp-layer-new theImage theWidth theHeight RGBA-IMAGE "Background" 100 NORMAL-MODE)))
        (surfaceLayer (car (gimp-layer-new theImage theWidth theHeight RGBA-IMAGE "Surface" 100 NORMAL-MODE)))
        (cloudLayer (car (gimp-layer-new theImage theWidth theHeight RGBA-IMAGE "Clouds" 100 HARDLIGHT-MODE)))
	  (shadowLayer (car (gimp-layer-new theImage theWidth theHeight RGBA-IMAGE "Shadow" 80 NORMAL-MODE)))
        (map1Layer (car (gimp-layer-new theImage theWidth theHeight RGBA-IMAGE "Bump 1" 100 NORMAL-MODE)))
        (map2Layer (car (gimp-layer-new theImage theWidth theHeight RGBA-IMAGE "Bump 2" 100 DIFFERENCE-MODE)))
        (theBlur 0)
        (angleRad (/ (* sunAngle *pi*) 180))
        (transX (* (sin angleRad) -1))
        (transY (cos angleRad))
        )

; Percent of maximum size that the planet is going to be
	(define theRatio (/ inSize 3000.0))
	(define tenth (/ inSize 10.0))

	(gimp-context-push)

	(gimp-image-add-layer theImage baseLayer 0)

; Get a black background, and add in distant stars
	(gimp-context-set-background '(255 255 255))
	(gimp-context-set-foreground '(0 0 0))
	(gimp-edit-fill baseLayer FOREGROUND-FILL)
	(plug-in-hsv-noise RUN-NONINTERACTIVE
		theImage baseLayer (- 9 hold) hue saturation value)

; Create the layer for the planet's surface
	(gimp-image-add-layer theImage surfaceLayer 0)
	(plug-in-plasma RUN-NONINTERACTIVE
		 theImage surfaceLayer (rand 4294967294) sTurb)
	(gimp-desaturate surfaceLayer)
	(plug-in-make-seamless RUN-NONINTERACTIVE theImage surfaceLayer)

; Create a layer of "clouds"
	(gimp-image-add-layer theImage cloudLayer 0)
	(plug-in-plasma RUN-NONINTERACTIVE
		theImage cloudLayer (rand 4294967294) cTurb)
	(gimp-desaturate cloudLayer)
	(plug-in-make-seamless RUN-NONINTERACTIVE theImage cloudLayer)

; Create a bump-map for geographical features on the planet.
	(gimp-image-add-layer theImage map1Layer 0)
	(plug-in-solid-noise RUN-NONINTERACTIVE
		theImage map1Layer 1 0 (rand 4294967294) bDetail bSize bSize)
	(gimp-image-add-layer theImage map2Layer 0)
	(plug-in-solid-noise RUN-NONINTERACTIVE
			 theImage map2Layer 1 0 (rand 4294967294) bDetail bSize bSize)
	(set! map (car(gimp-image-merge-down theImage map2Layer 1)))
	(gimp-invert map)
	(plug-in-make-seamless RUN-NONINTERACTIVE theImage map)

; Map the three layers to spheres.
    (plug-in-map-object RUN-NONINTERACTIVE theImage surfaceLayer 
			1			; mapping
			0.5 0.5 2.0		; viewpoint
			0.5 0.5 0.9		; object pos
			1.0 0.0 0.0		; first axis
			0.0 1.0 0.0		; 2nd axis
			0.0 0.0 0.0		; axis rotation
			2 '(255 255 255)	; light (type, color)
			-0.5 -0.5 2		; light position
			-1.0 -1.0 1.0	; light direction
			0.3 1 0.5 0.3 27	; material (amb, diff, refl, spec, high)
			TRUE			; antialias
			FALSE 		; tile
			FALSE 		; new image
			TRUE 			; transparency
			0.225  		; radius
			1 1 1 1 		; unused parameters
			-1 -1 -1 -1 -1 -1 -1 -1)

    (plug-in-map-object RUN-NONINTERACTIVE theImage cloudLayer 
			1			; mapping
			0.5 0.5 2.0		; viewpoint
			0.5 0.5 0.9		; object pos
			1.0 0.0 0.0		; first axis
			0.0 1.0 0.0		; 2nd axis
			0.0 0.0 0.0		; axis rotation
			2 '(255 255 255)	; light (type, color)
			-0.5 -0.5 2		; light position
			-1.0 -1.0 1.0	; light direction
			0.3 1 0.5 0.3 27	; material (amb, diff, refl, spec, high)
			TRUE			; antialias
			FALSE 		; tile
			FALSE 		; new image
			TRUE 			; transparency
			0.225  		; radius
			1 1 1 1 		; unused parameters
			-1 -1 -1 -1 -1 -1 -1 -1)

    (plug-in-map-object RUN-NONINTERACTIVE theImage map
			1			; mapping
			0.5 0.5 2.0		; viewpoint
			0.5 0.5 0.9		; object pos
			1.0 0.0 0.0		; first axis
			0.0 1.0 0.0		; 2nd axis
			0.0 0.0 0.0		; axis rotation
			2 '(255 255 255)	; light (type, color)
			-0.5 -0.5 2		; light position
			-1.0 -1.0 1.0	; light direction
			0.3 1 0.5 0.3 27	; material (amb, diff, refl, spec, high)
			TRUE			; antialias
			FALSE 		; tile
			FALSE 		; new image
			TRUE 			; transparency
			0.227  		; radius
			1 1 1 1 		; unused parameters
			-1 -1 -1 -1 -1 -1 -1 -1)

; Get the layers in the correct order
	(gimp-image-lower-layer theImage map)
	(gimp-image-raise-layer theImage surfaceLayer)

; Use the bumpmap, and scale the Depth to the size of the image
	(define depth (* 65 theRatio))
	(plug-in-bump-map RUN-NONINTERACTIVE theImage surfaceLayer map 135 40 depth 0 0 0 0 1 0 0)
	(plug-in-bump-map RUN-NONINTERACTIVE theImage surfaceLayer map 135 40 depth 0 0 0 0 1 0 0)

; Blur the bump-map layer to make an atmospheric glow around the planet
	(define blurratio (* 500 theRatio))
	(plug-in-gauss RUN-NONINTERACTIVE theImage map blurratio blurratio 0)

; To make the planet Earth-like, use the Land and Sea Gradient
    (if (= EarthLike TRUE)
	  (gimp-context-set-gradient "Land and Sea"))

; Change the lighting mode on the Cloud layer to Soft Light
    (if (= EarthLike TRUE)
	  (gimp-layer-set-mode cloudLayer SOFTLIGHT-MODE))

; Run the Gradient Map on the Cloud Layer.
    (if (= EarthLike TRUE)
	  (plug-in-gradmap RUN-NONINTERACTIVE theImage cloudLayer))

; Add in the Shadow Layer
    (if (= SunShadow  TRUE)
	(gimp-image-add-layer theImage shadowLayer 0))

    (if (= SunShadow  TRUE)
	(gimp-ellipse-select theImage 0 (* (- 4 SunScale) tenth) inSize inSize 2 1 0 0 ))

    (if (= SunShadow  TRUE)
	(gimp-edit-bucket-fill shadowLayer 0 0 100 0 FALSE 0 0 ))

    (if (= SunShadow  TRUE)
	(gimp-selection-none theImage))

    (if (= SunShadow  TRUE)
	(plug-in-gauss RUN-NONINTERACTIVE theImage shadowLayer blurratio blurratio 0))

    (if (= SunShadow  TRUE)
	(gimp-selection-layer-alpha surfaceLayer))

    (if (= SunShadow  TRUE)
	(gimp-selection-invert theImage))

    (if (= SunShadow  TRUE)
	(gimp-edit-cut shadowLayer))

    (if (= SunShadow  TRUE)
	(gimp-drawable-transform-rotate-default shadowLayer sunAngle TRUE (/ inSize 2.0) (/ inSize 2.0) TRUE 0))

    (if (= SunShadow  TRUE)
	(gimp-layer-resize-to-image-size shadowLayer))

	(gimp-display-new theImage)

  )
)

; Register the function with GIMP:

(script-fu-register
  "script-fu-planet-setup"
  _"_Planet Setup..."
  _"Create a planet, either black and white or earth-colored.  Shadow layers optional."
  "James Sambrook"
  "13 August 2008"
  "James Sambrook.  King George, VA, USA"
  ""
  SF-TOGGLE     _"Earth-like"                TRUE
  SF-TOGGLE     _"Sun Shadow"                TRUE
  SF-ADJUSTMENT _"Image size"                '(500 300 3000 1 50 0 0)
  SF-ADJUSTMENT _"Surface Turbulence"        '(4 1 15 0.1 1 1 0)
  SF-ADJUSTMENT _"Cloud Turbulence"          '(2.5 1 15 0.1 1 1 0)
  SF-ADJUSTMENT _"Bump Map Detail"           '(7 1 15 0.1 1 1 0)
  SF-ADJUSTMENT _"Bump Map Size"             '(4 1 15 0.1 1 1 0)
  SF-ADJUSTMENT _"Sun orientation (degrees)" '(45 0 360 1 10 1 0)
  SF-ADJUSTMENT _"Shadow Depth"              '(2 1 3 0.1 0.5 1 0)
  SF-ADJUSTMENT _"Background Star Density"   '(6 1 8 1 1 0 1)
  SF-ADJUSTMENT _"Hue"                       '(70 1 180 1 5 0 0)
  SF-ADJUSTMENT _"Saturation"                '(10 1 255 1 5 0 0)
  SF-ADJUSTMENT _"Value"                     '(100 1 255 1 5 0 0)

)

(script-fu-menu-register "script-fu-planet-setup"
                         "<Image>/Filters/SambrookJM/")
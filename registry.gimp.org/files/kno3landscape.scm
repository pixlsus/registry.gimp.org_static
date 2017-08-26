;this script written by and copyright Bryan Knowles, KNO3 Projects

(define (script-fu-kno3landscape W H thumb seed1 seed2 crack detail depth blur lean edge interpole remgrid flatten)
	;varible set up
	(if (= thumb TRUE) (set! W 400))
	(if (= thumb TRUE) (set! H 300))
	(if (= thumb TRUE) (set! lean 62))
	(if (= thumb TRUE) (set! edge 3))
	(define leanW (- W lean))
	(define leanH (- H lean))
	(define leanE (- W edge))
	(define crack2 (- 0 crack))

	;make new image, add layers, fill white, invert
	(define img (car (gimp-image-new W H 0)))
	(define cloud (car (gimp-layer-new img W H 1 "Cloud" 100 0)))
	(define map (car (gimp-layer-new img W H 1 "Map" 100 0)))
	(gimp-image-add-layer img cloud -1)
	(gimp-image-add-layer img map -1)
	(gimp-display-new img)
	(gimp-drawable-fill cloud 2)
	(gimp-drawable-fill map 2)
	(gimp-invert cloud)
	(gimp-invert map)

	;add noise, 0.1 x, 1.0 y
	(plug-in-solid-noise 1 img map 0 0 seed1 1 0.1 1.0)

	;brightness contrast, -crack/crack 
	(gimp-brightness-contrast map crack2 crack)

	;make, add, and fill noise layer
	(define noise (car (gimp-layer-new img W H 1 "Noise Layer" 100 0)))
	(gimp-image-add-layer img noise -1)
	(gimp-drawable-fill noise 2)
	(gimp-invert noise)
	(gimp-layer-set-mode noise 6)
	(plug-in-solid-noise 1 img noise 0 0 seed1 detail 4 4)

	;add noise to cloud
	(plug-in-solid-noise 1 img cloud 0 0 seed2 detail 4 4)	

	;make stone layer
	(set! map (car (gimp-image-merge-down img noise 0)))
	(plug-in-bump-map 1 img cloud map 135 45 depth 0 0 0 0 TRUE TRUE 0)
	(gimp-image-lower-layer-to-bottom img map)

	;copy, blur
	(define blurred (car (gimp-layer-copy cloud 1)))
	(gimp-image-add-layer img blurred -1)
	(plug-in-gauss 1 img blurred blur blur 0)

	;make grid layer
	(define grid (car (gimp-layer-new img W H 1 "Grid Layer" 100 0)))
	(gimp-image-add-layer img grid -1)
	(gimp-edit-clear grid)
	(plug-in-grid 1 img grid 1 16 8 '(0 0 0) 255 1 16 8 '(0 0 0) 255 0 2 6 '(0 0 0) 255)

	;displace
	(plug-in-displace 1 img grid 20 20 1 1 blurred blurred 0)

	;blur, mode, merge
	(plug-in-gauss 1 img grid 1 1 0)
	(gimp-layer-set-mode grid 19)
	(if (= TRUE remgrid) (gimp-layer-set-opacity grid 0))
	(define guide (car (gimp-image-merge-down img grid 0)))

	;set to darken only
	(gimp-layer-set-mode cloud 9)

	;perspective
	(define anchor (car (gimp-perspective guide interpole lean lean leanW lean edge leanH leanE leanH)))

	;fill, flatten, flush
	(gimp-drawable-fill map 0)
	(if (= TRUE flatten) (gimp-image-flatten img))
	(gimp-displays-flush)
	(gimp-image-clean-all img)
	

);end define

(script-fu-register "script-fu-kno3landscape"
		_"<Toolbox>/KNO3/Landscape Map"
		"Creates a Faux 3D Landscape Map"
		"KNO3"
		"KNO3"
		"July 24 2008"
		""
		SF-ADJUSTMENT "Width" '(800 200 1600 100 1 0 1)
		SF-ADJUSTMENT "Height" '(600 150 1200 100 1 0 1)
		SF-TOGGLE "Use Thumbnail Sizes" FALSE
		SF-ADJUSTMENT "Seed 1" '(26999 0 99999 1 1 0 1)
		SF-ADJUSTMENT "Seed 2" '(74999 0 99999 1 1 0 1)
		SF-ADJUSTMENT "Balance" '(80 -127 127 1 1 0 1)
		SF-ADJUSTMENT "Detail" '(15 1 15 1 1 0 1)
		SF-ADJUSTMENT "Depth" '(60 5 80 1 1 0 1)
		SF-ADJUSTMENT "Blur" '(40 1 100 1 1 0 1)
		SF-ADJUSTMENT "Lean" '(125 5 500 63 1 0 1)
		SF-ADJUSTMENT "Edge" '(5 0 100 5 1 0 1)
		SF-TOGGLE "Interpolate" FALSE
		SF-TOGGLE "Remove Grid" FALSE
		SF-TOGGLE "Flatten" TRUE
)
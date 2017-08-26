;;mhwaterpattern
;Kiki
;http://kikidide.yuki-mura.net/GIMP/gimp.htm
;


(define (script-fu-mhwaterpat width height seedr wtype type? rnd? more?)
  (let* ((img (car (gimp-image-new 256 256 RGB)))
	 (drawable (car (gimp-layer-new img width height RGB "drawable" 100 NORMAL)))

         (old-fg (car (gimp-palette-get-foreground)))
         (old-bg (car (gimp-palette-get-background)))
	 (float) (f-layer) (g-layer)
)
    (gimp-image-undo-disable img)

;;サイズ調整
    (gimp-image-resize img width height 0 0)

    (if (= rnd? TRUE) (set! seedr (rand 1677216)))

    (gimp-image-add-layer img drawable 0)			;レイヤー追加
    (plug-in-plasma 1 img drawable seedr 5)
    (gimp-desaturate drawable)
;;    (gimp-image-scale img width height)
;;    (gimp-layer-resize drawable width height 0 0)

    (gimp-channel-ops-offset drawable FALSE OFFSET-TRANSPARENT 1 (- height 1))
    (gimp-rect-select img 1 (- height 1) (- width 1) 1 REPLACE FALSE 0)
    (set! float (car (gimp-selection-float drawable 0 (- 1 height))))
    (gimp-floating-sel-to-layer float)
    (gimp-layer-scale float (- width 1) height FALSE)
    (set! drawable (car (gimp-image-merge-down img float EXPAND-AS-NECESSARY)))
    (set! drawable (car (gimp-image-flatten img)))		;delete alpha

(cond((= wtype 0)
    (plug-in-ripple 1 img drawable 40 10 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 100 5 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 50 10 1 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 60 20 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 200 35 1 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 25 10 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 200 15 1 1 1 TRUE TRUE)
)
    ((= wtype 1)
    (plug-in-ripple 1 img drawable 200 30 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 40 10 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 200 15 1 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 50 10 1 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 60 20 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 200 15 1 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 200 10 0 1 1 TRUE TRUE)
    
)
    ((= wtype 2)
    (plug-in-ripple 1 img drawable 20 5 1 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 150 10 1 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 75 5 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 30 20 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 100 40 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 15 10 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 150 5 1 1 1 TRUE TRUE)
))


    (if (= TRUE more?)
    (begin
    (plug-in-ripple 1 img drawable 30 20 0 1 1 TRUE TRUE)
    (if (< wtype 2)
    (begin
    (plug-in-ripple 1 img drawable 200 30 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 200 15 1 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 150 20 0 1 1 TRUE TRUE)
    ))
    (if (= wtype 2)
    (begin
    (plug-in-ripple 1 img drawable 50 30 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 25 15 1 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 80 20 0 1 1 TRUE TRUE)
    ))
    ))


    (set! f-layer (car (gimp-layer-copy drawable 0)))
    (gimp-image-add-layer img f-layer 0)
    (gimp-layer-set-mode f-layer SCREEN-MODE)
    (plug-in-ripple 1 img drawable 55 10 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 80 30 0 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 100 15 1 1 1 TRUE TRUE)
    (plug-in-ripple 1 img drawable 50 20 0 1 1 TRUE TRUE)

    (set! g-layer (car (gimp-layer-copy drawable 0)))
    (gimp-image-add-layer img g-layer 0)
    (gimp-palette-set-foreground '(0 0 0))
    (gimp-palette-set-background '(255 255 255))
    (gimp-edit-blend g-layer 0 0 LINEAR 100 0 REPEAT-NONE FALSE FALSE 0 0 FALSE 0 0 0 (* height 1.5))
    (gimp-layer-set-mode g-layer OVERLAY-MODE)

    (if (= type? 0)
    (begin
     (define (splineValueR)
       (let* ((a (cons-array 6 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  43 0)
         (set-pt a 2  255 255)
         a
       )
     )
     (gimp-curves-spline drawable RED-LUT 6 (splineValueR))
     (define (splineValueG)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  40 58)
         (set-pt a 2  192 218)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable GREEN-LUT 6 (splineValueG))
     (define (splineValueB)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  167 225)
         (set-pt a 2  255 242)
         a
       )
     )
     (gimp-curves-spline drawable BLUE-LUT 6 (splineValueB))
    ))

    (if (= type? 1)
    (begin
     (define (splineValue)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  98 66)
         (set-pt a 2  172 195)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable VALUE-LUT 8 (splineValue))
     (define (splineValueR)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  98 57)
         (set-pt a 2  202 219)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable RED-LUT 8 (splineValueR))
     (define (splineValueG)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  48 104)
         (set-pt a 2  183 211)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable GREEN-LUT 8 (splineValueG))
     (define (splineValueB)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  14 84)
         (set-pt a 2  65 130)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable BLUE-LUT 8 (splineValueB))
    ))

    (if (= type? 2)
    (begin
     (define (splineValueR)
       (let* ((a (cons-array 6 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  102 33)
         (set-pt a 2  255 164)
         a
       )
     )
     (gimp-curves-spline drawable RED-LUT 6 (splineValueR))
     (define (splineValueG)
       (let* ((a (cons-array 6 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  134 59)
         (set-pt a 2  255 255)
         a
       )
     )
     (gimp-curves-spline drawable GREEN-LUT 6 (splineValueG))
     (define (splineValueB)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  62 147)
         (set-pt a 2  158 220)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable BLUE-LUT 8 (splineValueB))
     (define (splineValue)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  74 149)
         (set-pt a 2  178 155)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable VALUE-LUT 8 (splineValue))
    ))

    (if (= type? 3)
    (begin
     (define (splineValueR)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  33 96)
         (set-pt a 2  160 241)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable RED-LUT 8 (splineValueR))
     (define (splineValueG)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  76 122)
         (set-pt a 2  193 232)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable GREEN-LUT 8 (splineValueG))
     (define (splineValueB)
       (let* ((a (cons-array 6 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  36 92)
         (set-pt a 2  255 255)
         a
       )
     )
     (gimp-curves-spline drawable BLUE-LUT 6 (splineValueB))
     (define (splineValue)
       (let* ((a (cons-array 6 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  205 255)
         (set-pt a 2  255 255)
         a
       )
     )
     (gimp-curves-spline drawable VALUE-LUT 6 (splineValue))
    ))


    (set! drawable (car (gimp-image-merge-visible-layers img 0)))


    (if (= type? 4)
    (begin
     (define (splineValue)
       (let* ((a (cons-array 6 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  129 34)
         (set-pt a 2  255 255)
         a
       )
     ) 
     (gimp-curves-spline drawable VALUE-LUT 6 (splineValue))
     (define (splineValueR)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  65 102)
         (set-pt a 2  130 117)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable RED-LUT 8 (splineValueR))
     (define (splineValueG)
       (let* ((a (cons-array 6 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  133 110)
         (set-pt a 2  255 255)
         a
       )
     )
     (gimp-curves-spline drawable GREEN-LUT 6 (splineValueG))
     (define (splineValueB)
       (let* ((a (cons-array 6 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  130 78)
         (set-pt a 2  255 255)
         a
       )
     )
     (gimp-curves-spline drawable BLUE-LUT 6 (splineValueB))
    ))

    (if (= type? 5)
    (begin
     (define (splineValue)
       (let* ((a (cons-array 6 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  133 38)
         (set-pt a 2  255 158)
         a
       )
     ) 
     (gimp-curves-spline drawable VALUE-LUT 6 (splineValue))
     (define (splineValueR)
       (let* ((a (cons-array 6'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  86 86)
         (set-pt a 2  255 200)
         a
       )
     )
     (gimp-curves-spline drawable RED-LUT 6 (splineValueR))
    ))

    (if (= type? 6)
    (begin
     (define (splineValue)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  20 38)
         (set-pt a 2  200 115)
         (set-pt a 3  255 234)
         a
       )
     ) 
     (gimp-curves-spline drawable VALUE-LUT 8 (splineValue))
     (define (splineValueR)
       (let* ((a (cons-array 6 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  86 21)
         (set-pt a 2  255 200)
         a
       )
     )
     (gimp-curves-spline drawable RED-LUT 6 (splineValueR))
     (define (splineValueG)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  133 78)
         (set-pt a 2  200 187)
         (set-pt a 3  255 200)
         a
       )
     )
     (gimp-curves-spline drawable GREEN-LUT 8 (splineValueG))
     (define (splineValueB)
       (let* ((a (cons-array 8 'byte)))
         (set-pt a 0  0 0)
         (set-pt a 1  100 58)
         (set-pt a 2  130 88)
         (set-pt a 3  255 255)
         a
       )
     )
     (gimp-curves-spline drawable BLUE-LUT 8 (splineValueB))
    ))





    (gimp-palette-set-foreground old-fg)
    (gimp-palette-set-background old-bg)

    (gimp-image-undo-enable img)
    (gimp-display-new img)
    (if (= TRUE rnd?) (gimp-message (string-append "seed=" (number->string seedr))))
))

(script-fu-register "script-fu-mhwaterpat"
			"<Toolbox>/Xtns/Script-Fu/MHPattern/water pattern"
			"Create watter pattern"
			"Kiki"
			"Kiki"
			"2005/1"
			""
			SF-ADJUSTMENT  _"width"       '(640 1 2048 1 2 0 1)
			SF-ADJUSTMENT  _"height"      '(480 1 1600 1 2 0 1)
			SF-ADJUSTMENT  _"seedr"      '(54986 1 16777216 1 10 0 1)
			SF-OPTION	"wave type?"	'(_"0(normal)" _"1(calm)" _"2(ripple)")
			SF-OPTION	"color type?"	'(_"0(water)" _"1(blue_green)" _"2(Ice_sea)"_"3(Sun set)" _"4(Red sea!)" _"5(Indian ink)" _"6(Midnight)")
			SF-TOGGLE	"random?"	FALSE
			SF-TOGGLE	"more wave?"	FALSE
)

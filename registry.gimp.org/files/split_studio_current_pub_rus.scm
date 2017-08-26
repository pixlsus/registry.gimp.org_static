;Split Studio ver. 3.0r5
;Extract masks of light and dark areas of the image and tone them into two different colors.
;Version history:
;==================================================================
;ver. 0.1а (September 2009)
; - broken script;
;==================================================================
;ver. 0.3 (September 2009)
; - working script, only with mask extracting;
;==================================================================
;ver. 0.5 (September 2009)
; - script's release;
;==================================================================
;ver. 0.7 (September 2009)
; - some changes in script's procedures;
;==================================================================
;ver. 1.0 (September 2009)
; - additional levels expression;
; - work with visible;
;==================================================================
;ver. 2.0 (23th October 2009)
; - added controls of the mask blending;
;==================================================================
;ver. 2.2 (7th December 2009)
; - remove some useless procedures;
; - interface optimization;
; - affect on contrast control;
; - undo/rebo support;
; - first public release;
;==================================================================
;ver. 2.4 (15th January 2010) #development release
; - faster layers merging;
; - source mask searching (#SPL);
;==================================================================
;ver. 3.0 (22th January 2010)
; - optimization in script's structure;
; - optional affect layer creation;
; - sublayer desaturation (optional);
;==================================================================
;ver. 3.0r1 (25th February 2010)
; - redesign source mask selection;
;==================================================================
;ver. 3.0r2 (13 March 2010)
; - bugfix for undo/rebo support;
;==================================================================
;ver. 3.0r3 (30 March 2010)
; - change name from SpliX to Split Studio (name conflict);
; - change default menu path;
;==================================================================
;ver. 3.0r4 (31 October 2010)
; - GAL API implementation;
;==================================================================
;ver. 3.0r5 (June 19 2012)
; - GIMP 2.8 native port;
;==================================================================

;List of input variables:
;image - processed image;
;sEdge - offset of the mask edge;
;sRez - width of the range in the mask;
;sLight - color for light mask;
;sDark - color for dark mask;
;lightboost - additional saturation for light area of the result image;
;opcLight - control for blending light mask;
;opcDark - control for blending dark nask;
;aff - control affection on contrast of the result image;
;mask_custom - custom mask mode switch;
;mask_def - layer with custom mask;
;srcswitch - toggle for saving source layers;
;viz - toggle for working with visible;
;des - toggle for sublayer's desaturation;
(define (script-fu-split3 image sEdge sRez sLight sDark lightboost opcLight opcDark aff mask_custom mask_def srcswitch viz des)


;Undo group start
(gimp-image-undo-group-start image)

  (let* (
	(sublayer (car (split3-source-handle image viz srcswitch)))
	(light (car (gimp-layer-copy sublayer FALSE)))
	(dark (car (gimp-layer-copy sublayer FALSE)))
	(aff-state)
	(affect)
	(aff-src)
	(mask-aff)
	(lightmask)
	(low-input (+ (+ 0 sEdge) sRez))
	(high-input (- (+ 255 sEdge) sRez))
	(reslayer)
	)

	;Checking and fix variables
	(if (> high-input 255)
	  (set! high-input 255)
	)
	(if (< low-input 0)
	  (set! low-input 0)
	)

	;Desaturation section
	(if (= des TRUE)
	  (gimp-desaturate sublayer)
	)
	(gimp-image-insert-layer image dark -1 -1)
	(gimp-image-insert-layer image light -1 -1)
	(gimp-item-set-name light "Светлый тон")
	(gimp-item-set-name dark "Темный тон")

	;Getting mask and affect source
	(set! mask-aff (split3-mask-handle image sublayer mask_def mask_custom))
	(set! lightmask (car mask-aff))
	(set! aff-src (cadr mask-aff))

	;affection section
	(if (> aff 0)
	  (begin
	    (set! affect (car (gimp-layer-copy aff-src FALSE)))
	    (set! aff-state (car (gimp-item-get-visible affect)))

	    ;cheking visibility
	    (if (= aff-state FALSE)
	      (gimp-item-set-visible affect TRUE)
	    )
	    (gimp-image-insert-layer image affect -1 -1)
	    (gimp-item-set-name affect "Контраст")
	    (gimp-layer-set-mode affect 5)
	    (gimp-desaturate affect)
	    (gimp-levels affect 0 low-input high-input 1.0 0 255)
	    (gimp-layer-set-opacity affect aff)
	    (gimp-image-lower-item image affect)
	  )
	)
	(gimp-layer-add-mask light lightmask)
	(gimp-levels lightmask 0 low-input high-input 1.0 0 255)
	(plug-in-colorify 1 image light sLight)
	(plug-in-colorify 1 image dark sDark)
	(gimp-hue-saturation light 0 0 0 lightboost)
	(gimp-layer-set-mode light 13)
	(gimp-layer-set-mode dark 13)
	(gimp-layer-set-opacity light opcLight)
	(gimp-layer-set-opacity dark opcDark)
	
	;final merging
	(if (= srcswitch FALSE)
	    (begin
		(set! reslayer (car (gimp-image-merge-down image dark 0)))
		(if (> aff 0)
		  (set! reslayer (car (gimp-image-merge-down image affect 0)))
		)
		(set! reslayer (car (gimp-image-merge-down image light 0)))
		(gimp-item-set-name reslayer "Результат")
	    )
	)

	;Display flushing
	(gimp-displays-flush)
  )

;end of the undo group
(gimp-image-undo-group-end image)

)

(script-fu-register
"script-fu-split3"
"Spli_t Studio 3"
"Тонировка светлых и темных участков изображения в разные тона"
"Непочатов Станислав"
"Свободная лицензия"
"31 октября 2010"
"*"
SF-IMAGE		"Изображение"			0
SF-ADJUSTMENT	"Сдвиг границы"			'(0 -120 120 10 30 1 0)
SF-ADJUSTMENT	"Резкость границы"		'(0 0 120 10 30 1 0)
SF-COLOR		"Светлый тон"			'(200 175 140)
SF-COLOR		"Темный тон"			'(80 102 109)
SF-ADJUSTMENT	"Усиление светлого"		'(75 0 100 10 30 1 0)
SF-ADJUSTMENT	"Прозрачность светлого"		'(100 0 100 10 25 1 0)
SF-ADJUSTMENT	"Прозрачность темного"		'(100 0 100 10 25 1 0)
SF-ADJUSTMENT	"Влияние на контраст"		'(0 0 100 10 25 1 0)
SF-TOGGLE	"Пользовательская маска"		FALSE
SF-DRAWABLE	"Маска-источник"			5
SF-TOGGLE	"Сохранение процедурных слоев"	FALSE
SF-TOGGLE	"Работать с видимым"		FALSE
SF-TOGGLE	"Обесцвечивание подслоя"		FALSE
)

(script-fu-menu-register
"script-fu-split3"
_"<Image>/Filters/RSS"
)

;split3-source-handle
;SERVICE PROCEDURE
;List of input variables:
;image - processed image;
;viz - toggle for working with visible;
;srcswitch - toggle for saving source layers;
;List of output variables:
;exit - (layer) proper source layer for main script;
(define (split3-source-handle image viz srcswitch)
(define exit)
  (let* (
	(active (car (gimp-image-get-active-layer image)))
	(exit-layer)
	)
	(if (= viz TRUE)
	  (begin
	    (gimp-edit-copy-visible image)
	    (set! exit-layer 
	      (car
		(gimp-edit-paste active TRUE)
	      )
	    )
	    (gimp-floating-sel-to-layer exit-layer)
	    (gimp-item-set-name exit-layer "Источник = Видимое")
	    (gimp-image-raise-item-to-top image exit-layer)
	  )
	  (begin
	    (set! exit-layer (car (gimp-layer-copy active FALSE)))
	    (gimp-image-insert-layer image exit-layer -1 -1)
	    (gimp-item-set-name exit-layer "Источник = Копия")
	  )
	)
	(set! exit exit-layer)
  )
(cons exit '())
)

;split3-mask-handle
;SERVICE PROCEDURE
;List of input variables:
;image - processed image;
;imput-layer - layer by default;
;def_layer - layer defined by user;
;mask_custom - custom mask mode;
;List of output variables:
;list - (LIST) result list with exit mask (CHANNEL) and exit layer (LAYER);
(define (split3-mask-handle image input_layer def_layer mask_custom)
(define o-mask 0)
(define o-layer 0)
(if (= mask_custom TRUE)
  (begin
    (set! o-layer def_layer)
    (set! o-mask (car (gimp-layer-create-mask def_layer 5)))
  )
  (begin
    (set! o-layer input_layer)
    (set! o-mask (car (gimp-layer-create-mask input_layer 5)))
  )
)
(list o-mask o-layer)
)
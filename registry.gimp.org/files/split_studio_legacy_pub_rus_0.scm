;Split Studio ver. 3.0r3
;Извлечение масок светлых и темных участков изображения с последующей тонировкой в разные оттенки.
;История версий:
;==================================================================
;ver. 0.1а (Сентябрь 2009)
; - нерабочий скрипт;
;==================================================================
;ver. 0.3 (Сентябрь 2009)
; - рабочий скрипт, только извлечение масок;
;==================================================================
;ver. 0.5 (Сентябрь 2009)
; - релиз скрипта;
;==================================================================
;ver. 0.7 (Сентябрь 2009)
; - некоторые изменения в процедурах скрипта;
;==================================================================
;ver. 1.0 (Сентябрь 2009)
; - дополнительные выражения для контроля уровней;
; - поддержка работы с видимым;
;==================================================================
;ver. 2.0 (23 Октябра 2009)
; - добавлен контроль над прозрачностью слоев;
;==================================================================
;ver. 2.2 (7 Декабря 2009)
; - удаление ненужных процедур;
; - оптимизация интерфейса;
; - управление влиянием на контраст;
; - поддержка стека отмены;
; - первый публичный релиз;
;==================================================================
;ver. 2.4 (15 Января 2010) #тестовый релиз (не публиковался)
; - более быстрое сведение готовых слоев;
; - поиск маски-источника (#SPL);
;==================================================================
;ver. 3.0 (22 Января 2010)
; - оптимизация структуры скрипта;
; - опциональное создание контрастирующего слоя;
; - обесцвечивание подслоя (опционально);
;==================================================================
;ver. 3.0r1 (25 Февраля 2010)
; - переработка указания маски-источника;
;==================================================================
;ver. 3.0r2 (13 Марта 2010)
; - исправление для отмены;
;==================================================================
;ver. 3.0r3 (30 Марта 2010)
; - смена имени SpliX на Split Studio, по причине конфликта имен;
; - смена пути в меню:
;==================================================================

;Список входных переменных:
;image - обрабатываемое изображение;
;sEdge - сдвиг границы маски;
;sRez - ширина диапозона в маске;
;sLight - цвет для светлого слоя;
;sDark - цвет для темного слоя;
;lightboost - дополнительное усиление насыщенности светлого слоя;
;opcLight - управление прозрачностью светлого слоя;
;opcDark - управление прозрачностью темного слоя;
;aff - управление контрастирующим слоем;
;mask_custom - переключатель на режим пользовательской маски;
;mask_def - слой с маской-источником;
;srcswitch - включение режима сохранения процедурных слоев;
;viz - включение режима работы с видимым;
;des - включение обесцвечивания подслоя;
(define (script-fu-split3 image sEdge sRez sLight sDark lightboost opcLight opcDark aff mask_custom mask_def srcswitch viz des)

;Начало группировки действий
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

	;Проверка диапазона переменых и поправка
	(if (> high-input 255)
	  (set! high-input 255)
	)
	(if (< low-input 0)
	  (set! low-input 0)
	)

	;Секция обесцвечивания
	(if (= des TRUE)
	  (gimp-desaturate sublayer)
	)
	(gimp-image-add-layer image dark -1)
	(gimp-image-add-layer image light -1)
	(gimp-drawable-set-name light "Светлый тон")
	(gimp-drawable-set-name dark "Темный тон")

	;Получение маски и источника контрастирующего слоя
	(set! mask-aff (split3-mask-handle image sublayer mask_def mask_custom))
	(set! lightmask (car mask-aff))
	(set! aff-src (cadr mask-aff))

	;секция управления контрастом
	(if (> aff 0)
	  (begin
	    (set! affect (car (gimp-layer-copy aff-src FALSE)))
	    (set! aff-state (car (gimp-layer-get-visible affect)))

	    ;проверка виидимости
	    (if (= aff-state FALSE)
	      (gimp-layer-set-visible affect TRUE)
	    )
	    (gimp-image-add-layer image affect -1)
	    (gimp-drawable-set-name affect "Контраст")
	    (gimp-layer-set-mode affect 5)
	    (gimp-desaturate affect)
	    (gimp-levels affect 0 low-input high-input 1.0 0 255)
	    (gimp-layer-set-opacity affect aff)
	    (gimp-image-lower-layer image affect)
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
	
	;финальное сведение слоев
	(if (= srcswitch FALSE)
	    (begin
		(set! reslayer (car (gimp-image-merge-down image dark 0)))
		(if (> aff 0)
		  (set! reslayer (car (gimp-image-merge-down image affect 0)))
		)
		(set! reslayer (car (gimp-image-merge-down image light 0)))
		(gimp-drawable-set-name reslayer "Результат")
	    )
	)

	;Обновление изображения
	(gimp-displays-flush)
  )

;Завершение группировки действий
(gimp-image-undo-group-end image)

)

(script-fu-register
"script-fu-split3"
"Spli_t Studio 3"
"Тонировка светлых и темных участков изображения в разные тона"
"Непочатов Станислав"
"Свободная лицензия"
"13 марта 2010"
"*"
SF-IMAGE	"Изображение"			0
SF-ADJUSTMENT	"Сдвиг границы"			'(0 -120 120 10 30 1 0)
SF-ADJUSTMENT	"Резкость границы"		'(0 0 120 10 30 1 0)
SF-COLOR	"Светлый тон"			'(200 175 140)
SF-COLOR	"Темный тон"			'(80 102 109)
SF-ADJUSTMENT	"Усиление светлого"		'(75 0 100 10 30 1 0)
SF-ADJUSTMENT	"Прозрачность светлого"		'(100 0 100 10 25 1 0)
SF-ADJUSTMENT	"Прозрачность темного"		'(100 0 100 10 25 1 0)
SF-ADJUSTMENT	"Влияние на контраст"		'(0 0 100 10 25 1 0)
SF-TOGGLE	"Пользовательская маска"	FALSE
SF-DRAWABLE	"Маска-источник"		5
SF-TOGGLE	"Сохранение процедурных слоев"	FALSE
SF-TOGGLE	"Работать с видимым"		FALSE
SF-TOGGLE	"Обесцвечивание подслоя"	FALSE
)

(script-fu-menu-register
"script-fu-split3"
_"<Image>/Filters/RSS"
)

;split3-source-handle
;СЛУЖЕБНАЯ ПРОЦЕДУРА
;Список входящих переменных:
;image - обрабатываемое изображение;
;viz - включение режима работы с видимым;
;srcswitch - включение режима сохранения процедурных слоев;
;Список возвращаемых переменных:
;exit - (LAYER) готовый слой-источник для основного скрипта;
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
	    (gimp-drawable-set-name exit-layer "Источник = Видимое")
	    (gimp-image-raise-layer-to-top image exit-layer)
	  )
	  (begin
	    (set! exit-layer (car (gimp-layer-copy active FALSE)))
	    (gimp-image-add-layer image exit-layer -1)
	    (gimp-drawable-set-name exit-layer "Источник = Копия")
	  )
	)
	(set! exit exit-layer)
  )
(cons exit '())
)

;split3-mask-handle
;СЛУЖЕБНАЯ ПРОЦЕДУРА
;Список входящих переменных:
;image - обрабатываемое изображение;
;imput-layer - слой по умолчанию;
;def_layer - слой указынный пользователем;
;mask_custom - режим пользовательской маски;
;список возвращаемых переменных:
;list - (LIST) готовый список с маской (CHANNEL) и со слоем (LAYER);
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
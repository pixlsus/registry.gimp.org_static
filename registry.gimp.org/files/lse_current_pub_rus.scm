;LSE v2.1r1
;
;LSE (Lightsaber effect) - скрипт создания эффекта светового меча;
;
;История версий:
;==================================================================
;ver. 0.3 (сентябрь 2009)
; - рабочий скрипт без совмещения слоев;
;==================================================================
;ver. 0.6 (октябрь 2009)
; - финальная сборка с совмещением слоев;
;==================================================================
;ver. 0.8 (ноябрь 2009)
; - верхний предел для переменной sabsize увеличен до 350;
;==================================================================
;ver. 0.9 (ноябрь 2009)
; - первый публичный релиз;
;==================================================================
;ver. 1.0 (декабрь 2009)
; - режим "белого" наложения;
; - перестройка скрипта и оптимизация;
; - лучшее качество размытия с помощью дополнительного прохода;
; - поддержка стека отмены;
;==================================================================
;ver. 1.0r1 (февраль 2010)
; - опциональная поддержка стека отмены.
; - исправление совмещения слоев.
;==================================================================
;ver. 1.0r2 (30 сентября 2010)
; - использование отдельного изображения для поддержки стека отмены.
;==================================================================
;ver. 2.0 (18 апреля 2011)
; - более быстрое ядро (эксперементально);
; - раздельные цвета для основного свечения и ядра;
; - цветовые профили;
;==================================================================
;ver. 2.1 (17 декабря 2011)
; - новое ядро теперь используется по умолчанию;
; - относительная система установки велечины свечения;
; - оптимизация и отчистка от старого кода;
;==================================================================
;ver 2.1r1 (1 августа 2012)
; - упрощение структуры цветовых профилей;
; - портирование на GIMP 2.8;
;==================================================================

;Cписок цветовых профилей
(define lse-presets
  (list
    (list
      "Цвета пользователя" 
      (quote 'nil)
    )
    (list
      "Цвет ядра => Цвет свечения" 
      (quote (set! sabColor coreColor))
    )
    (list
      "Цвет свечения => Цвет ядра"
      (quote (set! coreColor sabColor))
    )
    (list
      "Классический синий"
      (quote (begin (set! coreColor '(42 170 255)) (set! sabColor '(42 156 255))))
    )
    (list
      "Классический зеленый"
      (quote (begin (set! coreColor '(30 255 252)) (set! sabColor '(30 255 35))))
    )
    (list
      "Классический красный"
      (quote (begin (set! coreColor '(255 41 102)) (set! sabColor '(255 41 41))))
    )
    (list
      "Новый синий"
      (quote (begin (set! coreColor '(30 173 255)) (set! sabColor '(30 120 255))))
    )
    (list
      "Новый зеленый"
      (quote (begin (set! coreColor '(152 255 30)) (set! sabColor '(41 255 30))))
    )
    (list
      "Новый красный"
      (quote (begin (set! coreColor '(255 95 70)) (set! sabColor '(255 30 30))))
    )
    (list
      "Фиолетовый"
      (quote (begin (set! coreColor '(210 30 255)) (set! sabColor '(184 30 255))))
    )
    (list 
      "Оранжевый"
      (quote (begin (set! coreColor '(255 170 35)) (set! sabColor '(255 144 35))))
    )
    (list
      "Серебрянный"
      (quote (begin (set! coreColor '(159 172 195)) (set! sabColor coreColor)))
    )
    (list
      "Изумрудный"
      (quote (begin (set! coreColor '(120 174 94)) (set! sabColor '(94 174 96))))
    )
  )
)

;lse-get-presets
;Функция извлечения списка имен цветовых профилей
;Не имеет аргументов
;ВОЗВРАЩАЕТ:
;LIST - список имен профилей;
(define (lse-get-presets)
  (define presets-source lse-presets)
  (define presets-dlist '())
  (while (not (null? presets-source))
    (define preset (car presets-source))
    (set! presets-dlist (append presets-dlist (list (car preset))))
    (set! presets-source (cdr presets-source))
  )
  presets-dlist
)

;script-fu-lse
;Главная процедура
;СПИСОК АРГУМЕНТОВ:
;IMAGE - обрабатываемое изображение;
;LAYER - обрабатываемый слой;
;BOOLEAN - флаг активации нового ядра;
;INTEGER - номер цветового профиля;
;REAL - величина свечения ядра меча;
;REAL - величина основного свечения;
;COLOR - цвет свечения ядра;
;COLOR - цвет основного свечения;
;BOOLEAN - переключатель режима "белого" наложения;
(define (script-fu-lse image layer core_new color_preset coreSize sabSize coreColor sabColor sky_switch)

  (define used-preset)

  (if (= (car (gimp-drawable-has-alpha layer)) FALSE)
    (begin
      (gimp-message "Выбранный слой не имеет альфа-канала\nВыберите другой.")
      (quit)
    )
  )

  (let* (
	(imh (car (gimp-image-height image)))
	(imw (car (gimp-image-width image)))
	(b_side (if (> imh imw) imh imw))
	(sep-image (car (gimp-image-new imw imh 0)))
	(r_blade)
	(r_over)
	(rel_c_size (* (/ b_side 100) coreSize))
	(rel_b_size (* (/ b_side 100) sabSize))
	(current_preset)
	)

	(gimp-image-undo-group-start image)
	(gimp-image-undo-disable sep-image)

	;Обработка профилей
	(set! current_preset (list-ref lse-presets color_preset))
	(eval (cadr current_preset))
	(set! used-preset (car current_preset))

	(if (= core_new FALSE)
	  (set! r_blade (lse-oldcore sep-image layer rel_c_size rel_b_size coreColor sabColor TRUE))
	  (set! r_blade (lse-newcore sep-image layer rel_c_size rel_b_size coreColor sabColor TRUE))
	)

	(set! r_blade (car (gimp-layer-new-from-drawable r_blade image)))
	(gimp-image-insert-layer image r_blade -1 -1)
	(gimp-item-set-name r_blade "LSE Готовый меч")
	(gimp-layer-set-mode r_blade 4)
	(gimp-brightness-contrast r_blade 0 25)
	(set! r_over 
	  (car
	    (gimp-layer-copy r_blade TRUE)
	  )
	)
	(gimp-image-insert-layer image r_over -1 -1)
	(gimp-item-set-name r_over "LSE Оверлей")

	(if (= sky_switch TRUE)
	  (begin 
	    (gimp-layer-set-mode r_over 0)
	    (gimp-layer-set-opacity r_over 23)
	  )
	  (gimp-layer-set-mode r_over 5)
	)
	(gimp-image-undo-group-end image)
	(gimp-image-delete sep-image)

	;Обновление изображения
	(gimp-displays-flush)
  )
)

(script-fu-register
"script-fu-lse"
"<Image>/Filters/RSS/L_SE"
"Создание светового меча из слоя с силуэтом лезвия"
"Непочатов Станислав"
"Свободная лицензия"
"1 августа 2012"
"RGBA"
SF-IMAGE		"Изображение"				0
SF-DRAWABLE	"Слой"					0
SF-TOGGLE	"Использовать новое ядро"		TRUE
SF-OPTION	"Установить цветовой профиль"		(lse-get-presets)
SF-ADJUSTMENT	"Размер ядра свечения (%)"		'(4 0.2 25 1 3 1 0)
SF-ADJUSTMENT	"Размер внешнего свечения (%)"		'(15 0.8 50 5 10 1 0)
SF-COLOR		"Цвет ядра"				'(80 150 255)
SF-COLOR		"Цвет свечения"				'(30 78 255)
SF-TOGGLE	"Режим \"белого\" наложения"		FALSE
)

;lse-oldcore
;Процедура старого ядра
;СПИСОК АРГУМЕНТОВ:
;IMAGE - обрабатываемое изображение;
;LAYER - обрабатываемый слой;
;INTEGER - величина ядра свечения;
;INTEGER - величина основного свечения;
;COLOR - цвет ядра свечения;
;COLOR - цвет основного свечения;
;BOOLEAN - переключатель дополнительного размытия;
(define (lse-oldcore input_image layer coreSize sabSize coreColor sabColor pass)

  ;Объявление переменных
  (let* (
	(off_count 4)
	(blade)
	(big_glow)
	(big_glow_2)
	(big_glow_3)
	(soft_blade)
	(blade_glow)
	(blade_glow_2)
	(big_size)
	)

	;Начало процесса

	(set! blade (car (gimp-layer-new-from-drawable layer input_image)))
	(gimp-item-set-visible layer FALSE)
	(gimp-image-insert-layer input_image blade -1 -1)
	(gimp-item-set-name blade "Blade_FX")
	(set! soft_blade 
	  (car 
	    (gimp-layer-copy blade TRUE)
	  )
	)
	(gimp-image-insert-layer input_image soft_blade -1 -1)
	(gimp-item-set-name soft_blade "Soft_Blade")
	(plug-in-gauss-rle2 1 input_image soft_blade sabSize sabSize)
	(set! big_glow 
	  (car 
	    (gimp-layer-copy blade TRUE)
	  )
	)
	(gimp-image-insert-layer input_image big_glow -1 -1)
	(gimp-item-set-name big_glow "Big_Glow")
	(plug-in-colorify 1 input_image big_glow sabColor)
	(set! blade_glow 
	  (car 
	    (gimp-layer-copy soft_blade TRUE)
	  )
	)
	(set! big_size (* 3 sabSize))
	(plug-in-gauss-rle2 1 input_image big_glow big_size big_size)
	(set! big_glow_2 
	  (car 
	    (gimp-layer-copy big_glow TRUE)
	  )
	)
	(set! big_glow_3 
	  (car 
	    (gimp-layer-copy big_glow TRUE)
	  )
	)
	(gimp-image-insert-layer input_image big_glow_2 -1 -1)
	(gimp-image-insert-layer input_image big_glow_3 -1 -1)
	(gimp-item-set-name big_glow_2 "Big_Glow_2")
	(gimp-item-set-name big_glow_3 "Big_Glow_3")
	(gimp-image-insert-layer input_image blade_glow -1 -1)
	(gimp-item-set-name blade_glow "Blade_Glow")
	(plug-in-colorify 1 input_image blade_glow coreColor)
	(set! blade_glow_2 
	  (car 
	    (gimp-layer-copy blade_glow TRUE)
	  )
	)
	(gimp-image-insert-layer input_image blade_glow_2 -1 -1)
	(gimp-item-set-name blade_glow_2 "Blade_Glow_2")
	(while (>= off_count 0)
	  (gimp-image-raise-item input_image soft_blade)
	  (gimp-image-raise-item input_image blade)
	  (set! off_count (- off_count 1))
	)
	(plug-in-gauss-rle2 1 input_image blade coreSize coreSize)

	;Финальное совмещение слоев
	(set! blade 
	  (car 
	    (gimp-image-merge-down input_image soft_blade 0)
	  )
	)
	(set! blade_glow 
	  (car 
	    (gimp-image-merge-down input_image blade_glow_2 0)
	  )
	)
	(set! big_glow_2 
	  (car 
	    (gimp-image-merge-down input_image big_glow_3 0)
	  )
	)
	(set! big_glow 
	  (car 
	    (gimp-image-merge-down input_image big_glow_2 0)
	  )
	)
	(set! big_glow 
	  (car 
	    (gimp-image-merge-down input_image blade_glow 0)
	  )
	)
	(if (= pass TRUE)
	  (plug-in-gauss-rle2 1 input_image big_glow (* coreSize 2) (* coreSize 2))
	)
	(set! blade (car (gimp-image-merge-visible-layers input_image 0)))
	
	;Возвращение слоя
	blade
  )
)

;lse-newcore
;Процедура нового ядра
;СПИСОК АРГУМЕНТОВ:
;IMAGE - обрабатываемое изображение;
;LAYER - обрабатываемый слой;
;INTEGER - величина ядра свечения;
;INTEGER - величина основного свечения;
;COLOR - цвет ядра свечения;
;COLOR - цвет основного свечения;
;BOOLEAN - переключатель дополнительного размытия;
(define (lse-newcore input_image layer coreSize sabSize coreColor sabColor pass)
  (let* (
	(input_fore (car (gimp-context-get-foreground)))
	(input_back (car (gimp-context-get-background)))
	(imh (car (gimp-image-height input_image)))
	(imw (car (gimp-image-width input_image)))
	(orig_layer (car (gimp-layer-new-from-drawable layer input_image)))
	(blade)
	(big_glow)
	(soft_blade)
	(blade_glow)
	(blade_buffer)
	)

	(gimp-item-set-visible layer FALSE)

	(gimp-image-insert-layer input_image orig_layer -1 -1)
	(gimp-image-select-item input_image 0 orig_layer)
	(set! blade_buffer (car (gimp-selection-save input_image)))
	(gimp-image-remove-layer input_image orig_layer)
	(set! blade (car (gimp-layer-new input_image imw imh 1 "Blade_FX" 100 0)))
	(gimp-image-insert-layer input_image blade -1 -1)
	(gimp-context-set-foreground '(255 255 255))
	(gimp-selection-feather input_image coreSize)
	(gimp-edit-fill blade 3)
	(gimp-edit-fill blade 0)
	(gimp-selection-none input_image)
	(gimp-image-select-item input_image 0 blade_buffer)
	(set! soft_blade (car (gimp-layer-new input_image imw imh 1 "Soft_Blade" 100 0)))
	(gimp-image-insert-layer input_image soft_blade -1 -1)
	(gimp-selection-feather input_image sabSize)
	(gimp-edit-fill soft_blade 3)
	(gimp-edit-fill soft_blade 0)
	(gimp-selection-none input_image)
	(set! big_glow (car (gimp-layer-new input_image imw imh 1 "Big_Glow" 100 0)))
	(gimp-image-insert-layer input_image big_glow -1 -1)
	(gimp-image-select-item input_image 0 blade_buffer)
	(gimp-selection-feather input_image (* sabSize 3))
	(gimp-context-set-foreground sabColor)
	(gimp-context-set-background sabColor)
	(gimp-edit-fill big_glow 3)
	(gimp-edit-fill big_glow 0)
	(set! blade_glow (car (gimp-layer-new input_image imw imh 1 "Blade_Glow" 100 0)))
	(gimp-image-insert-layer input_image blade_glow -1 -1)
	(gimp-selection-none input_image)
	(gimp-image-select-item input_image 0 blade_buffer)
	(gimp-selection-feather input_image sabSize)
	(gimp-context-set-foreground coreColor)
	(gimp-context-set-background coreColor)
	(gimp-edit-fill blade_glow 3)
	(gimp-edit-fill blade_glow 0)
	(gimp-selection-none input_image)
	(gimp-item-set-visible blade FALSE)
	(gimp-item-set-visible soft_blade FALSE)

	(set! big_glow (car (gimp-image-merge-visible-layers input_image 0)))
	(if (= pass TRUE)
	  (plug-in-gauss-rle2 1 input_image big_glow (* coreSize 2) (* coreSize 2))
	)

	(gimp-item-set-visible blade TRUE)
	(gimp-item-set-visible soft_blade TRUE)
	(gimp-layer-set-opacity soft_blade 65)
	(gimp-image-raise-item input_image soft_blade)
	(gimp-image-raise-item input_image blade)
	(set! blade (car (gimp-image-merge-down input_image blade 0)))
	(set! blade (car (gimp-image-merge-down input_image soft_blade 0)))
	(gimp-context-set-background input_back)
	(gimp-context-set-foreground input_fore)

	;Возвращение слоя
	blade
  )
)
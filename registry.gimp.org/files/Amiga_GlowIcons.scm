;          __      __         __
;       __/ //_ __/ //_______/ //_
;       \_   _|_\_        \__    |
;   ______|  \    /  \___/   \   |______
;   |_ 01 |   |  ||    | |   |_  | 08  _|
;     \___|______||____| |_______|____/asd
;      _/                               \_
;     ||  t e a m   p o w e r a m i g a  ||
;    _\/__   ___                ___   __ \/_
;   //-/_/--/__/                \__\--\_\--\\
;     /\                                 /\
;     ||    Amiga Glow Icon Generator    ||
;     ||     special for GiMP 2.4.3      ||
;     ||     (Script-Fu Image ver.)      ||
;     ||                                 ||
;     ||   Author: Eugene Sobolev aka    ||
;     ||           aGGreSSor^tPA         ||
;     ||                                 ||
;     ||       amitrans(at)narod.ru      ||
;     ||                                 ||
;     ||       2:5030/675.48#fidonet     ||
;     ||       39:240/14.49#amiganet     ||
;     ||        500:812/1.49#zxnet       ||
;     ||                                 ||
;     || Saint-Petersburg, RUSSIA, 2008! ||
;     ||                                 ||
;     `------------------------(21/01/08)-'

(define (script-fu-glow-icons image 
			      drawable
			      glow-intensity
			      blur-intensity
			      icon-bright
			      icon-dec
			      glow-color
			      icon-scale
			      glow-pixelize
			      glow-noisify)

    (let*
        (
        ; запоминаем текущий цвет фона и размеры картинки
        (oldbg (car (gimp-palette-get-background)))
	(xsize (car (gimp-image-width image)))
	(ysize (car (gimp-image-height image)))
	(bglayer (car (gimp-layer-new image xsize ysize
				      RGBA-IMAGE "Glow" 100 NORMAL-MODE)))
        )
    
        ; начало буфера undo
	(gimp-undo-push-group-start image)

	; алгоритм
	
	(gimp-selection-none image)
	(if (= icon-scale FALSE)
	(gimp-image-scale image (- xsize icon-dec) (- ysize icon-dec))())
	(if (= icon-scale FALSE)
	(gimp-image-resize image xsize ysize (/ icon-dec 2) (/ icon-dec 2))())
	
	(gimp-brightness-contrast drawable icon-bright 0)
	
	(gimp-image-add-layer image bglayer -1)
	(gimp-edit-clear bglayer)
	(gimp-selection-layer-alpha drawable)
	
	(gimp-selection-grow image glow-intensity)
	(gimp-palette-set-background glow-color)
	(gimp-edit-fill bglayer 1)
	(plug-in-gauss-iir TRUE image bglayer blur-intensity TRUE TRUE)
	(if (= glow-noisify TRUE)
	(plug-in-noisify 1 image bglayer FALSE 1.0 1.0 1.0 0)())
	(if (= glow-pixelize TRUE)
	(plug-in-pixelize 1 image bglayer 4)())
	(gimp-image-raise-layer-to-top image drawable)
	(gimp-palette-set-background oldbg)
	(gimp-selection-none image)
	
	; конец буфера undo
	(gimp-undo-push-group-end image)
	
	; обновление картинки
	(gimp-displays-flush)
	
    )
)

; регистрация в PDB
(script-fu-register "script-fu-glow-icons"		; имя функции
_"Amiga Glow Icons"
_"Create an icon with glow effect (selected) like AMiGA desktop"	; описание
"Eugene Sobolev aka aGGreSSor (amitrans@narod.ru)"	; автор
"Public Domain"						; информация о копирайте
"20.01.2008"						; дата создания
"RGBA"							; тип изображения

; вид объекта интерфейса  Название		Значение по-умолчанию
SF-IMAGE		  ""			0
SF-DRAWABLE		  ""			0
SF-ADJUSTMENT		  _"Glow Intensity"	'(3 1 10 1 1 0 0)
SF-ADJUSTMENT		  _"Blur Intensity"	'(20 1 100 1 1 0 0)
SF-ADJUSTMENT		  _"Brightness"		'(30 0 120 10 1 0 0)
SF-ADJUSTMENT		  _"Decrease"		'(2 2 24 2 1 0 0)
SF-COLOR		  _"Glow Color"		'(255 230 40)
SF-TOGGLE		  _"Don't Scale"	TRUE
SF-TOGGLE		  _"Pixelize Glow"	TRUE
SF-TOGGLE		  _"Noisify Glow"	FALSE

)

(script-fu-menu-register "script-fu-glow-icons"
"<Image>/Script-Fu/Utils"			; место в меню
)

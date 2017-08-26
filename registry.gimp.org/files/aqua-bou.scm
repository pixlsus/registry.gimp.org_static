;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
;
; aqua-bou.scm
; Version 07.06.23 (For The Gimp 2.2) 23.06.2007
; A Script-Fu that create an 'Aqua Bou' Style Web Buttons
;
; Copyright (C) 2007 Marcos Pereira (majpereira) <majpereira@hotmail.com>
; ((C) 2005 Denis Bodor <lefinnois@lefinnois.net>)
; ((C) 2001 Iccii <iccii@hotmail.com>)
;----------------------------------------------------------------------------------
; Baseado nos scripts:
; => 'aqua pill' versão de 06/2001 para o Gimp 1.2, de Iccii.
; => 'aqua bou' versão 0.4.1 de 05/08/2005 para o Gimp 2.0 e 2.2, de Denis Bodor.
;----------------------------------------------------------------------------------
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;==================================================================================
;seleção na forma "pill" (pílula)
;selection in pill shape
(define (round-select-aquabou img xpadding ypadding width height ratio width-all
	 diameter quartheig)
    (let* ((x-ini (+ xpadding quartheig))
	(x-ini-b (- width-all diameter xpadding quartheig))
	(y-ini (+ ypadding quartheig))
	(xinret (+ xpadding (/ diameter 2) quartheig))
	(widret (- width-all (* xinret 2))))
	(gimp-ellipse-select img x-ini y-ini diameter height 2 TRUE 0 0)
	(gimp-ellipse-select img x-ini-b y-ini diameter height 0 TRUE 0 0)
	(gimp-rect-select img xinret y-ini widret height 0 0 0)))
;==================================================================================
;seleção na forma retangular
; selection in rectangular shape
(define (rect-select-aquabou img xpadding ypadding width height ratio width-all
	 diameter quartheig)
    (let* ((xineli (+ xpadding quartheig))
	(y-ini (+ ypadding quartheig))
	(xinret (+ xpadding (/ diameter 2) quartheig))
	(widret (- width-all (* xpadding 2) (/ height 2))))
	(gimp-rect-select img xineli y-ini widret height 0 0 0)))
;==================================================================================
; seleção na forma de placa arredondada
; selection in rounded board shape
(define (select-board-rounded img width height xpadding ypadding radius-b ratio width-all
	 quartheig topround baseround)
    (let* ((height-a (* radius-b 2))
	(diameter (* height-a ratio))
	(x-ini (+ xpadding quartheig))
	(x-ini-b (- width-all diameter xpadding quartheig))
	(y-ini (+ ypadding quartheig))
	(y-ini-b (+ y-ini radius-b))
	(heigret (- height height-a))
	(y-ini-c (+ heigret y-ini))
	(xinret (+ xpadding (/ diameter 2) quartheig))
	(widret (- width-all (* xinret 2)))
	(widret-b (+ widret diameter)))

	; topo arredondado
	; top rounded
	(if (eqv? topround TRUE)
	(begin
	(gimp-ellipse-select img x-ini y-ini diameter height-a 0 TRUE 0 0)
	(gimp-ellipse-select img x-ini-b y-ini diameter height-a 0 TRUE 0 0)
	(gimp-rect-select img xinret y-ini widret height-a 0 0 0)))
	; o meio que é reto
	; the midle what's rect
	(gimp-rect-select img x-ini y-ini-b widret-b heigret 0 0 0)
	; base arredondada
	; base rounded
	(if (eqv? baseround TRUE)
	(begin
	(gimp-ellipse-select img x-ini y-ini-c diameter height-a 0 TRUE 0 0)
	(gimp-ellipse-select img x-ini-b y-ini-c diameter height-a 0 TRUE 0 0)
	(gimp-rect-select img xinret y-ini-c widret height-a 0 0 0)))))
;==================================================================================
; efeito de refração para botão arredondado
; refractin effect to rounded button
(define (aquabou-refracting-round img cote width-all height xpadding ypadding ratio var54)
; para o define: img cote width-all height xpadding ypadding ratio
; to the define:...
    (let* ((quartheig (/ height 4))
	(x-ini (+ xpadding quartheig)) ; =40
	(y-ini (+ ypadding quartheig))
	(diameter (* height ratio))
	(xpad-xa (+ x-ini (* diameter (/ 5 88)))) ; =55 se x-ini=40 diameter=176 (5/88=var10)
	(ypad-ya (- y-ini (* height (/ 5 88)))) ; =20
	(ellidiam-a (+ diameter (* diameter (/ 5 44)))) ; =216
	(elliheig-a (+ height (* height (/ 5 44)))) ; =216
	(xpad-xb (- width-all x-ini diameter)) ; =296
	(xpad-xc (- width-all ellidiam-a xpad-xa)) ; =241
	(radiu (* height (/ 2 11))) ; =32 se height=176 (2/11=var32)
)
	(gimp-ellipse-select img x-ini y-ini diameter height 2 TRUE 0 0)
	(gimp-ellipse-select img xpad-xa ypad-ya ellidiam-a elliheig-a 1 TRUE 1 radiu)
	(gimp-ellipse-select img xpad-xb y-ini diameter height 0 TRUE 0 0)
	(gimp-ellipse-select img xpad-xc ypad-ya ellidiam-a elliheig-a 1 TRUE 1 radiu)
	(gimp-edit-fill cote 0)
	(gimp-selection-none img)))
;==================================================================================
; efeito de refração das laterais retas para botão plano
; refracting effect of rect sides to plain button
(define (aquabou-refracting-rect img cote width-all height xpadding ypadding ratio)
    (let* ((quartheig (/ height 4))
	(heig-b (/ height 8))
	(x-ini (+ xpadding quartheig)) ; =40
	(y-ini (+ ypadding quartheig))
	(diameter (* height ratio))
	(xpad-xa (+ x-ini (* diameter (/ 5 88)))) ; =55 se x-ini=40 diameter=176 (5/88=var10)
	(ypad-ya (+ y-ini (* height 0.02)))
	(widret (- width-all (* xpadding 2) (/ height 2)))
	(widret-b (- widret (* (- xpad-xa x-ini) 2)))
	(elliheig-a (- height (* height 0.02))) ; =216
	(radiu (* height (/ 2 11))) ; =32 se height=176 (2/11=var32)
)
	(gimp-rect-select img x-ini y-ini widret height 2 TRUE 0 0)
	(gimp-rect-select img xpad-xa ypad-ya widret-b elliheig-a 1 TRUE 1 radiu)
	(gimp-edit-fill cote 0)
	(gimp-selection-none img)
	(plug-in-gauss-rle2 1 img cote heig-b heig-b)
	(gimp-selection-all img)
	(gimp-rect-select img x-ini y-ini widret height 1 TRUE 0 0)
	(gimp-edit-clear cote)
	(gimp-selection-none img)))
;==================================================================================
; efeito de refração para placa arredondada
; refracting effect to rounded board
(define (aquabou-refracting-robrd img cote width height xpadding ypadding 
	 ratio width-all quartheig topround baseround radius-b)
(let* ((height-a (* radius-b 2))
	(diameter (* height-a ratio))
	(x-ini (+ xpadding quartheig))
	(x-ini-b (- width-all diameter xpadding quartheig))
	(y-ini (+ ypadding quartheig))
	(y-ini-b (+ y-ini radius-b))

	(heigpad (+ height-a (* height-a (/ 5 44))))
	(diampad (+ diameter (* diameter (/ 5 44))))

	(xpad-xa (+ x-ini (* diameter (/ 5 88))))
	(ypad-ya (- y-ini (* height-a (/ 5 88))))
	(xpad-xb (- width-all x-ini diampad (* diameter (/ 5 88))))
	(ypad-yb (- (+ y-ini height (* height-a (/ 5 88))) heigpad))
	(xpad-xc (+ xpad-xa (/ diampad 2)))
	(ypad-yc (+ ypad-ya (/ heigpad 2)))

	(widret-a (- width-all (* x-ini 2)))
	(widret-b (- width-all (* xpad-xa 2) diampad))
	(heighret-a (- ypad-yb ypad-ya))

	(xt-ab (+ widret-a x-ini))
	(yt-aa (- ypad-yc 1))
	(yt-ab (- (+ ypad-yc heighret-a) 1))

	(xt-ba (- xpad-xc 1))
	(xt-bb (- (+ xpad-xc widret-b) 1))
	(yt-bb (+ height y-ini))
	(radiu (* height-a (/ 2 11))))

	(select-board-rounded img width height xpadding ypadding radius-b ratio width-all
	 quartheig topround baseround)

	(gimp-ellipse-select img xpad-xa ypad-ya diampad heigpad 1 TRUE 1 radiu)
	(gimp-ellipse-select img xpad-xb ypad-ya diampad heigpad 1 TRUE 1 radiu)
	(gimp-ellipse-select img xpad-xa ypad-yb diampad heigpad 1 TRUE 1 radiu)
	(gimp-ellipse-select img xpad-xb ypad-yb diampad heigpad 1 TRUE 1 radiu)
	(gimp-edit-fill cote 0)

	(gimp-rect-select img x-ini (+ ypad-yc 1) widret-a (- heighret-a 2) 2 0 0)
	(gimp-rect-select img (+ xpad-xc 1) y-ini (- widret-b 2) height 0 0 0)
	(gimp-edit-clear cote)

	(if (eqv? topround TRUE)
	(begin
	(gimp-rect-select img x-ini (- ypad-yc 1) widret-a 2 2 0 0)
	(gimp-drawable-transform-scale-default cote x-ini yt-aa xt-ab yt-ab 0 0))
	(begin
	(gimp-rect-select img x-ini yt-ab widret-a 2 2 0 0)
	(gimp-drawable-transform-scale-default cote x-ini yt-aa xt-ab (+ yt-ab 2) 0 0)))
	(set! sides (car (gimp-image-get-floating-sel img))) ; give a name ("sides") to the floating selection
	(gimp-floating-sel-anchor sides) ; floating selection is anchored, called for your new name: "sides".

	(gimp-rect-select img (- xpad-xc 1) y-ini 2 height 2 0 0)
	(gimp-drawable-transform-scale-default cote xt-ba y-ini xt-bb yt-bb 0 0)
	(set! topo (car (gimp-image-get-floating-sel img))) ; give a name ("sides") to the floating selection
	(gimp-floating-sel-anchor topo)))
;==================================================================================
; efeito de reflexo de luz de topo para placa arredondada
; top light reflect effect to rounded board
(define (aquabou-reflh-robrd img reflh height radius-b ratio xpadding ypadding
	 width-all var10 var36 var40 var54 var82 var87)
	(let* (
; para o aquabou-reflh-round e aquabou-reflh-plain
; produzir efeito reflexo de luz do botão, arredondado ou plano
; precisa de: height ratio xpadding ypadding width-all var10 var36 var40 var82 var87
; need:...
	(halfwidall (/ width-all 2))
	(quartheig (/ height 4))
	(height-b (* radius-b 2))
	(diameter (* height-b ratio))
	(x-ini (+ xpadding quartheig)) ; =40
	(y-ini (+ ypadding quartheig))
	(xrect-a (+ x-ini (* diameter var82)))
	(yrect-a (+ y-ini (* height-b var10)))
	(xrect-b (- width-all xrect-a))
	(yrect-b (* height-b (+ var54 var10)))
	(xrect-c (+ x-ini (* diameter var40)))
	(yrect-c (+ y-ini (/ height-b 3.52)))
	(xrect-d (- width-all xrect-c))
	(widrect (- width-all (* xrect-a 2)))
	(gaus-c (* height-b var36))
	(refl-a (+ y-ini (* height-b var87)))) ; =87 (o original era 97, mas 87 tem efeito melhor)
	; efeito reflexo de luz
	(gimp-rect-select img xrect-a yrect-a widrect yrect-b 2 0 0)
	(set! chacha (car (gimp-selection-save img)))
	(gimp-selection-none img)
	(plug-in-gauss-iir 1 img chacha gaus-c 1 1)
	(gimp-levels chacha 0 123 133 1.0 0 255)
	(gimp-selection-load chacha)
	(gimp-image-remove-channel img chacha)
	(gimp-context-set-foreground '(0 0 0))
	(gimp-edit-blend reflh 1 0 0 100 0 0 FALSE FALSE 0 0 FALSE halfwidall refl-a halfwidall y-ini)
	(gimp-drawable-transform-perspective-default reflh
				xrect-a	; x0 122
				yrect-a	; y0 50
				xrect-b	; x1 122+268 (122+comprimento da pílula)
				yrect-a	; y1 50
				xrect-c	; x2 80 (borda+40)
				yrect-c	; y2 90
				xrect-d	; x3 122+268+42 (122+comprimento da pílula+40)
				yrect-c	; y3 90
                                0	; interpolation
                                0)	; cliping
	(gimp-edit-clear reflh)
	(set! floflo (car (gimp-image-get-floating-sel img)))
	(gimp-floating-sel-anchor floflo)))
;==================================================================================
; efeito de reflexo de luz de topo para botão arredondado
(define (aquabou-reflh-round img reflh height ratio xpadding ypadding width-all var10 var36 var40 var82 var54 var87)
	(let* (
; para o aquabou-reflh-round e aquabou-reflh-plain
; produzir efeito reflexo de luz do botão, arredondado ou plano
; precisa de: height ratio xpadding ypadding width-all var10 var36 var40 var82 var87
	(halfwidall (/ width-all 2))
	(quartheig (/ height 4))
	(diameter (* height ratio))
	(x-ini (+ xpadding quartheig)) ; =40
	(y-ini (+ ypadding quartheig))
	(xrect-a (+ x-ini (* diameter var82))) ; x0 (borda+82)=122
	(yrect-a (+ y-ini (* height var10))) ; y0=y1 50
	(xrect-b (- width-all xrect-a)); x1 122+268 (122+comprimento da pílula)
	(yrect-b (* height (+ var54 var10))) ; x2 80 (borda+40)=80 
	(xrect-c (+ x-ini (* diameter var40))) ; x2 80 (borda+40)=80
	(yrect-c (+ y-ini (/ height 3.52))) ; =90 40+50
	(xrect-d (- width-all xrect-c)); = x1 122+268+80 (122+comprimento da pílula+80)
	(widrect (- width-all (* xrect-a 2))) ; =268 (width-all-244)
	(gaus-c (* height var36)) ; =36
	(refl-a (+ y-ini (* height var87)))) ; =87 (o original era 97, mas 87 tem efeito melhor)
	; efeito reflexo de luz de  topo para formato arredondado
	(gimp-rect-select img xrect-a yrect-a widrect yrect-b 2 0 0)
	(set! chacha (car (gimp-selection-save img)))
	(gimp-selection-none img)
	(plug-in-gauss-iir 1 img chacha gaus-c 1 1)
	(gimp-levels chacha 0 123 133 1.0 0 255)
	(gimp-selection-load chacha)
	(gimp-image-remove-channel img chacha)
	(gimp-context-set-foreground '(0 0 0))
	(gimp-edit-blend reflh 1 0 0 100 0 0 FALSE FALSE 0 0 FALSE halfwidall refl-a halfwidall yrect-a)
	(gimp-drawable-transform-perspective-default reflh
				xrect-a	; x0 122
				yrect-a	; y0 50
				xrect-b	; x1 122+268 (122+comprimento da pílula)
				yrect-a	; y1 50
				xrect-c	; x2 80 (borda+40)
				yrect-c	; y2 90
				xrect-d	; x3 122+268+42 (122+comprimento da pílula+40)
				yrect-c	; y3 90
                                0	; interpolation
                                0)	; cliping
	(gimp-edit-clear reflh)
	(set! floflo (car (gimp-image-get-floating-sel img)))
	(gimp-floating-sel-anchor floflo)))
; efeito de reflexo de luz de topo para placa arredondada
;==================================================================================
; efeito de reflexo de luz de topo para formato plano
(define (aquabou-reflh-plain img reflh height ratio xpadding ypadding width-all var10 var36 var40 var82 var87)
    (let* ((var5 (* height (/ 5 176)))
; para o aquabou-reflh-round e aquabou-reflh-plain
; produzir efeito reflexo de luz do botão, arredondado ou plano
; precisa de: height ratio xpadding ypadding width-all var10 var36 var40 var82 var87
	(halfwidall (/ width-all 2))
	(quartheig (/ height 4))
	(diameter (* height ratio))
	(x-ini (+ xpadding quartheig)) ; =40
	(y-ini (+ ypadding quartheig))
	(xrect-a (+ x-ini (* diameter var82))) ; x0 (borda+82)=122
	(yrect-a (+ y-ini (* height var10))) ; y0=y1 50
	(xrect-b (- width-all xrect-a)); x1 122+268 (122+comprimento da pílula)
	(xrect-c (+ x-ini (* diameter var40))) ; x2 80 (borda+40)=80
	(yrect-c (+ y-ini (/ height 3.52))) ; =90 40+50
	(xrect-d (- width-all xrect-c)); = x1 122+268+80 (122+comprimento da pílula+80)
	(widrect (- width-all (* xrect-a 2))) ; =268 (width-all-244)
	(gaus-c (* height var36)) ; =36
	(refl-a (+ y-ini (* height var87))) ; =87 (o original era 97, mas 87 tem efeito melhor)
)
	; efeito reflexo de luz de  topo para formato plano
	(gimp-rect-select img  x-ini y-ini widrect y-ini 2 0 0)
	(set! chacha (car (gimp-selection-save img)))
	(gimp-selection-none img)
	; (plug-in-gauss-iir 1 img chacha (/ gaus-c 10) 1 1)
	(gimp-levels chacha 0 123 133 1.0 0 255)
	(gimp-selection-load chacha)
	(gimp-image-remove-channel img chacha)
	(gimp-context-set-foreground '(0 0 0))
	(gimp-edit-blend reflh 1 0 0 100 0 0 FALSE FALSE 0 0 FALSE halfwidall yrect-c halfwidall (/ y-ini 2))
	(gimp-drawable-transform-perspective-default reflh
				(+ x-ini var5) ; xrect-c	; x0 topo inicial
				(+ y-ini var5)	; y0 "
				(- width-all (+ x-ini var5))	; x1 topo final
				(+ y-ini var5)	; y1 "
				(+ x-ini var5) ; xrect-c	; x2 base inicial
				yrect-c	; y2 "
				(- width-all (+ x-ini var5))	; x3 base final
				yrect-c	; y3 "
                                0	; interpolation
                                0)	; cliping
	(gimp-edit-clear reflh)
	(set! floflo (car (gimp-image-get-floating-sel img)))
	(gimp-floating-sel-anchor floflo)))
;==================================================================================
;==================================================================================
;início de "script-fu-aqua-bou"
(define (script-fu-aqua-bou img fg-color bg-color width height xpadding ypadding
	 ratio shadow flatten layout-bou radius-b topround baseround)
    (let* ((width-all (car (gimp-image-width img)))
	(height-all (car (gimp-image-height img)))
	(fond (car (gimp-layer-new img width-all height-all 0 "Background" 100 0)))
	(base (car (gimp-layer-new img width-all height-all 1 "Base" 90 0)))
	(ombre (car (gimp-layer-new img width-all height-all 1 "Shadow" 70 0)))
	(reflh (car (gimp-layer-new img width-all height-all 1 "Top Reflect" 80 4)))
	(lum (car (gimp-layer-new img width-all height-all 1 "light" 100 5)))
	(cote (car (gimp-layer-new img width-all height-all 1 "Refracting" 70 5)))
;----------------------------------------------------------------------------------
; variáveis fixas de proporcionalidade
	(var10 (/ 5 88)) ; =10, se height=176 e height*var(n)
	(var30 (/ 15 88)) ; =30, se height=176 e height*var(n)
	(var36 (/ 9 44)) ; =36, se height=176 e height*var(n)
	(var40 (/ 5 22)) ; =40, se height=176 e height*var(n)
	(var45 (/ 45 176)) ; =45, se height=176 e height*var(n)
	(var58 (/ 29 88)) ; =58, se height=176 e height*var(n)
	(var74 (/ 37 88)) ; =74, se height=176 e height*var(n)
	(var82 (/ 41 88)) ; =82, se height=176 e height*var(n)
	(var54 (/ 27 88)) ; =84, se height=176 e height*var(n)
	(var87 (/ 87 176)) ; =87, se height=176 e height*var(n)

; variáveis de entrada
	(quartheig (/ height 4))
	(halfwidall (/ width-all 2)) ; =256
	(shri-a (* height var58))
	(gaus-a (* height var74))
	(trsl-a (/ height 3.52)) ; =50 se height=176 (25/88)=3.52=var50
	(gaus-b quartheig) ; =44
	(shri-b (* height var30)) ; =30
	(trsl-b (* height var45)) ; =45
	(diameter (* height ratio)))
;----------------------------------------------------------------------------------
	(gimp-context-push)
	; camadas para produzirem o efeito 'aqua bou'
	(gimp-image-add-layer img reflh 0)
	(gimp-image-add-layer img cote 1)
	(gimp-image-add-layer img lum 2)
	(gimp-image-add-layer img base 3)
	(gimp-image-add-layer img ombre 4)
	(gimp-image-add-layer img fond 5)

	; limpeza das camadas, pois geralmente elas vêm sujas
	(gimp-edit-clear reflh)
	(gimp-edit-clear cote)
	(gimp-edit-clear lum)
	(gimp-edit-clear base)
	(gimp-edit-clear ombre)
	(gimp-edit-clear fond)

	; ajusta cor de fundo conforme janela de entrada
	(gimp-context-set-foreground bg-color)
	(gimp-edit-fill fond 0)
	(set! maskbase (car (gimp-layer-create-mask base 0)))

	(gimp-layer-add-mask base maskbase)
	; limpeza da máscara, pois geralmente  vem suja
	(gimp-edit-clear maskbase)
;----------------------------------------------------------------------------------
	; ajusta cor de fundo para branco e desenho do formato aqua
	(gimp-context-set-foreground '(255 255 255))
	(gimp-edit-fill maskbase 0)
	(if (= layout-bou 0)
	(round-select-aquabou img xpadding ypadding width height ratio width-all
	 diameter quartheig)
	(if (= layout-bou 1)
	(rect-select-aquabou img xpadding ypadding width height ratio width-all
	 diameter quartheig)
	(select-board-rounded img width height xpadding ypadding radius-b ratio width-all
	 quartheig topround baseround)))
	(gimp-context-set-foreground fg-color)
	(gimp-edit-fill base 0)

	(gimp-selection-shrink img shri-a)
	(gimp-context-set-foreground '(160 160 160))
	(gimp-edit-fill maskbase 0)
	(gimp-selection-none img)
	(plug-in-gauss-rle2 1 img maskbase gaus-a gaus-a)
;----------------------------------------------------------------------------------
	; seleção da forma do botão ou placa, conforme formato solicitado
	(if (= layout-bou 0)
	(round-select-aquabou img xpadding ypadding width height ratio width-all
	 diameter quartheig)
	(if (= layout-bou 1)
	(rect-select-aquabou img xpadding ypadding width height ratio width-all
	 diameter quartheig)
	(select-board-rounded img width height xpadding ypadding radius-b ratio width-all
	 quartheig topround baseround)))

	; desloca a máscara selecionada
	(gimp-selection-translate img 0 trsl-a)
	(gimp-context-set-foreground fg-color)
;----------------------------------------------------------------------------------
; início do efeito de gota de sombra - "drop shadow"
	; 2 camadas transparentes para produzir o efeito
	(if (eqv? shadow TRUE)
	(begin
	(gimp-edit-fill ombre 0)
	(gimp-selection-none img)
	(plug-in-gauss-rle2 1 img ombre gaus-b gaus-b)
	(gimp-context-set-foreground fg-color)
	(set! maskombre (car (gimp-layer-create-mask ombre 2)))
	(gimp-layer-add-mask ombre maskombre)
	(gimp-edit-clear maskombre)
	(gimp-context-set-foreground '(0 0 0))
	(gimp-edit-fill maskombre 0)
	(if (= layout-bou 0)
	(round-select-aquabou img xpadding ypadding width height ratio width-all
	 diameter quartheig)
	(if (= layout-bou 1)
	(rect-select-aquabou img xpadding ypadding width height ratio width-all
	 diameter quartheig)
	(select-board-rounded img width height xpadding ypadding radius-b ratio width-all
	 quartheig topround baseround)))
	(gimp-selection-invert img)
	(gimp-edit-clear maskombre))
	(gimp-image-remove-layer img ombre))
;----------------------------------------------------------------------------------
	; efeito luminosidade de fundo
	(gimp-selection-none img)
	(gimp-context-set-foreground '(0 0 0))
	(gimp-edit-fill lum 0)
	(if (= layout-bou 0)
	(round-select-aquabou img xpadding ypadding width height ratio width-all
	 diameter quartheig)
	(if (= layout-bou 1)
	(rect-select-aquabou img xpadding ypadding width height ratio width-all
	 diameter quartheig)
	(select-board-rounded img width height xpadding ypadding radius-b ratio width-all
	 quartheig topround baseround)))
	(gimp-selection-shrink img shri-b)
	(gimp-selection-translate img 0 trsl-b)
	(gimp-context-set-foreground '(255 255 255))
	(gimp-edit-fill lum 0)
	(gimp-selection-none img)
	(plug-in-gauss-rle2 1 img lum trsl-a trsl-a)
;----------------------------------------------------------------------------------
	; efeito reflexo de luz de topo
	(if (= layout-bou 0) ; 0=rounded, 1=plain board and 2=rounded board
	; para botão arredondado
	(aquabou-reflh-round img reflh height ratio xpadding ypadding width-all var10 var36 var40 var82 var54 var87)
	(if (= layout-bou 1)
	; para placa plana
	(aquabou-reflh-plain img reflh height ratio xpadding ypadding width-all var10 var36 var40 var82 var87)
	; para placa arredondada
	(aquabou-reflh-robrd img reflh height radius-b ratio xpadding ypadding width-all var10 var36 var40 var54 var82 var87)
	))
;----------------------------------------------------------------------------------
	; efeito de refração conforme tipo de formato lateral: arredondado (0 ou 2) ou plano (1)
	(if (= layout-bou 0) ; 0=rounded, 1=plain and 2=rounded board
	(aquabou-refracting-round img cote width-all height xpadding ypadding ratio var54)
	(if (= layout-bou 1)
	(aquabou-refracting-rect img cote width-all height xpadding ypadding ratio)
	(aquabou-refracting-robrd img cote width height xpadding ypadding 
	 ratio width-all quartheig topround baseround radius-b)
	))

	(if (eqv? flatten TRUE) (gimp-image-flatten img))

	(gimp-context-pop)))
;==================================================================================
;==================================================================================
; produz régua horizontal
(define (script-fu-aqua-bou-hrule fg-color bg-color width height xpadding ypadding ratio shadow flatten)
    (let* ((diameter (* height ratio))
	(shadow-height (if (eqv? shadow TRUE) 1 0))
	(height-all (+ height (/ height 2) (* height shadow-height 0.5) (* ypadding 2)))
	(width-all  (+ width (/ diameter 2) (* xpadding 2)))
	(img (car (gimp-image-new width-all height-all 0))))
	(gimp-message-set-handler 0)
	(if (< (* height ratio) width)
	(begin
	(gimp-image-undo-disable img)
	(script-fu-aqua-bou img fg-color bg-color width height xpadding ypadding
	 ratio shadow flatten 0 0 0 0)
	(gimp-image-undo-enable img)
	(gimp-display-new img))
	(gimp-message "(en) Warning: Bar Length is too short to create your image! \
\
(pt-BR) Alerta: O comprimento da barra é muito curto \
para criar sua imagem!"))))
;----------------------------------------------------------------------------------
(script-fu-register "script-fu-aqua-bou-hrule"
		"Hrule..."
		"(en) Create an 'horizontal rule' image \
style 'aqua bou'. \
\
(pt-BR) Cria uma imagem de 'régua horizontal' \
estilo 'aqua bou'."
		"Marcos Pereira <majpereira@hotmail.com>"
		"Marcos Pereira (majpereira)"
		"10.06.2007"
		""
		SF-COLOR "Button color"		'(71 124 183)
		SF-COLOR "Background color"	'(255 255 255)
		SF-ADJUSTMENT "Width"		'(220 5 1500 1 1 0 1)
		SF-ADJUSTMENT "Height"		'(88 4 500 1 1 0 1)
		SF-ADJUSTMENT "Padding X"	'(10 0 100 1 10 0 1)
		SF-ADJUSTMENT "Padding Y"	'(10 0 100 1 10 0 1)
		SF-ADJUSTMENT "Round Ratio"	'(1 0.05 5 0.05 0.5 2 0)
		SF-TOGGLE     "Drop Shadow"	TRUE
		SF-TOGGLE     "Flatten"		FALSE)
(script-fu-menu-register "script-fu-aqua-bou-hrule"  "<Toolbox>/Xtns/Script-Fu/Web Page Themes/Aqua Bou")
;==================================================================================
; produz rodela aqua pill
(define (script-fu-aqua-bou-round fg-color bg-color baseradius xpadding ypadding ratio shadow flatten)
    (let* ((shadow-height (if (eqv? shadow TRUE) 1 0))
	(height baseradius)
	(diameter (* height ratio))
	(width (* diameter 1.05))
	(height-all (+ height (/ height 2) (* height shadow-height 0.5) (* ypadding 2)))
	(width-all  (+ width (/ diameter 2) (* xpadding 2)))
	(img (car (gimp-image-new width-all height-all 0))))

	(gimp-image-undo-disable img)
	(script-fu-aqua-bou img fg-color bg-color width height xpadding ypadding
	 ratio shadow flatten 0 0 0 0)
	(gimp-image-undo-enable img)
	(gimp-display-new img)))
;----------------------------------------------------------------------------------
(script-fu-register "script-fu-aqua-bou-round"
		    "Round..."
		    "(en) Create a round 'aqua bou' image. \
(pt-BR) Cria uma imagem de rodela 'aqua bou'."
		"Marcos Pereira <majpereira@hotmail.com>"
		"Marcos Pereira (majpereira)"
		"10.06.2007"
		    ""
		SF-COLOR "Button color"		'(71 124 183)
		SF-COLOR "Background color"	'(255 255 255)
		SF-ADJUSTMENT "Height"		'(88 4 400 1 1 0 1)
		SF-ADJUSTMENT "Padding X"	'(10 0 100 1 10 0 1)
		SF-ADJUSTMENT "Padding Y"	'(10 0 100 1 10 0 1)
		SF-ADJUSTMENT "Round Ratio"	'(1 1 5 0.05 0.5 2 0)
		SF-TOGGLE     "Drop Shadow"	TRUE
		SF-TOGGLE     "Flatten"		FALSE)
(script-fu-menu-register "script-fu-aqua-bou-round"  "<Toolbox>/Xtns/Script-Fu/Web Page Themes/Aqua Bou")
;==================================================================================
; produz botão aqua com texto
(define (script-fu-aqua-bou-button-text text
				size
				font
				text-color
				fg-color
				bg-color
				xpadding
				ypadding
				ratio
				blur
				shadow
				flatten
				antialias)
    (let* ((shadow-height (if (eqv? shadow TRUE) 1 0))
	(old-fg-color (car (gimp-context-get-foreground)))
	(old-bg-color (car (gimp-context-get-background)))
	(img (car (gimp-image-new 256 256 0)))
        (tmp (gimp-context-set-foreground text-color)) ; only change fg-color for text color
	(text-layer (car (gimp-text-fontname img -1 0 0 text 0 TRUE size 0 font)))
	(text-width  (car (gimp-drawable-width  text-layer)))
	(text-height (car (gimp-drawable-height text-layer)))

	(radius (/ (* ratio text-height) 4))
	(height (+ (* 2 ypadding) text-height))
	(width  (+ (* 2 (+ radius xpadding)) text-width))
	(shiftxy     (/ height 4))
	(height-all (+ height (* 2 shiftxy) (* height shadow-height 0.5)))
	(width-all  (+ width  (* 2 shiftxy))))

	; constrói a imagem do botão 'aqua pill'
	(gimp-context-set-foreground text-color)
	(gimp-context-set-background bg-color)
	(gimp-image-resize img width-all height-all
			        (+ shiftxy xpadding radius)
			        (+ shiftxy ypadding))
	(gimp-layer-set-offsets text-layer
			        (+ shiftxy xpadding radius)
			        (+ shiftxy ypadding))
	(gimp-layer-resize text-layer width-all height-all
			        (+ shiftxy xpadding radius)
			        (+ shiftxy ypadding))

	(script-fu-aqua-bou img fg-color bg-color width height 0 0
	 ratio shadow FALSE 0 0 0 0)

	; desenha o texto e seus efeitos
	(gimp-image-undo-group-start img)
	(gimp-image-raise-layer-to-top img text-layer)
	(gimp-image-lower-layer img text-layer)
	(gimp-image-lower-layer img text-layer)
	(set! text-layer-copy   (car (gimp-layer-copy text-layer FALSE)))
	(set! text-layer-shadow (car (gimp-layer-copy text-layer FALSE)))
	(gimp-image-add-layer img text-layer-copy 3)
	(gimp-image-add-layer img text-layer-shadow 4)
	(gimp-layer-set-mode text-layer-shadow 20)
	(gimp-drawable-offset text-layer-copy   0 1 (- (* text-height 0.025)) (- (* text-height 0.025)))
	;(gimp-drawable-offset text-layer-copy   0 1 (- (* xpadding 0.1)) (- (* ypadding 0.1)))
	(gimp-drawable-offset text-layer-shadow 0 1 (- (* xpadding 0.1)) (* height 0.1))
	(gimp-invert text-layer-copy)
	(gimp-selection-layer-alpha text-layer-shadow)
	(gimp-context-set-foreground bg-color)
	(gimp-edit-fill text-layer-shadow 0)
	(gimp-selection-grow img (+ 1 (* height blur 0.1)))	; expande a imagem em todas as direções
	(plug-in-gauss-iir2 1 img text-layer-shadow (+ 1 (* height blur 0.1)) (+ 1 (* height blur 0.1)))
	(gimp-selection-none img)
	(gimp-image-undo-group-end img)
	(gimp-image-undo-group-start img)
	(if (eqv? flatten TRUE)
		(gimp-drawable-set-name (car (gimp-image-flatten img)) text))
	(gimp-context-set-foreground old-fg-color)
	(gimp-context-set-background old-bg-color)
	(gimp-image-undo-group-end img)
	(gimp-displays-flush)
	(gimp-display-new img)
	))
;----------------------------------------------------------------------------------
(script-fu-register "script-fu-aqua-bou-button-text"
		"Button Text..."
		"(en) Create an 'aqua bou' button with text \
(pt-BR) Cria um botão 'aqua bou' com texto"
		"Marcos Pereira <majpereira@hotmail.com>"
		"Marcos Pereira (majpereira)"
		"11.06.2007"
		""
		SF-STRING	"Text"			"Click Me!"
		SF-ADJUSTMENT	"Font Size (pixels)"	'(50 2 500 1 1 0 1)
		SF-FONT		"Font" 			"Serif"	; Default setting
		SF-COLOR	"Text Color"		'(0 0 0)
		SF-COLOR	"Button color"		'(71 124 183)
		SF-COLOR	"Background Color"	'(255 255 255)
		SF-ADJUSTMENT	"Padding X"		'(10 0 100 1 10 0 1)
		SF-ADJUSTMENT	"Padding Y"		'(10 0 100 1 10 0 1)
		SF-ADJUSTMENT	"Round Ratio"		'(1 0.05 5 0.05 0.5 2 0)
		SF-ADJUSTMENT	"Text Blur Amount"	'(1 0.05 5 0.05 0.5 2 0)
		SF-TOGGLE	"Drop Shadow"		TRUE
		SF-TOGGLE	"Flatten"		TRUE
		SF-TOGGLE	"Antialias"		TRUE)
(script-fu-menu-register "script-fu-aqua-bou-button-text"  "<Toolbox>/Xtns/Script-Fu/Web Page Themes/Aqua Bou")
;==================================================================================
; produz uma placa plana
(define (script-fu-aqua-bou-plain-board fg-color bg-color width height xpadding ypadding shadow flatten)
    (let* ((shadow-height (if (eqv? shadow TRUE) 1 0))
	(height-all (+ height (/ height 2) (* height shadow-height 0.5) (* ypadding 2)))
	(width-all  (+ width (/ height 2) (* xpadding 2)))
	(img (car (gimp-image-new width-all height-all 0))))
	(gimp-message-set-handler 0)
	(gimp-image-undo-disable img)
	(script-fu-aqua-bou img fg-color bg-color width height xpadding ypadding
	 1 shadow flatten 1 0 0 0)
	(gimp-image-undo-enable img)
	(gimp-display-new img)))
;----------------------------------------------------------------------------------
(script-fu-register "script-fu-aqua-bou-plain-board"
		"Plain Board..."
		"(en) Create an plain board image \
'aqua bou' style. \
\
(pt-BR) Cria uma imagem de placa plana \
estilo 'aqua bou'."
		"Marcos Pereira <majpereira@hotmail.com>"
		"Marcos Pereira (majpereira)"
		"13.06.2007"
		""
		SF-COLOR "Board color"		'(71 124 183)
		SF-COLOR "Background color"	'(255 255 255)
		SF-ADJUSTMENT "Width"		'(220 5 2000 1 1 0 1)
		SF-ADJUSTMENT "Height"		'(88 4 1500 1 1 0 1)
		SF-ADJUSTMENT "Padding X"	'(10 0 100 1 10 0 1)
		SF-ADJUSTMENT "Padding Y"	'(10 0 100 1 10 0 1)
		SF-TOGGLE     "Drop Shadow"	TRUE
		SF-TOGGLE     "Flatten"		FALSE)
(script-fu-menu-register "script-fu-aqua-bou-plain-board"  "<Toolbox>/Xtns/Script-Fu/Web Page Themes/Aqua Bou")
;==================================================================================
; produz placa arredondada
(define (script-fu-aqua-bou-rounded-board fg-color bg-color width height xpadding
	 ypadding topround baseround radius-b ratio shadow flatten)
    (let* ((diameter (* radius-b ratio 2))
	(shadow-height (if (eqv? shadow TRUE) 1 0))
	(height-all (+ height (* radius-b 2) (* height shadow-height 0.5) (* ypadding 2)))
	(width-all  (+ width diameter (* xpadding 2)))
	(img (car (gimp-image-new width-all height-all 0))))
	(gimp-message-set-handler 0)
	(gimp-image-undo-disable img)
	(script-fu-aqua-bou img fg-color bg-color width height xpadding ypadding
	 ratio shadow flatten 2 radius-b topround baseround)
	(gimp-image-undo-enable img)
	(gimp-display-new img)))
;----------------------------------------------------------------------------------
(script-fu-register "script-fu-aqua-bou-rounded-board"
		"Rounded Board..."
		"(en) Create an rounded board image \
'aqua bou' style. \
\
(pt-BR) Cria uma imagem de placa arredondada \
estilo 'aqua bou'."
		"Marcos Pereira <majpereira@hotmail.com>"
		"Marcos Pereira (majpereira)"
		"16.06.2007"
		""
		SF-COLOR "Board color"		'(71 124 183)
		SF-COLOR "Background color"	'(255 255 255)
		SF-ADJUSTMENT "Width"		'(200 10 2000 1 1 0 1)
		SF-ADJUSTMENT "Height"		'(160 8 1500 1 1 0 1)
		SF-ADJUSTMENT "Padding X"	'(10 0 100 1 10 0 1)
		SF-ADJUSTMENT "Padding Y"	'(10 0 100 1 10 0 1)
		SF-TOGGLE     "Top Rounded"	TRUE
		SF-TOGGLE     "Base Rounded"	TRUE
		SF-ADJUSTMENT "Round Radius"	'(44 4 375 1 1 0 1)
		SF-ADJUSTMENT	"Round Ratio"	'(1 0.05 5 0.05 0.5 2 0)
		SF-TOGGLE     "Drop Shadow"	TRUE
		SF-TOGGLE     "Flatten"		FALSE)
(script-fu-menu-register "script-fu-aqua-bou-rounded-board"  "<Toolbox>/Xtns/Script-Fu/Web Page Themes/Aqua Bou")
;==================================================================================

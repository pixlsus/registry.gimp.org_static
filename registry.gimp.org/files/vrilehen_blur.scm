; Vrilehen Blur
; 
; Blur details - especially characters/words - while retaining the overall
; image "feeling".
;
;
; Copyright (c) 2008, Liam Vrilehen <vrilehen@gmail.com>
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;
; 1. Redistributions of source code must retain the above copyright notice,
;    this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.

(define (script-fu-vrilehen-blur image drawable displacement pixel repetitions colorDelta creatTiles)
	(let*
		(
			(dis displacement)
			(pix pixel)
		)

		(gimp-image-undo-group-start image)

		(while (>= repetitions 1)
			; add a bit of spatial noise 
			(plug-in-randomize-pick RUN-NONINTERACTIVE image drawable displacement 1 0 0)
			(plug-in-pixelize RUN-NONINTERACTIVE image drawable pix)
			(plug-in-antialias RUN-NONINTERACTIVE image drawable)

			; conter font -> letter -> word attack
			(plug-in-shift RUN-NONINTERACTIVE image drawable dis 0)
			(plug-in-pixelize RUN-NONINTERACTIVE image drawable pix)
			(plug-in-shift RUN-NONINTERACTIVE image drawable dis 1)
			(plug-in-pixelize RUN-NONINTERACTIVE image drawable pix)

			; stablization
			(plug-in-sel-gauss RUN-NONINTERACTIVE image drawable (+ (* 2 pix) 1) colorDelta)

			(set! dis (- dis 1))
			(if (< 1 dis)
				(set! dis displacement))
			(set! pix (- pix 1))
			(if (< 1 pix)
				(set! pix pixel))
			(set! repetitions (- repetitions 1))
		)

		; final tiles
		(if (= creatTiles TRUE)
			(plug-in-pixelize RUN-NONINTERACTIVE image drawable (+ (/ pixel 2) 1))
			(plug-in-sel-gauss RUN-NONINTERACTIVE image drawable pixel colorDelta)
			(plug-in-pixelize RUN-NONINTERACTIVE image drawable pixel)
		)

		(gimp-image-undo-group-end image)
		(gimp-displays-flush)
	)
)

(script-fu-register
	"script-fu-vrilehen-blur"
	_"_Vrilehen Blur"
	"Blur details while retaining the overall image layout."
	"Liam Vrilehen <vrilehen@gmail.com>"
	"copyright 2008, Liam Vrilehen"
	"2008-10-23"
	"RGB* GRAY*"
	SF-IMAGE	"The Image"	0
	SF-DRAWABLE	"The Drawable"	0
	SF-ADJUSTMENT	_"_Displacement (%)"	'(5 3 100 1 10 2 0)
	SF-ADJUSTMENT	_"Tile _Size"	'(5 3 200 1 20 2 0)
	SF-ADJUSTMENT	_"_Repetitons"	'(3 2 100 1 10 0 0)
	SF-ADJUSTMENT	_"_Color Stablization"	'(50 1 255 1 10 0 0)
	SF-TOGGLE	_"Create _Tiles"	FALSE
)

(script-fu-menu-register "script-fu-vrilehen-blur" "<Image>/Filters/Blur")

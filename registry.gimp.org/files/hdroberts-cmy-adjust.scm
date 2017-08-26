;
; cmy-adjust
;
; Copyright (C) 2010 Howard Roberts(howardroberts@comcast.net)
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; 2011/11/28 F. Collard: 2.7.4 compatibility

(define (cmy-adjust img drawable c_mode c_op m_mode m_op y_mode y_op)

(gimp-context-push)
(gimp-image-undo-group-start img)
(let* ( 
(width (car (gimp-image-width img)))
(height (car (gimp-image-height img)))
(old-color (car (gimp-context-get-foreground)))
(cyan-layer (car (gimp-layer-new img width height RGB-IMAGE "Cyan" 100 ADDITION-MODE)))
(magenta-layer (car (gimp-layer-new img width height RGB-IMAGE "Magenta" 100 ADDITION-MODE)))
(yellow-layer (car (gimp-layer-new img width height RGB-IMAGE "Yellow" 100 ADDITION-MODE)))
(myChannel (car (gimp-channel-new-from-component img RED-CHANNEL "Value")))
(cyan-color '(0 255 255))
(magenta-color '(255 0 255))
(yellow-color '(255 255 0))
(cmask 0)
(mmask 0)
(ymask 0)
)
(cond ((= c_mode 1)
(gimp-layer-set-mode cyan-layer SUBTRACT-MODE)
)
((= m_mode 1)
(gimp-layer-set-mode magenta-layer SUBTRACT-MODE)
)
((= y_mode 1)
(gimp-layer-set-mode yellow-layer SUBTRACT-MODE)
)
)
(gimp-image-insert-layer img cyan-layer 0 0)
(gimp-layer-copy cyan-layer FALSE) 
(gimp-image-insert-layer img yellow-layer 0 0)
(gimp-image-insert-layer img magenta-layer 0 0)

(gimp-image-raise-item-to-top img cyan-layer)
(set! cmask (car (gimp-layer-create-mask cyan-layer ADD-WHITE-MASK)))
(gimp-layer-add-mask cyan-layer cmask)
(set! mmask (car (gimp-layer-create-mask magenta-layer ADD-WHITE-MASK)))
(gimp-layer-add-mask magenta-layer mmask)
(set! ymask (car (gimp-layer-create-mask yellow-layer ADD-WHITE-MASK)))
(gimp-layer-add-mask yellow-layer ymask)

(gimp-layer-set-opacity cyan-layer c_op)
(gimp-layer-set-opacity magenta-layer m_op)
(gimp-layer-set-opacity yellow-layer y_op)
(set! cmask (car (gimp-layer-get-mask cyan-layer)))
(set! mmask (car (gimp-layer-get-mask magenta-layer)))
(set! ymask (car (gimp-layer-get-mask yellow-layer)))
(gimp-context-set-background '(0 0 0))
(gimp-image-insert-channel img myChannel 0 0)
(gimp-image-select-item img CHANNEL-OP-REPLACE myChannel)
(gimp-selection-invert img)
(gimp-edit-fill cmask 1)
(gimp-edit-fill mmask 1)
(gimp-edit-fill ymask 1)
(gimp-selection-none img)

(gimp-context-set-background cyan-color)
(gimp-edit-bucket-fill-full cyan-layer BG-BUCKET-FILL NORMAL-MODE 100 255 FALSE FALSE SELECT-CRITERION-COMPOSITE 0 0)
(gimp-context-set-background magenta-color)
(gimp-edit-bucket-fill-full magenta-layer BG-BUCKET-FILL NORMAL-MODE 100 255 FALSE FALSE SELECT-CRITERION-COMPOSITE 0 0)
(gimp-context-set-background yellow-color)
(gimp-edit-bucket-fill-full yellow-layer BG-BUCKET-FILL NORMAL-MODE 100 255 FALSE FALSE SELECT-CRITERION-COMPOSITE 0 0)
(gimp-context-set-background old-color)
)
(gimp-displays-flush)
(gimp-image-undo-group-end img)
(gimp-context-pop)
)

(script-fu-register "cmy-adjust"
"CMY Tone Adjustment"
"Adjust cyan, magenta, and yellow values in the red channel"
"Howard Roberts <howardroberts@comcast.net>"
"(c) 2010 Howard D. Roberts"
"Nov 10,2010"
"RGB*"
SF-IMAGE "Image" 0
SF-DRAWABLE "Layer"	 0
SF-OPTION	 _"Cyan Mode" '("Addition"
"Subtraction")
SF-ADJUSTMENT _"Cyan Opacity" '(7 0 20 0 1 0 0 0)	

SF-OPTION	 _"Magenta Mode" '("Addition"
"Subtraction")

SF-ADJUSTMENT _"Magenta Opacity"	'(7 0 20 0 1 0 0 0)
SF-OPTION	 _"Yellow Mode" '("Addition"
"Subtraction")
SF-ADJUSTMENT _"Yellow Opacity"	'(7 0 20 0 1 0 0 0)
)
(script-fu-menu-register "cmy-adjust"
"<Image>/Colors")
; Scan To Lineart
;
; by David Gowers <00ai99 AT gmail DOT com>

; Version 1 (20100220)

; Description: Converts a black and white 'scan' -- or other linework
; with no transparency, to a layer with transparency corresponding to white
; in original image and black to solidity, with intermediate brightnesses
; translating to intermediate levels of solidity of black.
; It also locks the alpha of the lineart layer so that
; it's easy to color the lines.
;
; In short -- converts white -> transparency, black -> solid, with full antialiasing.
; This is a one-step process, no choosing of parameters is needed.
;
; You are then expected to do your coloring on layers beneath.
;
; This is a more flexible lineart coloring system, as opposed to
; setting the lineart layer to multiply mode and coloring on the
; layers beneath -- that method prevents you from adding color to your lineart.
; This method makes it easy to do so.
;
; Written on behalf of everyone who has ever done a crappy coloring
; by painting on top of the original lineart layer and leaving smudges
; of uncolored gray and white, and even those who are doing so presently.
; This script should make coloring a lot easier for you!

; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html


; we need to know:
;   color-mode : convert to RGB if currently INDEXED or GRAY
;   layer-has-mask : store+restore channel <-> layer mask if there is already a layer mask.
;
; we may need to store:
;   saved-mask
;
; (if (eqv? (eqv? (car (gimp-image-base-type 1)) RGB) #f) (gimp-image-convert-rgb image)


(define (script_fu_scan_to_lineart image inLayer)

(let*
	(
	(mask 0)
    (tempmask 0)
    (newlayer 0)
)
	;  it begins here
    (set! mask (car (gimp-layer-get-mask (car (gimp-image-get-active-layer image)))))
	(gimp-context-push)
	(gimp-image-undo-group-start image)

    (if (eqv? (eqv? (car (gimp-image-base-type image)) RGB) #f) (gimp-image-convert-rgb image))


	;logging
	; (gimp-message-set-handler ERROR-CONSOLE)
	;(gimp-message-set-handler CONSOLE)
	(gimp-message-set-handler MESSAGE-BOX)
	;or start GIMP wwith "gimp --console-messages" to spawn a console box
	;then use this:
	;(gimp-message "foobar")

	;testing for functions defined
	;(if (defined? 'plug-in-shift) (gimp-message "It Exists") (gimp-message "Doesnt Exist"))

    (if (eqv? (eqv? mask -1) #f)
      (
       (set! mask (car (gimp-channel-copy mask)))
       (gimp-layer-remove-mask inLayer MASK-DISCARD)
      )
    )
    ; (gimp-message "one")

    (set! tempmask (car (gimp-layer-create-mask inLayer ADD-COPY-MASK)))
    (gimp-layer-add-mask inLayer tempmask)
    (gimp-invert tempmask) ; white is solid in the mask

    ; (gimp-message "two")

    (gimp-context-set-foreground (list 0 0 0))
    (gimp-drawable-fill inLayer FOREGROUND-FILL)

    (gimp-layer-remove-mask inLayer MASK-APPLY)

    ; (gimp-message "three")

    ; apply the stored mask
    (if (eqv? (eqv? mask -1) #f)
       (
        (gimp-layer-add-mask
          inLayer
          (car (gimp-layer-create-mask inLayer ADD-WHITE-MASK))
          )
        (gimp-channel-combine-masks (car (gimp-layer-get-mask inLayer) )
          mask CHANNEL-OP-INTERSECT 0 0)
        (gimp-channel-delete mask)
       )
    )

    ; (gimp-message "vo")

    (gimp-layer-set-lock-alpha inLayer TRUE)

	;done
	(gimp-image-undo-group-end image)
	(gimp-progress-end)
	(gimp-displays-flush)
	(gimp-context-pop)
)
)

(script-fu-register "script_fu_scan_to_lineart"
					"<Image>/Layer/Transparency/Convert Scan to Lineart"
					"Prepares a lineart for coloring."
					"David Gowers"
					"David Gowers"
					"Feb 2010"
					"RGB* GRAY* INDEXED*"
					SF-IMAGE      "image"      0
					SF-DRAWABLE   "drawable"   0

)

; also -- 'transfer color' : given a lineart layer and a color layer,
; sets LA[x,y].color = CO[x,y].color where LA[x,y].alpha > 0
; effectively uses LA as a layer mask for the color of CO.
;
; In fact, that is what we do:
;
; copy LA pixels to LA mask
; copy CO pixels to LA pixels
; apply layer mask.
;
; What about when there is already a layer mask on LA?
; we store it in a channel and restore it later.
;
; transfer color is useful when you are working with a software that lets you
; do awesome painting, but doesn't support layer masks.
; you do a messy painting covering the lines on another layer, and apply the colors of
; that layer to the lineart. voila, neat colored lines.
;
; TBC -- haven't written 'transfer color' yet.

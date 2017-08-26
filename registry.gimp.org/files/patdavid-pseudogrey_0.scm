; Pseudogrey v1
; Created by Patrick David <patdavid@gmail.com>
; http://blog.patdavid.net
;
; Script to convert a color image to a larger number of gray values than 256
; based on a tutorial originally posted to
; http://gimpchat.com/viewtopic.php?f=10&t=4245
; by gimpchat member lylejk
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

(define (script-fu-patdavid-pseudogrey Image Drawable)

	(let*
	  	(
			(PG (car (gimp-layer-new Image 2048 40 RGB "PG" 100 NORMAL)))
	  	)
		(gimp-image-undo-group-start Image)
		  	;set up gradient definition
			(gimp-gradient-new "pg-patdavid-tmp")
			(gimp-gradient-segment-set-left-color "pg-patdavid-tmp" 0 '(0 0 0) 100)
			(gimp-gradient-segment-set-right-color "pg-patdavid-tmp" 0 '(255 255 255) 100)

			;set active gradient
			(gimp-context-set-gradient "pg-patdavid-tmp")

			(gimp-image-add-layer Image PG -1)

			;create gradient filled layer
			(gimp-edit-blend PG CUSTOM NORMAL LINEAR 100 0 REPEAT-NONE FALSE TRUE 1 0 TRUE 0 0 2048 40)

			; sample colorize
			(plug-in-sample-colorize RUN-NONINTERACTIVE Image Drawable PG FALSE FALSE TRUE TRUE 0 255 1.0 0 255)

			; clean up
			(gimp-gradient-delete "pg-patdavid-tmp")
			;(gimp-image-remove-layer Image PG)

			(gimp-displays-flush)

		(gimp-image-undo-group-end Image)
	 )

)

; Finally register our script with script-fu
(script-fu-register "script-fu-patdavid-pseudogrey"
                    "Pseudogrey..."
                    "Pseudogrey mapping using Sample Colorize & gradient"
                    "Patrick David <patdavid@patdavid.net>"
                    "Patrick David"
                    "2012-06-19"
                    "RGB*"
			SF-IMAGE	"Image"		0
			SF-DRAWABLE	"Drawable"	0
)

(script-fu-menu-register "script-fu-patdavid-pseudogrey" "<Image>/Colors")

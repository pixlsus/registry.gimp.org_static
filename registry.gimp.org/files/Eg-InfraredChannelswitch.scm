;
; Infrared Channelswitch, V2.8
;
; Autolevel colors (aka trivial Auto Whitebalance) and switch Red and Blue channel in a color IR image
;
; Martin Egger (martin.egger@gmx.net)
; (C) 2012, Bern, Switzerland
;
; This script was tested with Gimp 2.8
;
; New versions will be distributed from http://registry.gimp.org/ only
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
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses>.
;
; Define the function
;
(define (script-fu-Eg-InfraredChannelswitch InImage InLayer InAuto InFlatten)
;
; Save history			
;
	(gimp-image-undo-group-start InImage)
;
	(let*	(
			(SwitchLayer (car (gimp-layer-copy InLayer TRUE)))
		)
		(gimp-image-insert-layer InImage SwitchLayer 0 -1)
;
; Autolevel the colors, if we need to
;
		(cond
			((= InAuto TRUE) (gimp-levels-stretch SwitchLayer))
		)
;
		(let*	(
				(RGBImage (car (plug-in-decompose TRUE InImage SwitchLayer "RGB" TRUE)))
				(RGBLayer (cadr (gimp-image-get-layers RGBImage)))
				(CompImage (car (plug-in-drawable-compose TRUE RGBImage (aref RGBLayer 2) (aref RGBLayer 1) (aref RGBLayer 0) -1 "RGB")))
				(CompLayer (cadr (gimp-image-get-layers CompImage)))
			)
			(gimp-selection-all CompImage)
			(gimp-edit-copy (aref CompLayer 0))
			(gimp-floating-sel-anchor (car (gimp-edit-paste SwitchLayer FALSE)))
			(gimp-image-delete CompImage)
			(gimp-image-delete RGBImage)
;
; Flatten the image, if we need to
;
			(cond
				((= InFlatten TRUE) (gimp-image-merge-down InImage SwitchLayer CLIP-TO-IMAGE))
				((= InFlatten FALSE) 
					(begin
						(gimp-item-set-name SwitchLayer "SwitchedChannels")
						(gimp-image-set-active-layer InImage InLayer)
					)
				)
			)
		)
	)
;
; Finish work
;
	(gimp-image-undo-group-end InImage)
	(gimp-displays-flush)
;
)
;
(script-fu-register 
	"script-fu-Eg-InfraredChannelswitch"
	_"Infrared _Channelswitch"
	"Switch Red and Blue channel in color IR images, Autolevel colors"
	"Martin Egger (martin.egger@gmx.net)"
	"Martin Egger, Bern, Switzerland"
	"28.02.2012"
	"RGB*"
	SF-IMAGE	"The Image"		0
	SF-DRAWABLE	"The Layer"		0
	SF-TOGGLE	"Autolevel Image first"	TRUE
	SF-TOGGLE	"Flatten Image"		FALSE
)
;
(script-fu-menu-register "script-fu-Eg-InfraredChannelswitch"
			 "<Image>/Filters/Eg")
;

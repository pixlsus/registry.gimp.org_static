; The GIMP -- an image manipulation program
; Copyright (C) 1995 Spencer Kimball and Peter Mattis
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
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(script-fu-register
  "script-fu-global-apply-color"
  "Global Apply Color"
  "Applies the selected color to all opened images in GIMP and saves those images."
  "Tag Costa"
  "2009, Tag Costa"
  "November 23, 2009"
  ""
  SF-COLOR "Choose a color to apply it to all open images" '(0 0 0)
)
(script-fu-menu-register "script-fu-global-apply-color" "<Toolbox>/Filters/Custom Scripts")

; Manual Steps:
; 1 - Colors > Desaturate (Lightness)
; 2 - Layer > Transparency > Alpha To Selection
; 3 - Create a new layer with the same size as the image with transparent background and mode set as color and add that layer to the image
; 4 - Fill the selection in the new layer with the color
; 5 - Merge all visible layers
; 6 - Save the image

(define (script-fu-global-apply-color user_selected_color)
	(gimp-context-set-foreground user_selected_color)
	(map
		(lambda (image_id)
			(gimp-image-undo-group-start image_id)
			(gimp-desaturate-full (car (gimp-image-get-active-drawable image_id)) DESATURATE-LIGHTNESS)
			(gimp-selection-layer-alpha (car (gimp-image-get-active-layer image_id)))
			(gimp-image-add-layer image_id (car (gimp-layer-new image_id (car (gimp-image-width image_id)) (car (gimp-image-height image_id)) RGBA-IMAGE "Color" 100 COLOR-MODE))  -1)
			(gimp-edit-fill (car (gimp-image-get-active-drawable image_id)) FOREGROUND-FILL)
			(gimp-image-merge-visible-layers image_id EXPAND-AS-NECESSARY)
			(gimp-image-undo-group-end image_id)
			(gimp-file-save RUN-NONINTERACTIVE image_id (car (gimp-image-get-active-drawable image_id)) (car (gimp-image-get-filename image_id)) (car (gimp-image-get-filename image_id)))
			(gimp-image-clean-all image_id)
			(gimp-displays-flush)
		)
		(vector->list (nth 1 (gimp-image-list)))
	)
)
;;;
;;; Criar uma marca d'agua para ser usada em uma imagem
;;; http://www.gimpdome.com/index.php?topic=5971.0
;;;
;
; This script was tested with Gimp 2.6
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
;;;

(define (sf-wm-create text font size show?)
  (let* ((img (car (gimp-image-new 256 256 RGB)))
         (old-fg (car (gimp-context-get-foreground)))
         (text-layer 0)
         (path (string-append gimp-directory "/patterns/photo-watermak.pat")))
    (gimp-context-set-foreground '(255 255 255))
    (set! text-layer (car (gimp-text-fontname img -1 0 0 text 5 TRUE size PIXELS font)))
    (script-fu-util-image-resize-from-layer img text-layer)
    (file-pat-save RUN-NONINTERACTIVE
		   img 
		   text-layer
		   path 
		   path
		   "Photo Watermark")
    (gimp-patterns-refresh)
    (gimp-context-set-foreground old-fg)
    (if (= show? FALSE)
	(gimp-image-delete img)
	(gimp-display-new img))))

;;;
;;; Aplicar marca d'agua em uma imagem
;;;
(define (sf-wm-apply img drawable)
  (let* ((old-pat (car (gimp-context-get-pattern)))
	 (layer 0)
	 (width (car (gimp-image-width img)))
	 (height (car (gimp-image-height img))))
    (gimp-image-undo-group-start img)
    (if (= 1 (car (gimp-patterns-get-list "Photo Watermark")))
	(begin
	  (set! layer (car (gimp-layer-new img width height RGBA-IMAGE "Watermark" 3 NORMAL-MODE)))
	  (gimp-image-add-layer img layer -1)
	  (gimp-context-set-pattern "Photo Watermark")
	  (gimp-displays-flush img)
	  (gimp-selection-all img)
	  (gimp-edit-fill layer PATTERN-FILL)
	  (gimp-selection-clear img)
	  (gimp-context-set-pattern old-pat))
	(gimp-message "Invalid pattern. Please, create a new one:\n [File->Create->Photo Watermark]\n"))
    (gimp-image-undo-group-end img)))

;;;
;;; Registro das funções
;;;
(script-fu-register "sf-wm-create"
                    "Photo Watermark"
                    "Create Watermark"
                    "Guaracy Monteiro <guaracy.bm@gmail.com>"
                    "Guaracy Monteiro"
                    "2009-11-23"
                    ""
		    SF-STRING 	  _"Watermark Text"   "My Watermark"
		    SF-FONT	  _"Font"             "Arial Bold"
		    SF-ADJUSTMENT _"Size (pixels)"    '(18 2 200 1 10 0 1)
		    SF-TOGGLE     _"Display image"    FALSE)

(script-fu-register "sf-wm-apply"
                    "Photo Watermark"
                    "Apply Watermark"
                    "Guaracy Monteiro <guaracy.bm@gmail.com>"
                    "Guaracy Monteiro"
                    "2009-11-23"
                    "RGB*"
		    SF-IMAGE 	  _"Image"     0
		    SF-DRAWABLE   _"Drawable"  0)

(script-fu-menu-register "sf-wm-create"
                          "<Image>/File/Create")

(script-fu-menu-register "sf-wm-apply"
                          "<Image>/Filters/Render")


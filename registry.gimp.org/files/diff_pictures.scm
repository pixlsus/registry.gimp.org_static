;; A script that compare images.
;;
;; This script load two images files and create:
;;   - a layer "ref" with the first image
;;   - a layer "out" with the second image
;;   - a layer "col-diff" containing white pixels where colors are
;;     different between the two images
;;   - a layer "alpha-diff" containing white pixels where alpha is
;;     different between the two images
;;   - a layer "diff" containing white pixel when either colors or
;;     alpha are different
;;   - an optional layer "grown-diff" where the white regions in 
;;     "diff" layer have grown by a given amount of pixels
;;
;; Copyright (C) 2011 Stephane SOPPERA
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


(define (script-fu-diff-pics infile1 infile2 ingrow)
  (let* ((img1 (car (gimp-file-load RUN-INTERACTIVE infile1 infile1)))
         (img2 (car (gimp-file-load RUN-INTERACTIVE infile2 infile2)))
	 (width (car (gimp-image-width img1)))
	 (height (car (gimp-image-height img1)))
	 (image (car (gimp-image-new width height RGB)))
	 (lay1 (car (gimp-layer-new-from-visible img1 image "ref")))
	 (lay2 (car (gimp-layer-new-from-visible img2 image "out")))
	 )
    ;; a function that adds a layer to the top of the layer
    ;; stack. This function is needed since gimp 2.7 deprecates
    ;; gimp-image-add-layer that was to be used in gimp 2.6
    (define (add-layer-on-top layer)
      (if (defined? 'gimp-image-insert-layer)
	  (gimp-image-insert-layer image layer 0 -1)
	  (gimp-image-add-layer image layer -1)))
    ;; a function that set the name of a layer. This function is
    ;; needed since gimp 2.7 deprecates gimp-drawable-set-name that
    ;; was to be used in gimp 2.6
    (define (set-layer-name layer name)
      (if (defined? 'gimp-item-set-name)
	  (gimp-item-set-name layer name)
	  (gimp-drawable-set-name layer name)))
    ;; a function that set the visibility of a to false layer. This
    ;; function is needed since gimp 2.7 deprecates
    ;; gimp-drawable-set-visible that was to be used in gimp 2.6
    (define (hide-layer layer)
      (if (defined? 'gimp-item-set-visible)
	  (gimp-item-set-visible layer FALSE)
	  (gimp-drawable-set-visible layer FALSE)))
    ;; a function that turns the alpha of a channel into the selection
    ;; of the image. This function is needed since gimp 2.7
    ;; deprecates gimp-selection-layer-alpha that was to be used in
    ;; gimp 2.6
    (define (select-alpha layer)
      (if (defined? 'gimp-image-select-item)
	  (gimp-image-select-item image CHANNEL-OP-REPLACE layer)
	  (gimp-selection-layer-alpha layer)))
    ;; a function that turns the white pixels of a layer into the
    ;; selection of the image. This function is needed since gimp 2.7
    ;; deprecates gimp-by-color-select that was to be used in gimp 2.7
    (define (select-white-pixels layer)
      (if (defined? 'gimp-image-select-color)
	  (begin
	    (gimp-context-set-antialias FALSE)
	    (gimp-context-set-feather FALSE)
	    (gimp-context-set-sample-merged FALSE)
	    (gimp-context-set-sample-transparent FALSE)
	    (gimp-image-select-color image CHANNEL-OP-REPLACE layer '(255 255 255)))
	  (gimp-by-color-select layer '(255 255 255) 0 CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE)))
    ;; disable undo before starting
    (gimp-image-undo-disable image)
    ;; push the context so that we don't change any user settings
    (gimp-context-push)
    ;; we don't need them any more after having created the new image
    (gimp-image-delete img1)
    (gimp-image-delete img2)
    ;; add the two reference layers
    (add-layer-on-top lay1)
    (add-layer-on-top lay2)
    ;; a function to compute the color difference between two adjacent
    ;; layers. The two layers will be lost and replace by a new layer
    ;; containing the difference.
    (define (make-diff-layer top-layer name)
      ;; change the composition mode of the second layer to difference
      (gimp-layer-set-mode top-layer DIFFERENCE-MODE)
      (let ((diff-layer (car (gimp-image-merge-down image top-layer EXPAND-AS-NECESSARY))))
	;; change layer name
	(set-layer-name diff-layer name)
	;; isolate pixels that are different using a thresold
	(gimp-threshold diff-layer 0 0)
	;; invert the color so that different pixels are white, identical pixel are black
	(gimp-invert diff-layer)
	;; return the created layer
	diff-layer
	)
      )
    ;; make the color difference & alpha difference layers
    (let* ((col-diff-layer 
	    (let ((lay1-opaque (car (gimp-layer-new-from-drawable lay1 image)))
		  (lay2-opaque (car (gimp-layer-new-from-drawable lay2 image))))
	      ;; add the two layers to the image
	      (add-layer-on-top lay1-opaque)
	      (add-layer-on-top lay2-opaque)
	      ;; remove the alpha by filling the image with an opaque color
	      ;; while RGB channel are deactivated
	      (gimp-image-set-component-active image RED-CHANNEL FALSE)
	      (gimp-image-set-component-active image GREEN-CHANNEL FALSE)
	      (gimp-image-set-component-active image BLUE-CHANNEL FALSE)
	      (gimp-edit-fill lay1-opaque FOREGROUND-FILL)
	      (gimp-edit-fill lay2-opaque FOREGROUND-FILL)
	      (gimp-image-set-component-active image RED-CHANNEL TRUE)
	      (gimp-image-set-component-active image GREEN-CHANNEL TRUE)
	      (gimp-image-set-component-active image BLUE-CHANNEL TRUE)
	      ;; create the diff layer
	      (make-diff-layer lay2-opaque "col-diff")
	      )
	    )
	   (alpha-diff-layer
	    (let ((lay1-alpha (car (gimp-layer-new-from-drawable lay1 image)))
		  (lay2-alpha (car (gimp-layer-new-from-drawable lay2 image))))
	      ;; add the two layers to the image
	      (add-layer-on-top lay1-alpha)
	      (add-layer-on-top lay2-alpha)
	      ;; fill the two layer with black
	      (gimp-context-set-background '(0 0 0))
	      (gimp-drawable-fill lay1-alpha BACKGROUND-FILL)
	      (gimp-drawable-fill lay2-alpha BACKGROUND-FILL)
	      ;; paint alpha with white in layers
	      (select-alpha lay1)
	      (gimp-edit-fill lay1-alpha WHITE-FILL)
	      (select-alpha lay2)
	      (gimp-edit-fill lay2-alpha WHITE-FILL)
	      ;; clear the selection
	      (gimp-selection-none image)
	      ;; create the diff layer
	      (make-diff-layer lay2-alpha "alpha-diff")
	      )
	    )
	   ;; add alpha and diff layers to obtain final diff
	   (diff-layer
	    (let ((col-diff-layer-copy (car (gimp-layer-new-from-drawable col-diff-layer image)))
		  (alpha-diff-layer-copy (car (gimp-layer-new-from-drawable alpha-diff-layer image))))
	      ;; add the two layers to the image
	      (add-layer-on-top col-diff-layer-copy)
	      (add-layer-on-top alpha-diff-layer-copy)
	      ;; change the composition mode of the second layer to difference
	      (gimp-layer-set-mode alpha-diff-layer-copy ADDITION-MODE)
	      (let ((diff-layer (car (gimp-image-merge-down image alpha-diff-layer-copy EXPAND-AS-NECESSARY))))
		;; change layer name
		(set-layer-name diff-layer "diff")
		diff-layer
		)
	      )
	    )
	   )
      ;; hide col and alpha difference layers
      (hide-layer col-diff-layer)
      (hide-layer alpha-diff-layer)
      ;; create a new diff layer using grow
      (if (> ingrow 0)
      	  (let ((grown-diff-layer (car (gimp-layer-new-from-drawable diff-layer image))))
	    ;; change layer name
	    (set-layer-name grown-diff-layer "grown-diff")
      	    ;; add the layer to the image
      	    (add-layer-on-top grown-diff-layer)
      	    ;; fill the layer in black
      	    (gimp-context-set-background '(0 0 0))
      	    (gimp-drawable-fill grown-diff-layer BACKGROUND-FILL)
      	    ;; select all white pixels of the diff
      	    (select-white-pixels diff-layer)
      	    ;; grow the selection
      	    (gimp-selection-grow image ingrow)
      	    ;; paint the selection in white
      	    (gimp-edit-fill grown-diff-layer WHITE-FILL)
      	    ;; clear the selection
      	    (gimp-selection-none image)
      	    )
      	  )
      )
    ;; restore gimp context
    (gimp-context-pop)
    ;; enable the undo for the image
    (gimp-image-undo-enable image)
    ;; display the created image
    (gimp-display-new image)
    )
  )

(script-fu-register
  "script-fu-diff-pics"
  _"<Toolbox>/File/Compare images..."
  "Compare two images"
  "Stephane SOPPERA"
  "copyright 2011, Stephane SOPPERA"
  "2011/12/04"
  ""
  SF-FILENAME "Ref" ""
  SF-FILENAME "Out" "" 
  SF-ADJUSTMENT "Add grow layer"    '(0 0 1000 1 10 0 1)
)


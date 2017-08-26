; Export Selection v 1.0
;
; This script pops up a GUI which allows you to save your selection to various places
;
; The source is your current selection (or whole image if none) applied to one of:
;    - the current layer
;    - all visible layers
;    - all layers
;
; The destination is one of:
;    - a PNG file (saved directly to disk)
;    - a new image in GIMP
;    - a new layer in the current image
;
; This code is released to the public domain.
;

; given a path string, returns a list
; where that string is broken on either / or \
(define (split-path path)
	(let*
	(
		(ret '())
	)
		(for-each
			(lambda (part)
				(set! ret (append ret (strbreakup part "/")))
			)
			(strbreakup path "\\")
		)
		ret
	)
)

(define (do-with-vis image vis f)
	(let*
	(
		(layersInfo (gimp-image-get-layers image))
		(numLayers (car layersInfo))
		(layerArray (cadr layersInfo))
		(layers (vector->list layerArray))
		(notVis (if (= TRUE vis) FALSE TRUE))
		(toRestore '())
	)
		; change layer visibility
		(for-each
			(lambda (lay)
				(when (not (= vis (car (gimp-drawable-get-visible lay))))
					(set! toRestore (append toRestore (list lay)))
					(gimp-drawable-set-visible lay vis)
				)
			)
			layers
		)
		
		; call their function
		(f)
		
		;restore visibility
		(for-each
			(lambda (lay)
				(gimp-drawable-set-visible lay notVis)
			)
			toRestore
		)
	)
)

(define (script-fu-export-selection inImage inLayer inLayerOption inDestOption)
	(let*
	(
		(layerIsInvis (= FALSE (car(gimp-drawable-get-visible inLayer))))
		(bufferName "")
		(imageName (car (gimp-image-get-name inImage)))
		(imageNameNoExt (unbreakupstr (butlast (strbreakup imageName ".")) "."))
		(filePath (car (gimp-image-get-filename inImage)))
		
		; Note: we always join paths with forward-slash, which works both on UNIX and Windows
		(fileDir (unbreakupstr (butlast (split-path filePath)) "/" ))
		
		(outputName (string-append imageNameNoExt "-sel"))
		
		; If we are adding a new layer to this existing image, allow undo
		(undoable (= inDestOption 2))
	)
		(if undoable
			(gimp-image-undo-group-start inImage)
			(gimp-image-undo-disable inImage)
		)
		(set! bufferName
			(case inLayerOption
				((0)	; current layer only
					
					; If we are using a layer, put the layer's name in the output name
					(set! outputName (string-append imageNameNoExt "-" (car (gimp-drawable-get-name inLayer))))
					(let*
					(
						(bufferRet "")
					)
						(do-with-vis inImage FALSE
							(lambda ()
								; Make the current layer visible, since inside this
								; block everything is invisible
								(gimp-drawable-set-visible inLayer TRUE)
								(set! bufferRet (car (gimp-edit-named-copy-visible inImage "export-selection_temp_buffer")))
							)
						)
						
						; Restore our layer's visibility to whatever it was before
						(when layerIsInvis
							(gimp-drawable-set-visible inLayer FALSE)
						)
						
						; return the buffer
						bufferRet
					)
				)
				((1)	; visible layers
					(car (gimp-edit-named-copy-visible inImage "export-selection_temp_buffer"))
				)
				((2)	; all layers
					(let*
					(
						(bufferRet "")
					)
						(do-with-vis inImage TRUE
							(lambda ()
								(set! bufferRet (car (gimp-edit-named-copy-visible inImage "export-selection_temp_buffer")))
							)
						)
						
						; return the buffer
						bufferRet
					)
				)
			)
		)
		
		(case inDestOption
			((0 1)
				; file
				(let*
				(
					(imageNew (car (gimp-edit-named-paste-as-new bufferName)))
					(imageNewPath (string-append (unbreakupstr (list fileDir outputName) "/") ".png"))
				)
					(case inDestOption
						((0)
							(gimp-file-save
								RUN-NONINTERACTIVE
								imageNew
								(car (gimp-image-get-active-layer imageNew))
								imageNewPath
								imageNewPath
							)
							(gimp-image-delete imageNew)
						)
						((1)
							(gimp-image-set-filename imageNew imageNewPath)
							(gimp-display-new imageNew)
						)
					)
					(gimp-displays-flush)
					(gimp-image-clean-all inImage)
				)
			)
			((2)
				; new layer
				(let*
				(
					(floatingSelNew (car (gimp-edit-named-paste inLayer bufferName TRUE)))
				)
					(gimp-floating-sel-to-layer floatingSelNew)
					(gimp-drawable-set-name floatingSelNew outputName)
					(gimp-displays-flush)
				)
			)
		)
		(if undoable
			(gimp-image-undo-group-end inImage)
			(gimp-image-undo-enable inImage)
		)
	)
)

(script-fu-register
	"script-fu-export-selection"                ;func name
	"Export Selection"                          ;menu label
	"Export a selection from one or more\
layers to a new image."                             ;description
	"jwdevel"                                   ;author
	"Public Domain"                             ;copyright notice
	"2009"                                      ;date created
	""                                          ;image type that the script works on
	SF-IMAGE    "Image"    0
	SF-DRAWABLE "Drawable" 0
	SF-OPTION "Layer:"       '("current layer" "visible layers" "all layers")     ;source layer
	SF-OPTION "Destination:" '("PNG file" "new image" "new layer")       ;destination iamge
)
(script-fu-menu-register "script-fu-export-selection" "<Image>/Script-Fu")


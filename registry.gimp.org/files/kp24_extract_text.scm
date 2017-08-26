; Version 1.0 05.04.2011 First attempt for Phyllis

; Extract text from a single file
(define (script-fu-extract_text_from_image imageFile text_output_stream)
  (let* (
          (Image (car (gimp-file-load RUN-NONINTERACTIVE imageFile imageFile)))  ; load the file  
          (layerList (vector->list (cadr (gimp-image-get-layers Image))))        ; get the list of layer ids
        )
        
        (display (car (gimp-image-get-filename Image)) text_output_stream)   ; output the file name
        (newline text_output_stream)
        (for-each                                                            ; scan through the layers
          (lambda (layerId)
            (begin
              (if (= (car (gimp-drawable-is-text-layer layerId)) TRUE)       ; is it a text layer?
                 (begin
                   (display (car (gimp-text-layer-get-text layerId)) text_output_stream)
                   (newline text_output_stream)
                 )
              )
            )
          )
          layerList
        )
        (newline text_output_stream)
        (gimp-image-delete Image)
  )
)

(define (script-fu-extract_text imageFile textFileName)
  (let* (
           (text_output_stream 0)
        )
        (set! text_output_stream (open-output-file textFileName))
        (script-fu-extract_text_from_image imageFile text_output_stream)
        (close-output-port text_output_stream) 
        (gimp-message "Finished")
  )
)

(define (script-fu-extract_text_batch_interactive pattern textFileName)
  (let* (
           (filelist (cadr (file-glob (string-append "*" pattern) 1)))
           (text_output_stream 0)
        )

        (set! text_output_stream (open-output-file textFileName))
        (for-each
          (lambda (imageFile)
            (script-fu-extract_text_from_image imageFile text_output_stream)
          )
          filelist
        )
        (close-output-port text_output_stream) 
        (gimp-message "Finished")
  )
)

(script-fu-register "script-fu-extract_text"
		    _"<Image>/contrib/Extract Text..."
		    "Extract Text from text layers"
		    ""
		    ""
		    "05/04/2011"
		    ""
                    SF-FILENAME       _"Input File name" ""
                    SF-FILENAME       _"Text File name" ""
)

(script-fu-register "script-fu-extract_text_batch_interactive"
		    _"<Image>/contrib/Batch Extract Text..."
		    "Extract Text from text layers"
		    ""
		    ""
		    "05/04/2011"
		    ""
                    SF-STRING         _"File pattern" "*.xcf"
                    SF-FILENAME       _"Text File name" ""
)

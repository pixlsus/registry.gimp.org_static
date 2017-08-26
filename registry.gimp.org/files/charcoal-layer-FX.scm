;
; -*-scheme-*-
;
; Script-Fu Charcoal-Layer-FX v1.0 by Micomicon - March 2011
; A Gimp script which takes the effect from "Charcoal.scm" and adds it as a layer or layers. 
; Opening the Layers Dialog and scrolling through the layer modes afterwards can produce some 
; interesting results akin to adding a watercolour wash, the strength of which can be varied by
; adjusting the opacity slider.
;
; Thanks to mahvin for the change of direction.
;
; Tested with Gimp 2.6
;
; Works on RGB or grayscale images but adding chalk will fail if applied to a layer 
; which does not contain any image or contains an image with too limited a tonal range 
; ie. "Add chalk highlights" won't work if there is nothing to make the chalk highlights from.
;
; To install (Linux): 
; Download and move the script file charcoal-layer-FX.scm to /yourhome/.gimp-2.x/scripts.
;
; The script is located in "<Image> / Filters / Artistic / Charcoal-Layer-FX..."
; and will be available the next time you start the Gimp.
;
; This software is in the public domain. 
;
; I have released this script into the wild in the hope that it may 
; prove useful. It comes with absolutely no warranty whatsoever, 
; without even the implied warranty of merchantability or fitness 
; for any particular purpose. See the GNU General Public License for 
; details. You may redistribute and/or modify this script or extract 
; segments from it without prior consent.



(define (script-fu-charcoal-layer-FX image drawable granuality lowpoint highpoint opacity active-layer? boost? invert? chalk?)

    (let* (

                (copy-layer -1)
                (image-layer -1)
                (charcoal-layer 0)
                (chalk-layer 0)                                         

          ) ; End of variable definitions


    ; Initialise message box
    (gimp-message-set-handler 0)

    ; Save the current settings
    (gimp-context-push)

    ; Start an undo group so the image can be restored with one undo
    (gimp-image-undo-group-start image)

    ; Clear any selections to avoid execution errors. 
    ; NB It will still fail if applied to a floating selection.
     (gimp-selection-none image)
	     

    ; If "work only on active layer" is checked then make a copy to process

       (gimp-progress-set-text _"Copying image")

       (if (equal? active-layer? TRUE)
          (begin

          (set! image-layer (car (gimp-image-get-active-layer image)))
          (set! charcoal-layer (car (gimp-layer-copy image-layer TRUE)))

          (gimp-image-add-layer image charcoal-layer 0)
          (gimp-drawable-set-name charcoal-layer "charcoal layer") )

       ; Else copy visible into a work-layer

          (begin
 
          (set! charcoal-layer (car (gimp-layer-new-from-visible image image "charcoal layer") ))

          (gimp-image-add-layer image charcoal-layer 0) ) 
          (gimp-image-set-active-layer image charcoal-layer) )

    ; Increase the contrast if required

      (when (equal? boost? TRUE)

       (gimp-progress-set-text _"Stretching levels")

         (begin

         (gimp-levels-stretch charcoal-layer)) )

    ; Add some texture
    (plug-in-sharpen RUN-NONINTERACTIVE image charcoal-layer granuality)
    
    (gimp-progress-set-text _"Creating texture")

    ; Creating chalk highlights

       (when (equal? chalk? TRUE)

       (gimp-progress-set-text _"Creating chalk highlights")

         (begin

        ; Make a working copy of the charcoal layer to strip out
        (set! copy-layer (car (gimp-layer-copy charcoal-layer TRUE))) 
        (gimp-image-add-layer image copy-layer 0)

        ; Add some white (50-75 = threshold range for white highlights)
        (gimp-image-set-active-layer image copy-layer)
        (set! drawable (car (gimp-image-get-active-drawable image)))  
        (gimp-drawable-set-name copy-layer "copy layer")
        (gimp-threshold drawable 50 75)

        ; Copy the chalk to a new transparent layer

        (gimp-by-color-select drawable '(255 255 255) 0 
         CHANNEL-OP-REPLACE FALSE FALSE 0 FALSE) 

        ; Check if layer is empty
           
        (if (= (car (gimp-selection-is-empty image)) TRUE)

          (begin

        ; Remove the potential chalk work layer
        (gimp-image-remove-layer image copy-layer)

          (gimp-message "Unable to add any chalk highlights.\nPerhaps you are working on an empty layer\nor a layer which has a too limited a tonal range?") )

          (begin

        (gimp-edit-cut drawable)

        (set! chalk-layer (car (gimp-image-get-selection image)))     
        (gimp-floating-sel-to-layer (car (gimp-edit-paste drawable 1)))
        (gimp-selection-none image)     
        (gimp-drawable-set-name (car (gimp-image-get-active-layer image)) "chalk layer") 

        ; Remove the now redundant work layer
        (gimp-image-remove-layer image copy-layer)

  ) )  ; End of chalk abort     
  ) )  ; End of chalk layer operations
      
    (gimp-progress-set-text _"Painting charcoal layer")

    ; Resume work on charcoal layer
    (gimp-image-set-active-layer image charcoal-layer) 

    ; Set the user defined thresholds
    (gimp-threshold charcoal-layer lowpoint highpoint)
 
    ; Retain negative image if required  

       (when (equal? invert? FALSE)

         (gimp-progress-set-text _"Inverting layer")

           (begin

              (gimp-invert charcoal-layer)) )


    ; Convert lowlights to charcoal grey   

       (when (equal? boost? FALSE)

           (begin
       
              (gimp-levels charcoal-layer 0 000 255 1 035 255) ) )

         (gimp-progress-set-text _"Adjusting opacity")

    ; Set the opacity of the charcoal layer (nominally 50%)
    (gimp-layer-set-opacity charcoal-layer opacity)

    ; Restore previous settings
    (gimp-context-pop)  
 
    ; Finish the undo group for the process
    (gimp-image-undo-group-end image)

    ; Display the updated image
    (gimp-displays-flush)
       

    ) ; End of Let block
)  ; End of Define block


(script-fu-register "script-fu-charcoal-layer-FX"
            _"<Image>/Filters/Artistic/Charcoal-Layer-FX..."
            "Adds a charcoal effect layer"
            "Micomicon"
            "Adrian Spriddell"
            "March 2011"
            "RGB* GRAY*"
            SF-IMAGE		"Image"     0
            SF-DRAWABLE		"Drawable"  0
	    SF-ADJUSTMENT       "Granuality"      '(65 0 90 1 5 0 0)
	    SF-ADJUSTMENT       "Lowlights crossover point"      '(70 0 127 1 10 0 0)
	    SF-ADJUSTMENT       "Highlights crossover point"     '(170 127 255 1 10 0 0)
            SF-ADJUSTMENT       "Opacity"     '(50 0 100 1 10 0 0)
	    SF-TOGGLE           "Work only on active layer" 1 
            SF-TOGGLE           "Boost contrast" 0 
            SF-TOGGLE           "Invert to negative" 0   
	    SF-TOGGLE           "Add chalk highlights" 0  
      
)

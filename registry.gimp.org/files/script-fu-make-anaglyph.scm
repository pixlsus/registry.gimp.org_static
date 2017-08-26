;
; make-anaglyph
;
;
; Steph Parker (steph.parker58@yahoo.com.au)
; On-line portfolio: <http://www.flickr.com/photos/stephpar>.

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
; This program creates stereoscopic 3D anaglyph photos from a stereo pair.
; In order to make use of this program you will first need a pair of images
; from slightly different angles or the same object. You then need to place
; both images as different layers in the same image window in Gimp with the
; right image as the Bottom layer and the left image as the next layer above
; it and no other layers in the image (if you use this script in other cir-
; cumstances then it probably won't work properly). The script finishes
; leaving the two images as seperate layers so that final alignment adjust-
; ments can be made before merging the layers down and saving the anaglyph.

; Any colours can be chosen for the two layers but it is recommended that 
; you only choose colours with individual RGB values of 0 or 255 and that
; the two colours compliment each other. The default colours, and the most
; commoly used colour combination, are Red (255 0 0) for the right image and
; cyan (0 255 255) for the left image. Other possible pairs are:
;       Red   (255 0 0) and Blue    (0 0 255)
;       Red   (255 0 0) and Green   (0 255 0)
;       Blue  (0 0 255) and Green   (0 255 0)
;       Blue  (0 0 255) and Yellow  (255 255 0)
;       Green (0 255 0) and Magenta (255 0 255)
; but be warned, not all colour pairs work equally well.

; To view the anaglyphs as 3D images you will need a pair of glasses in
; the colours that you have chosen with the colour of the left eye of 
; the glasses matching the colour applied to the right image (e.g.
; with the default red/cyan combination the red side of the glasses
; goes over the left eye and the cyan side on the right eye).

; And finaly, if you haven't been involved in 3D photography before
; it is highly addictive and standard 2D images will look flat when
; compared to anaglyphs.

; Define the function:
(define (script-fu-make-anaglyph inImg inDrawable inTopColour inBottomColour)
  (gimp-image-undo-group-start inImg)
  (gimp-context-push)
  
  (let*
    (
      ; create local variables
      (theLayersList
        (cadr
          (gimp-image-get-layers inImg)
        )
      )
      (theTopImageLayer
        (aref theLayersList 0)
      )
      (theBottomImageLayer
        (aref theLayersList 1)
      )
    )
    
    (gimp-context-set-foreground inTopColour)
    (gimp-context-set-background inBottomColour)
    
    (gimp-selection-all inImg)
    (gimp-edit-bucket-fill theTopImageLayer FG-BUCKET-FILL SCREEN-MODE 100 0 FALSE 0 0)
    (gimp-edit-bucket-fill theBottomImageLayer BG-BUCKET-FILL SCREEN-MODE 100 0 FALSE 0 0)
    (gimp-layer-set-mode theTopImageLayer MULTIPLY-MODE)
  )
  (gimp-displays-flush)
  (gimp-context-pop)
  (gimp-image-undo-group-end inImg)
)

(script-fu-register
  "script-fu-make-anaglyph"                        ;func name
  "Make Anaglyph"                                  ;menu label
  "Converts two images to a two-colour Anaglyph. Right image as Background, Left image as Layer 1"              ;description
  "Steph Parker"                             ;author
  "copyright 2008, Steph Parker"             ;copyright notice
  "27/6/2008"                          ;date created
  "RGB, RGBA"                     ;image type that the script works on
  SF-IMAGE "Image" 0
  SF-DRAWABLE "Drawable" 0
  SF-COLOR       "Top Layer Color (Cyan):"        '(0 255 255)     ;color variable
  SF-COLOR       "Bottom Layer Color (Red):"        '(255 0 0)     ;color variable
  
)

(script-fu-menu-register "script-fu-make-anaglyph" "<Image>/Stereo")


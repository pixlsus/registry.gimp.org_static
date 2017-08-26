;;;
;;; load_for_hdr.scm
;;;
;;; Copyright (C) 2011 Thilo Fromm <kontakt@thilo-fromm.de>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;
;  This is a helper script aimed at automating the loading procedure of the
;  GIMP HDR Workflow described at 
;	<http://www.instructables.com/id/HDR-photos-with-the-GIMP/>
;
;  Here's a quick overview of the HDR workflow:
;  - start with the the "center" image of an exposure series - this 
;     is the backtround image
;  - add the "dark" image(s) of the series as layers
;    -- add a layer mask
;    -- paste the desaturated dark image to the layer mask, thus
;        letting the "bright" parts of the dark image through while
;        blocking the "dark" parts
;  - add the "bright" image(s) of the exposure series
;    -- add a layer mask
;    -- paste the desaturated *inverted* bright image to the mask, thus
;        composing the "darker" areas of the bright image into the 
;        picture while discarding the brighter areas
;  - adjust by changing the brightness levels of the layer masks
;
;  The script automates all but the last step. After installing you'll get
;  two new menu entries in the "File" pulldown menu: 
;
;  	"Load images for HDR blending (3 images)..."
;  	"Load images for HDR blending (5 images)..."
;
;  The script assumes that you already loaded the "middle" image.
;  Clicking the menu entry will pop up a dialog which lets you load
;  the dark and bright picture(s). The script currently only supports
;  3 or 5 images in a series since that's what I need :) let me know 
;  if your use case differs.
;
;  After the images have been added in the dialog the script will 
;  generate corresponding layers, paste the image contents, generate
;  layer masks, and fill the masks accordingly.
;
;  You can adjust the HDR effects of each layer by playing with the layer's
;  opacity (which lets you play with the settings without actually changing 
;  anything in the layers) or by editing  the layer mask's brightness/contrast 
;  settings (which doesn't).
;
;  Adjusting a layer mask's brightness/contrast is as easy as 
;    - clicking the layer
;    - selecting "Colours -> Brightness/Contrast"
;  since all the newly created layers will be in "mask editing" mode after
;  the script finished.
;  
;
;  Have fun using this and let me know what you think.
;                        			-- thilo, feb. 10, 2011
;
;


(script-fu-register
          "load-3-hdr"
          "Load images for HDR blending (3 images) ..."
          "Loads a bright and a dark image as layers into \
	   the current image and creates layer masks suitable \
	   for HDR blending the whole image."
          "Thilo Fromm <kontakt@thilo-fromm.de>"
          "Copyright 2011, Thilo Fromm"
          "Februar 7 2011"
          "RGB* GRAY* INDEXED*"

          SF-IMAGE       "Current image" 0

          SF-FILENAME    "dark image" "(None)"
          SF-FILENAME    "bright image" "(None)"

          SF-OPTION	 "Desaturate image by" '("Lightness" "Luminosity" "Average")
          SF-OPTION	 "Invert bright image using" '("Standard GIMP invert" "'Value Invert' plugin")
)
(script-fu-menu-register "load-3-hdr" "<Toolbox>/File")
      
(script-fu-register
          "load-5-hdr"
          "Load images for HDR blending (5 images) ..."
          "Loads 2 bright and 2 dark images as layers into \
	   the current image and creates layer masks suitable \
	   for HDR blending the whole image."
          "Thilo Fromm <kontakt@thilo-fromm.de>"
          "Copyright 2011, Thilo Fromm"
          "Februar 7 2011"
          "RGB* GRAY* INDEXED*"

          SF-IMAGE       "Current image" 0

          SF-FILENAME    "dark image" "(None)"
          SF-FILENAME    "dark-ish image" "(None)"
          SF-FILENAME    "bright-ish image" "(None)"
          SF-FILENAME    "bright image" "(None)"

          SF-OPTION	 "Desaturate image by" '("Lightness" "Luminosity" "Average")
          SF-OPTION	 "Invert bright images using" '("Standard GIMP invert" "'Value Invert' plugin")
)
(script-fu-menu-register "load-5-hdr" "<Toolbox>/File")
 

; copy-image-as-float: Copy content of one image into floating selection of another image.
;
; This function copies the content of the source image's 
; active drawable (most often the current layer) 
; into a floating selection of another image.
;
;  dest-img  -  destination image
;   src-img  -  source image
;    RETURN  -  floating selection in the destination image

(define (copy-image-as-float dest-img src-img)

    (gimp-selection-all src-img)
    (gimp-edit-copy  (car (gimp-image-get-active-drawable src-img)))
    (car (gimp-edit-paste (
                        car (gimp-image-get-active-drawable dest-img))
                        0))
)

; copy-image-as-new-layer: Copy content of one image into a new layer of another image.
;
; This function copies the content of the source image's 
; active drawable (most often the current layer) 
; into a new layer of another image.
;
;  dest-img  -  destination image
;   src-img  -  source image
;    RETURN  -  handle of the new layer in the destination image

(define (copy-image-as-new-layer dest-img src-img)
    (let* ( 
            (l (copy-image-as-float dest-img src-img)))
        (gimp-floating-sel-to-layer l)
        l
    )
)

; copy-image-and-anchor: Copy content of one image into another image and "anchor" the copied content.
;
; This function copies the content of the source image's 
; active drawable (most often the current layer) 
; into another image and merge the copied content into 
; the destination image by "anchoring" the copied content.
;
;  dest-img  -  destination image
;   src-img  -  source image

(define (copy-image-and-anchor dest-img src-img)
    (let* ( 
            (l (copy-image-as-float dest-img src-img)))
        (gimp-floating-sel-anchor l)
    )
)

; layer-add-mask: Add a new layer mask to a layer
;
; This function creates a new layer mask 
; and adds the mask to an existing layer.
;
;    layer  -  the layer to add the mask to

(define (layer-add-mask layer)
    (let* ( 
            (m (car (gimp-layer-create-mask layer ADD-WHITE-MASK)) ))
        (gimp-layer-add-mask layer m )
    )
)

; process-single-image: load a source image for HDR processing into a layer of the destination image.
;
; This function loads one source image into a newly created layer of
; the HDR composition image. The layer will also have a layer mask added with
; the desaturated (and optionally inverted) source image in the mask.
;
;  dest-imgage       -  destination image
;  source-image      -  source image
;  dest-layer-name   -  descriptive name of the new layer in the destination image
;  desaturate-option -  determines how to desaturate the source image for the mask layer:
;                       0  -  desaturate by lightness
;                       1  -  desaturate by luminosity
;                       2  -  desaturate by average
;  invert-option     -  controls whether and how the desaturated source image is value-inverted
;                       before it is pasted as lasyer mask.
;                       0 - don't invert the source image
;                       1 - invert using plain old GIMP invert (Colours -> Invert)
;                       2 - invert using "Value Invert" plug-in

(define (process-single-image 
                dest-image 
                source-image 
                dest-layer-name 
                desaturate-option 
                invert-option) 
    (let* ( 
            (layer (copy-image-as-new-layer dest-image source-image)))

    (gimp-layer-set-name layer dest-layer-name)
    (gimp-desaturate-full (car (gimp-image-get-active-drawable source-image)) desaturate-option)

    (if (= 1 invert-option)
            (gimp-invert (car (gimp-image-get-active-drawable source-image))))
    (if (= 2 invert-option)
            (plug-in-vinvert RUN-NONINTERACTIVE source-image (car (gimp-image-get-active-drawable source-image))))

    (layer-add-mask layer)
    (copy-image-and-anchor dest-image source-image)
    (gimp-layer-set-edit-mask layer FALSE)

    layer
    )
)

; load-3-hdr: Load 2 images to create a HDR composed of 3 images
; load-5-hdr: Load 4 images to create a HDR composed of 5 images
;
; These functions load two or four additional images into new layers of 
; the current one. All new layers will have a layer mask added which
; will consist of the desaturated image loaded into the layer. The "bright"
; image(s) will also have the layer mask image inverted.

(define (load-3-hdr curr-image dark-file bright-file desaturate-option invert-option)
    (let* (
        (dark-image   (car (gimp-file-load 0 dark-file dark-file)))
        (bright-image (car (gimp-file-load 0 bright-file bright-file)))

        (layer0 (process-single-image curr-image dark-image "dark layer" desaturate-option 0))
        (layer1 (process-single-image curr-image bright-image "bright layer" desaturate-option (+ 1 invert-option)))
       )

    
     (gimp-layer-set-edit-mask layer0 TRUE)
     (gimp-layer-set-edit-mask layer1 TRUE)
    
     (gimp-image-delete dark-image)
     (gimp-image-delete bright-image)

    )
)

; documentation see load-3-hdr above.

(define (load-5-hdr curr-image dark-file darkish-file brightish-file bright-file desaturate-option invert-option)
    (let* (
        (dark-image   (car (gimp-file-load 0 dark-file dark-file)))
        (darkish-image   (car (gimp-file-load 0 darkish-file darkish-file)))
        (brightish-image (car (gimp-file-load 0 brightish-file brightish-file)))
        (bright-image (car (gimp-file-load 0 bright-file bright-file)))

        (layer0 (process-single-image curr-image dark-image "dark layer" desaturate-option 0))
        (layer1 (process-single-image curr-image darkish-image "dark-ish layer" desaturate-option 0))
        (layer2 (process-single-image curr-image brightish-image "bright-ish layer" desaturate-option (+ 1 invert-option) ))
        (layer3 (process-single-image curr-image bright-image "bright layer" desaturate-option (+ 1 invert-option)))
       )

    
     (gimp-layer-set-edit-mask layer0 TRUE)
     (gimp-layer-set-edit-mask layer1 TRUE)
     (gimp-layer-set-edit-mask layer2 TRUE)
     (gimp-layer-set-edit-mask layer3 TRUE)
    
     (gimp-image-delete dark-image)
     (gimp-image-delete darkish-image)
     (gimp-image-delete bright-image)
     (gimp-image-delete brightish-image)

    )
)

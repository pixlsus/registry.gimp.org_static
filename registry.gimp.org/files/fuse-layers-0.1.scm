
;; fuse-layers-0.1.scm: Place layers side by side.
;; Copyright (C) 2011 by Eduardo Hernàndez, coz.eduardo.hernandez@gmail.com
;;
;; Based on pandora-combine.scm
;; Copyright (C) 2006 by Akkana Peck, akkana@shallowsky.com.
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; To get a copy of the GNU General Public License see
;; <http://www.gnu.org/licenses/>.

;; How to use:
;; Open the image with all the layers, run this script on the image, then
;; select how many frames you want on the X axis

( define
    ( script-fu-fuse-layers orig_img drawable x_layers )

;;    ( gimp-context-push )
;;    ( gimp-image-undo-freeze orig_img )

    ( let*
        (
            ( layers ( gimp-image-get-layers orig_img ) )
            ( num_layers ( car layers ) )
            ( layer_array ( cadr layers ) )
            ( bottom_layer ( aref layer_array ( - num_layers 1 ) ) )

            ;; The actual X and Y size used for the layers are based on the
            ;; first layer
            ( orig_layer_w ( car ( gimp-drawable-width bottom_layer ) ) )
            ( orig_layer_h ( car ( gimp-drawable-height bottom_layer ) ) )
            ( image_w ( * x_layers orig_layer_w ) )
            ( y_layers( ceiling ( / num_layers x_layers ) ) )
            ( image_h ( * y_layers orig_layer_h ) )
            ( start_x ( / ( + ( - 0 image_w ) orig_layer_w ) 2 ) )
            ( start_y ( / ( + ( - 0 image_h ) orig_layer_h ) 2 ) )
            ( i ( - num_layers 1 ) ) ; Start from the bottom layer

            ;; Create image
            ( new_img ( car ( gimp-image-new image_w image_h RGB ) ) )

            ( new_layer
                ( car
                    ( gimp-layer-new
                        new_img
                        image_w
                        image_h
                        RGBA-IMAGE
                        "fused layer"
                        100
                        NORMAL-MODE
                    )
                )
            )
        )

        ( gimp-image-undo-disable new_img )
        ( gimp-image-add-layer new_img new_layer 0 )

        ( gimp-rect-select
            orig_img
            0
            0
            orig_layer_w
            orig_layer_h
            CHANNEL-OP-REPLACE
            FALSE
            0
        )

        ;; Loop over layers to copy & paste contents
        ( while( >= i 0 )
            ( let*
                (
                    ( curr_layer ( aref layer_array i ) )
                    ( float 0 )
                    ( x 0 )
                    ( y 0 )
                )
;                ( gimp-message ( number->string curr_layer ) )
                ( gimp-edit-copy curr_layer )
                ( set! float
                   ( car ( gimp-edit-paste new_layer FALSE ) )
                )
                ( set! x
                    ( + ( * orig_layer_w ( modulo i x_layers ) ) start_x )
                )
                ( set! y
                    ( + ( * orig_layer_h ( quotient i x_layers ) ) start_y )
                )
                ( gimp-layer-translate float x y )
                ( gimp-floating-sel-anchor float )
            )

            ( set! i ( - i 1 ) )
        )  

        ( gimp-image-undo-enable new_img )
        ( gimp-image-clean-all new_img)
        ( gimp-display-new new_img )
    )

    ( gimp-image-undo-thaw orig_img )
;;    ( gimp-context-pop )
    ( gimp-displays-flush )
)

( script-fu-register
  "script-fu-fuse-layers"
  "Fuse layers" 
  "Creates a new images with a copy of the \
contents of all the layers, side by side, \
in a single layer."
  "Eduardo Hernàndez"
  "Copyright 2011, Eduardo Hernàndez"
  "February 16, 2011"
  ""
  SF-IMAGE    "Image"                0
  SF-DRAWABLE "Drawable"             0
  SF-VALUE    "Layers on the X axis" "1"
) 
( script-fu-menu-register "script-fu-fuse-layers" "<Image>/Filters/Combine" )


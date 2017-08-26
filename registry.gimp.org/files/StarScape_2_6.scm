; Star Scape
; Script-Fu for GIMP 2.6, by LightningIsMyName
; 
; This Script is meant to create a star scape pattern
; 
; This script was written by LightningIsMyName
; Email: lightningismyname(at)gmail(dot)com 
; My Home Page: http://lightningismyname.deviantart.com/
;
; This Program is a free software, and you may use it for any purpose, comercial and non-commercial.
; You may not sell or redistribute this software alone or inside a bundle unless it's for FREE.
; You may modify this script only if:
; 1. You keep this message with my email, homepage, description, and release log.
; 2. You share alike with the same licensing, and without any profit.
;
; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
; even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
; 
; Release Log: Version 1.0 - September 2007
;              Initial release, for GIMP 2.4
;              Thanks to saulgoode from the GIMP Talk forums for helping me to debug this script
;
;              Version 1.1 - February 2009
;              Another release, for GIMP 2.6.
;              Updated the menu registration of the script

(define (script-fu-starscape width height seed XDensity YDensity incolor)

     (let*
           (
           (img (car
               (gimp-image-new width height RGB)))
            (layerone (car 
               (gimp-layer-new
               img
               width 
               height
               RGB-IMAGE
               "Star Scape"
               100
          NORMAL)))
            (layerclouds (car 
               (gimp-layer-new
               img
               width 
               height
               RGB-IMAGE
               "Clouds1"
               50
               18)))  ;18 is Hardlight mode
            (layercloudscopy (car 
               (gimp-layer-new
               img
               width 
               height
               RGB-IMAGE
               "Clouds2"
               50
               18)))
            (floating) ;Define new variable for later usage
)

      ;Add Black Layer
      (gimp-image-add-layer img layerone 0)

      ;Select All
      (gimp-rect-select img 0 0 width height ADD 0 0)

      ;Fill With Black - Had problms using the fill function so i used colorize instead
      (gimp-colorize layerone 0 0 -100)

       ;In Order to create the Stars we will use the hurl function on the black layer
       (plug-in-randomize-hurl 1 img layerone 1 1 TRUE 0)

       ;Desaturate the noise to achieve grayscale stars
       (gimp-desaturate layerone)

       ;Add the Cloud Layers
       (gimp-image-add-layer img layerclouds 0)
       (gimp-image-add-layer img layercloudscopy 0)

       ;Erase Junk from the clouds layers
        (gimp-edit-clear layerclouds) 
        (gimp-edit-clear layercloudscopy) 

        ;Adding the clouds to the layer 
        (plug-in-solid-noise 1 img layerclouds 0 0 seed 15 XDensity YDensity)

        ;copy the clouds
        (gimp-edit-copy layerclouds)

        ;paste the clouds - define as a new layer called floating
        (set! floating (car (gimp-edit-paste layercloudscopy 1)))

        ;Attach floating to second cloud layer
        (gimp-floating-sel-attach floating layercloudscopy)

        ;rotate cloud layer
        (plug-in-rotate 1 img layercloudscopy 2 FALSE)

        ;merge all layers
        (set! layerone(car (gimp-image-merge-visible-layers img 1)))

        ;Select All
        (gimp-rect-select img 0 0 width height ADD 0 0)

        ;set active forground color and fill layer with color on soft light mode
        (gimp-context-set-foreground incolor)
        (gimp-edit-bucket-fill layerone 0 19 100 0 0 0 0) 

        ;Display Final result
        (gimp-display-new img)


     )
)


(script-fu-register
      "script-fu-starscape"
              "<Image>/File/Create/Render/Star Scape..."
              "Draw a StarScape with a determined color, density and size"
              "LightningIsMyName (LIMN)"
              "LightningIsMyName (LIMN)"
              "September 2007"
              ""
              SF-VALUE      "Image Width (px)" "300"
              SF-VALUE      "Image Height (px)" "300"
              SF-ADJUSTMENT "Clouds - Random Seed"         '(0 0 1294967295 1 1 0 1)
              SF-ADJUSTMENT "Clouds Density - X Size"         '(4 0.1 16 0.1 1 1 1)
              SF-ADJUSTMENT "Clouds Density - Y Size"         '(4 0.1 16 0.1 1 1 1)
              SF-COLOR   "Color"  '(21 76 212)
)


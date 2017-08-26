;*************************************************************************************** 
; Toon Image script  for GIMP 2.6
; Version 1.0.1 - 17-12-2010 added halftone option, check for existence of GREYCstoration
;                            pop up message only.  Filter continues if non-existent. 
;                            Added G'MIC support as per forum request.
; Version 1.0.2 - 17-12-2010 Re-did the Half-tone the same as the comic strip effect
;                            Copyright (C) 2010 John Harris john@grynn.com>  
;                            Based on comic-book script by Joe Kitella <joe.kitella@gmail.com
; Version 1.0.3 - 17-12-2010 Revised Halftone effect again... Coding problem with layers. 
; Version 1.0.4 - 20-12-2010 Added last resort pre-filter of Selective Gaussian Blur as per
;                            forum request (Rob A>), tightened up the parameters from the request at 
;                            5/15 to 4/11. There was too much edge loss at the higher values.
; Version 1.0.5 - 21-12-2010 Added SF-OPTION to select the default smoothing type if all filters
;                            are installed. (Defaults to none installed).   
; Version 1.0.6 - 22-12-2010 Added Despeckle smoothing option, ... to display name, and did all edit-
;                            copy-pastes to a named buffer.  Cleaned up code, removed a section that was
;                            no longer being used. Generates better skin-tone colors than before.
;                 24-12-2010 Added Despeckle to filter register information.  Changed levels for truer colors.
; Version 1.0.7 - 31-12-2010 Added Level of colors to either flatten out the colors, or make more levels.
;                            Fixed the active layer display issue when not using halftone.  Added option to
;                            keep image on existing layer (non haltone). Default is keep. Gimp 2.7 fixes.
; ---------------------------------------------------------------------------------------
(define (script-fu-CarTOONize
			img
			drawable
                        Prefilter
                        filter-type
                        line-size
                        enable-shadow
                        shadow-intensity
                        cdepth
                        new-layer
                        half-tone
	)

  (gimp-image-undo-group-start img)

  (let* (
	 (width (car (gimp-drawable-width drawable)))
	 (height (car (gimp-drawable-height drawable)))
	 (old-selection (car (gimp-selection-save img)))
	 (image-type (car (gimp-image-base-type img)))
         (buffer (car (gimp-edit-named-copy drawable "temp-buffer")))
         (toon-img (car (gimp-edit-named-paste-as-new buffer)))
	 (layer-temp1 (car (gimp-layer-new toon-img width height 0 "temp1"  100 NORMAL-MODE)))
	 (layer-temp2 (car (gimp-layer-new toon-img width height 0 "temp2"  100 NORMAL-MODE)))
	 (layer-temp3 (car (gimp-layer-new toon-img width height 0 "temp3"  100 NORMAL-MODE)))
	 (layer-temp2a (car (gimp-layer-new toon-img width height 0 "temp2a"  100 NORMAL-MODE)))
	 (layer-temp4 (car (gimp-layer-new toon-img width height 0 "temp4"  100 NORMAL-MODE)))
	 (layer-temp5 (car (gimp-layer-new toon-img width height 0 "temp5"  100 NORMAL-MODE)))
	 (layer-temp6 (car (gimp-layer-new toon-img width height 0 "temp6"  100 NORMAL-MODE)))
	 (layer-temp7 (car (gimp-layer-new toon-img width height 0 "temp7"  100 NORMAL-MODE)))
 	 (layer-gmic (car (gimp-layer-new toon-img width height 0 "gmic"  100 NORMAL-MODE)))
 	 (layer-cartoonize (car (gimp-layer-new img width height 0 "CarTOONize"  100 NORMAL-MODE)))
 	 (layer-comic (car (gimp-layer-new img width height 0 "Comic-strip"  100 NORMAL-MODE)))
         (cdepth (* 4 cdepth))     
         (isfiltered  FALSE)
      ) 
    (gimp-image-undo-disable toon-img)

    (if (eqv? (car (gimp-selection-is-empty toon-img)) TRUE)
        (gimp-drawable-fill old-selection WHITE-IMAGE-FILL)) 
    (gimp-image-add-layer toon-img layer-gmic -1)
    (gimp-image-add-layer toon-img layer-temp7 -1) 
    (gimp-image-add-layer toon-img layer-temp6 -1) 
    (gimp-image-add-layer toon-img layer-temp5 -1)
    (gimp-image-add-layer toon-img layer-temp4 -1)
    (gimp-image-add-layer toon-img layer-temp3 -1)

    (if (= enable-shadow TRUE)
        (begin
           (gimp-image-add-layer toon-img layer-temp2a -1)
           (gimp-layer-set-mode layer-temp2a 3)
        )
    )

    (gimp-image-add-layer toon-img layer-temp1 -1)
    (gimp-image-add-layer toon-img layer-temp2 -1)
    (gimp-levels-stretch drawable)
    (set! buffer (car (gimp-edit-named-copy drawable "temp-buffer")))
    (gimp-floating-sel-anchor (car (gimp-edit-named-paste  layer-gmic buffer 0)))
    (if (= Prefilter TRUE)
     (if (= filter-type 2)
      (if (defined? 'plug-in-greycstoration)
        (begin
          (plug-in-greycstoration 1 toon-img layer-gmic 60 .70 1 .6 1.1 .8 30 2 0 0 4 10 15 7 1 2)
          (set! isfiltered TRUE)
         )
         (gimp-message "GREYCstoration must be installed to prefilter, no filter will be applied.")
  
      )
     )    
    )         
    (if (= Prefilter TRUE)
     (if (= filter-type 3)
      (if (= isfiltered FALSE)
        (if (defined? 'plug-in-gmic)
          (begin
            (plug-in-gmic 1 toon-img layer-gmic 1 "-gimp_anisotropic_smoothing 60,.7,1,1.1,1,1,30,2,0,1,1,1,1")
            (set! isfiltered TRUE)
          )
          (gimp-message "G'MIC must be installed to prefilter, no filter will be applied.")
        )  
      )
     )
    )
    (if (= Prefilter TRUE)
     (if (= filter-type 0)
      (if (= isfiltered FALSE)
        (if (defined? 'plug-in-sel-gauss)
          (plug-in-sel-gauss 1 toon-img layer-gmic 4 11)
        )
      )
     ) 
    )

    (if (= Prefilter TRUE)
     (if (= filter-type 1)
      (if (= isfiltered FALSE)
        (if (defined? 'plug-in-despeckle)
          (plug-in-despeckle 1 toon-img layer-gmic 7 1 -1 256)
        )
      )
     ) 
    )
    (set! buffer (car (gimp-edit-named-copy (car (gimp-image-get-active-layer toon-img)) "temp-buffer")))

    (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp1 buffer 0)))
    (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp3 buffer 0)))
    (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp4 buffer 0)))
    (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp7 buffer 0)))
    (if (= enable-shadow TRUE)
        (begin
           (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp2a buffer 0)))
           (gimp-desaturate-full layer-temp2a 2)
           (gimp-threshold layer-temp2a shadow-intensity 255)
        )
    )
    (gimp-layer-set-mode layer-temp3 15)
    (gimp-layer-set-mode layer-temp5 15)
    (gimp-layer-set-mode layer-temp4 3)
    (gimp-layer-set-mode layer-temp6 3)
    (gimp-layer-set-mode layer-temp2 16)
    (gimp-desaturate-full layer-temp3 2)
    (set! buffer (car (gimp-edit-named-copy layer-temp1 "temp-buffer")))
    (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp2 buffer 0)))
    
    (plug-in-gauss 1 toon-img layer-temp2 line-size line-size 1)
    (gimp-invert layer-temp2)
    (gimp-image-merge-down toon-img layer-temp2 0)
   
    (set! layer-temp1 (car (gimp-image-get-active-layer toon-img)))
    (gimp-threshold layer-temp1 245 255)
    (gimp-layer-set-mode layer-temp1 3)
    (gimp-image-merge-down toon-img layer-temp3 0)
    (set! layer-temp4 (car (gimp-image-get-active-layer toon-img)))
    (set! buffer (car (gimp-edit-named-copy layer-temp4 "temp-buffer")))
    (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp5 buffer 0)))
    (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-temp6 buffer 0)))
    (gimp-desaturate-full layer-temp5 2)
    (gimp-image-merge-down toon-img layer-temp5 0)
    (set! layer-temp4 (car (gimp-image-get-active-layer toon-img)))
    (gimp-layer-set-mode layer-temp4 3)
    (gimp-image-raise-layer toon-img layer-temp4)
    (gimp-image-merge-down toon-img layer-temp4 0)
    (set! layer-temp6 (car (gimp-image-get-active-layer toon-img)))
    (gimp-layer-set-mode layer-temp6 14)
    (gimp-image-merge-down toon-img layer-temp6 0)
    (set! layer-temp7 (car (gimp-image-get-active-layer toon-img)))
    (gimp-levels layer-temp7 0 0 255 1 0 225)

    (gimp-image-flatten toon-img)

    (gimp-image-convert-indexed toon-img 0 0 cdepth 0 0 "")
    (gimp-image-convert-rgb toon-img)
    (set! layer-temp1 (car (gimp-image-get-active-layer toon-img)))
    (set! buffer (car (gimp-edit-named-copy (car (gimp-image-get-active-drawable toon-img)) "temp-buffer" )))
    (if (= new-layer TRUE)
      (begin
         (gimp-image-add-layer img layer-cartoonize -1)
         (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-cartoonize buffer 0 )))
         (gimp-image-set-active-layer img layer-cartoonize)
      )
      (gimp-floating-sel-anchor (car (gimp-edit-named-paste drawable buffer 0 )))
    )
    (if (= half-tone TRUE)
     (begin
       (gimp-image-add-layer img layer-comic -1)
       (gimp-floating-sel-anchor (car (gimp-edit-named-paste layer-comic buffer 0)))
       (plug-in-unsharp-mask 0 0 layer-comic 4 10 0)
       (plug-in-newsprint 0 0 layer-comic 1 .3 4 1 0 15 0 75 0 0 0 6)
      )
    )
   (gimp-displays-flush)
   (gimp-image-undo-group-end img)
  )
)

(script-fu-register
  "script-fu-CarTOONize"
  _"<Image>/Filters/Artistic/CarTOONize..."
  "Toon a Picture.  Take a standard RGB or Grayscale picture, and cartoonize it.  You can adjust the thickness of the lines, as well as adjust the 'black' shadow intensity.  Now you can adjust the color depth, 1 is the flattest, 5 has the most levels.  Prefilter option requires and uses either the GREYCstoration Filter, the newer G'MIC Filter, Despeckle or the Selective Gaussian Blur.  Set it to FALSE if you don't want noise reduction.  Now have the option to keep it on existing layer (non haltone).  Is Gimp 2.7 ready."
  "Joe1GK <kgioj@yahoo.com>"
  "Joe1GK"
  "2010, December"
  "RGB* GRAY*"
  SF-IMAGE      "Image"	                     0
  SF-DRAWABLE   "Drawable"                   0
  SF-TOGGLE     _"Prefilter (Noise reduction)" TRUE
  SF-OPTION     _"Filter type (Noise Reduction)"   '("Selective Gaussian Blur Smoothing" "Despeckle Smoothing" "GREYCStoration Smoothing" "G'MIC Anistropic Smoothing")
  SF-ADJUSTMENT _"Line Thickness (pixels)"   '(10 2 30 1 10 0 1)
  SF-TOGGLE     _"Black Shadows"             TRUE
  SF-ADJUSTMENT _"Shadow Intensity"          '(35 0 50 1 10 0 1)
  SF-ADJUSTMENT _"Color Levels"              '(4 1 5 1 10 0 1)
  SF-TOGGLE     _"Generate as new layer"     FALSE
  SF-TOGGLE     _"Halftone"	             FALSE
)



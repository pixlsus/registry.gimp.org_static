(define (ColorDepth2CrossEye image EyeSpace depth)(let* ((dmap    (vector-ref (car(cdr (gimp-image-get-layers image))) 0 ))(graylay (vector-ref (car(cdr (gimp-image-get-layers image))) 1 ))(orignal (vector-ref (car(cdr (gimp-image-get-layers image))) 2 ))
(w       (car (gimp-image-width image) ))(h               (car (gimp-image-height image) ))(scale           (/ EyeSpace w ))(image2          (car(gimp-image-new     ( * 2 EyeSpace)  ( * h scale ) 0)))(driftmap        (car(gimp-layer-new image2 ( * 2 EyeSpace)  ( * h scale ) 0 "backlay2" 100 0)))
( front       0)( float       0))

(gimp-image-add-layer image2  driftmap 0)(gimp-drawable-fill     driftmap 2)
(set!  front           (car (gimp-layer-copy driftmap 1)))(gimp-image-add-layer image2  front 0)(gimp-channel-set-name driftmap "driftmap")(gimp-channel-set-name front "front")
(gimp-selection-all image)(gimp-edit-copy   dmap )(set! float    (car (gimp-edit-paste driftmap 1) ))
(gimp-drawable-transform-scale-default  float 0 0 EyeSpace ( * h scale )  1 0)(gimp-floating-sel-anchor float)
(gimp-desaturate-full driftmap 1)   ;LIGHTNESS (0),-LUMINOSITY (1), -AVERAGE (2) }
(gimp-selection-all image)(gimp-edit-copy   orignal )(set! float    (car (gimp-edit-paste front 1) ))(gimp-drawable-transform-scale-default  float 0 0 EyeSpace ( * h scale )  1 0)(gimp-floating-sel-anchor float)
(plug-in-displace 1 image2 front depth 0 1 0 driftmap driftmap 1)(gimp-selection-all image)(gimp-edit-copy   orignal )(set! float    (car (gimp-edit-paste front 1) ))(gimp-drawable-transform-scale-default  float EyeSpace 0 ( * 2 EyeSpace ) ( * h scale )  1 0)(gimp-floating-sel-anchor float)
(gimp-context-set-brush "Circle (05)")(gimp-context-set-foreground '(0 0 0))(gimp-rect-select image2 EyeSpace 0 ( * 2 EyeSpace) ( * h scale ) 0 0 1)(gimp-selection-shrink image2 2)(gimp-edit-stroke front)(gimp-rect-select image2 0 0  EyeSpace ( * h scale ) 0 0 1)(gimp-selection-shrink image2 2)(gimp-edit-stroke front)(gimp-selection-none image2)
(gimp-display-new       image2)

(gimp-image-set-filename image2 "Stereo_CrossEye") 

))(script-fu-register "ColorDepth2CrossEye"                    "ColorDepth2CrossEye"                    "Converts Color Depth Map to Stereo CrossEye"                    "Don Sauer www.idea2IC.com"                    "GNU General Public License v3+"                    "2/24/2012"                    "RGB*"                    SF-IMAGE      "Image" 0                    SF-ADJUSTMENT "Eye_Spacing" '(300 200 600 50 1 1 0)                    SF-ADJUSTMENT "depth"       '(5 0 10 50 1 1 0))(script-fu-menu-register "ColorDepth2CrossEye" "<Image>/Filters/Stereo 3D Stuff")
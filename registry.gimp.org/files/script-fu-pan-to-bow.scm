; Pan to Bow, V1.0
;
; http://flickr.com/photos/theilr, (c) 2009
;
; DESCRIPTION: Takes a wide panoramic image and bends it into a
; rainbow-shaped image in a way that preserves scale and aspect ratio.
;
; ONE MOTIVATION: On image hosting sites like flickr, images are
; displayed according to their largest linear dimension, so large
; aspect-ratio shots end up with very little real estate.  By bending
; the image into a semi (or full) circle, you can give your image a
; lot more area of the screen.  Not only that but since the linear
; dimension is smaller, the image details will actually be larger (by
; a factor of as much as pi=3.14159...) when the image is scaled to
; fit within the fixed linear dimension.
;
; USAGE NOTES:
;
; Although the new image keeps the same scale as the original, the
; created image will have a larger number of pixels (because of all
; the empty pixels).
;
; The attempt at non-distortion applies only to the central horizontal
; band of the image; depending on the initial aspect ratio, you will
; see distortion above and below that centerline.  (The larger the
; initial aspect ratio, the smaller the distortion will be.)
;
; This overwrites the current layer.  So you should copy the current
; layer if you want to keep it around.  But since the new layer will
; be of a different size than the current layer, you may prefer to 
; overwrite it.
;
; The part of the image that is outside the original image will be
; white.
;
; BUGS: 
;
; If the aspect ratio isn't wide enough, then aspect ratio cannot
; be preserved.  (This isn't actually a bug, it's basic geometry.)
; In particular, you want width/height > angle / 114.6.
; For instance, for default 180-degree bow, width/height > 1.57
; The program will still "work" in this case, but scale and aspect
; ratio will be compromised.
;
; Because it uses plug-in-polar-coords, it has, as an intermediate
; step, to resize the image, in some cases by quite a large factor.
; (And pan images tend to be pretty large to begin with.)  So there
; could be problems on computers with limited memory.  It might
; have made more sense to just enhance the plug-in directly.
;
; Disabled use of angle<45, since that leads to huge intermediate
; images!
;
; Might be a nice feature to toggle whether the user wants to copy
; the current layer before altering it.
;
; Currently, this tries to maintain scale along the middle of the 
; image; one can imagine situations when it would be preferable to
; keep the scale accurate at the top or the bottom of the image.
;
; I would prefer that the background be transparent instead of white;
; and if I create an image from scratch, and save it as an .xcf file,
; I can get the transparent background.  But when I start with a jpeg
; file, I get a white background.
;
; TESTING:
; This script was tested with GIMP 2.6.7
;
; COPYLEFT:
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License Version 3 as 
; published by the Free Software Foundation.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
; GNU General Public License at  http://www.gnu.org/licenses for
; more details.
;
; MENU LOCATION:
; The script is located in menu "<Image> / Filters / theilr / Pan to Bow"
; But you will probably want to change that.
;
; Version 1.0 (Nov 2009)
; =============================================================================


(define (script-fu-pan-to-bow inImage inLayer inAngleDegrees)

  (let* ((initWidth  (car (gimp-drawable-width  inLayer)))
	 (initHeight (car (gimp-drawable-height inLayer)))
         (degreesPerRadian 57.295779513)
	 (angleRadians (/ inAngleDegrees degreesPerRadian))
	 (bowRadius (/ initWidth angleRadians))
	 (padBelow (- bowRadius (/ initHeight 2)))
	 (padSides (* initWidth (/ (- (/ 360 inAngleDegrees) 1) 2)))
	 (newWidth  (+ initWidth  (* padSides 2)))
	 (newHeight (+ initHeight (max 0 padBelow)))
	 )

    (gimp-image-undo-group-start inImage)

    (gimp-image-resize inImage newWidth newHeight
		       padSides 0)
    (gimp-layer-resize inLayer newWidth newHeight
		       padSides 0)

    (gimp-layer-scale-full inLayer newWidth (* newHeight 2)
			   FALSE INTERPOLATION-NONE) 
    (gimp-image-resize inImage newWidth (* newHeight 2) 0 0)
    
    (plug-in-polar-coords RUN-NONINTERACTIVE
			  inImage inLayer
			  100 180 0 0 1)

    (plug-in-autocrop-layer RUN-NONINTERACTIVE
			    inImage inLayer)
    (gimp-image-resize-to-layers inImage)

    )

  (gimp-image-undo-group-end inImage)
  (gimp-displays-flush)
  )


(script-fu-register
  "script-fu-pan-to-bow"
  "<Image>/Filters/theilr/_Pan to Bow"
  "Convert a wide panoramic image into a rainbow-shaped \
   image in a way that preserves scale and aspect ratio"
  "www.flickr.com/photos/theilr"
  "(c) theilr"
  "November 2009"
  "RGB*"
  SF-IMAGE      "Image"   0
  SF-DRAWABLE   "Drawable" 0
  SF-ADJUSTMENT "Angle of Circle" '(180 45 360 1 10 0 SF-SLIDER)
)



		   

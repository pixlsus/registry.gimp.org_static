; Cheap HDR, V1.0
;
; AUTHOR: theilr (http://flickr.com/photos/theilr), (c) 2009
;
; This script was tested with GIMP 2.6.7
;
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
; DESCRIPTION:
; Provides a kind of poor-man's HDR (high dynamic range).  It doesn't actually
; increase the dynamic range; in fact, it _decreases_ it, overall, but in a 
; way that maintains local contrast.  But this gives you some headroom to 
; increase the contrast of the image overall, thus enhancing the local 
; contrast. 
;
; I've since learned that this also goes by the name "Contrast
; Masking"; I learned that from http://www.FarcryDesign.com/GIMP which
; includes a script-fu for essentially doing the same thing (actually
; her script is fancier).  I am grateful to her for making her script
; GPL, and any similarities of my script to hers are probably not
; coincidental.
;
; FarcryDesign.com notes that it is also described at Luminous Landscape
; http://www.luminous-landscape.com/tutorials/understanding-series/u-contrast-masking.shtml
; Interestingly (to me), they both recommend using a very small radius; my 
; experience is that this flattens the image too much, and I suggest 150 pixels
; as a minimum, and have used up to 1000 pixels.
;
; By the way, I think this is similar in spirit (and maybe even in
; some detail) to the concept of "Local Contrast Enhancement"
; http://www.luminous-landscape.com/tutorials/contrast-enhancement.shtml
;
; The script is located in menu "<Image> / Filters / theilr / Cheap HDR"
;
; USAGE NOTES:
;
; This produces a new layer which tones down the highlights and
; brightens the shadows.  If you like the result, you can merge that
; layer back on the original, but before you do that, you might like
; to: alter the opacity, change levels of the new layer, re-blur the
; new layer at a larger radius, or you can duplicate the new layer to
; make the effect more extreme (at which point it will look like Bad
; HDR).  Another thing you can do is run this twice, once at a
; relatively small radius and once at a much larger radius; then
; adjust opacities to taste.  Since the overall effect is
; contrast-reducing, you will probably want to bump up the contrast
; with Curves, after you've merged the layers
;
; SCRIPT SUMMARY:
; Copy layer, set to overlay mode, desaturate, invert, and blur (a lot)
;
; 
; Version 1.0 (Oct 2009) -- my first actual script-fu !!
; =============================================================================



(define (script-fu-cheap-hdr inImage inLayer inRadius inDesatMode)
  (gimp-image-undo-group-start inImage) 

  (let* 
      ( ;define local variables
       (newLayer (car (gimp-layer-copy inLayer TRUE)))
       )
    
    (gimp-image-add-layer inImage newLayer -1)
    (gimp-drawable-set-name newLayer "Cheap HDR")
    (gimp-layer-set-opacity newLayer 100)
    (gimp-layer-set-mode newLayer OVERLAY-MODE)

    (gimp-desaturate-full newLayer inDesatMode)
    (gimp-invert newLayer)
    (plug-in-gauss RUN-NONINTERACTIVE
		   inImage newLayer inRadius inRadius 0)

    )
  (gimp-image-undo-group-end inImage)
  (gimp-displays-flush)

  )

(script-fu-register "script-fu-cheap-hdr"
		    "<Image>/Filters/_theilr/Cheap _HDR"
		    "Lightens shadow, darkens highlight, but keeps contrast"
		    "theilr"
		    "(c) theilr"
		    "24 Oct 2009"
		    "RGB*"
		    SF-IMAGE "Image"  0
		    SF-DRAWABLE "Drawable" 0
		    ;SF-VALUE  "Radius" "150"
		    SF-ADJUSTMENT "Radius" '(150 0 1000 1 50 0 SF-SLIDER)
		    SF-OPTION "Desaturation" '("Lightness" "Luminosity" "Average")
		    )
;;(script-fu-menu-register "script-fu-cheap-hdr" "<Image>/Filters/_theilr")


  
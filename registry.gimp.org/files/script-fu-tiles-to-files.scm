; Tiles to Files, V1.0
;
; http://flickr.com/photos/theilr, (c) 2009
;
; DESCRIPTION: Partitions an image into tiles (with, optionally,
; either gaps or overlaps between them) and writes each tile as a
; separate jpeg file.  Note, this is almost identical to the
; Image->Transform->Guillotine function
;
; ONE MOTIVATION: At winkflash today, you can buy 4x6 inch prints for
; six cents each; so you could buy 36 of them for $2.16, and tile an
; area of size 24x36 inches.  But to buy a single 24x36 inch print
; will cost about $20.  Of course, the two aren't exactly equivalent,
; but what this script provides is a quick way to produce the separate
; jpeg files.
;
; USAGE NOTES: 
; As well as generating the files, it (by default) also makes
; an auxiliary image in a new window where each tile is a separate
; layer.  You can have that image deleted before you ever see it, but
; it might be useful if you want to check that the tiles are
; reasonable, or if you want to do some further processing to the
; individual tiles.  Otherwise, you'll probably want to close that
; image, when you are sure that the jpegs are what you wanted.
;
; If you give a blank filename, then files are not generated. 
;
; The tiles will always be the same size (this is not a bug) even if that
; means that a few pixels are dropped off the right or bottom margins.
; (By contrast, depending on how you do it, setting up guides and using 
; Guillotine, will get all the pixels, but some images will be slightly
; different sizes.)
;  
; BUGS: 
;
; This is almost identical to a setting up of regular guides, followed
; by the Image->Transform->Guillotine function.  eg, see
;
; http://registry.gimp.org/node/12003
; http://registry.gimp.org/node/20826
;
; The individual jpeg files lose the EXIF info from the original file.
; (I don't really understand all the parameters to file-jpeg-save; and
; I also remark that the Guillotine approach doesn't have this problem.)
;
; That the output can only be jpeg is probably a bug; the user might
; prefer other file formats, or (at the least) to be able to adjust
; the jpeg quality parameter.
;
; If you give a blank filename AND you check the toggle to delete the
; image of tiles, then this script will do a lot of work and not have 
; anything to show for it!
; 
; It might be better to do this in two steps. First generate the image
; of tiles.  Then (if you are happy with them) generate the files.  A
; separate routine that did layers-to-files might be useful in its own
; right.  
;
; Only works on RGB images; should probably extend to GRAYSCALE, etc.
;
; I've limited this to 100x100 tiles.  It's an arbitrary limit, but
; I'd think it should be plenty.  
;
; The number of overlap/gao pixels is limited to 200, again arbitrary
; and capricious.  Also, the number is the same for horizontal and
; vertical.  I can see where a percentage might be preferable.
;
; Negative overlap may not be the most intuitive way to make gaps.
;
; It might be cool to have an option to add some kind of border to
; all of the images.
;
; Height and Horizontal both start with the letter H; that's not my
; fault, but it can lead to ambiguities with abbreviations.
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
; The script is located in menu "<Image> / Filters / theilr / Tiles to Files"
; But you will probably want to change that.
;
; Version 1.0 (Nov 2009)
; =============================================================================

(define (script-fu-tiles-to-files inImage inLayer inHTiles inVTiles 
				  inOverlapPixels inFilePrefix inDeleteImage)

  (gimp-image-undo-group-start inImage)

  (let* (
        (fullWidth  (car (gimp-image-width  inImage)))
      	(fullHeight (car (gimp-image-height inImage)))
        (tileWidth (+ inOverlapPixels 
		      (/ (- fullWidth inOverlapPixels) inHTiles)))
        (tileHeight (+ inOverlapPixels 
		       (/ (- fullHeight inOverlapPixels) inVTiles)))
	(newImage (car (gimp-image-new tileWidth tileHeight RGB)))
        (hcnt 0)
        (vcnt 0)
	(tmpLayer)
	(selLayer)
	(outfname)
        )
    
    (set! vcnt 0)
    (while (< vcnt inVTiles)
	   (set! hcnt 0)
	   (while (< hcnt inHTiles) 
		  (set! outfname (string-append 
				  inFilePrefix
				  "-"
				  (number->string vcnt)
				  "-"
				  (number->string hcnt)
				  ".jpg"))
		  (gimp-rect-select inImage 
				    (* (- tileWidth  inOverlapPixels) hcnt)
				    (* (- tileHeight inOverlapPixels) vcnt)
				    tileWidth
				    tileHeight
				    CHANNEL-OP-ADD FALSE 0)

		  (gimp-edit-copy-visible inImage)

		  (gimp-selection-none inImage)

		  (set! tmpLayer 
			(car (gimp-layer-new newImage tileWidth tileHeight 
					     RGB-IMAGE outfname 100 
					     NORMAL-MODE)))

		  (gimp-image-add-layer newImage tmpLayer -1)

		  (set! selLayer
			(car (gimp-edit-paste tmpLayer FALSE)))

		  (gimp-floating-sel-anchor selLayer)

		  ;; if filename is empty, don't write jpeg files
		  (unless (equal? "x" (string-append "x" inFilePrefix))
		      (file-jpeg-save RUN-NONINTERACTIVE
				      newImage
				      tmpLayer
				      outfname
				      outfname
				      1 0 1 1 "tile" 0 1 0 0 )
		      )		  
		  
		  (set! hcnt (+ hcnt 1))
		  )
	   (set! vcnt (+ vcnt 1))
	   )

    ;; either delete the new image, or display it
    (if (= inDeleteImage TRUE)
	(gimp-image-delete newImage)
	(gimp-display-new newImage))

    (gimp-image-undo-group-end inImage)
    (gimp-displays-flush)
    )
  )

(script-fu-register
  "script-fu-tiles-to-files"
  "<Image>/Filters/theilr/_Tiles to Files"
  "Partition image into a grid of tiles,\n\
    and save each tile to a separate jpeg file"
  "www.flickr.com/photos/theilr"
  "(c) theilr"
  "November 2009"
  "RGB*"
  SF-IMAGE      "Image"   0
  SF-DRAWABLE   "Drawable" 0
  SF-ADJUSTMENT	"Number of Horizontal tiles"	'(3 1 100 1 10 0 SF-SPINNER)
  SF-ADJUSTMENT	"Number of Vertical tiles"	'(3 1 100 1 10 0 SF-SPINNER)
  SF-ADJUSTMENT "Gap(neg)/Overlap(pos) pixels"	'(0 -200 200 1 10 0 SF-SLIDER)
  SF-STRING     "Output filename prefix" 	""
  SF-TOGGLE     "Delete auxiliary image" 	FALSE
)

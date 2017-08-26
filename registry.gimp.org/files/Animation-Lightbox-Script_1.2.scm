;;
;;;Animation-Lightbox-Script.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;ben.donoghue@hotmail.co.uk
;;;25.08.11
;;;Version 1.1
;
;Project Lightbox:
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;
;INFO
;
;Compatible with Gimp 2.6 for both Linux and Windows
;
;ALL SETTINGS ARE BASED ON PAL (25fps)
;
;
;Only includes exporting for .tif files as they are lossless and most easily used.
;If you want to export frames as a different file download sg-save-all-layers.scm
;plugin (Saul Goode 2008) from the GIMP plugin website.
;
;Exporting to Video will require the GIMP Animation Package (GAP) which can be downloaded from the GIMP plugin resgistry
;http://registry.gimp.org/node/3700
;
;Highly recommend assigning shortcuts to speed up workflow.


;;;INSTALL

;Place all scripts in ".gimp-2.6/scripts"
;
;This should be in your main user folder for both LINUX and WINDOWS.
;
; Restart GIMP or If already running go to Filters - Script-Fu - Refresh Scripts
; on the menu bar.


;;;Known Issues
;
;
; - Cycle-up / Cycle-down can sometimes crash script-fu if pressed together. Simply Save
;   files and restart GIMP.
; - Failed script errors will occur if layers are not within naming convention. 
;   Generally can be avoided by using plugins to rename.

;Thanks to Adam Hodgson http://www.lagthemodeller.com/


;;;SCRIPTS
;;;-------
;
;
;;;Anim-canvas.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;23.05.11
;;;Version 1.0
;
;Project Lightbox
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;
;INFO
;
;Creates a new canvas for animating with a background layer. All based on 16:9 widescreen
;at 25 frames a second.


;SCRIPT
;......


; CREATE 1080P CANVAS

(define (lbox-hdlarge-new-image)
 (let* 	(

		(image (car (gimp-image-new 1920 1080 RGB)))
		(gimp-image-undo-disable image)
		(bglayer (car (gimp-layer-new image 1920 1080 RGBA-IMAGE "Background-01" 100 NORMAL-MODE)))

	)

	(gimp-drawable-fill bglayer WHITE-FILL)
	(gimp-image-add-layer image bglayer 0)
	(gimp-display-new image)

	(gimp-image-undo-enable image)
	(gimp-displays-flush)

	image)

)

;CREATE 720P CANVAS

(define (lbox-hdsmall-new-image)
 (let*	(

		(image (car (gimp-image-new 1280 720 RGB)))
		(gimp-image-undo-disable image)
		(bglayer (car (gimp-layer-new image 1280 720 RGBA-IMAGE "Background-01" 100 NORMAL-MODE)))

	)

	(gimp-drawable-fill bglayer WHITE-FILL)
	(gimp-image-add-layer image bglayer 0)
	(gimp-display-new image)

	(gimp-image-undo-enable image)
	(gimp-displays-flush)

	image)



)

;CREATE PAL 16:9 CANVAS

(define (lbox-sd-new-image)
 (let* 	(

		(image (car (gimp-image-new 720 576 RGB)))
		(gimp-image-undo-disable image)
		(bglayer (car (gimp-layer-new image 720 576 RGBA-IMAGE "Background-01" 100 NORMAL-MODE)))

	)

	(gimp-drawable-fill bglayer WHITE-FILL)
	(gimp-image-add-layer image bglayer 0)
	(gimp-display-new image)

	(gimp-image-undo-enable image)
	(gimp-displays-flush)

	image)



)

;Register scripts

(script-fu-register
	"lbox-hdlarge-new-image"
	"Create 1080p Canvas"
	"Creates a new HD Canvas"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	""
)

(script-fu-register
	"lbox-hdsmall-new-image"
	"Create 720p Canvas"
	"Creates a new HD Canvas"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	""
)

(script-fu-register
	"lbox-sd-new-image"
	"Create SD Canvas"
	"Creates a new PAL SD Canvas"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	""
)

(script-fu-menu-register "lbox-hdlarge-new-image"
                         "<Image>/Animate/New Canvas/")

(script-fu-menu-register "lbox-hdsmall-new-image"
                         "<Image>/Animate/New Canvas/")

(script-fu-menu-register "lbox-sd-new-image"
                         "<Image>/Animate/New Canvas/")


;
;;;Anim-add-paper.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;16.03.11
;;;Version 0.9
;
;Project Lightbox:
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;
;INFO
;
;ADD BG LAYER
;Creates a new layer named Background-xx and places it at bottom of stack (above any
;other Background-xxx layers there).
;Other Anim scripts use "Background" as label to differenciate from animation frames.
;
;ADD FRAME
;Creates a new layer named Frame-xxx(ms) and places it above active layer. The
;timing of the frame is included in the brackets, and allows a timed preview with the
;PLAYBACK script included with GIMP. All timing is based on digital playback at 25
;frames a second



;SCRIPT
;......


;;;
;;;Add BG
;;;


;NUMBER OF BG FRAMES
;
;Parameters : Image ID
;Returns    : Number of Layers marked "B"

(define (get-bg-num img)
  (let*(
	(numoflayers(car(gimp-image-get-layers img)))
	(framelist(cadr(gimp-image-get-layers img)))
	(frameno 0)
	(currentframe 0)
	(framename "")
	(check "")
	(Framecount 1)
	)

(while (> numoflayers 0)

	(set! currentframe(aref framelist frameno))
	(set! framename(car(gimp-drawable-get-name currentframe)))

	(set! check(string-ref framename 0))
	(set! check(string check))
	(if(string=? check "B")
		(set! Framecount(+ Framecount 1))
	)
	(set! numoflayers (- numoflayers 1))
	(set! frameno (+ frameno 1))
)


	Framecount)
)

;BG RENUMBER
;
;Parameters : BG Number
;Returns    : Double Digit BG Number

(define (renumber-bg input)

(let*(

	(input(number->string input))
	(output "")
	(numbera "")
	(numberb "")
	(input(string-append "00" input))
	(numbera(string-length input))
)
	(set! numberb (- numbera 2))
	(set! output(substring input numberb))
	output)
)


;NEW BACKGROUND LAYER
;
; Parameters    : Image Number

(define (lbox-new-bg img)

(gimp-image-undo-group-start img)

(let*(



;get image dimensions

		(canvasWidth(car(gimp-image-width img)))
		(canvasHeight(car(gimp-image-height img)))

;Get BG frame number

		(frameno(get-bg-num img))
		(framenoa(renumber-bg frameno))

		(filename(string-append "Background-" framenoa))
;create frame

		(animframe(car(gimp-layer-new img canvasWidth canvasHeight RGBA-IMAGE filename 100 NORMAL-MODE)))
	)


		(gimp-drawable-fill animframe WHITE-FILL)
		(gimp-image-add-layer img animframe 0)

;move frame

		(gimp-image-lower-layer-to-bottom img animframe)


		(while (> frameno 1)

			(gimp-image-raise-layer img animframe)
			(set! frameno (- frameno 1))
		)
	animframe)

(gimp-image-undo-group-end img)
(gimp-displays-flush)

)


;;;
;;;ADD FRAMES
;;;


;NUMBER OF ANIM FRAMES
;
;Parameters : Image ID
;Returns    : Number of layers marked "F"

(define (get-frame-num img)
  (let*(
	(numoflayers(car(gimp-image-get-layers img)))
	(framelist(cadr(gimp-image-get-layers img)))
	(frameno 0)
	(currentframe 0)
	(framename "")
	(check "")
	(Framecount 1)
	)

(while (> numoflayers 0)

	(set! currentframe(aref framelist frameno))
	(set! framename(car(gimp-drawable-get-name currentframe)))

	(set! check(string-ref framename 0))
	(set! check(string check))
	(if(string=? check "F")
		(set! Framecount(+ Framecount 1))
	)
	(set! numoflayers (- numoflayers 1))
	(set! frameno (+ frameno 1))
)


	Framecount)
)

;FRAME RENUMBER
;
;Parameters : Frame Number
;Returns    : Triple Digit Frame Number

(define (renumber-frame input)

(let*(

	(input(number->string input))
	(output "")
	(numbera "")
	(numberb "")
	(input(string-append "000" input))
	(numbera(string-length input))
)
	(set! numberb (- numbera 3))
	(set! output(substring input numberb))
	output)
)

;MOVE FRAME TO ABOVE ACTIVE LAYER
;
;Parameters : Image ID
;             New Frame ID
;             Current Active Layer ID

(define (move-frame img newframe active-layer)

(let*(

	(num-layers(car(gimp-image-get-layers img)))
	(layer-array(cadr(gimp-image-get-layers img)))
	(below-layer 0)
	(i 1)
	(break 0)

	)

;check if layer below is active layer otherwise lower layer

	(while (= break 0)

		(set! below-layer (aref layer-array i))

		(if (= below-layer active-layer)

			(set! break 1)
			(gimp-image-lower-layer img newframe)
		)

		(set! i (+ i 1))

	)
)
)

;NEW FRAME
;
;Parameters : Image ID
;             Timing
;Returns    : New Frame ID

(define (animate-new-frame img inTime)

(gimp-image-undo-group-start img)

(let*(


;get image dimensions

		(canvasWidth(car(gimp-image-width img)))
		(canvasHeight(car(gimp-image-height img)))
;Get animation frame number

		(frameno(get-frame-num img))
		(frameno(renumber-frame frameno))
		(filename(string-append "Frame-" frameno inTime))
;get active layer

		(active-layer(car(gimp-image-get-active-layer img)))
;create frame

		(animframe(car(gimp-layer-new img canvasWidth canvasHeight RGBA-IMAGE filename 50 NORMAL-MODE)))

	)
		(gimp-drawable-fill animframe WHITE-FILL)
		(gimp-image-add-layer img animframe 0)

;position frame above active layer

		(move-frame img animframe active-layer)




  		animframe)

(gimp-image-undo-group-end img)
(gimp-displays-flush)


)

;ADD DOUBLE EXPOSURE FRAME

(define (lbox-new-double-frame img)
 (let*(
	(timing "(80ms)")
	(animframe 0)
	)

	(set! animframe (animate-new-frame img timing))
 animframe)
)

;ADD SINGLE EXPOSURE FRAME

(define (lbox-new-single-frame img)
 (let*(
	(timing "(40ms)")
	(animframe 0)
	)

	(set! animframe (animate-new-frame img timing))

 animframe)
)




;Register scripts

(script-fu-register
	"lbox-new-bg"
	"Add BG Layer"
	"Creates a new background layer below animation frames"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	 SF-IMAGE    "Image"	0
)

(script-fu-menu-register "lbox-new-bg"
                         "<Image>/Animate/New Frame")

(script-fu-register
	"lbox-new-double-frame"
	"Add Frame on 2s"
	"Creates a new animation frame with double exposure timing (25fr/s)"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	 SF-IMAGE    "Image"	0
)

(script-fu-menu-register "lbox-new-double-frame"
                         "<Image>/Animate/New Frame/")

(script-fu-register
	"lbox-new-single-frame"
	"Add Frame on 1s"
	"Creates a new animation frame with single exposure timing (25fr/s)"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	 SF-IMAGE    "Image"	0
)

(script-fu-menu-register "lbox-new-single-frame"
                         "<Image>/Animate/New Frame/")

;
;;;Anim-addcolour.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;23.05.11
;;;Version 1.0
;
;Project Lightbox:
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;
;INFO
;
;Adds Colour layers above every layer marked "Frame".
;These can then be composited together at a later stage for either playback or export.



;SCRIPT
;......


;ADD COLOUR
;create layer above every "Frame" named Colour with multiply

(define (lbox-add-colour-layers img)

(gimp-image-undo-group-start img)

 (let*	(

	(num-layers (car (gimp-image-get-layers img)))
	(layer-array (cadr (gimp-image-get-layers img)))

	(i 0)

	(layerid 0)

	(layername "")
	(frcheck "")

	(lname-length 0)

	(canvasWidth(car(gimp-image-width img)))
	(canvasHeight(car(gimp-image-height img)))

	(colourlayer 0)

	(layerpos 0)


	(ci 0)
	(clayerid 0)
	(clayername "")

	)



(while (< i num-layers)


	(set! layerid (aref layer-array i))
	(set! layername(car(gimp-drawable-get-name layerid)))
	(set! frcheck(string-ref layername 0))
	(set! frcheck(string frcheck))

	(if (string=? frcheck "F")

		(
		begin
		(set! layername (substring layername 5 9))
		(set! layername (string-append "Colour" layername))

;check layer above see if match

		(set! clayerid (aref layer-array ci))
		(set! clayername(car(gimp-drawable-get-name clayerid)))
		(set! clayername(substring clayername 0 6))

		(if (not(string=? clayername "Colour"))

			(
			begin
			(set! colourlayer (car(gimp-layer-new img canvasWidth canvasHeight RGBA-IMAGE layername 100 MULTIPLY-MODE)))
			;(gimp-drawable-fill colourlayer WHITE-FILL)
			(gimp-image-add-layer img colourlayer layerpos)
			(set! layerpos (+ layerpos 1))
			)
		)
		)
	)
	(set! layerpos (+ layerpos 1))
	(set! ci i)
	(set! i (+ i 1))
)

)

(gimp-image-undo-group-end img)
(gimp-displays-flush)

)

(script-fu-register
	"lbox-add-colour-layers"
	"Add Colour Layers"
	"Creates Colour layers above every frame. These can be composited together at a later stage for both playback and exporting."
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	 SF-IMAGE    "Image"	0
)

(script-fu-menu-register "lbox-add-colour-layers"
                         "<Image>/Animate/New Frame")
;
;;;Anim-scroll.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;23.05.11
;;;Version 1.0
;
;Project Lightbox:
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;
;Based on the script
;;; cycle-layer-visibility.scm -*-scheme-*-
;;; Author: Edward Hutchins & Randall Sawyer
;;; Version 0.2 (2009)
;
;Changes
;I've changed the script slightly so it will only cycle through layers named "Frame-....."
;and keep all "Background=..." layers at the current visibility. This is so it works the
;other ANIM lightbox scripts.
;
;INFO
;
;Use with keyboard shortcuts to cycle through frames individually
;
;NOTE: will crash script-fu if you press both shortcuts at once. Simply save your file and restart GIMP. As there is no need to cycle up and down at the same time i don't think it is worth the time to fix.


;SCRIPT
;......

; TURN OFF VISIBILTY

(define (visibility-off img num-layers layer-array )
    (let*
        (
            (turn-off-layer 0)
            (i 0)
	    (layername "")
	    (BGcheck "")
        )


;set all layers to invisible


(while (equal?(>= i 0)(< i num-layers))


	(set! turn-off-layer (aref layer-array i))

;check if a background

	(set! layername(car(gimp-drawable-get-name turn-off-layer)))
	(set! BGcheck(string-ref layername 0))
	(set! BGcheck(string BGcheck))


	(if (not(string=? BGcheck "B"))

	(gimp-drawable-set-visible turn-off-layer 0)

	)

	(set! i (+ i 1))
   )
	)

)

;layertag 0 = just frame 1 = both on frame 2 = both on colour 3 = no frame

(define (layer-tag num-layers layer-array ltlayer-pos)

(let*(

	(currentid 0)
	(currentname "")
	(layertag 0)
	(vischeck 0)

)

(set! currentid (aref layer-array ltlayer-pos))
(set! currentname (car(gimp-drawable-get-name currentid)))
(set! currentname (substring currentname 0 6))

(if (string=? currentname "Frame-")

	(begin
	(set! layertag 0)
	(if (= ltlayer-pos 0) (set! ltlayer-pos 1))
	(set! currentid (aref layer-array (- ltlayer-pos 1)))
	(set! currentname (car(gimp-drawable-get-name currentid)))
	(set! currentname (substring currentname 0 6))

	(if (string=? currentname "Colour")

		(begin
		(set! vischeck (car(gimp-drawable-get-visible currentid)))
		(if (= vischeck 1)(set! layertag 1))
		(set! currentname "null")
		)
	)
	)
)

(if (string=? currentname "Colour")

	(begin
        (set! layertag 3)
	(if (= ltlayer-pos (- num-layers 1))(set! ltlayer-pos (- ltlayer-pos 1)))
	(set! currentid (aref layer-array (+ ltlayer-pos 1)))
	(set! currentname (car(gimp-drawable-get-name currentid)))
	(set! currentname (substring currentname 0 6))

	(if (string=? currentname "Frame-")

		(begin
		(set! vischeck (car(gimp-drawable-get-visible currentid)))
		(if (= vischeck 1)(set! layertag 2))
		(set! currentname "null")
		)
	)
	)
)

layertag)
)


;CYCLE-LAYER

(define (cycle-layer-frame img direction)

(gimp-image-undo-group-start img)

(let*
  (
  	(active-layer (car (gimp-image-get-active-layer img)))
        (layer-pos (car (gimp-image-get-layer-position img active-layer)))

	(new-layer-pos layer-pos)

	(new-layer-id 0)
	(num-layers (car (gimp-image-get-layers img)))
	(layer-array (cadr (gimp-image-get-layers img)))
	(i (- num-layers 1))
	(BGcheck "")
	(layername "")
	(BGnum 0)
	(BGskip 1)
        (DWNskip 0)

	(Ccheck-id 0)

	(layertag 0)
	(Colour-pos 0)

   )

;Get layer tag

	(set! layertag (layer-tag num-layers layer-array layer-pos))
        (if (= layertag 2)
            (if (= direction 0)
                  (set! DWNskip 1)
                )
            )

;Turn visibility off on all frames

	(visibility-off img num-layers layer-array)

;Search for next frame

(while (not(= BGskip 0))

       (if (= direction 0)

	(set! new-layer-pos (+ new-layer-pos 1))
	(set! new-layer-pos (- new-layer-pos 1))
)

	(if (> new-layer-pos i) (set! new-layer-pos 0))
	(if (< new-layer-pos 0) (set! new-layer-pos i))

	(set! new-layer-id(aref layer-array new-layer-pos))

	(set! layername(car(gimp-drawable-get-name new-layer-id)))
	(set! BGcheck(string-ref layername 0))
	(set! BGcheck(string BGcheck))

;If moving down with colour skip one frame

        (if (= DWNskip 1)
        (if (string=? BGcheck "F")
               (begin
            (set! BGcheck "Skip")
            (set! DWNskip 0)
            )
         )
)

;Check if frame
		(if (string=? BGcheck "F")
		(gimp-drawable-set-visible new-layer-id 1)
		)

;If frame break

(if (string=? BGcheck "F") (set! BGskip 0))

)

;Get Colour ID (ignores if no colour)

(set! Colour-pos (- (car(gimp-image-get-layer-position img new-layer-id)) 1))
(if (< Colour-pos 0)(set! Colour-pos 0))
(set! Ccheck-id (aref layer-array Colour-pos))

;Set correct visibility

(if (= layertag 1)
	(begin
	(gimp-drawable-set-visible Ccheck-id 1)
	(gimp-image-set-active-layer img new-layer-id)
	)
)

(if (= layertag 2)
	(begin
	(gimp-drawable-set-visible Ccheck-id 1)
	(gimp-image-set-active-layer img Ccheck-id)
	)
)

(if (= layertag 0)(gimp-image-set-active-layer img new-layer-id))
(if (= layertag 3)(gimp-image-set-active-layer img new-layer-id))

)

(gimp-image-undo-group-end img)
(gimp-displays-flush)

)

;MAIN Cycle up

(define (lbox-layer-up img)
 (let*((direction 1))(cycle-layer-frame img direction)))

;MAIN Cycle down

(define (lbox-layer-down img)
 (let*((direction 0))(cycle-layer-frame img direction)))


;Register Scripts

(script-fu-register
	"lbox-layer-up"
	"Cycle Up Frame"
	"Cycles the layer visibility to Frame above. (Dependant on layer names)"
	"Benjamin Donoghue"
	"2011"
	"March 2011"
	"RGB* RGBA*"
	 SF-IMAGE    "Image"	0
)

(script-fu-menu-register "lbox-layer-up"
                         "<Image>/Animate/Tools/Cycle Frames")

(script-fu-register
	"lbox-layer-down"
	"Cycle Down Frame"
	"Cycles the layer visibility to Frame below. (Dependant on layer names)"
	"Benjamin Donoghue"
	"2011"
	"March 2011"
	"RGB* RGBA*"
	 SF-IMAGE    "Image"	0
)

(script-fu-menu-register "lbox-layer-down"
                         "<Image>/Animate/Tools/Cycle Frames")


;
;;;Anim-onionskin.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;23.05.11
;;;Version 1.0
;
;Project Lightbox:
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;
;INFO
;
;Turns on the visiblity of layers to create onion-skinning effect. This can be done up, down or from the centre, Cycling 1, 3 and 5.
;Please note: Frames by default have 50% opacity. Changing the opacity will change the onion skinng effect.

;VISIBILITY OFF
;turns visibility off on all frames except those marked "B"

;SCRIPT
;......


;VIS OFF
;turns visibility off on all frames except those marked "B"

; TURN OFF VISIBILTY

(define (onion-visibility-off img)
    (let*
        (
            (num-layers(car(gimp-image-get-layers img)))
	    (layer-array(cadr(gimp-image-get-layers img)))
            (turn-off-layer 0)
            (i 0)
	    (layername "")
	    (BGcheck "")
        )


;set all layers to invisible


(while (equal?(>= i 0)(< i num-layers))


	(set! turn-off-layer (aref layer-array i))

;check if a background

	(set! layername(car(gimp-drawable-get-name turn-off-layer)))
	(set! BGcheck(string-ref layername 0))
	(set! BGcheck(string BGcheck))


	(if (not(string=? BGcheck "B"))

	(gimp-drawable-set-visible turn-off-layer 0)

	)

	(set! i (+ i 1))
   )
	)

)

(define (number-of-visframes img)


(let*(
	(numlayers(car(gimp-image-get-layers img)))
	(layer-array(cadr(gimp-image-get-layers img)))

	(FRAMEcount 0)

	(checklayer 0)
	(checkvis)

	(layername "")
	(FCheck "")
	(i 0)
      )

(set! i (- numlayers 1))

(while (>= i 0)

	(set! checklayer (aref layer-array i))

;check if frame is visible

	(set! checkvis (car(gimp-drawable-get-visible checklayer)))

;check if frame

	(set! layername(car(gimp-drawable-get-name checklayer)))
	(set! FCheck(string-ref layername 0))
	(set! FCheck(string FCheck))

	(if (string=? FCheck "F")

		(if (= checkvis 1)
			(set! FRAMEcount(+ FRAMEcount 1))
		)
	)

	(set! i(- i 1))
)
FRAMEcount)
)

(define(next-frame-down img layerpos)

(let*(

	(layername "")
	(check "")
	(layer-array(cadr(gimp-image-get-layers img)))
	(numlayers(car(gimp-image-get-layers img)))
	(newlayerid)
	(newlayerpos (+ layerpos 1))
	(break 0)


)

(if (> newlayerpos (- numlayers 1))(set! break 1))

(while (= break 0)

	(set! newlayerid (aref layer-array newlayerpos))

	(set! layername(car(gimp-drawable-get-name newlayerid)))
	(set! check(string-ref layername 0))
	(set! check(string check))

	(if (string=? check "F")(set! break 1))

	(set! newlayerpos(+ newlayerpos 1))
	(if (> newlayerpos (- numlayers 1))(set! break 1))
)

(if (not (string=? check "F"))

	(set! newlayerid
		(aref layer-array layerpos))
)

newlayerid)
)

(define(next-frame-up img currentlayer)

(let*(

	(layername "")
	(check "")
	(layer-array(cadr(gimp-image-get-layers img)))
	(numlayers(car(gimp-image-get-layers img)))
	(newlayerid)
	(newlayerpos (- currentlayer 1))
	(break 0)


)

(if (< newlayerpos 0) (set! break 1))

(while (= break 0)

	(set! newlayerid (aref layer-array newlayerpos))

	(set! layername(car(gimp-drawable-get-name newlayerid)))
	(set! check(string-ref layername 0))
	(set! check(string check))

	(if (string=? check "F")(set! break 1))

	(set! newlayerpos(- newlayerpos 1))
	(if (< newlayerpos 0)(set! break 1))
)

(if (not (string=? check "F"))

	(set! newlayerid
		(aref layer-array currentlayer))
)

newlayerid)
)

(define (lbox-onionskin-down img actlayer)

  (let*(
	(layerpos(car(gimp-image-get-layer-position img actlayer)))
	(frames(number-of-visframes img))
        ;(frames 1)
	(visible 0)
	(newlayerid 0)
	(nlayerpos 0)

	)

(gimp-image-undo-group-start img)

;If one frame check active layer visibility

;start if

(if (= frames 1)
	(
	begin
	(set! visible (car(gimp-drawable-get-visible actlayer)))
	(onion-visibility-off img)

		(if (= visible 1)


			(
				begin
				(gimp-drawable-set-visible actlayer TRUE)
				(set! newlayerid (next-frame-down img layerpos))
				(gimp-drawable-set-visible newlayerid TRUE)
				(set! layerpos(car(gimp-image-get-layer-position img newlayerid)))
				(set! newlayerid (next-frame-down img layerpos))
				(gimp-drawable-set-visible newlayerid TRUE)

			)

			(gimp-drawable-set-visible actlayer TRUE)
		)
	)
)

(set! nlayerpos layerpos)
;start if

(if (= frames 3)
	(
	begin
	(set! visible (car(gimp-drawable-get-visible actlayer)))

;check current layer

		(if (= visible 1)

			(
			begin
			(set! newlayerid (next-frame-down img layerpos))
			(set! nlayerpos (car(gimp-image-get-layer-position img newlayerid)))
			(set! visible (car(gimp-drawable-get-visible newlayerid)))

;check next layer

				(if (= visible 1)

					(
					begin
					(set! newlayerid (next-frame-down img nlayerpos))
					(set! nlayerpos (car(gimp-image-get-layer-position img newlayerid)))
					(set! visible (car(gimp-drawable-get-visible newlayerid)))

;check next layer

						(if (= visible 1)
;turn on layers
							(
							begin
							(onion-visibility-off img)
							(gimp-drawable-set-visible actlayer TRUE)
							(set! newlayerid (next-frame-down img layerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(set! layerpos(car(gimp-image-get-layer-position img newlayerid)))
							(set! newlayerid (next-frame-down img layerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(set! layerpos(car(gimp-image-get-layer-position img newlayerid)))
							(set! newlayerid (next-frame-down img layerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(set! layerpos(car(gimp-image-get-layer-position img newlayerid)))
							(set! newlayerid (next-frame-down img layerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(if (not (= (number-of-visframes img) 5))
								(if (not (= (number-of-visframes img) 4))(set! frames 0)))
							)

							(
							begin
							(onion-visibility-off img)
							(gimp-drawable-set-visible actlayer TRUE)
							)
						)
					)

					(
					begin
					(onion-visibility-off img)
					(gimp-drawable-set-visible actlayer TRUE)
					)

				)
			)

			(
			begin
			(onion-visibility-off img)
			(gimp-drawable-set-visible actlayer TRUE)
			)

		)
	)
)

;end if

(if (not (= frames 3))

		(if (not (= frames 1))(set! frames 0))
)

(if (= frames 0)

	(
	begin
	(onion-visibility-off img)
	(gimp-drawable-set-visible actlayer TRUE)
	)
)

 	(gimp-image-undo-group-end img)
        (gimp-displays-flush)

  )
)

(define (lbox-onionskin-up img actlayer)

  (let*(
	(layerpos(car(gimp-image-get-layer-position img actlayer)))
	(frames(number-of-visframes img))
        ;(frames 1)
	(visible 0)
	(newlayerid 0)
	(nlayerpos 0)

	)

(gimp-image-undo-group-start img)

;If one frame check active layer visibility

;start if

(if (= frames 1)
	(
	begin
	(set! visible (car(gimp-drawable-get-visible actlayer)))
	(onion-visibility-off img)

		(if (= visible 1)


			(
				begin
				(gimp-drawable-set-visible actlayer TRUE)
				(set! newlayerid (next-frame-up img layerpos))
				(gimp-drawable-set-visible newlayerid TRUE)
				(set! layerpos(car(gimp-image-get-layer-position img newlayerid)))
				(set! newlayerid (next-frame-up img layerpos))
				(gimp-drawable-set-visible newlayerid TRUE)

			)

			(gimp-drawable-set-visible actlayer TRUE)
		)
	)
)

(set! nlayerpos layerpos)
;start if

(if (= frames 3)
	(
	begin
	(set! visible (car(gimp-drawable-get-visible actlayer)))

;check current layer

		(if (= visible 1)

			(
			begin
			(set! newlayerid (next-frame-up img layerpos))
			(set! nlayerpos (car(gimp-image-get-layer-position img newlayerid)))
			(set! visible (car(gimp-drawable-get-visible newlayerid)))

;check next layer

				(if (= visible 1)

					(
					begin
					(set! newlayerid (next-frame-up img nlayerpos))
					(set! nlayerpos (car(gimp-image-get-layer-position img newlayerid)))
					(set! visible (car(gimp-drawable-get-visible newlayerid)))

;check next layer

						(if (= visible 1)
;turn on layers
							(
							begin
							(onion-visibility-off img)
							(gimp-drawable-set-visible actlayer TRUE)
							(set! newlayerid (next-frame-up img layerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(set! layerpos(car(gimp-image-get-layer-position img newlayerid)))
							(set! newlayerid (next-frame-up img layerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(set! layerpos(car(gimp-image-get-layer-position img newlayerid)))
							(set! newlayerid (next-frame-up img layerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(set! layerpos(car(gimp-image-get-layer-position img newlayerid)))
							(set! newlayerid (next-frame-up img layerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(if (not (= (number-of-visframes img) 5))
								(if (not (= (number-of-visframes img) 4))(set! frames 0)))
							)

							(
							begin
							(onion-visibility-off img)
							(gimp-drawable-set-visible actlayer TRUE)
							)
						)
					)

					(
					begin
					(onion-visibility-off img)
					(gimp-drawable-set-visible actlayer TRUE)
					)

				)
			)

			(
			begin
			(onion-visibility-off img)
			(gimp-drawable-set-visible actlayer TRUE)
			)

		)
	)
)

;end if

(if (not (= frames 3))

		(if (not (= frames 1))(set! frames 0))
)

(if (= frames 0)

	(
	begin
	(onion-visibility-off img)
	(gimp-drawable-set-visible actlayer TRUE)
	)
)

 	(gimp-image-undo-group-end img)
        (gimp-displays-flush)

  )
)

(define (lbox-onionskin-centre img actlayer)

  (let*(
	(layerpos(car(gimp-image-get-layer-position img actlayer)))
	(frames(number-of-visframes img))
        ;(frames 1
	(visible 0)
	(newlayerid 0)
	(dlayerpos 0)

	)

(gimp-image-undo-group-start img)

;If one frame check active layer visibility

;start if

(if (= frames 1)
	(
	begin
	(set! visible (car(gimp-drawable-get-visible actlayer)))
	(onion-visibility-off img)

		(if (= visible 1)


			(
				begin
				(gimp-drawable-set-visible actlayer TRUE)
				(set! newlayerid (next-frame-down img layerpos))
				(gimp-drawable-set-visible newlayerid TRUE)

				(set! newlayerid (next-frame-up img layerpos))
				(gimp-drawable-set-visible newlayerid TRUE)

			)

			(gimp-drawable-set-visible actlayer TRUE)
		)
	)
)
;end if
(set! dlayerpos layerpos)
;start if

(if (= frames 3)
	(
	begin
	(set! visible (car(gimp-drawable-get-visible actlayer)))

;check current layer

		(if (= visible 1)

			(
			begin
			(set! newlayerid (next-frame-down img layerpos))
			(set! visible (car(gimp-drawable-get-visible newlayerid)))

;check next layer

				(if (= visible 1)

					(
					begin
					(set! newlayerid (next-frame-up img layerpos))
					(set! visible (car(gimp-drawable-get-visible newlayerid)))

;check next layer

						(if (= visible 1)
;turn on layers
							(
							begin
							(onion-visibility-off img)
							(gimp-drawable-set-visible actlayer TRUE)
							(set! newlayerid (next-frame-down img layerpos))
							(set! dlayerpos(car(gimp-image-get-layer-position img newlayerid)))
							(gimp-drawable-set-visible newlayerid TRUE)
							(set! newlayerid (next-frame-down img dlayerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(set! newlayerid (next-frame-up img layerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(set! dlayerpos(car(gimp-image-get-layer-position img newlayerid)))
							(set! newlayerid (next-frame-up img dlayerpos))
							(gimp-drawable-set-visible newlayerid TRUE)
							(if (not (= (number-of-visframes img) 5))
								(if (not (= (number-of-visframes img) 4))(set! frames 0)))
							)

							(
							begin
							(onion-visibility-off img)
							(gimp-drawable-set-visible actlayer TRUE)
							)
						)
					)

					(
					begin
					(onion-visibility-off img)
					(gimp-drawable-set-visible actlayer TRUE)
					)

				)
			)

			(
			begin
			(onion-visibility-off img)
			(gimp-drawable-set-visible actlayer TRUE)
			)

		)
	)
)

;end if

(if (not (= frames 3))

		(if (not (= frames 1))(set! frames 0))
)

(if (= frames 0)

	(
	begin
	(onion-visibility-off img)
	(gimp-drawable-set-visible actlayer TRUE)
	)
)

	(gimp-image-undo-group-end img)
        (gimp-displays-flush)

  )
)

(script-fu-register
	"lbox-onionskin-down"
	"Cycle Onion Skinning Down"
	"Cycles the layer visibility of the frames below. Cycles through either 1 3 or 5 frames"
	"Benjamin Donoghue"
	"2011"
	"March 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
	SF-DRAWABLE "Current Layer" 0
)

(script-fu-menu-register "lbox-onionskin-down"
                         "<Image>/Animate/Tools/Onion-Skin")

(script-fu-register
	"lbox-onionskin-up"
	"Cycle Onion Skinning Up"
	"Cycles the layer visibility of the frames above. Cycles through either 1 3 or 5 frames"
	"Benjamin Donoghue"
	"2011"
	"March 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
	SF-DRAWABLE "Current Layer" 0
)

(script-fu-menu-register "lbox-onionskin-up"
                         "<Image>/Animate/Tools/Onion-Skin")

(script-fu-register
	"lbox-onionskin-centre"
	"Cycle Onion Skinning Centre"
	"Cycles the layer visibility of the frames above and below. Cycles through either 1 3 or 5 frames"
	"Benjamin Donoghue"
	"2011"
	"March 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
	SF-DRAWABLE "Current Layer" 0
)

(script-fu-menu-register "lbox-onionskin-centre"
                         "<Image>/Animate/Tools/Onion-Skin")

;
;;;Anim-opacity.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;23.05.11
;;;Version 1.0
;
;Lightbox
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;INFO
;
;
;Changes opacity on either all layers or layers start "Frame-xxx..."

;RENAME

;OPACITY ALL
;

(define (opacity-update-all img opacity)

(gimp-image-undo-group-start img)

(let*(

	(numoflayers(car(gimp-image-get-layers img)))
	(framelist(cadr(gimp-image-get-layers img)))
	(currentframe 0)
	(i 0)

	)



(set! i(- numoflayers 1))

(while (>= i 0)

	(set! currentframe(aref framelist i))
	(gimp-layer-set-opacity currentframe opacity)
	(set! i (- i 1))

)

)

(gimp-image-undo-group-end img)
(gimp-displays-flush)

)


;OPACITY FRAMES

(define (opacity-update-frames img opacity)

(gimp-image-undo-group-start img)

(let*(

	(numoflayers(car(gimp-image-get-layers img)))
	(framelist(cadr(gimp-image-get-layers img)))
	(currentframe 0)
	(i 0)
	(layername "")
	(frcheck "")

	)

(set! i(- numoflayers 1))

(while (>= i 0)

	(set! currentframe(aref framelist i))

	(set! layername(car(gimp-drawable-get-name currentframe)))
	(set! frcheck(string-ref layername 0))
	(set! frcheck(string frcheck))

	(if (string=? frcheck "F")(gimp-layer-set-opacity currentframe opacity))

	(set! i (- i 1))

)

)

(gimp-image-undo-group-end img)
(gimp-displays-flush)

)

(script-fu-register
	"opacity-update-all"
	"Change Opacity on All Layers"
	"Adjusts opacity on all layers in the file. Usful when importing .psd files"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
	SF-ADJUSTMENT _"Opacity" '(50 0 100 1 10 0 0)
)

(script-fu-menu-register "opacity-update-all"
                         "<Image>/Animate/Tools/Opacity")

(script-fu-register
	"opacity-update-frames"
	"Change Opacity on All Frames"
	"Adjusts opacity only on Frame layers. Usful when importing .psd files"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
	SF-ADJUSTMENT _"Opacity" '(50 0 100 1 10 0 0)
)

(script-fu-menu-register "opacity-update-frames"
                         "<Image>/Animate/Tools/Opacity")


;
;;;Anim-rename.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;23.05.11
;;;Version 1.0
;
;Project Lightbox:
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;INFO
;
;
;Renames all layers starting "Frame-".
;lbox-rename-frames by their position in the stack
;lbox-rename-frames-timing by their current timing (BASED ON 25FR/S)

;SCRIPT
;......


;RENAME
;
;Parameters : Image ID
;             Layer ID
;             Old Layer Name
;             New Layer Number
;Returns    : New Layer Name

(define (rename-layer img layerid layername input)

(let*(
	(layerprefix(substring layername 0 6))
        (layersuffix(substring layername 9))

	(newlayername "")
	(input(number->string input))
	(output "")
	(numbera "")
	(numberb "")
	(input(string-append "000" input))
	(numbera(string-length input))
	(checklength "")
	(check "")
)
	(set! numberb (- numbera 3))
	(set! output(substring input numberb))
	(set! newlayername (string-append layerprefix output layersuffix))

	(set! checklength(string-length newlayername))
	(set! checklength(- checklength 1))
	(set! check(string-ref newlayername checklength))
	(set! check (string check))

	(if(not(string=? check ")"))
		(set! newlayername (substring newlayername 0 (- checklength 1)))
	)

	(gimp-drawable-set-name layerid newlayername)
	newlayername)
)


;CHECK TIMING
;
;Parameters : Layer Name
;Returns    : Frames Held for (25fps)

(define (frame-hold checktiming)

(let*(
	(timelength 0)
	(framehold 0)
	(i 0)
	(remove 0)
	(checklength "")
	(check "")
)


;"frame-000-(40ms)"


	(set! checklength(string-length checktiming))
        (set! checklength(- checklength 1))
	(set! check(string-ref checktiming checklength))
	(set! check (string check))

	(if(not(string=? check ")"))
		(while (= i 0)

			(set! check(string-ref checktiming checklength))
			(set! check (string check))
			(if(not(string=? check ")"))(set! remove(+ remove 1)))
			(set! checklength(- checklength 1))
			(if (string=? check ")")(set! i(+ i 1)))
		)
	)


(set! checktiming(substring checktiming 10))

;"40ms)"

(set! timelength(string-length checktiming))
(set! timelength(- timelength 3))
(set! timelength(- timelength remove))

(set! checktiming(substring checktiming 0 timelength))

;"40"

(set! framehold(string->number checktiming))

;40

(set! framehold(/ framehold 40))

;1

;return framehold
framehold)
)


;RENAME FRAMES (Based on their timing)


(define(lbox-rename-frames-timing img)

(gimp-image-undo-group-start img)

(let*(

	(num-layers (car (gimp-image-get-layers img)))
	(layer-array (cadr (gimp-image-get-layers img)))
	(i 0)
	(layername "")
	(frcheck "")
	(layerid 0)
	(frcount 0)
	(x 0)
	(hold 1)
	(colourrename "")
	(oldlayername "")
	(clayername "")
	(frame 0)
)




(while (< x 2)

(set! i (- num-layers 1))

(while (>= i 0)


	(set! layerid (aref layer-array i))

;check if a Frame

	(set! layername(car(gimp-drawable-get-name layerid)))
	(set! frcheck(substring layername 0 6))

	(if (string=? frcheck "Colour")
		(if (= frame 1)
		(begin
		(set! layername (substring colourrename 5 9))
		(set! layername (string-append "Colour" layername))
		(gimp-drawable-set-name layerid layername)
		(set! frame 0)
		)
		(begin
		(set! layername (substring layername 0 10))
		(set! layername (string-append layername "#1"))
		(gimp-drawable-set-name layerid layername)
		)
		)
	)


	(set! clayername (substring layername 5 9))
	(set! clayername (string-append "Colour" clayername))

	(if(string=? frcheck "Frame-")
		(
		begin
		(set! frcount(+ frcount hold))
		(set! colourrename (rename-layer img layerid layername frcount))
		(set! hold(frame-hold layername))
		(set! frame 1)
		)
	)
	(set! oldlayername clayername)
	(set! i (- i 1))
)
	(set! frcount 0)
	(set! hold 1)
	(set! x (+ x 1))
)

)

(gimp-image-undo-group-end img)
(gimp-displays-flush)

)

;RENAME FRAMES (sequentially)

(define(lbox-rename-frames img)

(gimp-image-undo-group-start img)

(let*(

	(num-layers (car (gimp-image-get-layers img)))
	(layer-array (cadr (gimp-image-get-layers img)))
	(i 0)
	(layername "")
	(oldlayername "")
	(frcheck "")
	(layerid 0)
	(frcount 0)
	(x 0)
	(colourrename "")
	(clayername "")
	(frame 0)
)



(while (< x 2)

(set! i (- num-layers 1))

(while (>= i 0)


	(set! layerid (aref layer-array i))

;check if a Frame

	(set! layername(car(gimp-drawable-get-name layerid)))
	(set! frcheck(substring layername 0 6))

	(if (string=? frcheck "Colour")
		(if (= frame 1)
		(begin
		(set! layername (substring colourrename 5 9))
		(set! layername (string-append "Colour" layername))
		(gimp-drawable-set-name layerid layername)
		(set! frame 0)
		)
		(begin
		(set! layername (substring layername 0 10))
		(set! layername (string-append layername "#1"))
		(gimp-drawable-set-name layerid layername)
		)
		)
	)

	(set! clayername (substring layername 5 9))
	(set! clayername (string-append "Colour" clayername))

	(if(string=? frcheck "Frame-")
		(begin
		(set! frcount(+ frcount 1))
		(set! colourrename (rename-layer img layerid layername frcount))
		(set! frame 1)
		)
	)


	;(set! oldlayername clayername)
	(set! i (- i 1))
)
	(set! frcount 0)
	(set! x (+ x 1))
)

)

(gimp-image-undo-group-end img)
(gimp-displays-flush)

)


(script-fu-register
	"lbox-rename-frames-timing"
	"Rename Frames by Timing"
	"Renames Frames based on their Timing"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	 SF-IMAGE    "Image"	0
)

(script-fu-menu-register "lbox-rename-frames-timing"
                         "<Image>/Animate/Tools/Rename/")


(script-fu-register
	"lbox-rename-frames"
	"Rename Frames Sequentially"
	"Renames Frames based on their position"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	 SF-IMAGE    "Image"	0
)

(script-fu-menu-register "lbox-rename-frames"
                         "<Image>/Animate/Tools/Rename/")


;
;;;Anim-timing.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;23.05.11
;;;Version 1.0
;
;Project Lightbox:
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;
;INFO
;
;Retimes individual frames for use with PLAYBACK script based on 25 frames a second.
;Set single and double exposure to keyframes for ease of use



;SCRIPT
;......

;RETIME OTHER EXPOSURE
;
;Parameters : Image ID
;             Layer ID
;             Frame Hold

(define (lbox-retime-exposure img layer hold)

(gimp-image-undo-group-start img)

(let* (

	(layername(car(gimp-drawable-get-name layer)))
	(without-timing(substring layername 0 9))
	(new-timing hold)
	(newlayername "")
	(confirm-frame "")

	)

	(set! new-timing (/ (* new-timing 1000) 25))
	(set! new-timing (number->string new-timing))
	(set! new-timing (string-append "(" new-timing "ms)"))

	(set! newlayername (string-append without-timing new-timing))
	(set! confirm-frame(string-ref layername 0))
	(set! confirm-frame(string confirm-frame))

	(if (string=? confirm-frame "F")

	(gimp-drawable-set-name layer newlayername)

	)

)

(gimp-image-undo-group-end img)
(gimp-displays-flush)

)


;RETIME SINGLE EXPOSURE

(define (lbox-retime-single-exposure img layer)
 (let*((hold 1))(lbox-retime-exposure img layer hold)))


;RETIME DOUBLE EXPOSURE

(define (lbox-retime-double-exposure img layer)
 (let*((hold 2))(lbox-retime-exposure img layer hold)))


;Register scripts

(script-fu-register
	"lbox-retime-double-exposure"
	"Retime Frame to 'on 2s'"
	"Retimes current animation frame to double exposure "
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
	SF-DRAWABLE	"Current Frame" 0
)

(script-fu-menu-register "lbox-retime-double-exposure"
                         "<Image>/Animate/Tools/Timing/")

(script-fu-register
	"lbox-retime-single-exposure"
	"Retime Frame to 'on 1s'"
	"Retimes current animation frame to single exposure "
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
	SF-DRAWABLE	"Current Frame" 0
)

(script-fu-menu-register "lbox-retime-single-exposure"
                         "<Image>/Animate/Tools/Timing/")

(script-fu-register
	"lbox-retime-exposure"
	"Retime Frame to Custom Exposure"
	"Retimes current animation frame to custom exposure based on 25frs / second"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
	SF-DRAWABLE	"Current Frame" 0

	SF-ADJUSTMENT _"Hold frame for"        '(2 0 250 1 10 0 1)
)

(script-fu-menu-register "lbox-retime-exposure"
                         "<Image>/Animate/Tools/Timing")

;
;;;Anim-playback.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;23.05.11
;;;Version 1.0
;
;Project Lightbox:
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;
;INFO
;
;Composites together frames for playback. Uses the Playback plug-in in Gimp 2.6.



;SCRIPT
;......


;COMPOSITE

(define (bg-comp img newImg complayer inColour)

(let*(
	(num-layers (car (gimp-image-get-layers img)))
	(layer-array (cadr (gimp-image-get-layers img)))

	(layerid 0)
	(layername "")
	(frcheck "")

	(i (- num-layers 1))
	(vischeck 0)

	(newBGid 0)
	(num-BG 0)

	(complayer-name (car(gimp-drawable-get-name complayer)))
	(new-complayer complayer)



	)

(while (>= i 0)

	(set! layerid (aref layer-array i))
	(set! layername(car(gimp-drawable-get-name layerid)))
	(set! frcheck(string-ref layername 0))
	(set! frcheck(string frcheck))

	(if (string=? frcheck "B")

		(
		begin
		(set! vischeck (car(gimp-drawable-get-visible layerid)))
		(if (= vischeck 1)
			(
			begin
			(set! newBGid (car(gimp-layer-new-from-drawable layerid newImg)))

			(gimp-image-add-layer newImg newBGid 0)

			(set! num-BG (+ num-BG 1))
			)
		)
		)
	)

	(set! i (- i 1))
)

(if (>= num-BG 1)
	(begin

	;(set! num-BG (- num-BG 1))

(if (= inColour FALSE)
	(begin
	(gimp-image-add-layer newImg complayer 0)
	(gimp-layer-set-opacity complayer 100)
	(plug-in-colortoalpha 1 newImg complayer '(255 255 255))
	)
)



(if (= inColour TRUE)
	(gimp-image-raise-layer-to-top newImg complayer)
)

;merge layer with BG

(while (>= num-BG 1)

	(gimp-layer-set-mode new-complayer 0)
        (set! new-complayer (car(gimp-image-merge-down newImg new-complayer 0)))
	(set! num-BG (- num-BG 1))
)

	(gimp-drawable-set-name new-complayer complayer-name)

	)

	(if (= inColour FALSE)(gimp-image-add-layer newImg complayer 0))
)

new-complayer)
)


;add colour layer

(define (merge-with-colour img layerid newImg complayerid inLine inBG inAlpha)
 (let*	(

	(layer-array (cadr (gimp-image-get-layers img)))

	(layer-pos (car(gimp-image-get-layer-position img layerid)))
	(colour-pos (- layer-pos 1))
	(colourid 0)
	(colourname 0)
	(Ccheck 0)
	(i 1)
	(output 0)

	(rename "")

        (imgHeight (car(gimp-image-height img)))
        (imgWidth  (car(gimp-image-width img)))
        (removealpha 0)

	)


        (if (= inLine 1)
            (begin
	(gimp-image-add-layer newImg complayerid 0)
	(gimp-layer-set-opacity complayerid 100)
	(gimp-drawable-set-visible complayerid TRUE)
        (set! output complayerid)
            )
        )


	(set! rename(car(gimp-drawable-get-name layerid)))

(if (>= colour-pos 0)

	(begin
  	(set! colourid (aref layer-array colour-pos))
	(set! colourname(car(gimp-drawable-get-name colourid)))
	(set! Ccheck(substring colourname 0 6))

	(if (string=? Ccheck "Colour")
		(begin
		(set! colourid (car(gimp-layer-new-from-drawable colourid newImg)))
		(if (and (= inBG TRUE)(= inLine 1))
		(plug-in-colortoalpha 1 newImg complayerid '(255 255 255))
		)
		(gimp-image-add-layer newImg colourid 0)
                (set! output colourid)
		(gimp-drawable-set-visible colourid TRUE)
		(gimp-layer-set-opacity colourid 100)

                    (if (= inLine 1)
                            (begin
		(if (= inBG TRUE)

			(begin
			(gimp-layer-set-mode colourid 0)
			(gimp-image-raise-layer-to-top newImg complayerid)
			(set! output (car(gimp-image-merge-down newImg complayerid 0)))
			(gimp-drawable-set-name output rename)
                        (gimp-layer-set-mode output 0)
			)

			(set! output (car(gimp-image-merge-down newImg colourid 0)))
		)
                            )
                    )

                (if (and(= inBG FALSE)(= inAlpha 0))
                        (begin
                (set! removealpha (car(gimp-layer-new newImg imgWidth imgHeight 1 "White" 100 0)))
                (gimp-drawable-fill removealpha WHITE-FILL)
                (gimp-image-add-layer newImg removealpha 1)
                (set! output (car(gimp-image-merge-down newImg colourid 0)))
                        )
                        )
		)
	)
	(gimp-drawable-set-name output rename)
	)
)



 output)
)

(define (comp-animation-layers img inLine inColour inBG inAlpha)

 (let*	(

	(num-layers (car (gimp-image-get-layers img)))
	(layer-array (cadr (gimp-image-get-layers img)))

	(imgHeight (car(gimp-image-height img)))
	(imgWidth (car(gimp-image-width img)))

	(newImg 0)

	(i (- num-layers 1))

	(layerid 0)
	(layername "")
	(frcheck "")

	(newlayerid 0)

  	)

	(gimp-image-undo-disable img)

; create new image

	(set! newImg (car(gimp-image-new imgWidth imgHeight 0)))

;get layer marked F
;
;bottom layer working up, add at position 0 of new image.

;check layer name

  (while (>= i 0)

	(set! layerid (aref layer-array i))
	(set! layername(car(gimp-drawable-get-name layerid)))
	(set! frcheck(string-ref layername 0))
	(set! frcheck(string frcheck))

;if "F" add to new image

	(if(string=? frcheck "F")

                (begin
                (if (= inLine 1)(set! newlayerid (car(gimp-layer-new-from-drawable layerid newImg))))
;alt
;
		(if (= inColour TRUE)
			(begin

			(set! newlayerid (merge-with-colour img layerid newImg newlayerid inLine inBG inAlpha))
			)
		)

		(if (= inBG TRUE)

			(set! newlayerid (bg-comp img newImg newlayerid inColour))
		)

		(if (and (= inBG FALSE)(= inColour FALSE))(gimp-image-add-layer newImg newlayerid 0))

		)
	)

	(set! i (- i 1))
  )


	(gimp-displays-flush)
	(gimp-image-undo-enable img)

newImg)
)

(define (lbox-comp-playback img inColour inBG)
(let*(

	(newImg 0)
	(layer-array 0)
	(newlayerid 0)
        (inLine 1)
        (inAlpha 0)
	)

	(set! newImg(comp-animation-layers img inLine inColour inBG inAlpha))
	(set! layer-array (cadr (gimp-image-get-layers newImg)))
	(set! newlayerid (aref layer-array 0))
	(plug-in-animationplay 1 newImg newlayerid)
	(gimp-image-delete newImg)
	;(gimp-display-new newImg)

)
)

(define (lbox-comp-img img inColour inBG)
(let*(

	(newImg 0)
        (inLine 1)
        (inAlpha 0)
	)

	(set! newImg(comp-animation-layers img inLine inColour inBG inAlpha))
	;(gimp-image-delete newImg)
	(gimp-display-new newImg)

)
)

(script-fu-register
	"lbox-comp-playback"
	"Playback Animation"
	"Playback animation including composited elements"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
	SF-TOGGLE "Colour" FALSE
        SF-TOGGLE "Background" FALSE
)

(script-fu-menu-register "lbox-comp-playback"
                         "<Image>/Animate/Playback")



(script-fu-register
	"lbox-comp-img"
	"Composite Current Animation"
	"Composites current image into a new image"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
	SF-TOGGLE "Colour" FALSE
        SF-TOGGLE "Background" FALSE
)

(script-fu-menu-register "lbox-comp-img"
                         "<Image>/Animate/New Canvas")


;
;;;Anim-export.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;23.05.11
;;;Version 1.0
;
;Project Lightbox:
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;
;INFO
;
;Composites and exports frames as .tifs. This can be done either based on timing or on layer order.
;Video Export uses GIMP's existing plugin GAP (GIMP Animation Package). The script composites
;the layers together and creates timing before giving options for video export.


;SCRIPT
;......

;FILENAME
;framename = "framename" framenum = 1

(define (filename-timing framename framenum extention)

 (let*	(

	(Savefr-num-string "0000")

	(newstring "")

	(istring "")

	(checklength 0)

	(filename "")

	)

	(set! istring (number->string framenum))

;1
	(set! newstring (string-append Savefr-num-string istring))

;00001

	(set! checklength (string-length newstring))
;5
	(set! checklength (- checklength 3))
;3 = length of string. can include variable
;returns 2

	(set! newstring (substring newstring checklength))

;001

	(set! filename (string-append framename "-" newstring extention))


;framename-001.tif
;return filename

 	filename)
)

;SAVE LAYERS

(define(save-frames-as-tif img framename option dir timing)
 (let*	(

	(num-layers (car (gimp-image-get-layers img)))
	(layer-array (cadr (gimp-image-get-layers img)))
	(i (- num-layers 1))

	(layerid 0)

	(layername "")
	(frcheck "")

	(frame-num 1)
	(hold 0)

	(save-name "")

	(ext ".tif")

	)


	(gimp-image-undo-disable img)

;get bottom frame id

(while (>= i 0)



	(set! layerid (aref layer-array i))

;check if a frame

	(set! layername(car(gimp-drawable-get-name layerid)))
	(set! frcheck(string-ref layername 0))
	(set! frcheck(string frcheck))

	(if (string=? frcheck "F")
		(
		begin

		(if (= timing 1)(set! hold (frame-hold layername))(set! hold 1))

		(while (> hold 0)


			(set! save-name (filename-timing framename frame-num ext))
			(set! save-name (string-append dir DIR-SEPARATOR save-name))
			(file-tiff-save 1 img layerid save-name save-name option)
			(set! frame-num (+ frame-num 1))
			(set! hold (- hold 1))

		)
		)
	)

;end if

	(set! i (- i 1))
)

;end while

	(gimp-image-undo-enable img)

 )
)

(define(lbox-imgseq-export img framename option dir inLine inColour inBG inAlpha)

(let*(

	(newImg 0)
	(timing 1)

	)

	(set! newImg(comp-animation-layers img inLine inColour inBG inAlpha))
	(save-frames-as-tif newImg framename option dir timing)
	(gimp-image-delete newImg)

)
)

(define(lbox-layercomp-export img framename option dir inLine inColour inBG inAlpha)

(let*(

	(newImg 0)
	(timing 0)

	)

	(set! newImg(comp-animation-layers img inLine inColour inBG inAlpha))
	(save-frames-as-tif newImg framename option dir timing)
	(gimp-image-delete newImg)

)
)


;Video Export

(define (lbox-video-export img inColour inBG)
(let*(

	(newImg 0)
	(layer-array 0)
	(num-layers 0)
        (i 0)
        (layerid 0)
        (layername "")
        (frhold 1)
        (newlayer 0)
        (imgWidth (car(gimp-image-width img)))
        (imgHeight (car(gimp-image-height img)))
        (inAlpha FALSE)
        (inLine TRUE)

	)

	(set! newImg(comp-animation-layers img inLine inColour inBG inAlpha))
	(set! layer-array (cadr (gimp-image-get-layers newImg)))
        (set! num-layers (car (gimp-image-get-layers newImg)))
        (set! i (- num-layers 1))

        (while (>= i 0)
               (set! layerid (aref layer-array i))
               (set! layername (car(gimp-drawable-get-name layerid)))
               (if (= frhold 1)
               (set! frhold (frame-hold layername)))

               (if (> frhold 1)
                    (begin
                    (set! newlayer (car(gimp-layer-copy layerid FALSE)))
                    (gimp-image-add-layer newImg newlayer i)
                    (set! frhold (- frhold 1))
                    )
                    )

               (if (= frhold 1)(set! i (- i 1)))
               )

         (set! num-layers (car (gimp-image-get-layers newImg)))

  (plug-in-gap-vid-encode-master 0 newImg 0 "Animation" 0 num-layers imgWidth imgHeight 1 25 48 "" "plug_in_gap_enc_ffmpeg" "" "" 1)


	(gimp-image-delete newImg)
	;(gimp-display-new newImg)

)
)

(script-fu-register
	"lbox-video-export"
	"Export Animation as Video"
	"Exports animation as a video file"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	SF-IMAGE    "Image"	0
        SF-TOGGLE "Colour" FALSE
	SF-TOGGLE "Background" FALSE

)

(script-fu-menu-register "lbox-video-export"
                         "<Image>/Animate")

(script-fu-register
	"lbox-imgseq-export"
	"Export Animation as Image Sequence"
	"Exports frames based on their timing as .tif files"
	"Benjamin Donoghue"
	"2011"
	"Spring 2011"
	"RGB* RGBA*"
	SF-IMAGE	"Image"	0
	SF-STRING	"Filename" "Frame"
	SF-OPTION	"Compression" '("Uncompressed" "LZW Compressed")
	SF-DIRNAME	"Image Directory" "/home"
        SF-TOGGLE "Line" TRUE
        SF-TOGGLE "Colour" FALSE
        SF-TOGGLE "Background" FALSE
	SF-TOGGLE "Include Alpha" FALSE
)

(script-fu-menu-register "lbox-imgseq-export"
                         "<Image>/Animate")

;OLD
;
;(script-fu-register
;	"lbox-layercomp-export"
;	"Export Composited Layers"
;	"Exports frames based on their layer order as .tif files"
;	"Benjamin Donoghue"
;	"2011"
;	"Spring 2011"
;	"RGB* RGBA*"
;	SF-IMAGE	"Image"	0
;	SF-STRING	"Filename" "Layer"
;	SF-OPTION	"Compression" '("Uncompressed" "LZW Compressed")
;	SF-DIRNAME	"Image Directory" "/home"
;	SF-TOGGLE "Line" TRUE
;       SF-TOGGLE "Colour" FALSE
;      SF-TOGGLE "Background" FALSE
;	SF-TOGGLE "Include Alpha" FALSE
;)
;
;(script-fu-menu-register "lbox-layercomp-export"
;                         "<Image>/Animate");

;
;;;Anim-ForceRename.scm -*-scheme*-*-
;;;Author: Benjamin Donoghue
;;;25.08.11
;;;Version 1.1
;
;Project Lightbox:
;Part of series of GIMP scripts to better emulate animating traditionally with paper
;
;
;
;INFO
;
;Forces renames all layers to 'Frame-' Layers. Useful when importing multiple layers into GIMP for ;animating


;SCRIPT
;......


(define (lbox-batch-rename img  timing)

(gimp-image-undo-group-start img)

 (let*	(

	(numoflayers(car(gimp-image-get-layers img)))
	(layer-array(cadr(gimp-image-get-layers img)))
	(newlayerid 0)
	(x (- numoflayers 1))
	(newnum 1)
	(layerprefix "Frame-")
	(newlayername "")
	(layertiming "")
	(hold (* timing 40))
	(framenumber "000")
	(layersuffix "")
	(checklength 0)
	(checkcut 0)
		
	)

(while (> x -1)

	
	(set! numoflayers (- numoflayers 1))
	(set! newlayerid (aref layer-array x))



	(set! layersuffix (number->string newnum))
	(set! layersuffix (string-append framenumber layersuffix))
	(set! checklength (string-length layersuffix))
	(set! checkcut (- checklength 3))
	(set! layersuffix (substring layersuffix checkcut checklength))
	
	(set! layertiming (number->string hold)) 
	(set! layertiming (string-append "(" layertiming "ms)"))
	(set! newlayername (string-append layerprefix layersuffix layertiming))

	(gimp-drawable-set-name newlayerid newlayername)

	(set! newnum (+ newnum 1))

	(set! x (- x 1))

)

)
(gimp-image-undo-group-end img)
(gimp-displays-flush)

)


(script-fu-register
	"lbox-batch-rename"
	"Force Rename all Layers"
	"Force renames all layers to 'Frame' layers. Gives options for timing"
	"Benjamin Donoghue"
	"2011"
	"Summer 2011"
	"RGB* RGBA*"
	SF-IMAGE	"Image"	0
	SF-ADJUSTMENT _"Hold frame for"        '(2 0 250 1 10 0 1)
)

(script-fu-menu-register "lbox-batch-rename"
                         "<Image>/Animate/Tools/Rename")	
	







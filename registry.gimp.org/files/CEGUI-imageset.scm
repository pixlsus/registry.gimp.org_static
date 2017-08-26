;Copyright (c) 2008 Anton Lopatin <lope85@users.sourceforge.net>
;
; DESCRIPTION:
; this script will create a .imageset definition file, each visible layer will be treated as a separate image and exported
; then the script will create a duplicate image, merge it down, and save with provided filename (and extension).
;
; INSTALL:
; copy to ~/.gimp /scripts/
; refresh scripts (Gimp->Extras->Script-Fu-> Refresh)
;
; USAGE:
; make sure your image is of the size 2^n 
; click Export -> CEGUI Imageset ( Found in the top image menu)
; make sure that the output path ends with a slash
;
; This script is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;OTHER DEALINGS IN THE SOFTWARE.


(define (script-fu-writeimageset inImage ImagesetName ImagesetFile NativeHRes NativeVRes AutoScaled outFile outPath)

	;------------- create the .imageset file -------------------------------------------------------------------------------------------------------------
	(let ((p (open-output-file (string-append outPath outFile))))
	
		(define write-layername 
			(lambda(id)
				(display "\t<Image Name=" p)
				(write (car(gimp-drawable-get-name id)) p)
			)
		)
		
		(define write-layerposition 
			(lambda(id)
				(display " XPos=\"" p)
				(display (car(gimp-drawable-offsets id)) p)
				(display "\" YPos=\"" p)
				(display (cadr(gimp-drawable-offsets id)) p)
			)
		)
		
		(define write-layersize
			(lambda(id)
				(display "\" Width=\"" p)
				(write (car(gimp-drawable-width id)) p)
				(display "\" Height=\"" p)
				(write (car(gimp-drawable-height id)) p)
				(display "\" />" p)
			)
		)		
			
		(define write-layerline
			(lambda(id)
				(if (< 0 (car(gimp-drawable-get-visible id)))	;ignore invisible layers
				(begin
					(write-layername id)
					(write-layerposition id)
					(write-layersize id)
					(newline p)
				)
				)
			)
		)
		
		;---------------------------------------------------------------------------------------------------------------------------------
		;---------------- write the xml and imageset header --------------------------------------------------------------------
		(display "<?xml version=\"1.0\" ?>\n" p)
		(display "<Imageset Name=" p)(write ImagesetName p)
		(display " Imagefile=" p)    (write ImagesetFile p)
		(display " NativeHorzRes=" p)(write NativeHRes p)
		(display " NativeVertRes=" p)(write NativeVRes p)
		(display " AutoScaled=" p)(write AutoScaled p)
		(display ">\n" p)

		;---------------- create an image tag for each layer --------------------------------------------------------------------
		(for-each write-layerline (vector->list (cadr( gimp-image-get-layers inImage ))))

		;---------------- write the imageset footer --------------------------------------------------------------------------------
		(display "</Imageset>" p)
		(close-output-port p)
	) 	

	;------------- create the image file -------------------------------------------------------------------------------------------------------------	
	(let ((tempImage (car(gimp-image-duplicate inImage))))
		(let ((tempLayer (car( gimp-image-merge-visible-layers tempImage 0 ))))
			( gimp-layer-resize-to-image-size tempLayer )
			( gimp-file-save 0 tempImage tempLayer (string-append outPath ImagesetFile) ImagesetFile )
			( gimp-image-delete tempImage )
		)
	)
)


(script-fu-register "script-fu-writeimageset"
  "<Image>/Export/CEGUI Imageset..."
  "Create a CEGUI .imageset file, exporting the layers as images"
  "Anton Lopatin <lope85@users.sourceforge.net>"
  "Anton Lopatin"
  "6/18/08"
  "RGB* GRAY*"
  SF-IMAGE     "Image"                   0
  SF-STRING   _"Imageset-Name" 			   "temp"
  SF-STRING   _"Image-File" 			   "temp.tga"
  SF-STRING   _"NativeHorzRes"			   "800"
  SF-STRING   _"NativeVertRes"			   "600"
  SF-STRING	  _"AutoScaled"				   "true"
  SF-STRING   _"Imageset-File" 			   "temp.imageset"
  SF-STRING   _"Output Path"			   "/"
;  SF-TOGGLE     _"Keep selection"          TRUE
)

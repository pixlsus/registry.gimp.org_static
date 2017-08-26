;
; Copyright, V1.0
; Valter Brazzini (valter.brazzini@gmail.com)
; http://www.valterbrazzini.name
; (C) 2010, Florence, Italy
;
; This script was tested with Gimp 2.6
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses>.
; gimp -b '(VB-Copyright-batch "name-file.jpg" "String Text" "String Font" 2 0)' -i -b '(gimp-quit 0)'
; gimp -b '(VB-Copyright-batch "*.jpg" "String Text" "String Font" 2 0)' -i -b '(gimp-quit 0)'
; example : gimp -b '(VB-Copyright-batch "000010008.jpg" "\302\251 - 2010 VB" "Arial" 2 0)' -i -b '(gimp-quit 0)'
;
; Define the function
;
(define (VB-Copyright-batch pattern InText InFont InColorPre InPosition)
;	
; Save history
;
  (let* ((filelist (cadr (file-glob pattern 1))))
    (while (not (null? filelist))
           (let* ((filename (car filelist))
                  (InImage (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
		  (drawable (car (gimp-image-get-active-layer InImage)))
		  (TheWidth (car (gimp-image-width InImage)))
		  (TheHeight (car (gimp-image-height InImage)))
		  (Old-FG-Color (car (gimp-context-get-foreground)))
		  (FontSize (/ (* TheHeight 1.5) 100))
		  (BlurSize (* FontSize 0.07))
		  (text-size (gimp-text-get-extents-fontname InText FontSize PIXELS InFont))
		  (text-width (car text-size))
		  (text-height (car text-size))
		  (reserve-width (/ (* TheWidth 1) 100))
		  (reserve-height (/ (* TheHeight 1) 100))
		  (text-x 0)
		  (text-y 0))
	  ;
	  ; Generate copyright text on the image
;
; Select the text color
;
		  (cond
;
; Gray
;
			((= InColorPre 0) (gimp-context-set-foreground '(127 127 127)))
;
; Black
;
			((= InColorPre 1) (gimp-context-set-foreground '(15 15 15)))
;
; White
;
			((= InColorPre 2) (gimp-context-set-foreground '(240 240 240)))
		  )
;
	  ;	Select position
	  ;
		  (cond 
;
;	Bottom right
;
			((= InPosition 0) 
				(begin
					(set! text-x (- TheWidth (+ text-width reserve-width)))
					(set! text-y (- TheHeight (+ text-height reserve-height)))
				)
			)
;
;	Bottom left
;
			((= InPosition 1) 
				(begin
					(set! text-x reserve-width)
					(set! text-y (- TheHeight (+ text-height reserve-height)))
				)
			)
;
;	Bottom center
;
			((= InPosition 2)
				(begin
					(set! text-x (/ (- TheWidth text-width) 2))
					(set! text-y (- TheHeight (+ text-height reserve-height)))
				)
			)
;
;	Top right
;
			((= InPosition 3) 
				(begin
					(set! text-x (- TheWidth (+ text-width reserve-width)))
					(set! text-y reserve-height)
				)
			)
;
;	Top left
;
			((= InPosition 4) 
				(begin
					(set! text-x reserve-width)
					(set! text-y reserve-height)
				)
			)
;
;	Top center
;
			((= InPosition 5) 
				(begin
					(set! text-x (/ (- TheWidth text-width) 2))
					(set! text-y reserve-height)
				)
			)
;
;	Image center
;
			((= InPosition 6) 
				(begin
					(set! text-x (/ (- TheWidth text-width) 2))
					(set! text-y (/ (- TheHeight text-height) 2))
				)
			)
		  )

		  (let*	(
			  (TextLayer (car (gimp-text-fontname InImage -1 text-x text-y InText -1 TRUE FontSize PIXELS InFont)))
			  )
			  (gimp-layer-set-opacity TextLayer 80.0)
    ;
    ; Blur the text, if we need to
    ;
			  (plug-in-gauss TRUE InImage TextLayer BlurSize BlurSize TRUE)
    ;
    ; No Flatten the image
    ;
		          (gimp-drawable-set-name TextLayer "Copyright")
;				  (gimp-image-set-active-layer InImage InLayer)
			  (gimp-image-set-active-layer InImage drawable)
		      
		   )
		   (gimp-context-set-foreground Old-FG-Color)
		   (gimp-image-merge-visible-layers InImage EXPAND-AS-NECESSARY)
		   (set! drawable (car (gimp-image-get-active-layer InImage)))
		   (gimp-file-save RUN-NONINTERACTIVE InImage drawable filename filename)
		   (gimp-image-delete InImage)
	  )
	  (set! filelist (cdr filelist))
    )
  )
)
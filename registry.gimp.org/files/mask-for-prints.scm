; Mask-for-Prints - Create a new layer showing the visible extents of
; an image when printed on the selected print size.
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; Copyright (C) 2008 Kip Shaffer kip@shafferhouse.org
;
; Version 1.0 - Initial version Tested on GIMP-2.0, 16 April 2008

; Dimensions and names... defined as 'Portrait' orientation
;  -note, units don't matter, only ratio
(define sizes '(
   ("Wallets" 2.47 3.46 0 0)
   ("Die Cut Wallets" 2.47 3.46 0.095 0.065)
   ("3x5" 3 5 0 0)
   ("9x13" 9 13 0 0)
   ("4x6" 4 6 0 0)
   ("10x15" 10 15 0 0)
   ("5x7" 5 7 0 0)
   ("13x18" 13 18 0 0)
   ("8x10" 8 10 0 0)
   ("8x12" 8 12 0 0)
   ("8.5x11" 8.5 11 0 0)
   ("8.5x14" 8.5 14 0 0)
   ("10x13" 10 13 0 0)
   ("11x14" 11 14 0 0)
   ("16x20" 16 20 0 0)
   ("24x30" 24 30 0 0)
   ("30x40" 30 40 0 0)
   ("Square Prints" 1 1 0 0)
))

(define (size_name size)
   (nth 0 (nth size sizes))
)

(define (print_width size orientation)
   (if (= orientation 0)
        (nth 1 (nth size sizes))
        (nth 2 (nth size sizes))
   )
)

(define (print_height size orientation)
   (if (= orientation 0)
       (nth 2 (nth size sizes))
       (nth 1 (nth size sizes))
   )
)

(define (print_h_inset size orientation)
   (if (= orientation 0)
       (nth 3 (nth size sizes))
       (nth 4 (nth size sizes))
   )
)

(define (print_v_inset size orientation)
   (if (= orientation 0)
       (nth 4 (nth size sizes))
       (nth 3 (nth size sizes))
   )
)

(define (script-fu-mask-for-prints image drawable size orientation)

   (set! print_w (print_width size orientation))
   (set! print_h (print_height size orientation))

   (set! foreground_save (car(gimp-palette-get-foreground)))

   ;Begin
   (gimp-image-undo-group-start image)

   ;Get size of image
   (set! image_w (car (gimp-image-width image)))
   (set! image_h (car (gimp-image-height image)))
   
   ;Figure out scaling factor
   (set! scalefactor (min
         (/ image_w print_w)
	 (/ image_h print_h)))

   ;Figure out margins
   (set! h_margin (+ (/ (- image_w 
		   	   (* print_w scalefactor)) 
		        2)
		     (* (print_h_inset size orientation)
		        scalefactor))
   )
                     
   (set! v_margin (+ (/ (- image_h 
			   (* print_h scalefactor)) 
			2)
		     (* (print_v_inset size orientation)
		        scalefactor))
   )

;   (gimp-message (string-append 
;      "H-Margin=" (number->string h_margin) " "
;      "V-Margin=" (number->string v_margin) " "
;      "print_h_inset=" (number->string (print_h_inset size orientation)) " "
;      "scalefactor=" (number->string scalefactor) " "
;      (size_name size)
;   ))

   ;Make a new layer
   (set! new_layer (car (gimp-layer-new 
         image image_w image_h
         (car (gimp-drawable-type-with-alpha drawable)) ;Type
         (string-append "Mask for " (size_name size))   ;Name
         50                                             ;Opacity
         NORMAL-MODE                                    ;Mode
   )))

   ;Save Selection then clear

   (set! selection_saved (car (gimp-selection-is-empty image)))
   (if selection_saved 
      (set! selection_save (car(gimp-selection-save image)))
   )
   (gimp-image-add-layer image new_layer -1)       ;add to top
   (gimp-selection-none image)                     ;select whole image to clear
   (gimp-edit-clear new_layer)

   ;Select Top margin
   (if (> v_margin 0) 
      (gimp-rect-select image 0 0 image_w v_margin CHANNEL-OP-REPLACE 0 0)
   )

   ;Add Bottom margin
   (if (> v_margin 0) 
      (gimp-rect-select image 0 (- image_h v_margin) 
         image_w v_margin CHANNEL-OP-ADD 0 0)
   )

   ;Select Left margin
   (if (> h_margin 0) 
     (gimp-rect-select image 0 0 h_margin image_h CHANNEL-OP-ADD 0 0)
   )

   ;Add Right margin
   (if (> h_margin 0) 
      (gimp-rect-select image (- image_w h_margin) 0 
         h_margin image_h CHANNEL-OP-ADD 0 0)
   )

   ;Fill selection
   (gimp-palette-set-foreground '(0 0 0))                          ;Black Fill
   (gimp-edit-fill new_layer FOREGROUND-FILL)

   ;Finish
   (gimp-palette-set-foreground foreground_save)
   (if selection_saved 
      (gimp-selection-load selection_save)
   )
   (gimp-image-set-active-layer image drawable)
   (if selection_saved 
      (gimp-image-remove-channel image selection_save)
   )
   (gimp-image-undo-group-end image)

   (gimp-displays-flush)
)

(script-fu-register "script-fu-mask-for-prints"
		    _"<Image>/Script-Fu/Misc/_Mask for Prints..."
		    "Create a layer showing visible extents for selected print size. This allows you to preview what your picture will look like on various sizes of paper."
		    "Kip Shaffer <kip@shafferhouse.org>"
		    "Kip Shaffer"
		    "April 16, 2008"
		    ""
		    SF-IMAGE "Image" 0
		    SF-DRAWABLE "Drawable" 0
		    SF-OPTION _"Desired Print Size" '(
		       Wallet "Die Cut Wallets" 3x5 "9x13 cm" 4x6 "10x15 cm" 
		       5x7 "13x18 cm" 8x10 8x12 8.5x11 8.5x14 10x13 11x14 
		       16x20 24x30 30x40 "Square Prints"
                    )
		    SF-OPTION _"Print Orientation" '(
		       "Portrait" "Landscape"
                    )
)

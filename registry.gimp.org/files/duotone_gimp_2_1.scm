;
; duotone
;
; $Id: duotone-gimp-2.0.scm,v 2.1 2008/09/16 20:22:22 Giovanni Exp $
; Revision 2.1 2008/09/16 Giovanni
; Fixed some interpreter errors (undefined variables);
; Moved the set-pt and duotone-spline inside the script-fu-duotone so
; now are not global anymore;
; Added the gimp-image-undo-group-start/end when the script works on an
; image copy.
;
; $Id: duotone-gimp-2.0.scm,v 2.1 2005/01/02 20:22:22 alexios Exp $
; 
; $Log: duotone-gimp-2.0.scm,v $
; Revision 2.1  2005/01/02 20:22:22  alexios
; Fixed stupid sanity check bug that manifests in GIMP 2.2.
;
; Revision 2.0  2004/09/14 20:45:25  alexios
; Stepped version to recover CVS repository after near-catastrophic disk
; crash.
;
; Revision 1.3  2004/02/12 22:12:29  alexios
; Another beta version of GIMP 2.0, another release of Duotone.
;
; Revision 1.2  2004/01/31 17:00:21  alexios
; Added support for Indexed images (they're converted to RGB anyway).
;
; Revision 1.1  2004/01/31 16:52:27  alexios
; Initial revision.
;

; A simple script to emulate darkroom sepia toning (and other similar
; toning processes). It is based on how actual sepia toning looks
; like, supplemented by Eric R. Jeschke's GIMP tutorial:

;	      http://gimpguru.org/Tutorials/SepiaToning/

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

; Define the function:

(define (script-fu-duotone	inImage
				inLayer
				inTintColour
				inCopy
            inFlatten
	)
   ; Definizione di una procedura che aggiunge dei valori ad un array.
   (define (set-pt a index x y)
            (prog1
               (aset a (* index 2) x)
               (aset a (+ (* index 2) 1) y)
            )
          )
   ; Definizione dei punti (tre) che verranno utilizzati per piegare la curva dei colori (value).
   (define (duotone-spline)
     (let* ((a (cons-array 6 'byte)))
       (set-pt a 0 0 0)
       (set-pt a 1 128 128)
       (set-pt a 2 255 0)
       a))         

   (let* (
           (theImage (if (= inCopy TRUE) (car (gimp-image-duplicate inImage)) inImage))
           (theWidth (car (gimp-image-width theImage)))
           (theHeight (car (gimp-image-height theImage)))
           (theLayer nil)
           (mask nil)
           (oldForeColor nil)
         )
      ; Blocco per l'immagine copia la sequenza degli "undo".
      (if (= inCopy TRUE)
         (gimp-image-undo-group-start theImage))

      (if (< 0 (car (gimp-image-base-type theImage)))
          (gimp-image-convert-rgb theImage))

      ; Do the actual work.

      ; Copy the image.

      (gimp-selection-all theImage)
      (gimp-edit-copy inLayer)

      ; Make the tint layer

      (set! theLayer (car (gimp-layer-new 	theImage
                     theWidth
                     theHeight
                     RGBA-IMAGE
                     "Tint"
                     100
                     COLOR-MODE)))

      ; Add the layer to the image

      (gimp-image-add-layer theImage theLayer 0)

      ; Fill the layer with the tint

      (set! oldForeColor (gimp-context-get-foreground))
      (gimp-palette-set-foreground inTintColour)
      (gimp-edit-fill theLayer FG-IMAGE-FILL)

      ; Create a mask for the new layer

      (set! mask (car (gimp-layer-create-mask theLayer ADD-WHITE-MASK)))
      (gimp-layer-add-mask theLayer mask)
      (gimp-floating-sel-anchor (car (gimp-edit-paste mask TRUE)))
      (gimp-curves-spline mask HISTOGRAM-VALUE 6 (duotone-spline))

      ; Flatten the image, if we need to.

      (if (= inFlatten TRUE) (gimp-image-flatten theImage))

      ; Have we been working on a copy? If so display the new image.

      (if (= inCopy TRUE)
          (begin
            (gimp-image-clean-all theImage)
            (gimp-display-new theImage)
            )
          ()
      )

      ; The end.
      (gimp-context-set-foreground (car oldForeColor))
       
      ; Fine della sezione che non registra gli "undo".
      (if (= inCopy TRUE)
          (gimp-image-undo-group-end theImage))
   )
   (gimp-displays-flush)
)

; Register the function with the GIMP:

(script-fu-register
    "script-fu-duotone"
    _"<Image>/Filters/Decor/Duotone"
    "Produces a duotone photograph.

Some interesting values for the colour are Sepia (162 138 101) and Selenium (229 232 234). Play with the colour saturation for more interesting effects, or uncheck the Flatten box and then modify the new layer's opacity. "
    "Alexios Chouchoulas"
    "2004, Alexios Chouchoulas"
    "Revised 16st September 2008 by Giovanni (Italy)"
    "RGB* GRAY* INDEXED*"
    SF-IMAGE       "The Image"      0
    SF-DRAWABLE    "The Layer"      0
    SF-COLOR       _"Tint colour"   '(162 138 101)
    SF-TOGGLE      _"Work on Copy"  TRUE
    SF-TOGGLE      _"Flatten Image" TRUE
)

;;; End Of File.

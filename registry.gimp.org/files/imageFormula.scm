; Berengar W. Lehr (Berengar.Lehr@gmx.de)
; Medical Physics Group, Department of Diagnostic and Interventional Radiology
; Jena University Hospital, 07743 Jena, Thueringen, Germany
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
; If you use this script and/or like it the author would be happy to
; receive a postcard from you:
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(define (script-fu-formula formula filename size TextColor)
    (let*
        (
            (sizeName (cond
                ((= size 0) "normalsize")
                ((= size 1) "LARGE")
                ((= size 2) "Huge")
            ))
            (url (string-append "http://texify.com/img/%5C" sizeName "%5C!" formula ".gif"))
            (image (car (file-uri-load FALSE url url)))
            (drawable (car (gimp-image-get-active-layer image)))
            (width (car (gimp-image-width image)))
            (height (car (gimp-image-height image)))
            (background (car (gimp-layer-new image width height RGBA-IMAGE "Background" 100 NORMAL-MODE)))
            (AntiTextColor (list (- 255 (car TextColor)) (- 255 (cadr TextColor)) (- 255 (caddr TextColor))))
            (gradientName (car (gimp-gradient-new "NeuerFarbverlauf")))
            (activegradient (car (gimp-context-get-gradient)))
			(filename (string-append filename ".png"))
        )
        (gimp-image-convert-rgb image)
        (gimp-image-add-layer image background 1)
        (gimp-edit-fill background WHITE-FILL)
        (set! drawable (car (gimp-image-merge-visible-layers image EXPAND-AS-NECESSARY)))
        (gimp-gradient-segment-set-left-color gradientName 0 TextColor 100)
        (gimp-gradient-segment-set-right-color gradientName 0 AntiTextColor 100)
        (gimp-context-set-gradient gradientName)
        (plug-in-gradmap TRUE image drawable)
        (gimp-context-set-gradient activegradient)
        (plug-in-colortoalpha TRUE image drawable AntiTextColor)
        (file-png-save-defaults TRUE image drawable filename filename)
    )
)

; Register the function with GIMP:

(script-fu-register
    "script-fu-formula"
    _"_Image Formula..."
    _"Return an formula"
  "Berengar W. Lehr"
  "2010, Berengar W. Lehr / MPG@IDIR, UH Jena, Germany."
  "22th April 2010"
  ""
  SF-STRING      "Formular"    "E=mc^2"
  SF-STRING      "Filename"    "<Filename>.png"
  SF-ADJUSTMENT  "Size (0-Normal|1-Large|2-Huge)" '(1 0 2 1 1 1 1)
  SF-COLOR       "Textcolor" '(0 0 0)
)

(script-fu-menu-register "script-fu-formula" "<Image>/Script-Fu/")

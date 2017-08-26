; scale_pattern.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.2 (20090413)

; Description
; Scales the selected pattern and puts it in the clipboard to allow it to be used as a pattern
;
; Changes
; 1.1 - Added interactive calls to sharpen the pattern.
; 1.2 - If larger than 512x512 will save it, refresh patterns, and set the active pattern to be the saved file.

; License:
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
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html
(define (script-fu-scale_pattern inScale inSharpen)
(let* 
  (
    (inScale (/ inScale 100))
    (width (car (gimp-pattern-get-info (car(gimp-context-get-pattern)))))
    (height (cadr (gimp-pattern-get-info (car(gimp-context-get-pattern)))))
    (tempimage (car (gimp-image-new (* 3 width) (* 3 height) RGB)))
    (templayer (car (gimp-layer-new tempimage (* 3 width) (* 3 height) RGBA-IMAGE "temp" 100 NORMAL-MODE)))
    (filename (string-append gimp-directory DIR-SEPARATOR "patterns" DIR-SEPARATOR "scaledpat.pat"))
  )

  (gimp-image-add-layer tempimage templayer -1)
  (gimp-drawable-fill templayer PATTERN-FILL)
  (gimp-image-scale-full tempimage (* width inScale 3) (* height inScale 3) INTERPOLATION-LANCZOS)
  
  (if (= inSharpen 1)
    (plug-in-sharpen 0 tempimage templayer 10)
  )
  (if (= inSharpen 2)
    (plug-in-unsharp-mask 0 tempimage templayer 1 3 0)
  )

  (if (and (< (* width inScale) 512) (< (* height inScale) 512))
    (begin
      (gimp-rect-select tempimage (* width inScale) (* height inScale) (* width inScale) (* height inScale) CHANNEL-OP-REPLACE FALSE 0)
      (gimp-edit-copy templayer)
      (gimp-context-set-pattern (list-ref (cadr (gimp-patterns-get-list "")) 0))
    )
    (begin
      (gimp-image-crop tempimage (* width inScale) (* height inScale) (* width inScale) (* height inScale))
      (file-pat-save RUN-NONINTERACTIVE tempimage templayer filename "scaledpat.pat" "Scaled Pattern")
     	(gimp-patterns-refresh)
      (gimp-context-set-pattern "Scaled Pattern")
    )
  )
  (gimp-image-delete tempimage)
)
)
    

(script-fu-register "script-fu-scale_pattern"
          "<Patterns>/Scale Pattern..."
          "Scales the current pattern and make it the active pattern."
          "Rob Antonishen"
          "Rob Antonishen"
          "Jan 2009"
          ""
          SF-ADJUSTMENT  "Pattern Scale %"          (list 100 20 500 5 10 0 SF-SLIDER)
          SF-OPTION      "Sharpening" (list "None" "Sharpen" "Unsharp Mask")
)
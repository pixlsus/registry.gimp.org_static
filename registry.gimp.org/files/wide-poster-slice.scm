;    Wide Poster Slice script for the GIMP
;    Copyright (C) 2007 Stephen Cavilia

;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.

;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.

;    You should have received a copy of the GNU General Public License along
;    with this program; if not, write to the Free Software Foundation, Inc.,
;    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


(define (script-fu-wide-poster-slice image layer tileaw tileah)
  (gimp-image-undo-freeze image)
  (let* ((iw (car (gimp-image-width image)))
         (ih (car (gimp-image-height image)))
         (tscale (/ ih tileah))
         (tw (floor (* tileaw tscale)))
         (th (floor (* tileah tscale)))
         (tcount (+ (floor (/ iw tw)) 1))
         (step (/ (- iw tw) (- tcount 1)))
         (slice-step (lambda (n)
                       (gimp-rect-select image (floor (* n step)) 0 tw th 2 FALSE 0)
                       (gimp-edit-copy layer)
                       (let* ((newimage (car (gimp-edit-paste-as-new))))
                         (gimp-display-new newimage)
                         (gimp-image-clean-all newimage))
                       (if (< n (- tcount 1))
                           (slice-step (+ n 1))
                           )))
         )
    (slice-step 0)
    (gimp-selection-none image)
    )
  (gimp-image-undo-thaw image)
  )

(script-fu-register
 "script-fu-wide-poster-slice"
 "Wide Poster Slice"
 "Slice a wide image into small pages for printing"
 "Stephen Cavilia"
 "copyright 2008, Stephen Cavilia"
 "2008"
 ""
 SF-IMAGE       "Image"         0
 SF-DRAWABLE    "Drawable"      0
 SF-ADJUSTMENT  "Tile Width"    '(10 1 100 0.1 1 1 1)
 SF-ADJUSTMENT  "Tile Height"   '(8 1 100 0.1 1 1 1)
 )
(script-fu-menu-register
 "script-fu-wide-poster-slice"
 "<Image>/Filters/Script-Fu/Utils"
 )

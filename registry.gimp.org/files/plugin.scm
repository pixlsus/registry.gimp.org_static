; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

;; Save layers names into a file called gimp-layers
;; in the chosen directory.
;

(define (save-layers-names image dirname)
(define names "")
 (map (lambda (layer)
	(set! names (string-append names (car (gimp-item-get-name layer)) "\n")))
       (vector->list (cadr (gimp-image-get-layers image))))
 (call-with-output-file (string-append dirname "/gimp-layers")
  (lambda (output-port)
    (display names output-port)))
)

      
        
(script-fu-register "save-layers-names"
  "Save layers' names"
  "Save layers' names"
  "Marco Scarpetta"
  "Marco Scarpetta"
  "November 2011"
  "*"
  SF-IMAGE    "Image"    0
  SF-DIRNAME "File" "~")

(script-fu-menu-register "save-layers-names"
 "<Image>/Tools"
 )
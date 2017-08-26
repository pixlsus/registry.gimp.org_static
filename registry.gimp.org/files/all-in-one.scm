; This is a script for The GIMP
;
; Description: all sorts of convenient wrappers for personal use
;
; Runtime requirements: triple-border.scm, scale-to-size.scm
;
; Version 1.0
; Last changed: 11.06.2009
;
; Copyright (C) 2009 Dr. Martin Rogge <marogge@onlinehome.de>
;
; --------------------------------------------------------------------
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

(define (script-mr-all-in-one-black img draw)
  (script-mr-scale-to-size img draw 540)
  (script-mr-border img draw 1 '(255 255 255) 47 '(0 0 0) 0 '(255 255 255))
)

(script-fu-register 
  "script-mr-all-in-one-black"
  "<Image>/Tools/All-in-one Black"
  "My personal all-in-one workflow:\nScale image to 540xN, unsharp mask for the web, then create a 1+47 pix black border. Stepwise undo is supported."
  "Dr. Martin Rogge <marogge@onlinehome.de>"
  "Dr. Martin Rogge"
  "11/06/2009"
  "RGB* GRAY*"
  SF-IMAGE    "Image"         0
  SF-DRAWABLE "Drawable"      0
)

(define (script-mr-all-in-one-white img draw)
  (script-mr-scale-to-size img draw 540)
  (script-mr-border img draw 1 '(0 0 0) 47 '(255 255 255) 0 '(0 0 0))
)

(script-fu-register 
  "script-mr-all-in-one-white"
  "<Image>/Tools/All-in-one White"
  "My personal all-in-one workflow:\nScale image to 540xN, unsharp mask for the web, then create a 1+47 pix white border. Stepwise undo is supported."
  "Dr. Martin Rogge <marogge@onlinehome.de>"
  "Dr. Martin Rogge"
  "11/06/2009"
  "RGB* GRAY*"
  SF-IMAGE    "Image"         0
  SF-DRAWABLE "Drawable"      0
)

(define (script-mr-border-black img draw)
  (script-mr-border img draw 1 '(255 255 255) 47 '(0 0 0) 0 '(255 255 255))
)

(script-fu-register 
  "script-mr-border-black"
  "<Image>/Tools/Border Black"
  "Create a 1+47 pix black border"
  "Dr. Martin Rogge <marogge@onlinehome.de>"
  "Dr. Martin Rogge"
  "21/12/2004 to 11/06/2009"
  "RGB* GRAY* INDEXED*"
  SF-IMAGE    "Image"         0
  SF-DRAWABLE "Drawable"      0
)

(define (script-mr-border-white img draw)
  (script-mr-border img draw 1 '(0 0 0) 47 '(255 255 255) 0 '(0 0 0))
)

(script-fu-register 
  "script-mr-border-white"
  "<Image>/Tools/Border White"
  "Create a 1+47 pix white border"
  "Dr. Martin Rogge <marogge@onlinehome.de>"
  "Dr. Martin Rogge"
  "11/06/2009"
  "RGB* GRAY* INDEXED*"
  SF-IMAGE    "Image"         0
  SF-DRAWABLE "Drawable"      0
)


;; FILE   gimp_diving.scm
;; DATE   2008-09-25
;; COPYRIGHT 2008
;; AUTHOR Jeremy Bluteau and Thomas Amory <postmaster@epug.net>
;; The credit for this process goes to a posting on DigitalDiver.net by David Kusner  (nickname - "mandrake").
;;  and this website http://pt010.da-kine.info/adjustments.htm
;;
;; DESCRIPTION
;; This script acts as a red fitler on diving photos.
;; To launch it, goto the menu <Image>/Script-Fu/Enhance/Diving red filter
;;
;; Basically, create a new layer containing the corrected picture
;; You can adjust the red level (sometimes, green might works better !)
;; and set if the white balance has to be performed...
;; Enjoy !
 
;; LICENCE
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

 

(define (script-fu-diving img drawable balance red-level)
 

(gimp-progress-update 0.0) ;;progress bar

;; duplicate selected layer 3 times
(define dup1 (car (gimp-layer-copy drawable TRUE)))
(gimp-image-add-layer img dup1 0)

(gimp-progress-update 0.1) ;;progress bar

(define dup2 (car (gimp-layer-copy drawable TRUE)))
(gimp-image-add-layer img dup2 0)

(gimp-progress-update 0.2) ;;progress bar

(define dup3 (car (gimp-layer-copy drawable TRUE)))
(gimp-image-add-layer img dup3 0)

(gimp-progress-update 0.3) ;;progress bar

;;desaturate the dup2
(gimp-desaturate dup2)

(gimp-progress-update 0.4) ;;progress bar

;; fill dup3 with red-level color, define in parameter of the function
(gimp-context-set-foreground red-level)
(gimp-drawable-fill dup3 FOREGROUND-FILL)

(gimp-progress-update 0.5) ;;progress bar

;;merge it down with multiply operation
(gimp-layer-set-mode dup3 MULTIPLY-MODE)
(define dup5 (car (gimp-image-merge-down img dup3 0)))

(gimp-progress-update 0.6) ;;progress bar

;;merge this new layer with the third one with screen operation
(gimp-layer-set-mode dup5 SCREEN-MODE)

(define dup6 (car (gimp-image-merge-down img dup5 0)))

(gimp-progress-update 0.7) ;;progress bar

;;perform white balance if needed
(if (= balance TRUE)
	(begin
		(gimp-levels-stretch dup6)
	)
)

(gimp-progress-update 0.9) ;;progress bar

;;refresh display
(gimp-displays-flush)
(gimp-progress-update 1.0) ;;progress bar	
)



;; RECORD this script in the 
(script-fu-register "script-fu-diving" ;; nom du script
"<Image>/Script-Fu/Enhance/Diving red filter"
"This script acts as a red fitler on diving photos.\nTo launch it, goto the menu <Image>/Script-Fu/Enhance/Diving red filter\n\nBasically, create a new layer containing the corrected picture. 
You can adjust the red level (sometimes, green might works better !)
and set if the white balance has to be performed...\nEnjoy !" ;; commentaires
"Jeremy Bluteau and Thomas Amory" ;; auteur
"2008 under GPL" ;; copyright
"2008-09-25" ;; date
"" ;; types d'images supportés par le script
SF-IMAGE "Image" 0 ;; image dans lequel le calque est ajouté
SF-DRAWABLE "Drawable" 0 ;; calque actif de l'image
SF-TOGGLE "Balance des blancs ?" TRUE
SF-COLOR "Couleur rouge" '(255 0 0) 
;;SF-ADJUSTMENT "Niveau de rouge" '(100 0 100 1 10 0 1)

) ;; fin du register

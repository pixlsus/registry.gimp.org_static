; script-fu-calendar.scm
;
; Plugin for The Gimp 2.0 (http://www.gimp.org)
;
; CHANGE-LOG:
; 0.2  - initial release
; 0.99 - Changed lots of global functions and variables to local
;        Added support for local holydays (German only yet). One Bug remains:
;        The "Buß- und Bettag" in Sachsen isn't added yet.
; 1.0  - The "Buß- und Bettag" in Sachsen was added.
;        Selection for start of the week
;        language-selection
; 1.1  - Added the possibility to create a calendar without an image from 
;        gimp/extensions
; 1.1.1- Bugfix (1st of November in DE-NW
; 1.2  - Funktion for narrow and wide calendars.
; 1.2.1- enabled anti aliasing for fonts, unsing foregroundcolor for text 
;        uotput
; 1.2.2- Made some changes to make usage with The Gimp 2.4 possible and removed 
;	 one major bug. I really don't know why this worked before.
; 1.2.3- French language and bugfix for english calendar by Jean-François Bardou. Thank you!
;
; Copyright (C) 2008 Moritz Mekelburger <moritz@mekelburger.org>
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
;
; Creates a Calendar.

(script-fu-register "script-fu-calendar"
                    "<Image>/Utils/_Kalender"
                    "Erstellt einen Monatsklender in der Auswahl..."
                    "Moritz Mekelburger <moritz@mekelburger.org>"
                    "Moritz Mekelburger"
                    "2008/01/10"
                    ""
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Drawable" 0
		    SF-ADJUSTMENT "Monat" '(1 1 12 1 2 0 1)
		    SF-ADJUSTMENT "Jahr" '(2010 1900 3000 1 10 0 1)
                    SF-OPTION "Land" '("Keine Feiertage"
					"DE-Bundesweit" 
					"DE-BW" 
					"DE-BY"
					"DE-BE"
					"DE-BR"
					"DE-HB"
					"DE-HH"
					"DE-HE"
					"DE-MV"
					"DE-NI"
					"DE-NW"
					"DE-RP"
					"DE-SL"
					"DE-SN"
					"DE-ST"
					"DE-SH"
					"DE-TH")
		    SF-OPTION "Wochenanfang" '("Montag" "Sonntag")
		    SF-OPTION "Sprache" '("de" "en" "ca" "fr")
		    SF-FONT   "Font" "Sans Bold"
		    SF-COLOR      "Hintergrund" '(256 256 256)
		    SF-COLOR      "Feiertage" '(191 191 191)
		    SF-OPTION "Layout" '("Wochenansicht" "2 Spalten" 
					   "2 Zeilen")
                    )

(script-fu-register "script-fu-new-image-calendar"
                    "<Toolbox>/Xtns/Utils/Kalender..."
                    "Erstellt einen Monatsklender in der Auswahl..."
                    "Moritz Mekelburger <moritz@mekelburger.org>"
                    "Moritz Mekelburger"
                    "2008/01/10"
                    ""
		    SF-ADJUSTMENT "Breite" '(400 1 1200 1 20 0 1)
		    SF-ADJUSTMENT "Höhe" '(300 1 1200 1 20 0 1)
		    SF-ADJUSTMENT "Monat" '(1 1 12 1 2 0 1)
		    SF-ADJUSTMENT "Jahr" '(2010 1900 3000 1 10 0 1)
                    SF-OPTION "Land" '("Keine Feiertage"
					"DE-Bundesweit" 
					"DE-BW" 
					"DE-BY"
					"DE-BE"
					"DE-BR"
					"DE-HB"
					"DE-HH"
					"DE-HE"
					"DE-MV"
					"DE-NI"
					"DE-NW"
					"DE-RP"
					"DE-SL"
					"DE-SN"
					"DE-ST"
					"DE-SH"
					"DE-TH")
		    SF-OPTION "Wochenanfang" '("Montag" "Sonntag")
		    SF-OPTION "Sprache" '("de" "en" "ca" "fr")
		    SF-FONT   "Font" "Sans Bold"
		    SF-COLOR      "Hintergrund" '(256 256 256)
		    SF-COLOR      "Feiertage" '(255 0 0)
                    )

(define (script-fu-new-image-calendar breite
				      hoehe
				      monat
				      jahr
				      land
				      wochenanfang
				      sprache
				      meinFont
				      hintergrund
				      feiertage)
  (let* 
      (
       (neuesBild (car (gimp-image-new breite hoehe 0)))
       (neuerLayer (car (gimp-layer-new neuesBild breite hoehe 1 "Ebene 1" 0 0)))
       )

    (gimp-image-undo-disable neuesBild)
    (gimp-image-add-layer neuesBild neuerLayer -1)
    (gimp-display-new neuesBild)
    (gimp-selection-all neuesBild)
    (gimp-selection-shrink neuesBild (/ breite 40))
    (script-fu-calendar neuesBild neuerLayer monat jahr land wochenanfang 
			sprache meinFont hintergrund feiertage 0)
    (gimp-image-undo-enable neuesBild)
    ))

(define (script-fu-calendar image 
			    drawable
			    monat
			    jahr
			    land
			    wochenanfang
			    sprache
			    meinFont
			    hintergrund
			    feiertage
			    layout)

  (let* ((old-bg (car (gimp-context-get-background)))
	 (old-fg (car (gimp-context-get-foreground)))
	 (shadow-layer 0))
    (gimp-image-undo-group-start image)

    (define (div divisor
		 quotient)
      ;;Division ohne Rest
      (let* 
	  (
	   (ergebnis 0)
	   )
	(if (> quotient 0)
	    (while (>= divisor 0)
		   (begin
		     (set! divisor (- divisor quotient))
		     (set! ergebnis (+ ergebnis 1))
		     )
		   ))
	(- ergebnis 1)
	)
      )
    
    (define (mod divisor
		 quotient)
      ;;Rest einer Division
      (- divisor (* quotient (div divisor quotient)))
      )
    
    (define (tagDerWoche tag 
			 monat 
			 jahr)
      ;;Gibt zu einem Datum den zugehörigen Wochentag aus
      ;;1: Montag, 2: Dienstag, ... 6: Samstag, 7:Sonntag
      
      (if (< monat 3)
	  (begin (set! monat (+ monat 10))
		 (set! jahr (- jahr 1)))
	  (begin (set! monat (- monat 2)))
	  )
      (let* 
	  (
	   (jahrhundert (div jahr 100))
	   (jr (mod jahr 100))
	   (wochentag (mod (- (+ (div (- (* 26 monat) 2) 10) tag jr 
				 (div jr 4) (div jahrhundert 4)) 
			      (* 2 jahrhundert)) 7))
	   )
	(while (< wochentag 1)
	       (set! wochentag (+ wochentag 7))
	       )
	(+ wochentag)
	)
      )

    (define (tagDerWoche* tag monat jahr wochenanfang)
      ;;Gibt zu einem Datum den zugehörigen Wochentag aus
      ;;wochenanfang=0: 1: Montag, 2: Dienstag, ... 6: Samstag, 7:Sonntag
      ;;wochenanfang=1: 0:Sonntag, 1: Montag, 2: Dienstag, ... 6: Samstag
      (if (= wochenanfang 0)
	  (tagDerWoche tag monat jahr)
	  (mod (tagDerWoche tag monat jahr) 7)))
    
    (define (tageImMonat monat jahr)
      ;;berechnet, wie viele Tage ein Monat hat
      (if (or (= monat 4) (= monat 6) (= monat 9) (= monat 11))
	  (+ 30)
	  (if (= monat 2)
	      (if (or (and (= (mod jahr 4) 0) 
			   (> (mod jahr 100) 0)) 
		      (= (mod jahr 400) 0))
		  (+ 29)
		  (+ 28))
	      (+ 31))
	  )
      )
    
    (define (tagDesJahres tag monat jahr)
      ;;berechnet, der wievielte Tag eines Jahres das angegebene Datum ist
      (let* (
	     (aktuellerMonat 1)
	     (tage 0)
	     )
	(while (< aktuellerMonat monat)
	       (begin
		 (set! tage (+ tage (tageImMonat aktuellerMonat jahr)))
		 (set! aktuellerMonat (+ aktuellerMonat 1))
		 ))
	(+ tage tag)
	))
    
    
    (define (wocheDesJahres tag monat jahr)
      (div (tagDesJahres (+ tag (tagDerWoche 1 1 jahr) -2) 
			 monat jahr) 7)
      )
    
    (define (wocheDesJahres* tag monat jahr wochenanfang)
      (div (tagDesJahres (+ tag (tagDerWoche* 1 1 jahr wochenanfang) -1) 
			 monat jahr) 7)
      )
    
    (define (wocheDesMonats tag monat jahr)
      ;;berechnet, in der wievielten Woche des Monats ein Tag liegt
      (+ 1 (- (wocheDesJahres tag monat jahr) 
	      (wocheDesJahres 1 monat jahr))))
    (define (wocheDesMonats* tag monat jahr wochenanfang)
      ;;berechnet, in der wievielten Woche des Monats ein Tag liegt
      (+ 1 (- (wocheDesJahres* tag monat jahr wochenanfang) 
	      (wocheDesJahres* 1 monat jahr wochenanfang))))
    
	(define (feiertag tag monat jahr feiertagFarbe land)
	;;bestimmt, ob es sich bei dem Tag um einen Feiertag handelt, 
	;;und setzt die Schriftfarbe entsprechend
      
		(define (ostern jahr)
		;;Diese Berechnung des Osterdatums gibt die Woche des Jahres zurück, 
		;;in die der Ostersonntag fällt.
			(let* 
				(
				(c (div jahr 100))
				(g (mod jahr 19))
				(h (mod (+ (- c (div c 4) (div (+ (* 8 c) 13) 25)) (* 19 g) 15) 30))
				(i (- h (* (div h 28) (- 1 (div 29 (+ h 1))) (div (- 21 g) 11))))
				(j (mod (+ jahr (div jahr 4) i 2 (- c) (div c 4)) 7))
				(l (- i j))
				(ostermonat (+ 3 (div (+ l 40) 44)))
				(ostertag (+ l 28 (- (* 31 (div ostermonat 4)))))
				)
				(wocheDesJahres ostertag ostermonat jahr)
			)
		)
      
		(define (neujahr tag monat jahr)
			(and (= monat 1) (= tag 1))
		)
		(define (tagDerArbeit tag monat jahr)
			(and (= monat 5) (= tag 1))
		)
		(define (tagDerDtEinheit tag monat jahr)
			(and (= monat 10) (= tag 3))
		)
		(define (weihnachten tag monat jahr)
			(and (= monat 12) (or (= tag 25) (= tag 26)))
		)
      		(define (karfreitag tag monat jahr)
			(and 
				(= (tagDerWoche tag monat jahr) 5)
	     			(= (wocheDesJahres tag monat jahr) (ostern jahr))
			)
		)
		(define (ostermontag tag monat jahr)
			(and 
				(= (tagDerWoche tag monat jahr) 1)
	     			(= (wocheDesJahres tag monat jahr) (+ (ostern jahr) 1))
	     		)
	     	)
	     	(define (pfingstmontag tag monat jahr)
			(and 
				(= (tagDerWoche tag monat jahr) 1)
				(= (wocheDesJahres tag monat jahr) (+ (ostern jahr) 8))
			)
		)
      (define (christiHimmelfahrt tag monat jahr)
	(and (= (tagDerWoche tag monat jahr) 4)
	     (= (wocheDesJahres tag monat jahr) (+ (ostern jahr) 6))))
      (define (fronleichnam tag monat jahr)
	(and (= (tagDerWoche tag monat jahr) 4)
	     (= (wocheDesJahres tag monat jahr) (+ (ostern jahr) 9))))
      (define (heiligeDreiKoenige tag monat jahr)
	(and (= tag 6) (= monat 1)))
      (define (mariaHimmelfahrt tag monat jahr)
	(and (= tag 15) (= monat 8)))
      (define (reformationstag tag monat jahr)
	(and (= tag 31) (= monat 10)))
      (define (allerheiligen tag monat jahr)
	(and (= tag 1) (= monat 11)))
      (define (buszUndBetTag tag monat jahr)
	(and (= (tagDerWoche tag monat jahr) 3)
	     (= (wocheDesJahres tag monat jahr) 
		(- (wocheDesJahres (- 24 (tagDerWoche 24 12 jahr)) 12 jahr) 
		   4))))

	(if 
		(or 
			(= (tagDerWoche tag monat jahr) 7)
			(and 
				(> land 0) 
				(< land 17) 
				(or 
					(neujahr tag monat jahr)
			      		(karfreitag tag monat jahr)
			      		(ostermontag tag monat jahr)
			      		(christiHimmelfahrt tag monat jahr)
			      		(pfingstmontag tag monat jahr)
			      		(tagDerArbeit tag monat jahr)
			      		(tagDerDtEinheit tag monat jahr)
			      		(weihnachten tag monat jahr)
			      	)
			)
			(and 
				(= land 2) ;DE-BW
				(or 
					(allerheiligen tag monat jahr)
					(heiligeDreiKoenige tag monat jahr)
					(fronleichnam tag monat jahr)
				)
			)
			(and 
				(= land 3) ;DE-BY
				(or 
					(allerheiligen tag monat jahr)
					(heiligeDreiKoenige tag monat jahr)
					(fronleichnam tag monat jahr)
					(mariaHimmelfahrt tag monat jahr)
				)
			)
;			(and (= land 4) ;DE-BE
;			)
			(and 
				(= land 5) ;DE-BR
				(reformationstag tag monat jahr)
			)
;	      		(and (= land 6) ;DE-HB 
;			)
;			(and (= land 7) ;DE-HH
;		  	)
			(and 
				(= land 8) ;DE-HE
				(fronleichnam tag monat jahr)
			)
			(and 
				(= land 9) ;DE-MV
				(reformationstag tag monat jahr)
			)
;	      		(and (= land 10) ;DE-NI
;		  	)
	      		(and 
	      			(= land 11) ;DE-NW
				(or 
					(fronleichnam tag monat jahr)
					(allerheiligen tag monat jahr)
				)
			)
			(and 
				(= land 12) ;DE-RP
				(or 
					(fronleichnam tag monat jahr)
					(allerheiligen tag monat jahr)
				)
			)
			(and 
				(= land 13) ;DE-SL
				(or 
					(fronleichnam tag monat jahr)
					(mariaHimmelfahrt tag monat jahr)		   
					(allerheiligen tag monat jahr)
				)
			)
			(and 
				(= land 14) ;DE-SN
				(or
					(fronleichnam tag monat jahr)
					(reformationstag tag monat jahr)
					(buszUndBetTag tag monat jahr)
				)
			)
			(and 
				(= land 15) ;DE-ST
		  		(or
					(heiligeDreiKoenige tag monat jahr)
					(reformationstag tag monat jahr)
				)
			)
;			(and (= land 16) ;DE-SH
;			)
			(and 
				(= land 17) ;DE-TH
				(or
					(fronleichnam tag monat jahr)
					(reformationstag tag monat jahr)
				)
			)
		)
		(gimp-palette-set-foreground feiertagFarbe)
		(gimp-palette-set-foreground old-fg)
	)
)

;      (if (or (= (tagDerWoche tag monat jahr) 7)
;	      (if (and (> land 0) (< land 17)) ;DE
;		  (or (neujahr tag monat jahr)
;		      (karfreitag tag monat jahr)
;		      (ostermontag tag monat jahr)
;		      (christiHimmelfahrt tag monat jahr)
;		      (pfingstmontag tag monat jahr)
;		      (tagDerArbeit tag monat jahr)
;		      (tagDerDtEinheit tag monat jahr)
;		      (weihnachten tag monat jahr)
;		      ))
;	      (if (= land 2) ;DE-BW
;		  (or (heiligeDreiKoenige tag monat jahr)
;		      (fronleichnam tag monat jahr)))
;	      (if (= land 3) ;DE-BY
;		  (or (heiligeDreiKoenige tag monat jahr)
;		      (fronleichnam tag monat jahr)
;		      (mariaHimmelfahrt tag monat jahr)))
;	      (if (= land 4) ;DE-BE
;		  )
;	      (if (= land 5) ;DE-BR
;		  (reformationstag tag monat jahr))
;	      (if (= land 6) ;DE-HB
;		  )
;	      (if (= land 7) ;DE-HH
;		  )
;	      (if (= land 8) ;DE-HE
;		  (fronleichnam tag monat jahr))
;	      (if (= land 9) ;DE-MV
;		  (reformationstag tag monat jahr))
;	      (if (= land 10) ;DE-NI
;		  )
;	      (if (= land 11) ;DE-NW
;		  (or 
;		   (fronleichnam tag monat jahr)
;		   (allerheiligen tag monat jahr)))
;	      (if (= land 12) ;DE-RP
;		  (or 
;		   (fronleichnam tag monat jahr)
;		   (allerheiligen tag monat jahr)))
;	      (if (= land 13) ;DE-SL
;		  (or 
;		   (fronleichnam tag monat jahr)
;		   (mariaHimmelfahrt tag monat jahr)		   
;		   (allerheiligen tag monat jahr)
;		   ))
;	      (if (= land 14) ;DE-SN
;		  (or
;		   (fronleichnam tag monat jahr)
;		   (reformationstag tag monat jahr)
;		   (buszUndBetTag tag monat jahr)
;		   ))
;	      (if (= land 15) ;DE-ST
;		  (or
;		   (heiligeDreiKoenige tag monat jahr)
;		   (reformationstag tag monat jahr)
;		   ))
;	      (if (= land 16) ;DE-SH
;		)
;	      (if (= land 17) ;DE-TH
;		  (or
;		   (fronleichnam tag monat jahr)
;		   (reformationstag tag monat jahr)
;		   ))
;	      )
;	  )
  
  (let* 
      (
       ;;Variablen
       (auswahlMaske (car (gimp-selection-save image)))
       (auswahlHoehe (- (nth 4 (gimp-selection-bounds image))
			     (nth 2 (gimp-selection-bounds image))))
       (auswahlBreite (- (nth 3 (gimp-selection-bounds image))
			      (nth 1 (gimp-selection-bounds image))))
       (monatString '())
       (woTagString '())
       (tagString '("NULL" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"
			 "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24"
			 "25" "26" "27" "28" "29" "30" "31"))
       (tim (tageImMonat monat jahr))
       (wim (+ (wocheDesMonats tim monat jahr wochenanfang) 1))
       (textBreite )

       ;;ein neuer Layer wird erstellt
       (textBG (car (gimp-layer-new image 
					 (car (gimp-image-width image)) 
					 (car (gimp-image-height image))
					 1
					 "Text-BG" 
					 60 
					 0
					 )
			 )
	     )
       ;;eine Kanalmaske (alpha-Kanal) wird erstellt
       (textBG-alpha (car (gimp-layer-create-mask textBG 2)))
       ;;Die Textebene wird erstellt
       (textLayer (car (gimp-layer-new image 
					    (car (gimp-image-width image)) 
					    (car (gimp-image-height image))
					    1
					    "Text" 
					    100 
					    0
					    )
			    )
	     )
       (textbreite 0)
       (links 0)
       (oben 0)
       )

    (define (druckeMonatWochenLayout tag monat jahr image meinFont 
			 feiertage land wochenanfang)
      ;;druckt die MonatsÃ¼besicht in Wochen aufgeteilt
      ;;die ursprÃ¼ngliche Auswahlmaske wird wieder geladen
      (if (<= tag tim)
	  (begin
	    (feiertag tag monat jahr feiertage land)
	    (gimp-selection-load auswahlMaske)
	    (set! textBreite (car (gimp-text-get-extents-fontname 
				   (nth tag tagString)
				   (div auswahlHoehe 9)
				   0 meinFont)))
	    (gimp-floating-sel-anchor 
	     (car (gimp-text-fontname image textLayer 
				      (- (+ (nth 1 
						 (gimp-selection-bounds image))
					    (* (/ (* auswahlBreite 0.02) 2))
					    (* (/ (* auswahlBreite 0.98) 7) 
					       (- 
						(if (= 0 wochenanfang)
						    (tagDerWoche
						     tag monat jahr)
						    (tagDerWoche* 
						     tag monat jahr 
						     wochenanfang)) 
						(- wochenanfang)
						0.5)))
					 (/ textBreite 2))
				      (+ (nth 2 (gimp-selection-bounds image)) 
					 (/ auswahlHoehe (- 12 wim))
					 (* auswahlHoehe 
					    (/ 2 (* 3 (- wim 1)))
					    (if (= 0 wochenanfang)
						(+ (wocheDesMonats 
						    tag monat jahr) 1)
						(+ (wocheDesMonats*
						    tag monat jahr
						    wochenanfang)  
						   1))
					    (/ 4 5))) 
				      (nth tag tagString)
				      0 1 (div auswahlHoehe 9) 0 meinFont)))
	    (druckeMonatWochenLayout (+ tag 1) monat jahr image meinFont 
				     feiertage land wochenanfang))
	  ))

    (define (druckeMonat tag monat jahr image meinFont feiertage land 
			 wochenanfang layout)
      ;;druckt die Monatsübesicht in zwei Spalten oder Zeilen
      
      (let* 
	  (
	   (textGroesse 0)
	   ;; tam gibt das gleiche aus, wie (aufgerundet (/ tim 2))
	   (tam (- tim (div tim 2)))
	   )
	(if (<= tag tim)
	    (begin
	      (feiertag tag monat jahr feiertage land)
	      (gimp-selection-load auswahlMaske)
	      (if (= 1 layout)
		  (begin
		    (set! textGroesse (div auswahlHoehe (+ 2 tam)))
		    (set! textBreite (car (gimp-text-get-extents-fontname 
					   (nth tag tagString)
					   textGroesse 0 meinFont)))
		    (if (<= tag tam)
			(begin 
			  (set! links (/ auswahlBreite 4))
			  (set! oben (* (* (/ auswahlHoehe tam) 
					   0.98) (- tag 1))))
			(begin
			  (set! links (+ (/ auswahlBreite 4) 
					 (/ auswahlBreite 2)))
			  (set! oben (* (* (/ auswahlHoehe tam) 
					   0.98) (- (- tag 1) 
						    tam)))
			  )
			)
		    )
		  (begin 
		    (set! textGroesse (/ auswahlHoehe 2.5))
		    (set! textBreite (car (gimp-text-get-extents-fontname 
					   (nth tag tagString)
					   textGroesse
					   0 meinFont)))
		    
		    (if (<= tag tam)
			(begin 
			  (set! links (* (/ auswahlBreite tam) (- tag 0.5)))
			  (set! oben 0))
			(begin
			  (set! links (* (/ auswahlBreite tam) (- (- tag 0.5) tam)))
			  (set! oben (div auswahlHoehe 2))
			  )
			)
		    ))
	      (gimp-floating-sel-anchor 
	       (car (gimp-text-fontname image textLayer 
					(+ (nth 1 (gimp-selection-bounds 
						   image))
					   (- links (div textBreite 2)))
					(+ (nth 2 (gimp-selection-bounds 
						   image)) oben)
					(nth tag tagString)
					0 1 textGroesse 0 meinFont)))
	      
	      (druckeMonat (+ tag 1) monat jahr image meinFont feiertage 
			   land wochenanfang layout)
	      ))))
    
    (define (druckeWochentage nummer image meinFont feiertage wochenanfang)
      (if (< nummer 8)
	  (begin
	    (gimp-selection-load auswahlMaske)
	    (set! textBreite (car (gimp-text-get-extents-fontname 
				   (nth nummer woTagString)
				   (div auswahlHoehe 9)
				   0 meinFont)))
	    (gimp-floating-sel-anchor 
	     (car (gimp-text-fontname image textLayer 
				      (- (+ (nth 1 (gimp-selection-bounds 
						    image))
					    (* (/ (* auswahlBreite 0.02) 2))
					    (* (/ (* auswahlBreite 0.98) 7) 
					       (- (mod nummer 
						       (* wochenanfang 7))
						  (- wochenanfang)
						  0.5)))
					 (/ textBreite 2))
				      (+ (nth 2 (gimp-selection-bounds image)) 
					 (/ auswahlHoehe (- 12 wim))
					 (* auswahlHoehe 
					    (/ 2 (* 3 (- wim 1)))
					    (/ 4 5))) 
				      (nth nummer woTagString)
				      0 1 (div auswahlHoehe 9) 0 meinFont)))
	    (druckeWochentage (+ nummer 1) image meinFont feiertage 
			      wochenanfang)
	    )
	  ())
      )
    
    (cond 
     ((= sprache 0)
	(set! monatString '("NULL" "Januar" "Februar" "März" "April" "Mai" 
			"Juni" "Juli" "August" "September" "Oktober" 
			"November" "Dezember"))
	 (set! woTagString '("NULL" "Mo" "Di" "Mi" "Do" "Fr" "Sa" "So"))
	 )
     ((= sprache 1)
      (set! monatString '("NULL" "January" "February" "March" "April" "May" 
			  "June" "July" "August" "September" "October" "November" 
			  "December"))
      (set! woTagString '("NULL" "Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"))
	)
     ((= sprache 2)
      (set! monatString '("NULL" "Gener" "Febrer" "Març" "Abril" "Maig" 
			  "Juny" "Juliol" "Agost" "Setembre" "Octubre" "Novembre" 
			  "Desembre"))
      (set! woTagString '("NULL" "Dl" "Dm" "Dx" "Dj" "Dv" "Ds" "Dg"))
        )
     ((= sprache 3)
      (set! monatString '("NULL" "Janvier" "Février" "Mars" "Avril" "Mai" 
			  "Juin" "Juillet" "Août" "Septembre" "Octobre" "Novembre" 
			  "Décembre"))
      (set! woTagString '("NULL" "Lu" "Ma" "Me" "Je" "Ve" "Sa" "Di"))
	)      
    )
 
    (gimp-palette-set-foreground hintergrund)
    ;;Layer wird dem Bild hinzugefügt
    (gimp-image-add-layer image textBG 0)
    ;;der neue Layer wird innerhalb der selection Weiß gefüllt
    (gimp-drawable-fill textBG 0)
    (gimp-palette-set-foreground old-fg)
    ;;und textBG hinzugefügt
    (gimp-layer-add-mask textBG textBG-alpha)
    ;;textBG-alpha wird Weiß gefüllt, die Auswahl mit schwarz
    (gimp-drawable-fill textBG-alpha 0)
    (gimp-edit-fill textBG-alpha 1)
    ;;die Auswahl wird entfernt
    (gimp-selection-none image)
    ;;danach wird der Gauss-Filter angewendet
    (plug-in-gauss-rle 1 image textBG-alpha (/ auswahlBreite 20) 1 1)
    
    ;;textLayer wird dem Bild hinzugefügt
    (gimp-image-add-layer image textLayer 0)
    ;;textLayer wird durchsichtig gefüllt
    (gimp-drawable-fill textLayer 3)
    ;;die ursprüngliche Auswahlmaske wird wieder geladen
    (gimp-selection-load auswahlMaske)
    ;;die Breite des zu schreibenden Monatsnamens wird bestimmt
    (set! textBreite (car (gimp-text-get-extents-fontname 
			   (nth monat monatString)
			   (div auswahlHoehe 5)
			   0 meinFont)))
    (if (= layout 0)
	;;Monatsname und Wochentage werden nur bei Wochenansicht geschrieben
	(begin
	 ;;der Monatsname wird geschrieben
	 (gimp-floating-sel-anchor 
	  (car (gimp-text-fontname image textLayer 
				   (- (+ (nth 1 (gimp-selection-bounds image)) 
					 (/ (- (nth 3 (gimp-selection-bounds 
						       image))
					       (nth 1 (gimp-selection-bounds 
						       image)))
					    2))
				      (/ textBreite 2))
				   (nth 2 (gimp-selection-bounds image)) 
				   (nth monat monatString)
				   0 1 (div auswahlHoehe 5) 0 meinFont)))
	 (druckeWochentage 1 image meinFont feiertage wochenanfang)
	 (druckeMonatWochenLayout 1 monat jahr image meinFont feiertage land 
				  wochenanfang)
	 )
	(druckeMonat 1 monat jahr image meinFont feiertage land 
			   wochenanfang layout)
	)
   
    ;;die ursprüngliche Auswahlmaske wird wieder geladen
    (gimp-selection-load auswahlMaske)
    (gimp-image-remove-channel image auswahlMaske)
    ;;Cleanup
    (gimp-palette-set-background old-bg)
    (gimp-palette-set-foreground old-fg)
    (gimp-image-set-active-layer image drawable)
    (gimp-image-undo-group-end image)
    (gimp-displays-flush)))
  )

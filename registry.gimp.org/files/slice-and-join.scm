; Studio Tecnico Arch. Giuseppe Conte  
; via Roma, 28
; 72026 - San Pancrazio Salentino (BR) - Italy
;
; Plugin  : slice-and-join.scm
; Author  : Arch. Giuseppe Conte 
; Update  : Single Tech Games, Anonymous & Teseracto
; Date    : 03 marzo 2009 - San Pancrazio Salentino (BR)
; Revision: 23 mayo 2012
;
; Version : 1.1
; Last version at: http://xoomer.virgilio.it/lwcon/gimp/
; Help guide at  : http://xoomer.virgilio.it/lwcon/gimp/scripts/image-slice-and-join.htm
;
; Description: 
; Slice: subdivide the active image in M x N Rows and Columns and save any rectangular portion in the new files.
;				Slice save into file *.rcm all the data necessary in order to reconstruct the image
; Join: read data from *.rcm file and reconstructs an image subdivided with image-slice script-fu.
;
; File *.rcm: is structured in rows 
;				sample: 
;				row 1 : "ATGrcm"
;				row 2 : imageWidth
;				row 3 : imageHeight
;				row 4 : number cols
;				row 5 : number rows
;				row 6 : filename portion 1,1 
;				row 7 : filename portion 1,2
;				row 8 : filename portion 2,1 
;				row 9 : filename portion 2,2
;				row 10: ........  
; 
; -----------------------------------------------------------------------------
;
; License:
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
; -----------------------------------------------------------------------------
;
; Define the function:

(define (script-fu-image-slice inImage inLayer nRighe nColonne option dataDir)

	(let* (	(orFilename (car (gimp-image-get-filename inImage)))
					(orWidth (car (gimp-image-width inImage)))
					(orHeight (car (gimp-image-height inImage)))
					(newW (/ orWidth nColonne))
					(newH (/ orHeight nRighe))
					(name-length 0)
					(inizio 0)
					(extension "")
					(type 0)
					(contaR 0)
					(contaC 0)
					(inXorig 0)
					(inYorig 0)
					(inWidth 0)
					(inHeight 0)
					(cnt 0)
					(filename "")
					(newimage 0)
					(newlayer 0)
					(activelayer 0)
					(floating-sel 0)  
					(colonna "")
					(riga "")
					(post "")
					(joinsaveFile "")
					(nf 0)
					(rcmFile "")
					(filenamex "")
					(contador "")
				)


	;; salva il file origine se non è stato salvato 
	;; imposta il nome del file per il salvataggio dei dati
	;; imposta il nome del file per il salvatggio dei vari pezzi

	(if (= (string-length orFilename) 0) 
		(begin
			(set! orFilename "image.xcf")
			(gimp-file-save 1 inImage inLayer orFilename orFilename)
			(set! rcmFile (string-append dataDir "\\" orFilename ".rcm"))
			(set! filename (string-append dataDir "\\" orFilename ))
		);end begin

		(begin
			(set! rcmFile (string-append dataDir "\\" (solonomefile orFilename) ".rcm"))
			(set! filename (string-append dataDir "\\" (solonomefile orFilename) ))
		);end begin
	);end if
	

;;determina l'estensione del file origine
	(set! name-length (string-length orFilename))
	(set! inizio (- name-length 4))
	(set! extension (substring orFilename inizio name-length))

;;fine estensione file
	
;;leggo le dimensioni dell'immagine
	(set! inHeight (car (gimp-image-height inImage)))
	(set! inWidth (car (gimp-image-width inImage)))

;;apro il file per la scrittura delle informazioni di recupero
	 (set! nf (open-output-file rcmFile))
	 ;scrittura signature, dimensioni immagine numero di righe e colonne
	 (write "ATGrcm" nf)
	 (newline nf)
	 (write inWidth nf)
	 (newline nf)
	 (write inHeight nf)
	 (newline nf)
	 ;(write inWidth nf)
	 ;(newline nf)	 
	 (write nColonne nf)
	 (newline nf)
	 (write nRighe nf)


;;fine scrittura dati

;imposta l'estensione se il file deve essere salvato in un altro formato

(cond
	( (= option 1)	(set! extension ".png"))
	( (= option 2)	(set! extension ".jpg"))
	( (= option 3)	(set! extension ".bmp"))
	( (= option 4)	(set! extension ".tif"))
	( (= option 5)	(set! extension ".xcf"))		
)

;determina il tipo di immagine (RGB, GRAY, INDEXED)
(set! type (car (gimp-image-base-type inImage)))
	  
;;imposto il ciclo per la suddivisione dell'immagine
(set! contaR 1)
(set! contaC 1)
(set! inXorig 0)
(set! inYorig 0)
(set! inWidth newW)
(set! inHeight newH)
(set! cnt 1)

;;imposta percorso e nome file per il salvataggio ******-> Disattivo la linea seguente
;(set! filename (car (gimp-image-get-filename inImage)))
	  
(while ( <= contaR nRighe)
	
	(set! inWidth newW)
	(set! inHeight newH)
	
	(while ( <= contaC nColonne)
		
;seleziona e copia una porzione dell'immagine
  		(gimp-rect-select inImage inXorig inYorig inWidth inHeight REPLACE FALSE 0)
  		(gimp-edit-copy inLayer)

;imposta la nuova immagine ed incolla la selezione
		(set! newimage (car (gimp-image-new inWidth inHeight type)))
		(set! newlayer (car (gimp-layer-new newimage  inWidth inHeight type "Sfondo" 0 NORMAL)))
    (gimp-image-add-layer newimage newlayer 0)
    (gimp-drawable-fill newlayer BG-IMAGE-FILL)(gimp-layer-add-alpha newlayer) (gimp-edit-clear newlayer)
		(set! activelayer (car (gimp-image-set-active-layer newimage newlayer)))
		(set! floating-sel (car (gimp-edit-paste newlayer FALSE)))
      (gimp-floating-sel-anchor floating-sel)

;;imposta ed assegna un nome al nuovo file (nomefile-riga-colonna)
	  ;(set! filename (car (gimp-image-get-filename inImage))) 
	  (set! contador (number->string cnt)) 
	  (set! cnt (+ cnt 1)) 
	  ;(set! colonna (number->string contaC))
	  ;(set! riga (number->string contaR))
	  (set! post "")
	  ;(set! post (string-append riga colonna contador))
	  (set! post (string-append contador))
	  (set! filename (filename-basename filename))

	  (set! filenamex (string-append filename post extension))

	  (gimp-image-set-filename newimage filenamex)	  
	  (gimp-file-save 1 newimage newlayer filenamex filenamex)

	 ;salva il nome del file sul file *.rcm
	 (newline nf) 
	 (write (solonomefile filenamex) nf)

	(set! inXorig (+ inXorig newW))  ;origine x rettanglo di selezione
	(set! contaC (+ contaC 1))

	);end while col
	
	(set! contaC 1)
	(set! inYorig (+ inYorig newH))
	(set! inXorig 0)
	(set! contaR (+ contaR 1))
	
);end while row

	(close-output-port nf)
  (gimp-displays-flush)
  (gimp-selection-none inImage)
  
  );let
);def

(script-fu-register
 "script-fu-image-slice"
 _"<Toolbox>/Xtns/ATG/Image slice"
 "Slice the image in MxN Rows and Columns and save any rectangular portion in the new files."
 "Arch. Giuseppe Conte"
 "2009, Conte Giuseppe"
 "20 marzo, 2009 - Ver. 1.0"
 "RGB* GRAY* INDEXED*"
 SF-IMAGE "The Image" 0
 SF-DRAWABLE "The Layer" 0
 SF-ADJUSTMENT "Rows   " '(3 0 99999999 1 10 0 1)
 SF-ADJUSTMENT "Columns" '(3 0 99999999 1 10 0 1)
 SF-OPTION "Save as" '("Default" ".png" ".jpg" ".bmp" ".tif" ".xcf")
 SF-DIRNAME "Salva in " (string-append "" gimp-data-dir "/" )
  
)

;;Single tech games: this function cuts the extension from the filename, esta funcion corta la extension del nombre de archivo
(define (filename-basename filename)
(car (strbreakup filename ".")))

;;;
;;; Join Image

;;funzioni di servizio
;;; Estrae il solo percorso
;;; funziona




(define (solopath x)
	(let* ( (cnt 0)
					(lung 0)
					(fine 0)
				)
	(set! lung (string-length x))
	(while (< cnt lung)
				(if (equal? (substring x cnt (+ cnt 1)) "\\") (set! fine cnt) )
				(set! cnt (+ cnt 1))
	);while
	;(gimp-message (number->string fine))
	(substring x 0 (+ fine 1))
	
	);let

);def


(define (solonomefile x)
	(let* ( (cnt 0)
					(lung 0)
					(fine 0)
				)
	(set! lung (string-length x))
	(while (< cnt lung)
				(if (equal? (substring x cnt (+ cnt 1)) "\\") (set! fine cnt) )
				(set! cnt (+ cnt 1))
	);while
	;(gimp-message (number->string fine))
	(substring x (+ fine 1) lung)
	
	);let

);def


;;fine funzioni di servizio

(define (script-fu-image-join joinFile)
	(let* (	(signature "ATGrcm")
					(imageHeight 0)
					(imageWidth 0)
					(nRows 0)
					(nCols 0)
					(tearFile "")
					(openFile "")
					(lettura "")
					(imageID 0)
					(newlayer 0)
					(activelayer 0)
					(cntRow 1)
					(cntCol 1)
					(tempFilename "")
					(activePath "")
					(rawFilename "")
					(addlayer 0)
					(startx 0)
					(starty 0)
					(layerWidth 0)
					(layerHeight 0)
				)

		;(gimp-message joinFile)
		(set! activePath (solopath joinFile))
		;(gimp-message activePath)
		
		(set! openFile (open-input-file joinFile ))
		(set! lettura (read openFile))
		
		(cond
			(	(equal? signature lettura )
				(set! lettura (read openFile))
				(set! imageWidth lettura)
				(set! lettura (read openFile))
				(set! imageHeight lettura)
				(set! imageID (car (gimp-image-new imageWidth imageHeight 0)))
				(set! newlayer (car (gimp-layer-new imageID  imageWidth imageHeight 1 "Sfondo" 1 0)))
    		(gimp-image-add-layer imageID newlayer 0)
				(set! activelayer (car (gimp-image-set-active-layer imageID newlayer)))
      	
      	(gimp-display-new imageID)
      	(gimp-displays-flush)
      	
      	;apro le immagini una ad una, leggo le dimensioni le copio e le incollo
      	
      	;leggo il numero di righe e di colonne
      	(set! nCols (read openFile))
      	(set! nRows (read openFile))
      	(set! cntRow 0)
      	(set! cntCol 0)
      	
      	(while (< cntRow nRows)
      	
      		(while (< cntCol nCols)
      			(set! tempFilename (read openFile))
      			(set! rawFilename (string-append activePath tempFilename))
      			;apro l'immagine e l'aggiungo come layer
      			(set! addlayer (car (gimp-file-load-layer 1 imageID rawFilename)))
      			(gimp-image-add-layer imageID addlayer -1)
      			(gimp-layer-translate addlayer startx starty)
      			(set! layerHeight (car (gimp-drawable-height addlayer)))
      			(set! layerWidth (car (gimp-drawable-width addlayer)))
      			(set! startx (+ startx layerWidth))
      			
      			(set! cntCol (+ cntCol 1))
      		);end while colonne
      		
      		(set! startx 0)
      		(set! starty (+ starty layerHeight)) 
      		(set! cntRow (+ cntRow 1))
      		(set! cntCol 0)
      	
      	);end while righe
      	
      	;(gimp-display-new imageID)
				(gimp-displays-flush)
				
			); cond 1
			
			(	else (gimp-message "File errato")
			); cond 2
		);cond
		
		(close-input-port openFile)


	);let
);def


(script-fu-register
 "script-fu-image-join"
  _"<Toolbox>/Xtns/ATG/Image join"
 "It reconstructs an image subdivided with image-slice script-fu."
 "Arch. Giuseppe Conte"
 "2009, Conte Giuseppe"
 "20 marzo, 2009 - Ver. 1.0"
	""
 SF-FILENAME "Apri file " (string-append "" gimp-data-dir "*.rcm" )
 
)
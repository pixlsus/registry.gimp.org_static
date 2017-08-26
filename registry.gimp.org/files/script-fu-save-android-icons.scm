; --------------------------------------------------------------------------
; Save Android Icons 
; Copyright (C) 2011 Goran Siric ( http://www.izvornikod.com )
; All rights reserved
;
; Based on script "Phoca Save Icons" maded by Jan Pavelka ( http://www.phoca.cz )

; This script is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; The GNU General Public License can be found at
; http://www.gnu.org/copyleft/gpl.html.
; --------------------------------------------------------------------------

; Update 06.02.2012
; - added support for XHDPI screens
; - added support for action bar icons 
; - added support for custom icon sizes

(define (script-fu-save-android-icons 
		iconType 
		customW_mdpi
		customH_mdpi
		saveMode
		useNamingConvention 
		image 
		folder 
		name 
		interpolation 
		interlace 
		compression 
		bKGD 
		gAMA 
		oFFs 
		pHYs 
		tIME 
		comment 
		svtrans)

(let* (
	(newImage 0)
	(newDraw 0)
	(newName "")
	(rawName "")
	(y 0)
	(partName "")
	(stdFolderName "")
	(formatsW (cons-array 4 'byte))
	(formatsH (cons-array 4 'byte))
	(namePrefix "")
	(useNamePrefix "")
	(fS 0)
	(customW_ldpi (round (* customW_mdpi 0.75) ))
	(customW_hdpi (round (* customW_mdpi 1.5) ))
	(customW_xhdpi (round (* customW_mdpi 2.0) ))
	(customH_ldpi (round (* customH_mdpi 0.75) ))
	(customH_hdpi (round (* customH_mdpi 1.5) ))
	(customH_xhdpi (round (* customH_mdpi 2.0) ))
)

;	"Android icons type"		'("Launcher Icons 36/48/72/96" "Menu Icons" "Status Bar Icons" "Tab Icons" "Dialog Icons" "List View Icons")
;	"Android icons type"		'("Launcher Icons" "Menu Icons" "Action Bar Icons" "Status Bar Icons" "Tab Icons" "Dialog Icons" "List View Icons")

; "Launcher Icons" "Menu Icons"
(cond ( 
	(or (= iconType 0) (= iconType 1) )
	(aset formatsW 0 36)
	(aset formatsW 1 48)
	(aset formatsW 2 72)
	(aset formatsW 3 96)
	(aset formatsH 0 36)
	(aset formatsH 1 48)
	(aset formatsH 2 72)
	(aset formatsH 3 96)
	)
)

(cond (
	( or (= iconType 2) (= iconType 3) )
	(aset formatsW 0 18)
	(aset formatsW 1 24)
	(aset formatsW 2 36)
	(aset formatsW 3 48)
	(aset formatsH 0 18)
	(aset formatsH 1 24)
	(aset formatsH 2 36)
	(aset formatsH 3 48)
	)
)

(cond ( 
	(or (= iconType 4) (or (= iconType 5) (= iconType 6) ) )
	(aset formatsW 0 24)
	(aset formatsW 1 32)
	(aset formatsW 2 48)
	(aset formatsW 3 64)
	(aset formatsH 0 24)
	(aset formatsH 1 32)
	(aset formatsH 2 48)
	(aset formatsH 3 64)
	)
)

; if custom icon type is selected
(cond ( (= iconType 7)
	(aset formatsW 0 customW_ldpi)
	(aset formatsW 1 customW_mdpi)
	(aset formatsW 2 customW_hdpi)
	(aset formatsW 3 customW_xhdpi)
	(aset formatsH 0 customH_ldpi)
	(aset formatsH 1 customH_mdpi)
	(aset formatsH 2 customH_hdpi)
	(aset formatsH 3 customH_xhdpi)
 )
)

( cond ((= iconType 0 ) (set! namePrefix "ic_launcher_")) )
( cond ((= iconType 1 ) (set! namePrefix "ic_menu_")) )
( cond ((= iconType 2 ) (set! namePrefix "ic_menu_")) )
( cond ((= iconType 3 ) (set! namePrefix "ic_stat_notify_")) )
( cond ((= iconType 4 ) (set! namePrefix "ic_tab_")) )
( cond ((= iconType 5 ) (set! namePrefix "ic_dialog_")) )
( cond ((= iconType 6 ) (set! namePrefix "ic_")) )
( cond ((= iconType 7 ) (set! namePrefix "custom_")) )


(cond ( (= useNamingConvention TRUE) (set! useNamePrefix namePrefix) ))



(while (< y 4)

	( cond ( (= y 0) (set! stdFolderName "drawable-ldpi" )))
	( cond ( (= y 1) (set! stdFolderName "drawable-mdpi" )))
	( cond ( (= y 2) (set! stdFolderName "drawable-hdpi" )))
	( cond ( (= y 3) (set! stdFolderName "drawable-xhdpi" )))
	
	(set! newImage (car (gimp-image-duplicate image)))
	; set that all layers have same size as image
	(map (lambda (x) (gimp-layer-resize-to-image-size x)) (vector->list (cadr (gimp-image-get-layers newImage))))
	
	(gimp-image-merge-visible-layers newImage 0)
	(gimp-image-scale-full newImage (aref formatsW y) (aref formatsH y) interpolation)
	(set! newDraw (car (gimp-image-get-active-drawable newImage)))
		
	(cond ( (= saveMode 0)
		(set! newName (string-append folder "/" stdFolderName "/" useNamePrefix name  ".png"))
		(set! rawName (string-append useNamePrefix name "-" partName "0.png"))
		)
	)
	
	(cond ( (= saveMode 1)
		(set! partName (string-append (number->string (aref formatsW y)) "x" (number->string (aref formatsH y)) ))
		(set! newName (string-append folder "/" useNamePrefix name "_" partName ".png"))
		(set! rawName (string-append useNamePrefix name  ".png"))
		)
	)
	

	(file-png-save2 1 newImage newDraw newName rawName interlace compression bKGD gAMA oFFs pHYs tIME comment svtrans)

	(gimp-image-delete newImage)
	
	(set! y (+ y 1))
)


)
)

(script-fu-register	"script-fu-save-android-icons"
					"<Image>/Script-Fu/Android/Save Android Icons ..."
					"Save Icons For Android Platorm"
					"(c) Goran Siric ( http://www.izvornikod.com ) 2011"
					"License GPLv3"
					"22 September 2011"
					"RGB* GRAY* INDEXED*"
					SF-OPTION	"Android icons type"		'("Launcher Icons (36x36 48x48 72x72 96x96)" "Menu Icons  (36x36 48x48 72x72 96x96)" "Action Bar Icons (18x18 24x24 36x36 48x48)" "Status Bar Icons (18x18 24x24 36x36 48x48)" "Tab Icons (24x24 32x32 48x48 64x64)" "Dialog Icons (24x24 32x32 48x48 64x64)" "List View Icons (24x24 32x32 48x48 64x64)" "Custom Icon  mdpi=baseline ldpi=0.75x hdpi=1.5x xhdpi=2x")
					SF-ADJUSTMENT	"Custom Icon - mdpi width "			'(48 0 99999 1 10 0 1)
					SF-ADJUSTMENT	"Custom Icon - mdpi height"			'(48 0 99999 1 10 0 1)
					SF-OPTION	"Save mode"		'("Save to drawable-ldpi,mdpi,hdpi,xhdpi folders below Root folder" "Append resolution to file name")
					SF-TOGGLE	"Use android naming convention"					TRUE
					SF-IMAGE 	"Image"				0
					SF-DIRNAME	"Root folder"		""
					SF-STRING 	"Name" 				""
					SF-ENUM 	"Interpolation" 	'("InterpolationType" "cubic")
					SF-TOGGLE		"Use Adam7 interlacing"				FALSE
					SF-ADJUSTMENT	"Deflate Compression factor (0-9)"  	'(9 0 9 1 10 0 0)
					SF-TOGGLE		"Write bKGD chunk"						TRUE
					SF-TOGGLE		"Write gAMA chunk"						FALSE
					SF-TOGGLE		"Write oFFs chunk"						FALSE
					SF-TOGGLE		"Write pHYs chunk"						TRUE
					SF-TOGGLE		"Write tIME chunk"						TRUE
					SF-TOGGLE		"Write comment"						TRUE
					SF-TOGGLE		"Preserve color of transparent pixels" TRUE
					
)
; A Script-Fu Script that Opens all files in a directory ; Creates and store away a copy of the files , the filename of the copy
; has a users defined suffix
; "Scaled" , "Leveled " and "Sharpened"
; Written in June 2011

(define (script-fu-batch-sls globexp suffix ratio bsharpen bautolevel bsharpum1 bsharpum2 bsharpum3)

(let* ((filelist (cadr (file-glob globexp 1))))

(while (not (null? filelist))
     (let* ((fname (car filelist))
        
     (img (car (gimp-file-load RUN-NONINTERACTIVE fname fname))))
     (let* (
           (xdrawable   (car (gimp-image-active-drawable img)))
           (cur-width  (car (gimp-image-width img)))
           (cur-height (car (gimp-image-height img)))
                       (new-width  (* ratio cur-width))
                       (new-height (* ratio cur-height))
           (new_ratio      (min (/ new-width cur-width) (/ new-height cur-height)))
           (width      (* new_ratio cur-width))
           (height     (* new_ratio cur-height))
       )                  
       (gimp-image-undo-disable img)
       (gimp-image-scale-full img width height INTERPOLATION-LANCZOS)
; Sharpen if the user wants so
       (if (= bsharpen TRUE) (plug-in-sharpen RUN-NONINTERACTIVE img xdrawable 40))
; Level if the user wants so
       (if (= bautolevel TRUE) (gimp-levels-stretch xdrawable))
; Sharp with Mask if the user wants so
       (if (= bsharpum1 TRUE) (plug-in-unsharp-mask RUN-NONINTERACTIVE img xdrawable 30.0 0.7 4))
       (if (= bsharpum2 TRUE) (plug-in-unsharp-mask RUN-NONINTERACTIVE img xdrawable 60.0 1.0 6))
       (if (= bsharpum3 TRUE) (plug-in-unsharp-mask RUN-NONINTERACTIVE img xdrawable 10.0 0.5 3))

	  ;(gimp-message "Nu skall vi bara spara!")
       
       (gimp-file-save RUN-NONINTERACTIVE img xdrawable (string-append (car (strbreakup fname ".")) suffix) (string-append (car (strbreakup fname ".")) suffix))
		
       ;(gimp-message "Nu har vi sparat!")
	   
       ;(gimp-display-delete img) Lite synd att vi inte kan köra denna ... Måste testa på bibliotek med mycket filer så det inte blir overflow
     )
    (set! filelist (cdr filelist)))))
)


; Register in Gimp Menu
(script-fu-register "script-fu-batch-sls"
		    _"_Batch Scale/Level/Sharpen ..."
		    "Scale/Levele and Sharpen many Pictures, Save them in new files"
		    "Sven Tryding"
		    "2011, Sven Tryding"
		    "June, 2011"
		    ""
		    SF-STRING "Path" "C:\\Users\\Public\\Pictures\\*.jpg"
              SF-STRING "Suffix" "_small.jpg"

              SF-VALUE "Scaling ratio (min 0.01, max 5)" "0.30"
              SF-TOGGLE "Sharpen (40%)" TRUE
		    SF-TOGGLE "AutoLevel" TRUE
              SF-TOGGLE "Sharp With UnSharpMask 30 0.7 4 (Medium)" TRUE
              SF-TOGGLE "Sharp With UnSharpMask 60 1.0 6 (Strong)" FALSE
              SF-TOGGLE "Sharp With UnSharpMask 10 0.5 3 (Light)" FALSE)

(script-fu-menu-register "script-fu-batch-sls"
			 "<Toolbox>/_Filters/_Script-Fu")


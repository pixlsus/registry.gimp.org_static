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
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; Purpose:
; Get GIMP version (so scripts can be backwards compatible to 2.6)
;
;
; Example usage:
;
;   ; Determine if GIMP version is greater than 2.6.X
;   (set! gimpVersionHigherThan2pt6ptX (isGimpVersionHigherThan2pt6ptX))
;
;   (if (= gimpVersionHigherThan2pt6ptX TRUE)
;      (begin
;        (gimp-image-insert-layer theImage ImageCopyLayer 0 -1) ; (gimp-image-insert-layer image layer parent position)
;      ) ; end begin condition is true
;      (begin
;      	; This works in 2.6.x but is deprecated in 2.7.x and higher.
;		   (gimp-image-add-layer theImage ImageCopyLayer -1)
;      ) ; end begin condition is false
;  	) ; end if
;
;
; Revision History
; 2012 05 06 - Rev 01
; 1) Moved GIMP version "get" function into its own script file.
; ===========================================================================



; ===========================================================================
; Function: getRelVerGIMP
; Returns:	3 values
;	1: numeric value of Release (i.e. 2, for GIMP 2.6.12)
;	2: numeric value of Version (i.e. 6, for GIMP 2.6.12)
;	3: numeric value of Minor Release (i.e. 12, for GIMP 2.6.12)
; ===========================================================================
(define (getRelVerGIMP
				; no input arguments
                    		)
   (let*(
   	; Local variable definitions
      (textMessage ())
      (relVerGimp ())			; Full GIMP version i.e. 2.6.12, 2.7.5, or 2.8.0
      (charRelGimp ())			; character value of Release (i.e. 2)
      (charVerGimp ())			; character value of Version (i.e. 6)
      (charMinorRelGimp ())	; character value of Minor Release (i.e. 12)
      (numRelGimp ())			; numeric value of Release (i.e. 2)
      (numVerGimp ())			; numeric value of Version (i.e. 6)
      (numMinorRelGimp ())		; numeric value of Minor Release (i.e. 12)
      (listReturnValues ())	; return values for function
  		) ; end of variable definitions


   ; Save the current release/version for GIMP into a string.
   (set! relVerGimp (car(gimp-version)))
   ; Parse the string into its character components.
   ;(set! listRelVerGimp (string->list relVerGimp))
   (set! charRelGimp (substring relVerGimp 0 1))
   (set! numRelGimp (string->number charRelGimp))
   (set! charVerGimp (substring relVerGimp 2 3))
   (set! numVerGimp (string->number charVerGimp))
   (set! charMinorRelGimp (substring relVerGimp 4 5))
   (set! numMinorRelGimp (string->number charMinorRelGimp))

	(if (= 1 2) ; change to (= 1 1) to get this to display
		(begin
         ; Debug display relver
      	(set! textMessage (string-append "GIMP RelVer " relVerGimp))

      	(set! textMessage (string-append textMessage " "))
      	(set! textMessage (string-append textMessage charRelGimp))
      	(set! textMessage (string-append textMessage " "))
      	(set! textMessage (string-append textMessage charVerGimp))
      	(set! textMessage (string-append textMessage " "))
      	(set! textMessage (string-append textMessage charMinorRelGimp))

      	(set! textMessage (string-append textMessage " "))
      	(set! textMessage (string-append textMessage (number->string numRelGimp)))
      	(set! textMessage (string-append textMessage " "))
      	(set! textMessage (string-append textMessage (number->string numVerGimp)))
      	(set! textMessage (string-append textMessage " "))
      	(set! textMessage (string-append textMessage (number->string numMinorRelGimp)))
      	(gimp-message textMessage)
      ) ; end begin
	) ; end if

   ; Return values
   (set! listReturnValues (list numRelGimp numVerGimp numMinorRelGimp))

   ) ; end of let

) ; end define getRelVerGIMP

; ===========================================================================
; Function: isGimpVersionHigherThan2pt6ptX
; Returns:	1 value
;	TRUE	if GIMP is greater than 2.6.X
;	FALSE if GIMP is less than or equal to 2.6.X
; ===========================================================================
(define (isGimpVersionHigherThan2pt6ptX
				; no input arguments
                    		)
   (let*(
   	; Local variable definitions
      (gimpVersionHigherThan2pt6ptX FALSE)
      (numRelGimp ())				; numeric value of Release (i.e. 2)
      (numVerGimp ())				; numeric value of Version (i.e. 6)
      (numMinorRelGimp ())			; numeric value of Minor Release (i.e. 12)
      (listRelVerMinorGIMP ())	; return values (list) from function getRelVerGIMP
      (listVerMinorGIMP ())		; second to end elements of list, to get Version
      (listMinorGIMP ())			; third  to end elements of list, to get Minor Release
      (returnValue ())				; return value for function
  		) ; end of variable definitions

   ; Get GIMP Release/Version/Minor Version.
   (set! listRelVerMinorGIMP (getRelVerGIMP))

   ; Extract the individual values.
   (set! numRelGimp (car listRelVerMinorGIMP))			; first element is Release
   ;(gimp-message (number->string numRelGimp))

   ; Get the second element to the end of the list.
   (set! listVerMinorGIMP (cdr listRelVerMinorGIMP))
   (set! numVerGimp (car listVerMinorGIMP))				; second element is Version
   ;(gimp-message (number->string numVerGimp))

   ; Get the third element to the end of the list.
   (set! listMinorGIMP (cdr listVerMinorGIMP))
   (set! numMinorRelGimp (car listMinorGIMP))			; third element is Minor Release
   ;(gimp-message (number->string numMinorRelGimp))

	; Determine whether or not the current release is
	; greater than 2.6.X
   (set! gimpVersionHigherThan2pt6ptX FALSE)
   (if (>= numRelGimp 2) ; Test for greater than or equal to
   	(begin
         (if (> numVerGimp 6)
         	(begin
         		(set! gimpVersionHigherThan2pt6ptX TRUE)
            ) ; end begin condition is true
         	(begin
         		; If Release is greater than, then this is higher than 2.6.X
         		(if (> numRelGimp 2)
               	(begin
               		(set! gimpVersionHigherThan2pt6ptX TRUE)
                  ) ; end begin condition is true
         		) ; end if
            ) ; end begin condition is false
        	) ; end if
      ) ; end begin condition is true
  	) ; end if

   ; Return value
   (set! returnValue gimpVersionHigherThan2pt6ptX)

   ) ; end of let

) ; end define isGimpVersionHigherThan2pt6ptX


; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; @@@  This GIMP LayerGroups subsystem has been developed     @@@
; @@@  in 2008/2009 by Miguel Oliveira (melocotone at gmail   @@@
; @@@  dot com) based on initial work done by Joseph Miller   @@@
; @@@  (josephcmiller2 at gmail dot com)                      @@@
; @@@               Version 1.0a, June 30, 2009               @@@
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;-----------------------------------
; GIMP LayerGroups Subsystem
; This script will not work with older versions of GIMP (versions prior to version 2.4)
; because it makes use of language features only available in the new scheme interpreter
; used with newer versions of GIMP
; ---------------------------------------------------------------------------------------
; Version changes:
;----------------------------------------------------------------------------------------
; Version 1.0a, June 30, 2009
;  - Corrected bugs when moving layer groups above and below a reference layer.
;    The order of the moved layers was being reversed. Now they mantain the original stack order. 
;  -  Added the following new capabilities:
;     a) Reverse layer stack order of a layer group
;     b) Reverse layer stack order of a group of linked layers
;     c) Delete a group of linked layers
;  -  Added the option to place the LayerGroups subsystem under the GIMP layer menu instead of directly at the Image menu, 
;     by just changing one character in the script (change '#f' to '#t' at line 35 of this script 
;     (at the line immediately AFTER the line beginning with '(define pub_layer-groups-in-GIMP-layer-menu'    )
;----------------------------------------------------------------------------------------
; Initial version: Version 1.0, June 4, 2009
; ---------------------------------------------------------------------------------------




;=============================================================================
(define pub_layer-groups-in-GIMP-layer-menu 
#f                     ; <<<<<<<<< change '#f' to '#t' if you want the layergroups subsystem to appear under the 'Layer' menu of GIMP
) 
;=============================================================================

; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  GENERAL PURPOSE FUNCTIONS    &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

;-----------------------------------
(define (layerNameToSearchSize currLayerName layerGroupName)
; substrings the name of the current layer to a name the same size as the passed layerGroupName
	(let*
		(
			(lenToMatch (string-length layerGroupName))
			(lenCurrLayer (string-length currLayerName) )
			(currName "")
		)
		(if (>= lenCurrLayer lenToMatch)
			(begin
				(set! currName (substring currLayerName 0 lenToMatch)) ; returns currName size-limited to the size of the layerGroupName
			)
			; (begin
				; (set! currName "")
			; )
		)


	);let
)


; --------------------
(define (getTimeStamp )
; @@@ Returns a string with a time stamp in the format "YYYY-MM-DD hh:mm:ss.microsecs" like "2009-02-27 11:28:53.281250" 
; @@@ Other Usefull Time related functions
; @@@ (time)
; @@@   Returns the current local time, as a list of integer containing:
; @@@   (year month day-of-month hour min sec milliseconds)
; @@@    The year is expressed as an offset from 1900.
; @@@ (gettimeofday)
; @@@    Returns a list containing the number of seconds from the beginning of the day, and microseconds within the current second.
; @@@ (usleep microseconds)
; @@@    microseconds: integer
	(let*
		(
			
			(aTime (time)) ; (year month day-of-month hour min sec milliseconds)
							;  0      1       2        3    4   5     6
			(iY 0) (iM 0) (iD 0) (iH 0) (imi 0) (iss 0) (ims 0)
			
			;..(iTimeOfDay (car (gettimeofday)))
			(iMicroSecs (car (cdr(gettimeofday))))
			
			(sTime "")
		)
		(set! iY (car aTime) ) (set! aTime (cdr aTime) )
		(set! iM (car aTime) ) (set! aTime (cdr aTime) )
		(set! iD (car aTime) ) (set! aTime (cdr aTime) )
		(set! iH (car aTime) ) (set! aTime (cdr aTime) )
		(set! imi (car aTime) ) (set! aTime (cdr aTime) )
		(set! iss (car aTime) ) (set! aTime (cdr aTime) )
		;...(set! ims (car aTime) )
		(set! ims iMicroSecs )
		(set! sTime (string-append "" (i2sfs (+ iY 1900) 4) "-" (i2sfs iM 2) "-"  
				(i2sfs iD 2) " " (i2sfs iH 2) ":" (i2sfs imi 2) ":" 
				(i2sfs iss 2) "." (i2sfs ims 6) 
				  ""))  ; 
		; --
		sTime ;
	)
)
;-----------------------------------------------
(define (waitMilisecs iMiliWait)
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@22
;@@@ wait the given amount of miliseconds by just looping around until the clock reaches
;@@@ the actual time + the number of miliseconds specified. This algoritm will not work 
;@@@ when the routine starts just before midnight and the miliseconds to wait mean that the next moment
;@@@ will be just after midnight. In this case the routine will finish immediatly without waiting the specified
;@@@ amount of time
;@@@ whil
	(let*
		(
			(ii 0)
			(iSecondsDay (car (gettimeofday)))
			(iMicroSecs (car (cdr(gettimeofday))))
			(iMiliMoment (+ (* iSecondsDay 1000 ) (/ iMicroSecs 1000) ) )
			(iNextMoment (+ iMiliMoment iMiliWait ) )
			(oOut '())
			
		)
		(if (>= iNextMoment 86400000)
			(begin
				(set! iNextMoment (- iNextMoment 86400000  ))
			)
			;---else
			(begin
			)
		); this algoritm will not work around midnight...
		;(set! oOut (cons iNextMoment oOut) )
		;(set! oOut (cons iMiliMoment oOut) )
		(while (< iMiliMoment iNextMoment )
			(set! iSecondsDay (car (gettimeofday)) )
			(set! iMicroSecs (car (cdr(gettimeofday))))
			(set! iMiliMoment (+ (* iSecondsDay 1000 ) (/ iMicroSecs 1000) )) 
			;(set! oOut (cons iMiliMoment oOut) )
			
		)
		;-----return value
		;...iMiliMoment 
		;...(set! oOut (reverse oOut) )
		oOut
	)
)
;-------------------------------------
(define (i2s iValue)  ;easyer to write than (number->string ....
; @@@ integer to string
	(let* (
			(ss (number->string iValue) )
		)
		ss 
	)
)
;-------------------------------------
(define (i2sfs iValue nDecimals)  ;easyer to write than (number->string ....
; @@@ integer to string fixed size
	(let* (
			(ss (number->string iValue) )
			(iLen (string-length ss))
			(ii 0)
		)
		(while (< iLen nDecimals)
			(set! ss (string-append "0" ss) )
			(set! iLen (+ iLen 1))
		)
		ss 
	)
)
(define (b2s isValue)
; @@ boolean to string conversion
	(let*
		(
			(ii 0)
			(sOut (iif isValue "#t" "#f"))
		)
		;-----return value
		sOut 
	)
)

; ===========================================
(define (replicateString iTimes sString )
	(let*
		(
			(sOut "")
			(iCount iTimes)
		)
		(while (> iCount 0)

			(set! iCount (- iCount 1) )
			(set! sOut (string-append sOut sString))
		)
		sOut


	);let
)
; ===========================================
(define (string-LeftOf sPattern sString )
	(let*
		(
			;(sOut "")
			(iCount 0)
			(sOut (car (strbreakup  sString sPattern)))
		)
		sOut

	);let
)
;---------------------------
(define (isTrue boolValue)
;@@@ just to avoid having to write (eq? xxx #t)
	(let* 
		(
			(isT #f )
		)
		(if (eq? boolValue #t) 
			(set! isT #t)
		)
		isT ;
	)
)
;---------------------------
(define (isFalse boolValue)
;@@@ just to avoid having to write (eq? xxx #f)
	(let* 
		(
			(isF #f )
		)
		(if (eq? boolValue #f) 
			(set! isF #t)
		)
		isF ;
	)
)
;---------------------------
(define (iif isCond xResultTrue xResultFalse)
;@@@ interactive IF, just like in calc / excell
	(let* 
		(
			(xResult #f )
		)
		(if (eq? isCond #t) 
			(set! xResult xResultTrue)
			;--else
			(set! xResult xResultFalse)
		)
		xResult ;
	)
)
;------------------------------------################################################### @#@#@#@#@#@#@#@#
(define (isSubstringMatch sStr iStartPos1 sSubStr iStartPos2 )
; @@@@@@ checks if string sSubStr (starting at pos iStartPos2) matches exactly the substring portion of sStr which starts at iStartPos1 os sStr.
; @@@@@@ returns #t if it matches, #f if not
	(let* 	(

				(iLen1 (string-length sStr))
				(iLen2 (string-length sSubStr))

				(cc1 "")
				(cc2 "")
				(isCont #t)
				(isMatch #t)
				(ii1 iStartPos1)
				(ii2 iStartPos2)
			)
	; ------
	(while (eq? isCont #t)
		(set! cc1 (string-ref sStr ii1))
		(set! cc2 (string-ref sSubStr ii2))
		(if (not (char=? cc1 cc2))
			(begin
				(set! isMatch #f)
				(set! isCont #f)
			)
		)
		;------------
		(set! ii2 (+ ii2 1))
		(if (>= ii2 iLen2)
			(set! isCont #f)
			
		)
		(set! ii1 (+ ii1 1))
		(if (>= ii1 iLen1)
			(set! isCont #f)
		)
	) ;//while
	isMatch ;// return value
	)
)
;-------------------------------
(define (isSubstring sStr sSub)
; @@@ returns true if sSub is a substring of sStr (or is equal)
	(let*
		(
			(ii 0)
			(isOut #f)
		)
		(set! ii (iPosSubstring sStr 0 sSub #f))
		(if (>= ii 0)
			(begin
				(set! isOut #t)
			)
			;---else
			(begin
			)
		)
		;-----return value
		isOut 
	)
)
;------------------------------------#####################################################<<<<<<<<<<<<<<<<<<<<<<<<<
(define (iPosSubstring sStr iStart sSub isRight2Left)
; @@@@@@ Returns the next position (after iStart) in string sStr where substring sSub can be found
; @@@@@ If isRight2Left==#t it moves from right to left when searching, otherwise from left to right. If not found returns -1
; @@@@@ If isRight2Left==#t and iStart<0 then this means: "start searching from the end of the string "
	(let* 	(
				
				(iLenStr (string-length sStr)) ; 
				(iLenSub (string-length sSub))
				(iLenMaxCheck (- iLenStr iLenSub)) ; // compare both strings only up to this position
				
				(isCont #t)
				(isMatch #f)
				(ii (min iStart (- iLenStr 1)))
				;(jj 0)
				(iPosOut -1)
				(iDelta 1)
			)
	; ------
	(if (eq? isRight2Left #t )
		(begin
			(set! iDelta -1 )
			(if (< iStart 0 ) ; -1 indicates start comparing from the end of the string
				(set! ii iLenMaxCheck )
				
			) ; endif
			(if (> ii iLenMaxCheck )
				(set! ii iLenMaxCheck )
				
			) ; endif
		)
		;---else
		(begin
		)
	) ; endif
	(if (< iLenMaxCheck 0)
		(set! isCont #f )
	)
	(if (< ii 0)
		(set! isCont #f )
	)
	;------------
	(while (eq? isCont #t)
		(set! isMatch  (isSubstringMatch sStr ii sSub 0 ))
		
		(if (eq? isMatch #t)
			(begin
				(set! iPosOut ii )
				(set! isCont #f )
			)
		)
		;------------
		(set! ii (+ ii iDelta))
		(if (> ii iLenMaxCheck)
			(set! isCont #f)
			;---else
			(if (< ii 0 )
				(begin
					(set! isCont #f)
				)
			) ; endif
		)
	) ;//while
	iPosOut ; return value
	)
)
;-----------------
(define (stringRightOf sStr sPattern)
	(let* 
		(
			(iLenStr (string-length sStr))
			(iLenPatt (string-length sPattern))
			(ii (iPosSubstring sStr 0 sPattern #f) )
			(sOut "")
			
		)
		(if (>= ii 0)
			(set! sOut (substring sStr (+ ii iLenPatt) iLenStr))
		)
		sOut ; return
	
	)
)
;-----------------
(define (stringLeftOf sStr sPattern)
	(let* 
		(
			(iLenStr (string-length sStr))
			;...(iLenPatt (string-length sPattern))
			(ii (iPosSubstring sStr (- iLenStr 1) sPattern #t) )
			(sOut "")
			
		)
		(if (>= ii 0)
			(set! sOut (substring sStr 0 ii))
		)
		sOut ; return
	
	)
)
;--------------------
(define (isStringMatch sStr1 sStr2)
; @@@@@ 
	(let* 
		(
			(ii 0 )
			(iLen1 (string-length sStr1))
			(iLen2 (string-length sStr2))
			(isEqual #f)
		)
		(if (= iLen1 iLen2)
			(begin
				(set! ii (iPosSubstring sStr1 0 sStr2 #f) )
				(if (= ii 0)
					(set! isEqual #t)
				)
			
			)
		)
		isEqual ;
	
	)


)
; ##############################################
; (substring string start end)
(define (sSubstring sStr iStart iLenSub )
;@@@ substring variant: bullet proof and working with substring length instead of last position 
;@@@ if <iLenSub> <0 then it means: from iStart up to the the length of <sStr>
	(let*
		(
			(ii 0)
			(iLen (string-length sStr))
			(iMaxSize (- iLen iStart))
			(iSize iLenSub)
			(sOut "")
		)
		
		(if (< iSize 0) ; means to the end of the string
			(set! iSize iMaxSize)
		)
		(if (and (> iMaxSize 0) (> iSize 0))
			(begin
				(if (> iSize iMaxSize )
					(begin
						(set! iSize iMaxSize)
					)
				) ; endif
				(set! sOut (substring sStr iStart (+ iStart iSize)) )
			)
			
		) ; endif
		sOut ; return
	)
)
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
(define (sReplaceSubString sStr sSub sSubNew isReplaceAll)
; @@@@ Replaces first found pattern (or all if isReplaceAll ==#t) sSub by sSubNew in string sStr. If no pattern is found, the 
; @@@ original sStr is returned without any modification
	(let* 
		(
			(ii 0 )
			(jj 0)
			
			(sOut "")
			(ss1 "")
			(ss2 "")
			(ss3 "")
			(iLenStr (string-length sStr))
			(iLenSub (string-length sSub))
			(iLenSubNew (string-length sSubNew))
			(isCont #t)
		)
		(while (isTrue isCont)
			(set! jj (iPosSubstring sStr ii sSub #f))
			(if (>= jj 0)
				(begin
					(set! ss1 (substring sStr ii jj) )
					(set! ss2 (substring sStr (+ jj iLenSub ) iLenStr) ) ; the rest of the string
					(set! sOut (string-append sOut ss1 sSubNew ) )
					
					(if (isTrue isReplaceAll)
						(begin
							
							(set! ii (+ jj iLenSub) )
						)
						;------else
						(set! isCont #f )
						
					)
				)
				;--else  sSub not found
				(begin
					(set! isCont #f )
					;...(set! jj iLenStr)
					(set! ss2 (substring sStr ii iLenStr) ) ; the rest of the string
					
					
				)
			)
		
		)
		(set! sOut (string-append sOut ss2 ) )
		sOut ;
	
	)


)
;------------------------------------#####################################################<<<<<<<<<<<<<<<<<<<<<<<<<
(define (subString-pos sStr sSub)
; @@@@@@ Returns the first position in string sStr where substring sSub can be found. If not found returns -1
	(let* 	(	
				(iPosOut (iPosSubstring sStr 0 sSub #f))
			)
	; ------
	iPosOut ; return value
	)
)
(define (sExtractDelimited sStr iStart  sPatt1 sPatt2)
; @@@@ Extracts a substring of sStr which is delimited by an opening pattern <sPatt1> and a closing pattern <sPatt2>. 
; @@@ Starts looking for the patterns, inside <sStr>, beginning at position <iStart> of <sStr>. Think of the patterns as oepning and closing parentesis
; @@@@ Takes care of situations wheren there are "parentesis" inside parentesis, see example.
; @@@@ If no pattern is found, it returns an empty string. If the patterns are found, it returns the "content" inside the delimiting patterns, thus exluding the delimiters
; @@@@ The delimiters can be any size, any text, for example "<OPEN>" and "<CLOSE>"
; @@@@@ example: (sExtractDelimited  sStr iStart  sPatt1 sPatt2)
; @@@@@ sStr='abcdefgh(ijkl(mnopq)rstu)vwxyz'   iStart=0  sPatt1="("  sPatt2=")"    --> This call must return 'ijkl(mnopq)rstu'
; @@@@@       0....:....1....:....2....:....3
; @@@@@ 
	(let* 
		(
			(aList (aExtractDelimitedFull sStr iStart  sPatt1 sPatt2))
			(sOut (car aList))
		)
		sOut ;
	)
)
;------------------------------------#####################################################<<<<<<<<<<<<<<<<<<<<<<<<<
(define (aExtractDelimitedFull sStr iStart  sPatt1 sPatt2)
; @@@@ Extracts a substring of sStr which is delimited by an opening pattern <sPatt1> and a closing pattern <sPatt2>. 
; @@@ Starts looking for the patterns, inside <sStr>, beginning at position <iStart> of <sStr>. Think of the patterns as oepning and closing parentesis
; @@@@ Takes care of situations wheren there are "parentesis" inside parentesis, see example.
; @@@@ If no pattern is found, it returns an empty string. If the patterns are found, it returns the "content" inside the delimiting patterns, thus exluding the delimiters
; @@@@ RETURNS a list containing the following elements: '( <extractedString> <iPosBeginFirstPattern> <iPosAfterSecondParamter> )
; @@@@ The delimiters can be any size, any text, for example "<OPEN>" and "<CLOSE>"
; @@@@@ example: (sExtractDelimited  sStr iStart  sPatt1 sPatt2)
; @@@@@ sStr='abcdefgh(ijkl(mnopq)rstu)vwxyz'   iStart=0  sPatt1="("  sPatt2=")"    --> This call must return 'ijkl(mnopq)rstu'
; @@@@@       0....:....1....:....2....:....3
; @@@@@ 
	(let* 	(
				(iLenStr (string-length sStr)) ; 
				
				(isCont #t)
				(isMatchPatt1 #f)
				(isMatchPatt2 #f)
				(ii iStart )
				;(jj 0)
				(iPosOut -1)
				(iDelta 1)
				(isRight2Left #f)
				;------
				(iLenPatt1 (string-length sPatt1))
				(iLenPatt2 (string-length sPatt2))
				(iPattLevel -1)
				(iPosPatt1 -1)
				(iPosPatt2 -1)
				(isIncr_ii #f)
				(sOut "")
				;------
				(aListOut '())
				(iPatt2After -1)
				(iPatt1Pos -1)
			)
	; ------
	
	;------------
	(while (eq? isCont #t)
		(if (< (- iLenStr ii) iLenPatt1) ; do we have enough chars left to compare with sPatt1 ?
			(set! isCont #f )
		)
		(if (isTrue isCont)
			(begin
				(set! isMatchPatt1  (isSubstringMatch sStr ii sPatt1 0 ))
				(set! isIncr_ii #t)
				(if (eq? isMatchPatt1 #t)
					(begin
						(set! iPattLevel (+ iPattLevel 1))
						(if (= iPattLevel 0)
							(begin
								(set! iPosPatt1 ii) ; remember the position of the found pattern 1
								(set! ii (+ ii iLenPatt1 ) ) ; correct for the length of patt1 (can be 5 long...) 
								(set! isIncr_ii #f)
							)
						)
					)
				)
				(if (>= iPattLevel 0) ; means we have already found left pattern, thus start looking for pattern 2
					(begin
						(if (< (- iLenStr ii) iLenPatt2) ; do we have enough chars left to compare with sPatt2 ?
							(set! isCont #f )
						)
						(if (isTrue isCont)
							(begin
								(set! isMatchPatt2  (isSubstringMatch sStr ii sPatt2 0 ))
								(if (eq? isMatchPatt2 #t)
									(begin
										(if (= iPattLevel 0) ; if we are at pattlevel 0, means we found the matching closing pattern
											(begin
												(set! iPosPatt2 ii) ; remember the position of the found pattern 2
												(set! isCont #f ) ; we are finished finding matching patterns (open/close)
												(set! isIncr_ii #f)
											)
										)
										(set! iPattLevel (- iPattLevel 1))
									)
								)
								
							)
						
						)
						
					)
				)
				;------------
				(if (isTrue isIncr_ii)
					(set! ii (+ ii iDelta))
				)
			)
		)
	) ;//while
	(set! iPatt1Pos iPosPatt1)
	(if (and (>= iPosPatt1 0) (>= iPosPatt2 0) ) ; we found matching open and close patterns
		(begin
			
			(set! iPosPatt1 (+ iPosPatt1 iLenPatt1 )  ) ; we do not include the pattern itself in the result
			(set! sOut (substring sStr iPosPatt1 iPosPatt2) )
			(set! iPatt2After (+ iPosPatt2 iLenPatt2 ) )
		)
	)
	
	(set! aListOut (cons iPatt2After aListOut) )
	(set! aListOut (cons iPatt1Pos aListOut) )
	(set! aListOut (cons sOut aListOut) )
	aListOut ; return value
	)
)

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@
;------------------------------------#####################################################<<<<<<<<<<<<<<<<<<<<<<<<<
(define (stringSplit sStr sSeparator   )
; @@@ given a string <sStr> containing substrings separated by a given <sSeparator>,
; @@@    generates (returns) a flat list where each list element contains each individual subtring, without any separator
	(let* 	(
				(oListOut (stringSplitData sStr sSeparator  #f ))
			)
	; ------
	oListOut ; return value
	)
)
(define (stringSplitData_ExtractValueType sElem )
	(let*
		(
			(ii 0)
			
		)
		(cond
			( (string=? (sSubstring sElem 0 2) "S^") ; 
				(set! sElem (stringRightOf sElem "S^")) 
			)
			( (string=? (sSubstring sElem 0 2) "N^") ; 
				(set! sElem (string->number (stringRightOf sElem "N^")) )
			)
			( (string=? (sSubstring sElem 0 2) "B^") ; 
				(set! sElem (iif (string=? (stringRightOf sElem "B^") "t")   #t  #f ))
			)
			
		)
		;-----return value
		sElem 
	)
)
;------------------------------------#####################################################<<<<<<<<<<<<<<<<<<<<<<<<<
(define (stringSplitData sStr sSeparator  isIdentifyDataTypes )
; @@@ given a string <sStr> containing substrings separated by a given <sSeparator>,
; @@@    generates (returns) a flat list where each list element contains each individual element , without any separator
; @@@ if isIdentifyDataTypes is true, then it converts automatically the string data to the original datatype (recognized by prefixes "S^" "N^" "B^" in the string)
	(let* 	(
				
				(iLenStr (string-length sStr)) ; 
				(iLenSep (string-length sSeparator))
				(iLenMaxCheck (- iLenStr iLenSep)) ; // compare both strings only up to this position
				
				(isCont #t)
				(isMatch #f)
				(ii  (- iLenStr 1))
				;(jj 0)
				(iPosOut -1)
				(iDelta -1)
				;------
				(isRight2Left #t)
				(sElem "")
				(iPosLast iLenStr)
				(oListOut '())
			)
	; ------
	(if (eq? isRight2Left #t ) ; we are moving from right to left, in order not to have to reverse the list at the end
		(begin
			(set! iDelta -1 )
			(set! ii iLenMaxCheck )
			
			(if (> ii iLenMaxCheck )
				(set! ii iLenMaxCheck )
				
			) ; endif
		)
		;---else
		(begin
		)
	) ; endif
	(if (< iLenMaxCheck 0)
		(set! isCont #f )
	)
	(if (< ii 0)
		(set! isCont #f )
	)
	;------------
	(while (eq? isCont #t)
		(set! isMatch  (isSubstringMatch sStr ii sSeparator 0 ))
		
		(if (eq? isMatch #t)
			(begin
				(set! sElem (substring sStr (+ ii iLenSep) iPosLast) )
				(set! sElem (stringSplitData_ExtractValueType sElem ) )
				; (if (eq?  isIdentifyDataTypes #t)
					; (cond
						; ( (string=? (sSubstring sElem 0 2) "S^") ; 
							; (set! sElem (stringRightOf sElem "S^")) 
						; )
						; ( (string=? (sSubstring sElem 0 2) "N^") ; 
							; (set! sElem (string->number (stringRightOf sElem "N^")) )
						; )
						; ( (string=? (sSubstring sElem 0 2) "B^") ; 
							; (set! sElem (iif (string=? (stringRightOf sElem "B^") "t")   #t  #f ))
						; )
						
					; )
				
				; )
				(set! oListOut (cons sElem oListOut) )
				(set! iPosLast ii)
				
			)
			;--
			(begin
				
			)
		)
		;------------
		(set! ii (+ ii iDelta))
		(if (< ii 0 ) ; we have just finished scanning the whole string, looking for separators
			(begin
				(if (= iPosLast 0) ; the last found delimiter /separator was exactly at pos 0! means empty string as first element
					(begin 
						(set! oListOut (cons "" oListOut) )
					)
					;------ else, take the begin of the string up to the last delimiter
					(begin
						(set! sElem (substring sStr 0 iPosLast) )
						(set! sElem (stringSplitData_ExtractValueType sElem ) )
						(set! oListOut (cons sElem oListOut) )
					)
				)
				(set! isCont #f)
			)
		) ; endif
		
	) ;//while
	oListOut ; return value
	)
)
;----------------------------------
(define (stringAglomerate aList sSep isSaveDataTypes)
	(let* 
		(
			(ii 0 )
			(iLenList (length aList))
			(xElem "")
			(sData "")
			(sType "")
			(sOut "")
		)
		(while (< ii iLenList)
			(set! xElem (list-ref aList ii))
			(cond
				; ((list? xElem)
					; (begin
						; (set! isList #t)
						; (set! sType "L^")
					; )
				; )
				((string? xElem)
					(begin
						(set! sData xElem)
						(set! sType "S^")
					)
				)
				((number? xElem)
					(begin
						(set! sData (number->string xElem))
						(set! sType "N^")
					)
				)
				((boolean? xElem)
					(begin
						(set! sType "B^")
						(if (eq? xElem #t)
							(set! sData "t")
							(set! sData "f")
						)
						
					)
				)
			
			)
			(if (> ii 0)
				(set! sOut (string-append sOut sSep))
			)
			(if (isTrue isSaveDataTypes)
				(set! sOut (string-append sOut sType sData))
				;--else
				(set! sOut (string-append sOut sData))
			)
			
			(set! ii (+ ii 1))
		)
		;------
		sOut ; return
	
	)


)

; LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
; LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
; LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL  List related functions LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
; LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
; LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
; @@@ if we have (list-ref oList iPos), why not also ...(list-insert-before oList iPos oNewElem) and (list-delete oList iPos)...?
;=========================================================
(define (list-insert-before oList iPos oNewElem )
; @@@ Inserts oNewElem before list element referenced by iPos, returning the modified list
; @@@ If you try to insert PAST the last element, nothing will happen (no insert will be done)
; @@@  oList = '( "0-aa" "1-bb" "2-cc" "3-dd" "4-ee") iPos= 3 elem="insBefore3"
; @@@  oListResult after calling = '( "0-aa" "1-bb" "2-cc" "insBefore3" "3-dd" "4-ee")
	(let*
		(
			(iLen (length oList))
			(iLast (- iLen 1))
			
			(oElem '())
		)
		(if (<= iPos iLast )
			(begin
				(if (<= iPos 0 ) ; insert here
					(begin
						(set! oList (cons oNewElem oList) )
					)
					;---else insert further down in the list
					(begin
						(set! oElem (car oList) )
						(set! oList (cons oElem  (list-insert-before (cdr oList) (- iPos 1) oNewElem ) )) ; RECURSION
					)
				) ; endif
			)
			
		) ; endif
		;---
		oList
	)
)
;====================================================
(define (list-insert-after oList iPos oNewElem )
; @@@ we have (list-ref oList iPos), why not also ...(list-insert-before oList iPos oNewElem) and (list-delete oList iPos)...?
; @@@ Inserts oNewElem AFTER list element referenced by iPos, returning the modified list
; @@@
; @@@  oList = '( "0-aa" "1-bb" "2-cc" "3-dd" "4-ee") iPos= 3   elem="insAfter3"
; @@@  oListResult after calling = '( "0-aa" "1-bb" "2-cc" "3-dd" "insAfter3" "4-ee") 
	(let*
		(
			(iLen (length oList))
			(iLast (- iLen 1))
			
			(oElem '())
		)
		(if (> iPos iLast )
			(begin
				(set! iPos iLast)
			)
			
		) ; endif
		;...(set! iPos (+ iPos 1))
		(if (>= iPos 0 )
			(begin
				
				(if (<= iPos 0 ) ; insert here
					(begin
						(set! oElem (car oList) )
						(set! oList (cons oElem (cons oNewElem (cdr oList)) ))
					)
					;---else insert further down in the list
					(begin
						(set! oElem (car oList) )
						(set! oList (cons oElem  (list-insert-after (cdr oList) (- iPos 1) oNewElem ) )) ; RECURSION
					)
				) ; endif
			)
			
		) ; endif
		;---
		oList
	)
)

;=========================================================
(define (list-delete oList iPos  )
; @@@ we have (list-ref oList iPos), why not also ...(list-insert-before oList iPos oNewElem) and (list-delete oList iPos)...?
; @@@ DELETES element referenced by iPos, returning the modified list
; @@@ Delete element iPos from this list
; @@@  oList = '( "0-aa" "1-bb" "2-cc" "3-dd" "4-ee") iPos= 3 
; @@@  oListResult after calling = '( "0-aa" "1-bb" "2-cc"  "4-ee")
	(let*
		(
			(iLen (length oList))
			;(iLast (- iLen 1))
			;..(ii iPos)
			(oElem '())
		)
		(if (and (>= iPos 0) (< iPos iLen ))
			(begin
				(if (<= iPos 0 ) ; delete here
					(begin
						(set! oList (cdr oList) )
					)
					;---else insert further down in the list
					(begin
						(set! oElem (car oList) )
						(set! oList (cons oElem  (list-delete (cdr oList) (- iPos 1) ) )) ; RECURSION
					)
				) ; endif
			)
			
		) ; endif
		;---
		oList
	)
)
;=========================================================
(define (list-replace oList iPos oNewElem )
; @@@ we have (list-ref oList iPos), why not also ...(list-insert-before oList iPos oNewElem) and (list-delete oList iPos)...?
; @@@ Replaces list element referenced by iPos, by oNewElem, returning the modified list
; @@@ Replace element referenced by iPos by new element oNewElem and returns the list with the replacement in place
; @@@  oList = '( "0-aa" "1-bb" "2-cc" "3-dd" "4-ee") iPos= 3 oNewElem = "3-Replaced"
; @@@  oListResult after calling (list-replace ..) = '( "0-aa" "1-bb" "2-cc" "3-Replaced" "4-ee")
	(let*
		(
			(iLen (length oList))
			
			(oElem '())
		)
		(if (and (>= iPos 0) (< iPos iLen ))
			(begin
				(if (<= iPos 0 ) ; replace here
					(begin
						(set! oList (cons oNewElem (cdr oList) ) )
					)
					;---else insert further down in the list
					(begin
						(set! oElem (car oList) )
						(set! oList (cons oElem  (list-replace (cdr oList) (- iPos 1) oNewElem ) )) ; RECURSION
					)
				) ; endif
			)
			
		) ; endif
		;---
		oList
	)
)
;=========================================================
(define (list-replace-insert oList iPos oReplaceElem oInsertElem )
; @@@ Replaces element referenced by iPos by new element oReplaceElem then inserts element oInsertElem immediatly after and returns the list 
; @@@ with the replacement and insertion in place
; @@@  oList = '( "0-aa" "1-bb" "2-cc" "3-dd" "4-ee") iPos= 3 oReplaceElem = "3-Replaced" oInsertElem="3-InsertedAfter"
; @@@  oListResult after calling (list-replace-insert ..) = '( "0-aa" "1-bb" "2-cc" "3-Replaced" "3-InsertedAfter" "4-ee")
; @@@ Can of course be achieved by using (list-replace and (list-insert-after but, combining both actions in one, improves performance
	(let*
		(
			(iLen (length oList))
			
			(oElem '())
		)
		(if (and (>= iPos 0) (< iPos iLen ))
			(begin
				(if (<= iPos 0 ) ; replace here
					(begin
						(set! oList (cons oReplaceElem (cons oInsertElem (cdr oList)) ) )
					)
					;---else insert further down in the list
					(begin
						(set! oElem (car oList) )
						(set! oList (cons oElem  (list-replace-insert (cdr oList) (- iPos 1) oReplaceElem oInsertElem ) )) ; RECURSION
					)
				) ; endif
			)
			
		) ; endif
		;---
		oList
	)
)


;=========================================================
(define (list-split oList iPos  )
; @@@ SPLITS a list in 2 sub-lists A and B, returning a 'capsule' list containing both sub-lists. The split is done at element iPos of the original list (which goes to list B)
; @@@ All elements preceeding iPos go to list A
; @@@ Ex:
; @@@  oList = '( "0-aa" "1-bb" "2-cc" "3-dd" "4-ee")    iPos= 3 
; @@@  oListResult after calling list-split = '(  ("0-aa" "1-bb" "2-cc")  ("3-dd" "4-ee")   ) 
	(let*
		(
			(iLen (length oList))
			
			(ii 0)
			(oElem '())
			(oListLeft '())
			(oListCapsule '())
		)
		
		(if (and (>= iPos 0) (< iPos iLen )) ;
			(begin
				;--------
				(while (< ii iPos)
					(set! oElem (car oList) )
					(set! oListLeft (cons oElem oListLeft))
					(set! oList (cdr oList))
					(set! ii (+ ii 1))
				)
				(if (> iPos 1 ) ;
					(begin
						(set! oListLeft (reverse oListLeft)) ; reverse if there is more than 1 element at the left list
					)
					
				) ; endif
				
			);--else
			(begin
				;--- we return a capsule with an empty list on the left and the original list on the right
			)
		) ; endif
		(set! oListCapsule (cons  oList oListCapsule))
		(set! oListCapsule (cons  oListLeft oListCapsule))
		;---
		oListCapsule
	)
)
;---------------------------
(define (list-ref-3d oList iDim1 iDim2 iDim3 )
;@@@ get list item in 3 dimensions. Comparable with: array[iDim1][iDim2][iDim3]
;@@@ Ex: '( ( ( "elem1A1" "elem1A2") ("elem1B1" "elem1B2") ) ( ( "elem2A1" "elem2A2") ("elem1B1" "elem1B2") ) )
;@@@ list = ( listElem0 listElem1 listElem2 listElem3 )
;@@@ listElem2 = ( listElem2SubElem0 listElem2SubElem1 listElem2SubElem2 listElem2SubElem3 )
;@@@ listElem2SubElem2 =( listElem2SubElem2Subelem0 listElem2SubElem2Subelem1 )
;@@@ If iDim2 < 0 then dimension 2 and 3 is ignored and only element iDim1 of oList will be returned (equivalent to (list-ref oList iDim1) or array[iDim1]
;@@@ If iDim3 < 0 then dimension 3 is ignored and only element oList[iDim1][iDim2] will be returned

	(let*
		(
			(ii 0)
			
			(oOut '())
			(oSubList '())
		)
		(if (and (list? oList) (< iDim1 (length oList) ))
			(begin
				;(set! oOut "A")
				(set! oSubList (list-ref oList iDim1))
				(cond
					((>= iDim2 0)
						;(set! oOut "B")
						(if (and (list? oSubList) (< iDim2 (length oSubList) )) ;@@@@@@@@@@@@@@@@@@@@@@@@@
							(begin
								;(set! oOut "C")
								(set! oSubList (list-ref oSubList iDim2))
								(cond
									((>= iDim3 0)
										;(set! oOut "D")
										(if (and (list? oSubList) (< iDim3 (length oSubList) )) 
											(begin
												(set! oSubList (list-ref oSubList iDim3))
												(set! oOut oSubList)
											)
										) ; endif
									)
									((< iDim3 0) ; means: do not nest further, return elem iDim2
										(set! oOut oSubList)
									)	
								)
							)
						) ; endif
					)
					((< iDim2 0) ; means: do not nest further, return elem iDim1
						(set! oOut oSubList)
					)
				)
			)
		) ; endif
		;--------return
		oOut ;
	)
)
;-------@@@@@@@@@@@@@@@@@@@@@-----------------------------------------------------		
(define (add2ListInKeyedOrder aList oNewElem   )
;-------@@@@@@@@@@@@@@@@@@@@@-----------------------------------------------------
; @@@ builds a list containing "keyed" elements which are lists. The top (first or index 0) item of each element is a key (string) which determines the sequence,
; @@@ from alphabetic small to alphabetic large. 
; @@@ Elements are added in alphabetic order of the first element (the key) of the element list. Ex: ( ("aa" 25)  ("jj" 18) ("yy" 77) ). 
; @@@ Notice that the element containing the 'key' "yy" is located AFTER the one containing thr key "jj". Thus elements are allways 'sorted' after being added
; @@@ Returns the original list with the oNewElem INSERTED at the correct order position, with the elements containing the 'smallest' alphabetic keys on top of the list
; @@@ Ex: Add element ("pp" 33) to existing list ( ("aa" 25)  ("jj" 18) ("yy" 77) )
; @@@  Starting from the beginning of the list, compare with the key "pp" with the the key of each element (the (car ) of the element ) until we can add (on top of the others because further
; @@@ elems are 'heavier'. Thus element containing "pp" is compared with "aa", then with "jj" then with "yy" and inserted before the element containing "yy" 
; @@@ The subsequent items of each element (after the key) are just any data you might need
; @@@
	(let*
		(
			(ii 0)
			;///iLen (length aList))
			(sTopElem "")
			(oTopElem '())
			(sNewElem (car oNewElem ))
			(aListOut '() )
			
		)
		
		(if (null? aList )
			
			(begin
				(set! aListOut (cons oNewElem aList) )
			)
			;---else NOT empty list
			(begin
				(set! oTopElem (car aList) ) ; ; (car aList)
				(set! sTopElem (car oTopElem) )
				
				(if (string>? sNewElem sTopElem )
					(begin
						
						(set! aListOut (cons oTopElem (add2ListInKeyedOrder (cdr aList) oNewElem )))
						
					) ; endif
					;---else "pp" <= "yy"
					(begin
						(set! aListOut (cons oNewElem aList) )
					)
				)
				
			) ; endif
			
		)
		;-----
		aListOut
		
	)
)

; =====================================================================================================================================
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ FILE RELATED @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; =====================================================================================================================================


; @@@@@@@@@@@@@@@@@@@@@@@@@@@@
(define (list2StringLinesOption aListStrings isLineNrsPrefix )
;@@@ given a flat list of strings (only strings) generates a string with lines separated by newline char
	(let*
		(
			(ii 0)
			(iLen (length aListStrings))
			(sElem "")
			(sOut "")
			(isAddNL #f)
		)
		(while (< ii iLen )
			(set! sElem (list-ref aListStrings ii) )
			(if (eq? isAddNL #t )
				(begin
					(set! sOut (string-append sOut "\n" (iif (isTrue isLineNrsPrefix) (string-append (i2s ii) ")-") "") sElem))
				)
				;---else
				(begin
					(set! sOut (string-append sOut (iif (isTrue isLineNrsPrefix) (string-append (i2s ii) ")-") "") sElem))
					(set! isAddNL #t)
				)
			) ; endif
			
			(set! ii (+ ii 1) )
		)
		sOut
	)
)
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@
(define (list2StringLines aListStrings  )
;@@@ given a flat list of strings (only strings) generates a string with lines separated by newline char
	(let*
		(
			(sOut (list2StringLinesOption aListStrings #f ))
		)
		sOut
	)
)
; (stringSplit sStr sSeparator   )
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@
(define (stringLines2List sStringLines  )
; @@@ given a multiline string, generates a flat list containing each line in each list entry
	(let*
		(			
			(oListOut (stringSplit sStringLines "\n"   ))
			
		)
		
		oListOut;
	)
)

;///////////////////////////////
(define (sFileWrite sStr iPort)
;@@@ writes (appends) string sStr to a file represented by port iPort
	(let*
		( 	(ii 0)
			(iLen (string-length sStr))
			(iWaitAfter 256)
			(iDeltaCnt iWaitAfter)
			(cc 0)
		)
		;..(set! iLen (string-length ss1))
		(set! ii 0)
		(while (< ii iLen)
			; (set! iDeltaCnt (- iDeltaCnt 1))
			; (if (<= iDeltaCnt 0)
				; (begin
					; (set! iDeltaCnt iWaitAfter)
					; (waitabit)
				; )
			; )
			(set! cc (string-ref sStr ii) )
			(write-char cc iPort)
			(set! ii (+ ii 1))
		)
	)
)
(define (waitabit )
	(let* 
		(
			(ii (realtime ) )
			(jj ii)
		)
		(while (= jj ii)
			(set! jj (realtime ))
		)
	
	)


)
;///////////////////////////////
(define (string2File sStr sFileName)
;@@@ creates file sFileNmae and outputs the string sStr to it, and closes the file
	(let*
		( 	
			(iPort 0)
			(iPrevPort (current-output-port ))
			(bOut #f)
		)
	
		(set! iPort (open-output-file sFileName))
		(if (output-port? iPort) 
			(begin
				(set-output-port iPort) ; undocumented call (at least not in Tiny Scheme standard r5rs document). Is required here because of some bug in Tiny Scheme, 
										; leading to an error "set-output-port: needs 1 argument(s)". See http://article.gmane.org/gmane.comp.video.gimp.devel/13451
				(sFileWrite sStr iPort)
				(close-output-port iPort)
				(set-output-port iPrevPort)
				(set! bOut #t)
			)
		)
		bOut ;
	)
)
;///////////////////////////////
(define (isFileExists sFileName)
	(let*
		( 	(ii 0)
			
			(iPort 0)
			(cc 0)
			(sOut "")
			(isCont #t)
			(isExists #f)
		)
		
		(set! iPort (open-input-file sFileName))
		(if (input-port? iPort) ; #### how to check if file exists ?
			(begin
				(set! isExists #t)
				(close-input-port iPort)
			)
		)
		isExists ; return value
	)
)
;///////////////////////////////
(define (sFile2String sFileName)
	(let*
		( 	(ii 0)
			
			(iPort 0)
			(iPrevPort (current-input-port ))
			(cc 0)
			(sOut "")
			(isCont #t)
		)
		
		(set! iPort (open-input-file sFileName))
		(if (input-port? iPort) ; #### how to check if file exists ?
			(begin
				(set-input-port iPort)
				(set! ii 0)
				(while (eq? isCont #t)
					; (while (not (char-ready? iPort))
					
					; )
					(set! cc (read-char iPort) )
					(if (eof-object? cc)
						(set! isCont #f)
						;--else
						(set! sOut (string-append sOut (string cc)))
					)
					(set! ii (+ ii 1))
				)
				(close-input-port iPort)
				(set-input-port iPrevPort)
			)
		)
		sOut ; return value
	)
)
;=====================
(define (getCurrImageFileName image )
	(let*
		(
			(sFilName (car (gimp-image-get-filename image)))
		)
		sFilName ;
	)
)
;=====================
(define (getFileNameNoExtension sFileName )
	(let*
		(
			(sOut sFileName)
			(iStart (- (string-length sFileName) 1) )
			(ii (iPosSubstring sFileName iStart "." #t))
		)
		(if (>= ii 0)
			(set! sOut (substring sFileName 0 ii) )
		)
		sOut ;
	)
)
;=====================
(define (getImageRelatedFileName image  sExt )
; @@@ defines a filename like the image file name, but with a different extension
; @@@ Ex: imagefile = "D:/mydir/image01.xcf"; (getImageRelatedFileName image  ".{gg}" ) --> generates string "D:/mydir/image01.{gg}"
; @@@ Version ignore feature: If the filename contains the pattern "@@" foloweed by some text indicating a version number, then the text starting from the pattern "@@"
; @@@ onwards will be ignored. This makes it possible to create files related to images that are common to all versions of the same image
; @@@ Ex: imagefile = "D:/mydir/my-image@@version023.xcf"; (getImageRelatedFileName image  ".{gg}" ) --> generates string "D:/mydir/my-image.{gg}"
; @@@ this pattern is located from right to left in the file name, thus if you have several "@@" patterns in the file name, only the last will be used for this feature.
; @@@ Ex: imagefile = "D:/mydir/my-image@@very-dark@@version023.xcf"; (getImageRelatedFileName image  ".{gg}" ) --> generates string "D:/mydir/my-image@@very-dark.{gg}"
	(let*
		(
			(sFileName  (getFileNameNoExtension (getCurrImageFileName image )) )
			(ii (iPosSubstring sFileName -1 "@@" #t) )
			(sOut "")
		)
		(if (> ii 0 ) ; if the pattern "@@" is the first text in the file name (would be position ii=0), ignore the version feature
			(begin
				(set! sFileName (substring sFileName 0 ii) )
			)
		)
		(set! sOut (string-append sFileName  sExt ) )
		sOut ;
	)
)
;=====================
(define (getCurrImageDir image )
; @@@ returns the full path name (including the last slash) of the directory where the current image file is located
	(let*
		(
			(ii 0)
			(sFilName (car (gimp-image-get-filename image)))
			(sDirOut "")
		)
		(if (string? sFilName)
			(begin
				(set! ii (iPosSubstring sFilName -1 "/" #t))
				(if (< ii 0)
					(set! ii (iPosSubstring sFilName -1 "\\" #t)) ; search for windows environmnts
				)
				(if (>= ii 0 )
					(begin
						(set! sDirOut (substring sFilName 0 (+ ii 1) ) )
					)
					;---else
					(begin
					)
				) ; endif
				
			)
			;---else
			(begin
				;..(set! sDirOut sFilName )
			)
		) ; endif
		sDirOut ;
	)
)
(define (sFullFilePathName sDir sFileName )
; @@@ generates a full path file name, given a full path directory name and a simple file ma,e
	(let*
		(
			(ii 0)
			(sDirSep "")
			(iLenDir (string-length sDir))
			(sOut "")
			(sLast (iif (> iLenDir 0) (substring sDir (- iLenDir 1) iLenDir ) "/" ) )
			(cc "")
		)
		
		(if (not (string=? sLast "/"))
			(begin
				(if (not (string=? sLast "\\"))
					(begin
						(set! sDirSep "/" )
					)
					
				)
			)
		)
		
		(set! sOut (string-append sDir sDirSep sFileName))
		sOut;
	)
)
; (((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((()))))))))))))))))))))))))))))))
(define (saveTextData image sFileName sTextData )
;@@@ save a string sTextData to file sFileName which will be created /overwritten in the same directory as the current image
;@@@ returns the full filename (incl path) where the file was saved
	(let*
		(
			
			(sFileNameFullPath (sFullFilePathName (getCurrImageDir image ) sFileName ))
		)
		(string2File sTextData sFileNameFullPath)
		sFileNameFullPath ;
	)
)
(define (loadTextData image sFileName  )
;@@@ load a string from file sFileName which will is located in the same directory as the current image
	(let*
		(
			(sTextData "")
			(sFileNameFullPath (sFullFilePathName (getCurrImageDir image ) sFileName ))
		)
		(set! sTextData (sFile2String sFileNameFullPath))
		sTextData;
	)
)
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ GIMP related generic functions @@@@@@@@@@@@@@@@@@@@@@@@@@@
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;================ Generic function ===========================
(define (getCurrLayerObj image iPos)
; @@@ after some layer has been added, the layer sequence can change..this function rereads the 
; list of all layers and returns the layer located at iPos position in the layer stack
	(let*
		(
			(layers (gimp-image-get-layers image))
			(num-layers (car layers))
			(layer-array (cadr layers))
			
			(ii 0)
			(iOut -1)
		)
		(if (< iPos num-layers)
			(begin
				(set! iOut (aref layer-array iPos))
			)
			;---else
			(begin
			)
		)
		;-----return value
		iOut 
	)
)
;--------------------------------------------
(define (getLayerObjGivenLayerName image iPosStartSearch sLayerName2Find)
; @@@ locate the layerID of the layer with the given <sLayerName2Find> 
; @@@ starts searching from <iPosStartSearch> forwards 
; @@@ returns the id of the layer or -1 if not found
	(let*
		(
			(layers (gimp-image-get-layers image))
			(num-layers (car layers))
			(layer-array (cadr layers))
			(isCont #t)
			(layerID 0)
			(sName "")
			(ii 0)
			(iOut -1)
		)
		(if (or (>= iPosStartSearch num-layers)  (< iPosStartSearch 0))
			(begin
				(set! iPosStartSearch 0 )
			)
			
		)
		(set! ii iPosStartSearch)
		(if (>= ii num-layers)
			(begin
				(set! isCont #f)
			)
		)
		(while (isTrue isCont)
			(set! layerID (aref layer-array ii))
			(set! sName (car (gimp-layer-get-name layerID )) )
			(if (string=? sName sLayerName2Find)
				(begin
					(set! iOut layerID)
					(set! isCont #f)
				)
				
			)
			;------------
			(set! ii (+ ii 1))
			(if (>= ii num-layers)
				(begin
					(set! isCont #f)
				)
				;---else
				(begin
				)
			)
		)
		;-----return value
		iOut 
	)
)


; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& END GENERAL PURPOSE FUNCTIONS &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
; &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

; ===========================================
(define (duplicateThisLinkedLayer image layer layer-name destinGroupName iImageDestination) ; duplictes the layer, adds to the image and set LINK on
	(let*
		(
			(layer-copy 0)

			(iDestImage image)
			(new-layer-name "")
			;// (isAllowDuplicates #f)
		)
		(if (not (= iImageDestination 0))
			(set! iDestImage iImageDestination)
		)
		; (if (not (= iDestImage image)) ; if the duplicate is going to be added to another image, then the current name may be duplicated there...
			; (set! isAllowDuplicates #t) ; ... but the name can still clash with another name in the other image!
		; )
		(set! new-layer-name (string-append  destinGroupName ".[LD]." layer-name)) ; // LD=layer duplicated
		(set! layer-copy (car (gimp-layer-new-from-drawable layer iDestImage)))
		(gimp-layer-set-name layer-copy new-layer-name) ; name changed to "<destinationGroupName>.[LD].<LayerNameSuffix>"


		(gimp-image-add-layer iDestImage layer-copy -1)
		; ////@@@ (gimp-layer-set-linked layer-copy TRUE)
		layer-copy

	);let
)
;--------------------------------
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(define (script-fu-gimp-layer-group-duplicate-linked image destinGroupName isDupToOtherImage iImageDestination)
; duplicate all linked layers to a new name as follows: '<destinGroupName>._<oldLayerName>' in order to reclassify existing layers
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(iDestImage 0)
		; //(layer-name-group "")
		)

	;(gimp-image-undo-disable image)
	;//////////////(script-fu-gimp-layer-group-unlinkall image ) ; remove all links, in order to link all duplicates
	; @@@@@if we unlink all, we loose the link status of each layer...
	(if (= isDupToOtherImage TRUE)
		(begin
			(set! iDestImage iImageDestination)
		)
	)
	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(if (=  (car(gimp-layer-get-linked layer)) TRUE)  ;
			(begin
				(set! layer-name (car (gimp-layer-get-name layer)))
				;//(set! layer-name-group (string-append destinGroupName " " layer-name ))
				(duplicateThisLinkedLayer image layer layer-name destinGroupName  iDestImage);
				; (gimp-layer-set-name layer layer-name-group) ; name changed to "<lgname> <oldName>"
			)
		)
		; ~~~~~~~~~~~~~~~~~~~~~

		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	;(gimp-image-undo-enable image)
	)
)
; =================================================
(define (script-fu-gimp-layer-group-rename-linked image lgname)
; rename all linked layers to a new name as follows: 'lgname oldLayerName' in order to reclassify existing layers
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group "")
		)

	; (gimp-image-undo-disable image)

	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(if (=  (car(gimp-layer-get-linked layer)) TRUE)  ;
			(begin
				(set! layer-name (car (gimp-layer-get-name layer)))
				(set! layer-name-group (string-append lgname ".[LR]." layer-name )) ;// LR=layer renamed
				(gimp-layer-set-name layer layer-name-group) ; name changed to "<lgname> <oldName>"
			)
		)
		; ~~~~~~~~~~~~~~~~~~~~~

		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	; (gimp-image-undo-enable image)
	)
)

; script-fu-gimp-layer-group-hide-linked-layers
; ==============================@@@@@@@@@@@@@@@@@@===================
(define (script-fu-gimp-layer-group-hide-linked-layers image )
; delete all linked layers in the current image
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group "")
		)

	; (gimp-image-undo-disable image)

	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(if (=  (car(gimp-layer-get-linked layer)) TRUE)  ;
			(begin
				(gimp-layer-set-visible layer FALSE)

			)
		)
		; ~~~~~~~~~~~~~~~~~~~~~

		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	; (gimp-image-undo-enable image)
	)
)
; ==============================@@@@@@@@@@@@@@@@@@===================
(define (script-fu-gimp-layer-group-show-linked-layers image )
; delete all linked layers in the current image
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group "")
		)

	; (gimp-image-undo-disable image)

	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(if (=  (car(gimp-layer-get-linked layer)) TRUE)  ;
			(begin
				(gimp-layer-set-visible layer TRUE)

			)
		)
		; ~~~~~~~~~~~~~~~~~~~~~

		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	; (gimp-image-undo-enable image)
	)
)
; @#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@##@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@##@#@#@#@#@#@#@#@#@#@##@#@#@#
; @#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@##@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@##@#@#@#@#@#@#@#@#@#@##@#@#@#
; @@@																																		@@@@@
; @#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@##@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@##@#@#@#@#@#@#@#@#@#@##@#@#@#`
; @#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@##@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@##@#@#@#@#@#@#@#@#@#@##@#@#@#


(define (manage-add-list-id-item-in-keyed-order  aGList oNameAndID     iCall )
;@@@ aGList = aGroupedListLayerNames
	(let*
		(
			(ii 0)
			(jj 0)
			(kk 0)
			(iSplitAt 30) ; split threshold
			(iSplitPos 15)
			(oCurrList '())
			(oLeftList '())
			(sKey "")
			(ss "")
			(oL '())
			(iLen 0)
			(isCont #t)
			;--
			
		)
		(if (null? aGList)
			(set! aGList (cons oCurrList aGList ))
		) ; endif
		(set! iLen (length aGList))
		(set! sKey (car oNameAndID))
		
		(if (> iLen 1 )
			(begin
				
				(set! ii (- iLen 1))
				(set! jj 0)
				(set! kk 0)
				(while (isTrue isCont)
					
					(set! ss (list-ref-3d aGList ii jj kk) )
					(if (or (string>=? sKey  ss) (= ii 0) ) ; jj =first element of group of keys, kk=first element of every oNameAndId record 
						(begin ; we add the new record to this group
							
							(set! oCurrList (list-ref aGList ii))
							(set! oCurrList (add2ListInKeyedOrder  oCurrList oNameAndID ) )
							
							(if (> (length oCurrList) iSplitAt )
								(begin
									(set! oL (list-split oCurrList iSplitPos) )
									(set! aGList (list-replace-insert aGList ii (list-ref oL 0) (list-ref oL 1) ) )
									
								)
								;---else NO-SPLIT
								(begin
									(set! aGList (list-replace aGList ii oCurrList ))
									
								)
							) ; endif
							(set! isCont #f )
						)
						
					) ; endif
					
					;----------------------------
					(set! ii (- ii 1))
					(if (< ii 0)
						(set! isCont #f)
					)
				)
				
			)
			; else ;---else aGList contains only one element
			(begin
				(set! ii 0)
				
				(set! oCurrList (car aGList))
				(set! oCurrList (add2ListInKeyedOrder  oCurrList oNameAndID ) )
				(if (> (length oCurrList) iSplitAt )
					(begin
						
						(set! oL (list-split oCurrList iSplitPos) )
						(set! aGList (list-replace-insert aGList 0 (list-ref oL 0) (list-ref oL 1) ) )
						
					)
					;---else
					(begin
						(set! aGList (list-replace aGList ii oCurrList ))
					)
				) ; endif
			)
		) ; endif
		
		;---------return-
		aGList
	)
)


;-------------------------------------------------------------------
(define (collect-gimp-layer-names image)
; @@@ collects the current list of layer names and IDs, in alphabetic order of the names, for this list to be used
; @@@ as a searcheable 'database' of layer names and iDS 
	(let*
		(
			(ii 0)
			(layers (gimp-image-get-layers image))
			(num-layers (car layers))
			(layer-array (cadr layers))
			(layer 0)
			(layer-name "")
			(sLayerNameAndID "")
			(oNameAndID '())
			(aListLayerNames '())
			(aGroupedListLayerNames '())
			
			(iCnt 0)
			(aGrpi '())
			
			(iCall 1)
			
		)
		
		(set! ii (- num-layers 1) ) ;
		(while (>= ii 0)
			; check the first word in the name of each layer, if it matches lgname, perform the action
			(set! layer (aref layer-array ii))
			(set! layer-name (car (gimp-layer-get-name layer)))
			
			(set! oNameAndID '())
			(set! oNameAndID (cons layer oNameAndID))
			(set! oNameAndID (cons layer-name oNameAndID) )
			
			(set! aGroupedListLayerNames  (manage-add-list-id-item-in-keyed-order aGroupedListLayerNames  oNameAndID     iCall ) ) ; 
			
			(set! ii (- ii 1))
			(set! iCall (+ iCall 1))
		)
		;----------
		
		(set! iCnt (length aGroupedListLayerNames ))
		(set! ii 0)
		(set! aListLayerNames (list-ref aGroupedListLayerNames ii) )
		
		(set! ii (+ ii 1))
		(while (< ii iCnt)
			(set! aGrpi (list-ref aGroupedListLayerNames ii))
			
			(set! aListLayerNames (append aListLayerNames aGrpi ))
			(set! ii (+ ii 1))
		)
		
		aListLayerNames ;
	)
)


;-------------------
(define (find-gimp-layer-by-name aListLayerNames sNameLayer)
; @@@ aListLayerNames is sorted, with 'smaller' names on top. Ex: ( "aLayer{@}->34" "cLayer{@}->18" "mLayer{@}->13" "zLayer@}->99")
; @@@ we use 'binary seARCH'
; @@@ return layerID or -1 if not found
	(let*
		(
			(ii 0)
			(iCnt (length aListLayerNames))
			(sNm "")
			(iLayerID -1)
			;--
			(iLow 0)
			(iHigh (- iCnt 1))
			(iDelta 0)
			;...(sLayerAndId "")
			(oLayerAndId '())
			(sLayer "")
			
			(isCont #t)
			
			(iCntIterations 0)
		)
		;-----check extremities
		(set! oLayerAndId (list-ref aListLayerNames iLow))
		(set! sNm (car oLayerAndId ))
		

		(if (string=? sNm sNameLayer ) ;
			(begin ; found
				(set! iLayerID (car (cdr oLayerAndId ) ))
				(set! isCont #f)
			)
			
		) ; endif
		(set! oLayerAndId (list-ref aListLayerNames iHigh))
		(set! sNm (car  oLayerAndId ))
		
		(if (and (isTrue isCont) (string=? sNm sNameLayer ) )
			(begin ;found
				(set! iLayerID (car (cdr oLayerAndId ) ))
				(set! isCont #f)
			)
		) ; endif
		(while (isTrue isCont)
			(set! iCntIterations (+ iCntIterations 1))
			(set! iDelta (- iHigh iLow) )
			(set! ii (round (/ (+ iLow iHigh) 2) ) ) ; try an element between the iHigh and the iLow items
			(set! oLayerAndId (list-ref aListLayerNames ii))
			(set! sNm (car  oLayerAndId ))
			(cond
				((string=? sNameLayer sNm  ) ; found the layer name ?
					(set! iLayerID (car (cdr oLayerAndId ) ))
					(set! isCont #f)
				)
				((string<? sNameLayer sNm  ) ; our searched layer is alphabetic smaller than the one currently in the i-th element
					(set! iHigh ii) ; we can set our  iHigh mark lower 
				)
				((string>? sNameLayer sNm  ) ; our layer is alphabetic larger than the one currently in the i-th element
					(set! iLow ii) ; we can set our iLow mark higher
				)
			)
			
			(if (<= iDelta 2 ) ; if the elements iHigh and iLow were 2 appart, and we checked the one between them in the middle, than we are finished
				(begin ; if 
					(set! isCont #f)
				)
				;---else
				(begin
				)
			) ; endif
		)
		;------
		
		iLayerID ;return
		
	)
)

; ###########################################################################################################################################################
; ==============================@@@@@@@@@@@@@@@@@@===================############################################################################################################
; ###########################################################################################################################################################
(define (script-fu-gimp-layer-group-visiblelayers-save-restore image sNameSaveSet iLoadSave ) ; iLoadSave: 0==load  1==save
; delete all linked layers in the current image
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(jj 0)
		(layer 0)
		(layer-name "")
		(layer-name-group "")
		;---
		(isVisible #f)
		(sInfo "")
		(sInfo2 "")
		(aListVisible '())
		(aListLayers '())
		(iLenListVisible 0)
		(sFileName "")
		(sData "")
		(sData1 "")
		(sPatt1 "")
		(sPatt2 "")
		(sSepLogical "~^")
		
		(isCont #f)
		
		)

	
	(gimp-image-undo-disable image)
	(set! sFileName (getImageRelatedFileName image  (string-append "{" sNameSaveSet "}.XCF^V") ))
	(cond
		((= iLoadSave 0) ;save
			(set! ii (- num-layers 1) ) ;
			(while (>= ii 0)
				; check the first word in the name of each layer, if it matches lgname, perform the action
				(set! layer (aref layer-array ii))
				(set! layer-name (car (gimp-layer-get-name layer)))
				(set! sInfo (car (gimp-layer-get-visible layer)) )
				
				(set! sInfo (string-append (iif (= sInfo TRUE) "t" "f") sSepLogical layer-name) )
				
				(set! aListVisible (cons sInfo aListVisible ))
				
				; ~~~~~~~~~~~~~~~~~~~~~
				(set! ii (- ii 1))
			)
			;-------
			
			(set! sInfo (stringAglomerate aListVisible "\n" #f ))
			
			(set! sData (string-append "<NAME>" sNameSaveSet "</NAME>" "\n<VISIBILITY>\n"  sInfo  "\n</VISIBILITY>\n" " ") )
			(if (not (string2File sData sFileName))
			
			)
			
		)
		((= iLoadSave 1) ;load/restore
			(if (isFileExists sFileName)
				(begin
					(gimp-progress-set-text _"Loading file data...")
					(set! sData (sFile2String sFileName))
					(gimp-progress-set-text _"Parsing loaded data...")
					;-------------
					
					;-------
					(set! sPatt1 "\n<VISIBILITY>\n" )
					(set! sPatt2  "\n</VISIBILITY>\n" )
					(set! sData (sExtractDelimited sData 0  sPatt1 sPatt2))
					
					; --------------
					
					(if (> (string-length sData) 0)
						(begin
							;-------------
							
							(set! aListVisible (stringSplitData sData "\n"  #f))
							
							(set! iLenListVisible (length aListVisible))
							
							;---------------------
							(gimp-progress-set-text _"Building alphabetic list of layer names...")
							(set! aListLayers (collect-gimp-layer-names image)) ; collect a list of all layernames in alphabetic order
							
							(set! ii 0)
							(set! jj 0)
							(gimp-progress-set-text _"Setting individual visibility of each layer..")
							(while (< ii iLenListVisible)
								(set! sInfo (list-ref aListVisible ii) ) ;
								(set! sInfo2 (stringLeftOf sInfo sSepLogical))
								(set! layer-name  (stringRightOf sInfo sSepLogical))
								
								(set! isVisible (iif (string=? sInfo2 "t") #t #f))
								;---------------
								
								(set! layer (find-gimp-layer-by-name aListLayers layer-name) )
								
								(if (>= layer 0 )
									(begin
										(set! jj (+ jj 1))
										(if (isTrue isVisible)
											(begin
												
												(gimp-layer-set-visible layer TRUE)
											)
											;---- else
											(begin
												
												(gimp-layer-set-visible layer FALSE)
											)
										)
										
									) ; ' else... layer can have been renamed and thus not exist anymore
									
								) ; endif
								(set! ii (+ ii 1 ))
								
							)
							
						)
					)
					
					
				)
			)
		
		)
	)
	;--------
	
	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	
	)
)

; ==============================@@@@@@@@@@@@@@@@@@2===================
(define (script-fu-gimp-layer-group-delete-linked image )
; delete all linked layers in the current image
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group "")
		)

	

	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(if (=  (car(gimp-layer-get-linked layer)) TRUE)  ;
			(begin
				(gimp-image-remove-layer image layer);

			)
		)
		; ~~~~~~~~~~~~~~~~~~~~~

		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	
	)
)
;---------------------------
(define (script-fu-gimp-layer-group-move-to-top-linked-bis image )
	(let* ()
		(script-fu-gimp-layer-group-move-to-top-linked image )
	)
)
; ==============================@@@@@@@@@@@@@@@@@@2=================== #######@########@@@@@@@@@##############@@@@@@@@@@@@#
(define (script-fu-gimp-layer-group-move-to-top-linked image )
; MOVE all *inked* layers in the top
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii (- num-layers 1)) ;// start looking for linked layers from the BOTTOM up, to maintain the relative order of the moved layers
		(layer 0)
		(layer-name "")
		(layer-name-group "")
		)

	;  (gimp-image-undo-disable image)

	
	(while (>= ii 0) ; num-layers
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(if (=  (car(gimp-layer-get-linked layer)) TRUE)  ;
			(begin
				(gimp-image-raise-layer-to-top image layer);

			)
		)
		; ~~~~~~~~~~~~~~~~~~~~~

		(set! ii (- ii 1))
	)

	(gimp-displays-flush)
	; (gimp-image-undo-enable image)
	)
)
(define (script-fu-gimp-layer-group-move-to-bottom-linked-bis image )
	(let* ()
		(script-fu-gimp-layer-group-move-to-bottom-linked image )
	)
)
; ==============================@@@@@@@@@@@@@@@@@@2===================
(define (script-fu-gimp-layer-group-move-to-bottom-linked image )
; MOVE all *inked* layers in the top
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group "")
		)

	; (gimp-image-undo-disable image)

	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(if (=  (car(gimp-layer-get-linked layer)) TRUE)  ;
			(begin
				(gimp-image-lower-layer-to-bottom image layer);

			)
		)
		; ~~~~~~~~~~~~~~~~~~~~~

		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	; (gimp-image-undo-enable image)
	)
)

;;  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(define (script-fu-gimp-layers-group-list-of-all-layer-names image listTitle xCoord yCoord)
	(let* (
		(layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group "")
		(sTimeStamp (getTimeStamp ))
		;...(iLenTitle (string-length (string-trim listTitle)))
		(sLineSep (replicateString 80 "-") )
		(string-list-layer-names (string-append listTitle "  [" sTimeStamp "]\n" sLineSep "\n" ))
		(new-text-layer 0)
		;...(layer-info "")
		(oInfoV "")
		(oInfoL "")
		
		
		)

	;(gimp-image-undo-disable image)
	(set! string-list-layer-names (string-append  string-list-layer-names (car (gimp-image-get-filename image))  "\n<LayerStackSeq> - {<layerID>} [<Visible>] [<Linked>] <Layer Name>\n" sLineSep "\n") )
	(set! ii 0)
	(while (< ii num-layers)

		(set! layer (aref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		(set! oInfoV (car (gimp-layer-get-visible layer)) )
		(set! oInfoL (car (gimp-layer-get-linked layer)) )
		; (i2sfs iValue nDecimals) 
		(set! string-list-layer-names (string-append string-list-layer-names (i2sfs ii 4) " - {"(i2sfs layer 6) "}  [" (iif (= oInfoV TRUE) "X" "_") "] [" (iif (= oInfoL TRUE) "X" "_")
				"]  "   layer-name "\n"))

		(set! ii (+ ii 1))
	)
	(set! new-text-layer (car (gimp-text-fontname image -1 xCoord yCoord string-list-layer-names 1 FALSE 9 1 "System") ))

	;...(set! layer-name (getTimeStamp ) )
	(gimp-layer-set-name new-text-layer (string-append "LIST-OF-ALL-LAYERS." sTimeStamp  ))
	(gimp-displays-flush)
	;(gimp-image-undo-enable image)
	)
)
;---------------------------------------------------------
(define (script-fu-gimp-layer-group-show-or-hide image lgname isOnlyLinkedLayers sFilter isSetVisible)
	; @@@@@@@@@@@@@@@@@@@@@@@@@@@@ renames all layers belonging to a layer group
	(let* (
			(layers (gimp-image-get-layers image))
			(listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(ii 0)
			(layer 0)
		)

	(gimp-image-undo-disable image)
	(set! ii 0)
	(while (< ii num-layers)
		(set! layer (list-ref layer-array ii))
		(if (isTrue isSetVisible)
			(gimp-layer-set-visible layer TRUE)
			(gimp-layer-set-visible layer FALSE)
		)
		(set! ii (+ ii 1))
	)
	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
(define (script-fu-gimp-layer-group-show image lgname isOnlyLinkedLayers sFilter)
	(let* (
		
		)
		(script-fu-gimp-layer-group-show-or-hide image lgname isOnlyLinkedLayers sFilter #t)
	
	)
)
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
(define (script-fu-gimp-layer-group-hide image lgname isOnlyLinkedLayers sFilter)
	(let* (
		
		)
		(script-fu-gimp-layer-group-show-or-hide image lgname isOnlyLinkedLayers sFilter #f)
	
	)
)
; -----------------------------------------------
(define (generate-new-layer-name sCurrLayerName sLayerGroupOldParm sLayerGroupNew isAllowDuplicateName)
; @@@@@ Given current layer name belinging to group sLayerGroupOld, generate a new name by substituting old group by new group in the name
; @@@@@@ currentlayername= <sLayerGroupOld><suffix>  newLayerName= <sLayerGroupNew><suffix>
; @@@ ex: AAA.BBB.whatever   -> AAA.NEWBBB.whatever
	(let*
		(
			(sLayerGroupOld sLayerGroupOldParm)
			(iLen-layer-group-name (string-length sLayerGroupOld))
			(iLen-layer-name (string-length sCurrLayerName))

			(layer-name-sufix (substring sCurrLayerName iLen-layer-group-name iLen-layer-name) )
			;///(infix-name "")
			(new-layer-name sLayerGroupNew)

		)
		(if (= iLen-layer-group-name 0)
			(set! sLayerGroupOld "[GROUP]")
		)
		(if (= (string-length (string-trim new-layer-name)) 0)
			; empty destination group name becomes the original group name
			(begin
				(set! new-layer-name sLayerGroupOld)
			)
		)
		(if (equal? new-layer-name sLayerGroupOld)
			(if (not isAllowDuplicateName)
				; equal destination group name and source group name lead to the insertion of ".DUPLICATED" in the destination group name
				(begin
					(set! new-layer-name (string-append new-layer-name ".DUPLICATED"))
				)
			)
		)
		(set! new-layer-name (string-append new-layer-name layer-name-sufix)) ; add the suffix (after the group name) of the current layer name
		new-layer-name ;return value

	);let
)
; ===========================================
(define (renameThisLayer image layer layer-name lgname sNewLayerGroupName) ; duplictes the layer, adds to the image and set LINK on
	(let*
		(
			(new-layer-name (generate-new-layer-name layer-name lgname sNewLayerGroupName #f))
		)
		(gimp-layer-set-name layer new-layer-name) ; name changed to "<sNewLayerGroupName><LayerNameSuffix>"
		new-layer-name

	);let
)
; ===========================================
(define (duplicateThisLayer image layer layer-name lgname destinGroupName iImageDestination) ; duplictes the layer, adds to the image and set LINK on
	(let*
		(
			(layer-copy 0)

			(iDestImage image)
			(new-layer-name "")
			(isAllowDuplicates #f)
		)
		(if (not (= iImageDestination 0))
			(set! iDestImage iImageDestination)
		)
		(if (not (= iDestImage image)) ; if the duplicate is going to be added to another image, then the current name may be duplicated there...
			(set! isAllowDuplicates #t) ; ... but the name can still clash with another name in the other image!
		)
		(set! new-layer-name (generate-new-layer-name layer-name lgname destinGroupName isAllowDuplicates))
		(set! layer-copy (car (gimp-layer-new-from-drawable layer iDestImage)))
		(gimp-layer-set-name layer-copy new-layer-name) ; name changed to "<destinationGroupName><LayerNameSuffix>"


		(gimp-image-add-layer iDestImage layer-copy -1)
		; //@@(gimp-layer-set-linked layer-copy TRUE)
		layer-copy ;;// returns the duplicated layer

	);let
)
(define (isTrueCheck xValue)
; @@@ some gimp calls return 0/1 instead of TRUE/FALSE like stated in the docs. This function detects this and returns true if xValue is numeric contais the numeric value 1
	(let* 
		(
			(ii 0 )
			(isTrue #f)
		)
		(cond 
			((number? xValue)
				(if (= xValue 1)
					(set! isTrue #t)
				)
			)
			((boolean? xValue)
				(set! isTrue xValue)
				
			)
		)
		isTrue ;
	)


)
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;---------------------------------------------------------
(define (script-fu-gimp-layer-group-duplicate image lgname isOnlyLinkedLayers sFilter destinGroupName isOnAnotherImage iImageDestination)
	; @@@@@@@@@@@@@@@@@@@@@@@@@@@@ delete all layers belonging to a layer group
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(layer-name "")
			(ii 0)
			(layer 0)
			(iImageDestin iImageDestination ) ; the image whereto to copy the layers
			;--
			(oneDuplicatedLayer 0)
			(cntDuplicatedLayers 0)
			(listDuplicatedLayers '())
		)

	(gimp-image-undo-disable image)
	
	(if (> num-layers 0)
		(begin
			(if (eq? isOnAnotherImage FALSE)
				(set! iImageDestin 0 ) ; if the toggle 'duplicate on another image' is not set, ignore the iImageDestination parameter: zero means 'current image' 
			)
			(set! ii (- num-layers 1) )
			(while (>= ii 0)
				
				(set! layer (list-ref layer-array ii))
				(set! layer-name (car (gimp-layer-get-name layer)))
				;----------------------------------------------------
				(set! oneDuplicatedLayer (duplicateThisLayer image layer layer-name lgname destinGroupName iImageDestin));
				(set! cntDuplicatedLayers (+ cntDuplicatedLayers 1))

				(set! listDuplicatedLayers (cons oneDuplicatedLayer listDuplicatedLayers ))
				;---------------------------------------------------
				(set! ii (- ii 1))
			)
			(set! listDuplicatedLayers (cons cntDuplicatedLayers listDuplicatedLayers) )
			;-----------
			(if (= iImageDestin 0) ;.. duplicated in current image ?
				(begin
					(script-fu-gimp-layer-group-unlinkall image ) ; remove all links, in order to link all duplicates
					(do-link-this-list-of-layers listDuplicatedLayers image ); @@@@@@
				)
				;---else
				(begin
					(script-fu-gimp-layer-group-unlinkall iImageDestin ) ; remove all links, in order to link all duplicates
					(do-link-this-list-of-layers listDuplicatedLayers iImageDestin ); @@@@@@
				)
			)
			
		
		)
		
	)
	

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
; ------@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@---------------------------------------

; --------------#######################################33
(define (do-link-this-list-of-layers listLayers image )
	(let* 	(
				(cntLayers  (car listLayers)) ; @@@@@@@@ why are you crashing here ?
				(layer-array  (cdr listLayers))
				(theLayer 0)
				(ii 0)
			)
			(while (< ii cntLayers)
				(set! theLayer (list-ref layer-array ii))
				(gimp-layer-set-linked theLayer TRUE)
				
				; --------
				(set! ii (+ ii 1))
			)
		

	)
)
; --------------#######################################33
(define (do-set-visibility-this-list-of-layers listLayers image isVisible)
	(let* 	(
				(cntLayers  (car listLayers)) ; @@@@@@@@ why are you crashing here ?
				(layer-array  (cdr listLayers))
				(theLayer 0)
				(ii 0)
			)
			(if (isTrue isVisible)
				(begin
					(while (< ii cntLayers)
						(set! theLayer (list-ref layer-array ii))
						(gimp-layer-set-visible theLayer TRUE)
						; --------
						(set! ii (+ ii 1))
					)
				
				)
				;---else
				(begin
					(while (< ii cntLayers)
						(set! theLayer (list-ref layer-array ii))
						(gimp-layer-set-visible theLayer FALSE)
						; --------
						(set! ii (+ ii 1))
					)
				)
			)
			
		

	)
)
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(define (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter isSelectAllIfNoCriteria) ; 
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; @@ Support function used by other functions: collects a list of layers that satisfy the conditions: 
; @@ 1) belong to a layergroup name <lgname> (unless it is empty, then all layers apply)
; @@ 2) if <isOnlyLinkedLayers> is TRUE then also the layers to be selected have to be linked 
; @@ 3) if <sFilter> is not empty, then the layer name must contain the <sFilter> text somewhere in the name
; @@ The returned list is in correct sequence and the top element (wich must be removed) is the count of layers that
; @@ satisfy the criteria 
	(let* 
		(
			(layers (gimp-image-get-layers image))
			(num-layers (car layers))
			(layer-array (cadr layers))
			(ii (- num-layers 1))
			(layer 0)
			(layer-name "")
			(layer-name-group "")
			(isLinked #f)
			(nLinkValue 0)
			(isSelectThisOne #t)
			(iLenFilter (string-length (string-trim sFilter)))
			(iLenLayerGroup (string-length (string-trim lgname)))
			(isCheckThisLayer #t)

			; ------------
			(oneLayer 0)
			(cntLayers 0)
			(listSelectedLayers '())
			;-------
			(isMultipleFilters #f)
			(isORFilters #f)
			(listFilters '() )
			
		)

	;--------------------------------
	; (if (and (= iLenLayerGroup 0) (= iLenFilter 0) )
		; (set! ii -1) ; to prevent the while loop to be entered
		;;-------- else
		; (set! ii (- num-layers 1)) ; normal case, start from the last layer upwards
	; )
	(if (> iLenFilter 0)
		(begin
			;..(set! isMultipleFilters (iif (>= (iPosSubstring sFilter 0 "|" #f) 0) #t #f) ) ; OR filter
			; (isSubstring sStr sSub)
			(set! isMultipleFilters (isSubstring sFilter "|")  ) ; OR filter
			(if (isTrue isMultipleFilters)
				(begin
					(set! isORFilters #t)
					(set! listFilters (stringSplit sFilter "|"  ) )
					
				)
				;---else, not a OR filter, try a AND filter
				(begin
					;..(set! isMultipleFilters (iif (>= (iPosSubstring sFilter 0 "^" #f) 0) #t #f) ) ; AND filter
					(set! isMultipleFilters (isSubstring sFilter "^")  ) ; OR filter
					(if (isTrue isMultipleFilters)
						(begin
							(set! isORFilters #f) ; it is a AND filter
							(set! listFilters (stringSplit sFilter "^"  ) )
							
						)
						;---else
						(begin
							
						)
					)
				
				)
			)
			
		)
		;---else
		(begin
		)
	)
	(set! ii (- num-layers 1)) ; 
	(while (>= ii 0) ; old = (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		; /// (set! layer-name-group (car (strbreakup layer-name " ")))
		(set! isCheckThisLayer #t )
		(if (> iLenLayerGroup 0 ) ; compare the first part of the layer's name with lgname
			(begin
				(set! layer-name-group (layerNameToSearchSize layer-name lgname)) ; added to allow for subgrouping
				(set! isCheckThisLayer (equal? layer-name-group lgname) )
			)
		)
		(if (eq? isCheckThisLayer #t )
			
			(begin
				
				(set! isSelectThisOne #t ) ;...isSelectAllIfNoCriteria )
				(if (eq? isOnlyLinkedLayers TRUE)
					(begin
						;@@@@@@@@@@@@@@@@@@ it should return TRUE/FALSE, according to the docs, but it retuns 0/1
						(set! nLinkValue (car (gimp-layer-get-linked layer)))
						(set! isLinked  (isTrueCheck nLinkValue ) )
						; @@@@@@@@@@@@@@@@@@@@@
						(set! isSelectThisOne isLinked ) ;
						
					)
					;------else
					(begin
						(set! isSelectThisOne #t)
					)
				)
				(if (and (eq? isSelectThisOne #t) (> iLenFilter 0)  )
					(begin
						(if (isTrue isMultipleFilters); multiple filter strings separated either by "|" or "^"
							(begin
								(set! isSelectThisOne (isSelectLayerByMultipleFilters layer-name listFilters isORFilters ) )
								
							)
							;---else just a simple filter
							(begin
								(set! isSelectThisOne (isSelectLayerByFilter layer-name sFilter) )
								
							)
						)
					)
					
				)
				(if (eq? isSelectThisOne #t)
					(begin
						(set! cntLayers (+ cntLayers 1))
						(set! listSelectedLayers (cons layer listSelectedLayers ))
						
					)
				)
			)
		)
		(set! ii (- ii 1)) ; =old = (+ ii 1)
	)
	;------------
	
	;===============================================
	(set! listSelectedLayers (cons cntLayers listSelectedLayers) )
	;----------- return value
	listSelectedLayers
	;-------------------
	
	)
)
; @@@@@@@@@>>>>>>>>>>>>>>
;;  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter isSelectAllIfNoCriteria)
(define (collectListLinkedLayers image )
	(let*
		(
			(layers (gimp-image-get-layers image))
			(num-layers (car layers))
			(layer-array (cadr layers))
			(ii (- num-layers 1))
			(layer 0)
			
			(isLinked #f)
			(nLinkValue 0)
			(cntLayers 0)
			(listLayers '())
			
		)
		;-----return value
		(while (>= ii 0)
			(set! layer (aref layer-array ii))
			(set! nLinkValue (car (gimp-layer-get-linked layer)))
			;...(set! isLinked  (isTrueCheck nLinkValue ) )
			(if (= nLinkValue TRUE ); is linked
				(begin
					(set! cntLayers (+ cntLayers 1))
					(set! listLayers (cons layer listLayers ))
				)
				;---else
				(begin
				)
			)
			(set! ii (- ii 1)) ; =old = (+ ii 1)
		)
		(set! listLayers (cons cntLayers listLayers ))
		;---------------
		listLayers
	)
)
;-------------
(define (collectListVisiblity  image isVisible)
	(let*
		(
			(layers (gimp-image-get-layers image))
			(num-layers (car layers))
			(layer-array (cadr layers))
			(ii (- num-layers 1))
			(layer 0)
			
			(isLinked #f)
			(nVisibleValue 0)
			(cntLayers 0)
			(listLayers '())
			
		)
		;-----return value
		(if (isTrue isVisible)
			(begin
				(while (>= ii 0)
					(set! layer (aref layer-array ii))
					(set! nVisibleValue (car (gimp-layer-get-visible layer)))
					
					(if (= nVisibleValue TRUE ); is visible
						(begin
							(set! cntLayers (+ cntLayers 1))
							(set! listLayers (cons layer listLayers ))
						)
						;---else
						(begin
						)
					)
					(set! ii (- ii 1)) ; =old = (+ ii 1)
				)
			
			)
			;---else
			(begin
				(while (>= ii 0)
					(set! layer (aref layer-array ii))
					(set! nVisibleValue (car (gimp-layer-get-visible layer)))
					
					(if (= nVisibleValue FALSE ); is NOT visible
						(begin
							(set! cntLayers (+ cntLayers 1))
							(set! listLayers (cons layer listLayers ))
						)
						;---else
						(begin
						)
					)
					(set! ii (- ii 1)) ; =old = (+ ii 1)
				)
			)
		)
		
		(set! listLayers (cons cntLayers listLayers ))
		;---------------
		listLayers
	)
)

;---------------------------------------------------------
(define (saveRestoreStatusFromMemory image iMemorySlot iLoadSave iActivity oPubSlots )
; @@@ generic function for saving lists in memory slots
; @@@@@ iActivity =1 --> save restore LINKS, iActivity =2 -->save restore visibility
	(let* (
			( listSelectedLayers '() )
			(ii 0)
			(layer 0)
			(iOrigImage -1 )
			
		)

	
	(cond
		((= iLoadSave 0) ; save
			(cond
				((= iActivity 1) ; LINKS
					(set! listSelectedLayers (collectListLinkedLayers image ))
				)
				((= iActivity 2) ; VISIBILITY
					(set! listSelectedLayers (collectListVisiblity  image #t) )
				)
			)
			
			(vector-set! oPubSlots iMemorySlot (cons image listSelectedLayers))
			
		)
		((= iLoadSave 1) ; restore
			(set! listSelectedLayers (vector-ref oPubSlots iMemorySlot )  )
			(if (not (null? listSelectedLayers) )
				(begin
					(set! iOrigImage (car listSelectedLayers ) )
					(if (= image iOrigImage) ; protection against trying to restore settings of a non-current image
						(begin
							(cond
								((= iActivity 1) ; LINKS
									(do-link-this-list-of-layers (cdr listSelectedLayers) image )
								)
								((= iActivity 2) ; VISIBILITY
									(do-set-visibility-this-list-of-layers (cdr listSelectedLayers) image #t)
								)
							)
						)
						;---else
						(begin
						)
					)
					
				)
				;---else
				(begin
				)
			)
		
		)
	)
	(gimp-displays-flush)
	
	)
)
(define pub_listLinkedLayers #('() '() '()  '() '() '() '() '() '() '()   '() '() '()  '() '() '() '() '() '() '()  '() '() '() '() '() '() '())  )

(define pub_listVisibleLayers #('() '() '() '() '() '() '() '() '()  '()  '() '() '()  '() '() '() '() '() '() '()  '() '() '() '() '() '() '())  )



;---------------------------------------------------------
(define (script-fu-gimp-layer-group-linked-layers-save-restore image iMemorySlot iLoadSave)
	(let*
		(
			(ii 0)
			
		)
		; (saveRestoreStatusFromMemory image iMemorySlot iLoadSave iActivity oPubSlots )
		(saveRestoreStatusFromMemory image iMemorySlot iLoadSave 1 pub_listLinkedLayers )
		
	)
)
; (script-fu-gimp-layer-group-visible-layers-save-restore image iMemorySlot iLoadSave)
;---------------------------------------------------------
(define (script-fu-gimp-layer-group-visible-layers-save-restore image iMemorySlot iLoadSave)
	(let*
		(
			(ii 0)
			
		)
		; (saveRestoreStatusFromMemory image iMemorySlot iLoadSave iActivity oPubSlots )
		(saveRestoreStatusFromMemory image iMemorySlot iLoadSave 2 pub_listVisibleLayers )
		
	)
)




;---------------------------------------------------------
(define (script-fu-gimp-layer-group-delete image lgname isOnlyLinkedLayers sFilter)
	; @@@@@@@@@@@@@@@@@@@@@@@@@@@@ delete all layers belonging to a layer group
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(ii 0)
			(layer 0)
		)

	(gimp-image-undo-disable image)

	(set! ii 0)
	(while (< ii num-layers)
		
		(set! layer (list-ref layer-array ii))
		(gimp-image-remove-layer image layer);
		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
;---------------------------------------------------------
(define (script-fu-gimp-layer-group-rename image lgname isOnlyLinkedLayers sFilter sNewLayerGroupName)
	; @@@@@@@@@@@@@@@@@@@@@@@@@@@@ renames all layers belonging to a layer group
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(ii 0)
			(layer 0)
			(layer-name "")
		)

	(gimp-image-undo-disable image)
	(set! ii 0)
	(while (< ii num-layers)
		(set! layer (list-ref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		(renameThisLayer image layer layer-name lgname sNewLayerGroupName)
		
		(set! ii (+ ii 1))
	)
	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)

; -------------------------------------------------
(define (saveThisLayerAsImageFile layer sFileName sFileExt)
	(let*
		(
			(oBuf (car (gimp-edit-named-copy layer "ttt")))
			(oImage (car (gimp-edit-named-paste-as-new oBuf)))
			
		)
		(if (string-ci=? sFileExt "gif")
			(begin
				(if (not (= (car (gimp-image-base-type oImage)) INDEXED))
					(begin
						(gimp-image-convert-indexed oImage NO-DITHER MAKE-PALETTE 255 FALSE FALSE "")
					)
					
				)
			)
			
		)
		(gimp-file-save RUN-NONINTERACTIVE oImage (car (gimp-image-get-active-layer oImage)) sFileName sFileName)
		(gimp-buffer-delete oBuf)
		(gimp-image-delete oImage)
		;-----return value
		sFileName 
	)
)
;---------------------------------------------------------
(define (script-fu-gimp-layer-group-save-to-individual-image-files image lgname isOnlyLinkedLayers sFilter isAlternativeDir sOutputDir sFileNamePrefix sFileExt)
	; @@@@@@@@@@@@@@@@@@@@@@@@@@@@ renames all layers belonging to a layer group
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(ii 0)
			(layer 0)
			(layer-name "")
			(iLenExt (string-length sFileExt))
			(sFileName "")
			(sDirName (getCurrImageDir image ))
		)

	;(gimp-image-undo-disable image)
	(if (= iLenExt 0)
		(begin
			(set! sFileExt "png")
		)
		;---else
		(begin
		)
	)
	;--------------------
	(if (eq? isAlternativeDir TRUE)
		(begin
			(if (> (string-length (string-trim sOutputDir)) 0)
				(begin
					(set! sDirName sOutputDir)
				)
				
			)
			
		)
		
	)
	
	;----------------------
	(set! ii 0)
	(while (< ii num-layers)
		(set! layer (list-ref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		(set! sFileName (sFullFilePathName sDirName (string-append sFileNamePrefix (string-trim layer-name) "." sFileExt) )  )
		(saveThisLayerAsImageFile layer sFileName sFileExt)
		
		(set! ii (+ ii 1))
	)
	(gimp-displays-flush)
	;(gimp-image-undo-enable image)
	)
)
; ###############@@@@@@@@@@@@@@@@@@@@@@@@@########################@@@@@@@@@@@@@@@@@@@@@@@@@@@@########################
; ###############@@@@@@@@@@@@@@@@@@@@@@@@@########################@@@@@@@@@@@@@@@@@@@@@@@@@@@@########################
; ###############@@@@@@@@@@@@@@@@@@@@@@@@@########################@@@@@@@@@@@@@@@@@@@@@@@@@@@@########################
; ###############@@@@@@@@@@@@@@@@@@@@@@@@@########################@@@@@@@@@@@@@@@@@@@@@@@@@@@@########################
;---------------------------------------------------------
(define (script-fu-gimp-layer-group-pattern-rename image lgname isOnlyLinkedLayers sFilter sPattern sNewPattern isMultipleReplace )

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@ rename some patterns in layers of a layergroup or addoc selection
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(ii 0)
			(layer 0)
			(layer-name "")
			(sNewLayerName "")
		)
	(if (= num-layers 0 ) ; if there where no filters, select all layers
		(begin
			(set! listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #t ) )
			(set! num-layers (car listSelectedLayers))
			(set! layer-array (cdr listSelectedLayers ))
		)
		
	)
	(gimp-image-undo-disable image)
	(set! ii 0)
	(while (< ii num-layers)
		(set! layer (list-ref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		;...(renameThisLayer image layer layer-name lgname sNewLayerGroupName)
		(set! sNewLayerName (sReplaceSubString layer-name sPattern sNewPattern isMultipleReplace) )
		; (isStringMatch sStr1 sStr2)
		(if (not (string=? layer-name sNewLayerName )) 
			(gimp-layer-set-name layer sNewLayerName) ; 
		
		)
		
		(set! ii (+ ii 1))
	)
	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)


;=========================================================
(define (script-fu-gimp-layer-group-showall image)
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group ""))

	(gimp-image-undo-disable image)

	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(gimp-layer-set-visible layer TRUE)
		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
;=========================================================
(define (script-fu-gimp-layer-group-hideall image)
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group ""))

	(gimp-image-undo-disable image)

	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(gimp-layer-set-visible layer FALSE)
		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
;=========================================================
(define (script-fu-gimp-layer-group-showallbut image lgname)
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group ""))

	(gimp-image-undo-disable image)

	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		
		(set! layer-name-group (layerNameToSearchSize layer-name lgname)) ; added to allow for subgrouping
		(if (not (equal? layer-name-group lgname))
			; compare the first part of the layer's name with lgname
			(gimp-layer-set-visible layer TRUE)
		)
		(if (equal? layer-name-group lgname)
			; compare the first part of the layer's name with lgname
			(gimp-layer-set-visible layer FALSE)
		)
		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)

(define (script-fu-gimp-layer-group-hideallbut image lgname)
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group ""))

	(gimp-image-undo-disable image)

	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		
		(set! layer-name-group (layerNameToSearchSize layer-name lgname)) ; added to allow for subgrouping
		(if (not (equal? layer-name-group lgname))
			; compare the first part of the layer's name with lgname
			(gimp-layer-set-visible layer FALSE)
		)
		(if (equal? layer-name-group lgname)
			; compare the first part of the layer's name with lgname
			(gimp-layer-set-visible layer TRUE)
		)
		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)

;-------------------######################################################--------------------------------------
(define (script-fu-gimp-layer-group-drop-shadow image lgname isOnlyLinkedLayers sFilter offx offy blur color opacity resize)
	; @@@@@@@@@@@@@@@@@@@@@@@@@@@@ renames all layers belonging to a layer group
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(ii 0)
			(layer 0)
			(layer-name "")
			(sNewLayerName "")
			(oShadowLayer 0)
			(iCnt 0)
			(iPos 0)
		)

	; (gimp-image-undo-disable image)
	(set! ii 0)
	(while (< ii num-layers)
		(set! layer (list-ref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		;--------------------------------------------------------------------------------------------------------------
		(script-fu-drop-shadow image layer offx offy blur color opacity resize)
		;--------------------------------------------------------------------------------------------------------------
		;--- the drop shadow script creates a new layer (below the current layer) with a name 'Drop Shadow' 'Drop Shadow#1' etc...
		;---we want the name of this drop shadow to be the same name as the layer that got shadowed folowed by a sufix '.[Shadow].nnn' where nnn is a sequence number
		; --- we try to locate the created layer with the name "Drop Shadow" by searching the layerarray 2 positions above the last known position ii, to be on the safe side
		;(set! iPos (- ii 2) )
		(set! iPos (- (car (gimp-image-get-layer-position image layer)) 2) )
		(set! oShadowLayer (getLayerObjGivenLayerName image iPos "Drop Shadow" )); 
		(if (>= oShadowLayer 0) 
			(begin  ; rename this layer
				; (generate-new-layer-name sCurrLayerName sLayerGroupOldParm sLayerGroupNew isAllowDuplicateName); (generate-new-layer-name layer-name lgname lgname #t)
				(set! sNewLayerName (string-append  layer-name ".[DShadow]." (i2sfs iCnt 3)  )) 
				(gimp-layer-set-name oShadowLayer sNewLayerName) ; 
				(set! iCnt (+ iCnt 1) ) ; sequence number
			)
			;---else
			(begin
			)
		)
		
		(set! ii (+ ii 1))
	)
	(gimp-displays-flush)
	; (gimp-image-undo-enable image)
	)
)
;----------------------#########################################33-----------------------------------
(define (script-fu-gimp-layer-group-perspective-shadow image lgname isOnlyLinkedLayers sFilter
			alpha rel-distance rel-length shadow-blur shadow-color shadow-opacity interpolation allow-resize
		)
	; @@@@@@@@@@@@@@@@ 
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(ii 0)
			(layer 0)
			(layer-name "")
			(sNewLayerName "")
			(oShadowLayer 0)
			(iCnt 0)
			(iPos 0)
		)

	; (gimp-image-undo-disable image)
	(set! ii 0)
	(while (< ii num-layers)
		(set! layer (list-ref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		;--------------------------------------------------------------------------------------------------------------
		(script-fu-perspective-shadow image layer alpha rel-distance rel-length shadow-blur shadow-color shadow-opacity interpolation allow-resize )
		;--------------------------------------------------------------------------------------------------------------
		
		;--- the perspective shadow script creates a new layer (above the current layer as stated in the docs) with a name 'Perspective Shadow' 'Perspective Shadow#1' etc...
		;---we want the name of this shadow to be the same name as the layer that got shadowed folowed by a sufix '.[Shadow].nnn' where nnn is a sequence number
		
		; --- we try to locate the created layer with the name "Perspective Shadow" by searching the layerarray 2 positions above the last known position ii, to be on the safe side
		; (set! iPos (- ii 2) )
		(set! iPos (- (car (gimp-image-get-layer-position image layer)) 2) )
		(set! oShadowLayer (getLayerObjGivenLayerName image iPos "Perspective Shadow" )); Perspective Shadow
		
		
		(if (>= oShadowLayer 0) 
			(begin  ; rename this layer
				; (generate-new-layer-name sCurrLayerName sLayerGroupOldParm sLayerGroupNew isAllowDuplicateName); (generate-new-layer-name layer-name lgname lgname #t)
				(set! sNewLayerName (string-append  layer-name ".[PShadow]." (i2sfs iCnt 3)  )) 
				(gimp-layer-set-name oShadowLayer sNewLayerName) ; 
				(set! iCnt (+ iCnt 1) ) ; sequence number
			)
			;---else
			(begin
			)
		)
		
		(set! ii (+ ii 1))
	)
	(gimp-displays-flush)
	; (gimp-image-undo-enable image)
	)
)
; =======================================########################33=================================================
(define (script-fu-gimp-layer-group-translucent-3D-effect image lgname isOnlyLinkedLayers sFilter
		hl-offset-x hl-offset-y hl-color hl-opacity-comp ds-color ds-opacity ds-blur ds-offset-x ds-offset-y keep-selection	
		)
	; @@@@@@@@@@@@@@@@ 
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(ii 0)
			(layer 0)
			(layer-name "")
			(sNewLayerName "")
			(oShadowLayer 0)
			(iCnt 0)
			(iPos 0)
		)

	; (gimp-image-undo-disable image)
	(set! ii 0)
	(while (< ii num-layers)
		(set! layer (list-ref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		;--------------------------------------------------------------------------------------------------------------
		(script-fu-xach-effect image layer hl-offset-x hl-offset-y hl-color hl-opacity-comp ds-color ds-opacity ds-blur ds-offset-x ds-offset-y keep-selection	 )
		;--------------------------------------------------------------------------------------------------------------
		(set! iPos 0 ) ; (set! iPos (car (gimp-image-get-layer-position image layer)) )
		; Highlight
		(set! oShadowLayer (getLayerObjGivenLayerName image iPos "Highlight" )); 
		(if (>= oShadowLayer 0) 
			(begin  ; rename this layer
				; (generate-new-layer-name sCurrLayerName sLayerGroupOldParm sLayerGroupNew isAllowDuplicateName); (generate-new-layer-name layer-name lgname lgname #t)
				(set! sNewLayerName (string-append  layer-name ".[Translucent3D-High]." (i2sfs iCnt 3)  )) 
				(gimp-layer-set-name oShadowLayer sNewLayerName) ; 
				(set! iCnt (+ iCnt 1) ) ; sequence number
			)
			
		)
		(set! oShadowLayer (getLayerObjGivenLayerName image iPos "Shadow" )); Perspective Shadow
		(if (>= oShadowLayer 0) 
			(begin  ; rename this layer
				; (generate-new-layer-name sCurrLayerName sLayerGroupOldParm sLayerGroupNew isAllowDuplicateName); (generate-new-layer-name layer-name lgname lgname #t)
				(set! sNewLayerName (string-append  layer-name ".[Translucent3D-Shadow]." (i2sfs iCnt 3)  )) 
				(gimp-layer-set-name oShadowLayer sNewLayerName) ; 
				(set! iCnt (+ iCnt 1) ) ; sequence number
			)
			
		)
		;-------------++++++++++++++
		; still to do: the Translucend 3D effect throws the new created layers to the top of the stack (in a big image hundreds of layers upwards)
		; This should not be done. We have to correct that 'throw' by throwing the created layers back to the position just ABOVE each of the processed layers.
		
		(set! ii (+ ii 1))
	)
	(gimp-displays-flush)
	; (gimp-image-undo-enable image)
	)
)

; --------------------------------
(define (isSelectLayerByFilter layer-name sFilter)
	(let* (
				(ii 0 )
				(iLenFilter (string-length (string-trim sFilter)) )
				(iPosFilterFound -1)
				(isSelect #f)
			)
		(if (> iLenFilter 0)
			(begin
				(set! iPosFilterFound (subString-pos layer-name sFilter) )
				(if (>= iPosFilterFound 0) ; means that the filter text  was found in the layer name
					(set! isSelect #t )
				)
			)
					
		)	
		isSelect
	)
)
; --------------------------------
(define (isSelectLayerByMultipleFilters layer-name listFilters isORFilters )
	(let* (
				(ii 0 )
				(sFilter "")
				(iCntFilters (length listFilters))
				(iLenFilter 0 ) 
				(iPosFilterFound -1)
				(isSelect #f)
				(isSelectOut (iif isORFilters #f #t))
			)
		(while (< ii iCntFilters)
			(set! sFilter (string-trim (list-ref listFilters ii) ) )
			
			(if (> (string-length sFilter) 0)
				(begin
					;....(set! isSelect (isSelectLayerByFilter layer-name sFilter) )
					(set! iPosFilterFound (subString-pos layer-name sFilter) )
					(set! isSelect (iif  (>= iPosFilterFound 0)  #t #f ) )
					(if (isTrue isORFilters) 
						(begin
							(if (isTrue isSelect) ; under OR filters, it is enough if one of the filters applies
								(begin
									(set! isSelectOut #t)
									(set! ii iCntFilters) ; to quit loop
								)
								;---else
								(begin
								)
							)
						)
						;---else is AND filters
						(begin
							(if ((isFalse isSelect)) ; under AND filters, all filters must be present
								(begin
									(set! isSelectOut #f)
									(set! ii iCntFilters) ; to quit loop
								)
								;---else
								(begin
								)
							)
						)
					)
					
				)
				
			)
			;---------
			(set! ii (+ ii 1))
		)
		
		isSelectOut
	)
)
;---------------------------------------------------
(define (MultipleFilterDetection sFilter)
; @@ utility function
	(let*
		(
			(ii 0)
			(iLenFilter (string-length (string-trim sFilter)))
			(listFilters '() )
			(isMultipleFilters #f)
			(isORFilters #f)
			(oListOut "")
		)
		(if (> iLenFilter 0)
			(begin
				(set! isMultipleFilters (isSubstring sFilter "|")  ) ; OR filter
				(if (isTrue isMultipleFilters)
					(begin
						(set! isORFilters #t)
						(set! listFilters (stringSplit sFilter "|"  ) )
					)
					;---else, not a OR filter, try a AND filter
					(begin
						(set! isMultipleFilters (isSubstring sFilter "^")  ) ; OR filter
						(if (isTrue isMultipleFilters)
							(begin
								(set! isORFilters #f) ; it is a AND filter
								(set! listFilters (stringSplit sFilter "^"  ) )
							)
							
						)
					
					)
				)
				
			)
			
		)
		(set! oListOut (cons isMultipleFilters (cons isORFilters (cons listFilters oListOut))) )
		;-----return value
		oListOut 
	)
)
; #####################################################################################################
; #####################################################################################################
(define (script-fu-gimp-layer-group-link image lgname sFilter isUnlinkParm) ; @$@$@$@$@$@$@$@$@$
	(let* (
			(layers (gimp-image-get-layers image))
			(num-layers (car layers))
			(layer-array (cadr layers))
			(ii 0)
			(layer 0)
			(layer-name "")
			(layer-name-group "")
			;...(iLenFilter (string-length (string-trim sFilter)) )
			(isLink #t)
			(iLenFilter (string-length (string-trim sFilter)))
			(iLenLayerGroup (string-length (string-trim lgname)))
			(isCheckThisLayer #t)
			(isUnlink isUnlinkParm )
			;-------
			(isMultipleFilters #f)
			(isORFilters #f)
			(listFilters '() )
			(oL)
		
		)

	(if (> iLenFilter 0)
		(begin
			(set! oL (MultipleFilterDetection sFilter))
			(set! isMultipleFilters (car oL) )
			(set! isORFilters (car (cdr oL) ) )
			(set! listFilters  (car (cdr (cdr oL) ) ) )
			
			
		)
		
	)
	(gimp-image-undo-disable image)

	(set! ii 0)
	(if (and (= iLenLayerGroup 0) (= iLenFilter 0) )
		(set! ii num-layers) ; to prevent the while loop from being entered
		; -------- else
		(set! ii 0) ; normal case, start from the first layer upwards
	)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		(set! isCheckThisLayer #t )
		(if (> iLenLayerGroup 0 )
			(begin
				(set! layer-name-group (layerNameToSearchSize layer-name lgname)) ; added to allow for subgrouping ; compare the first part of the layer's name with lgname
				(set! isCheckThisLayer (equal? layer-name-group lgname) )
			)
		)
		; ///(set! layer-name-group (car (strbreakup layer-name " ")))
		; (set! layer-name-group (layerNameToSearchSize layer-name lgname)) ; added to allow for subgrouping
		(if (eq? isCheckThisLayer #t) 
			(begin
				(if (> iLenFilter 0) 
					(begin
						(if (isTrue isMultipleFilters); multiple filter strings separated either by "|" or "^"
							(begin
								(set! isCheckThisLayer (isSelectLayerByMultipleFilters layer-name listFilters isORFilters ) )
							)
							;---else just a simple filter
							(begin
								(set! isCheckThisLayer (isSelectLayerByFilter layer-name sFilter) )
							)
						)
					)
						
				)
				(if (isTrue isCheckThisLayer) 
					(if (eq? isUnlink TRUE)
						(gimp-layer-set-linked layer FALSE)
						(gimp-layer-set-linked layer TRUE)
					)
					
				)
			)
		)
		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)

(define (script-fu-gimp-layer-group-unlinkall image )
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(layer-name "")
		(layer-name-group ""))

	(gimp-image-undo-disable image)

	(set! ii 0)
	(while (< ii num-layers)
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(gimp-layer-set-linked layer FALSE)
		; (set! layer-name (car (gimp-layer-get-name layer)))
		; /// (set! layer-name-group (car (strbreakup layer-name " ")))
		; (set! layer-name-group (layerNameToSearchSize layer-name lgname)) ; added to allow for subgrouping
		; (if (equal? layer-name-group lgname)
			; ; compare the first part of the layer's name with lgname
			; (gimp-layer-set-linked layer FALSE)
		; )
		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)




;=====================================================================================================================
(define (getListLinkedLayersAndRefLayer image sRefLayerName iLayerID sFilter)  ; 
; --------------------------------------------------------------------------------------------------------
; @@@ Creates a list of all layers of the image that are linked. Each entry is the LAYER ID number, which can be used to extract extra info about the layer
; @@@ While it collects the layers that are LINKED, it identifies the REFERENCE layer. If sRefLayerName is not an empty string, it identifies 
; @@@ the first (from the top of all layers) layer name that starts with the given sRefLayerName and returns its INDEX (on the complete layer set) on the top of the list
; @@@ If the sFilter is specified (non-empty string) it filters the group of linked layers to layers have 
; @@@ a specified filter text in the layer name
; @@@ If sRefLayerName is an empty string, it identifies the layer whith the same identification number as iLayerID and 
; @@@ it returns (on top of the returned list) the index of it on the complete set of layers of the image 
; @@@ NEW: if sRefLayerName is an empty string AND iLayerID==-99, it considers the first linked layer found as the REFERENCE LAYER
; @@@ 
; -----------------------------------------------------------------------------------------------------
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(lglist '())
		
		(iDestinationLayer -1)
		; (layer-movecount 0)
		;--------
		; (jj 0)
		; (isProceed #t)
		; (iDeltaMove 0)
		; (iLayer2Move 0)
		(iLenRefLayerName (string-length (string-trim sRefLayerName) ) )
		(isRefLayerByName #f)
		(isFindRefLayer #t)
		(layer-name "")
		(layer-name-group "")
		;-------
			(iLenFilter (string-length (string-trim sFilter)))
			(isMultipleFilters #f)
			(isORFilters #f)
			(listFilters '() )
			(oL)
			(isSelectThisLayer #t)
			(isRefFirstLinkedLayer #f)
		)
	
	(if (> iLenRefLayerName 0)
		(begin
			(set! isRefLayerByName #t)
		)
		;--- else
		(begin 
			(if (= iLayerID -99)
				(begin
					(set! isRefFirstLinkedLayer #t)
				)
				
			)
		)
	)
	(if (> iLenFilter 0)
		(begin
			(set! oL (MultipleFilterDetection sFilter))
			(set! isMultipleFilters (car oL) )
			(set! isORFilters (car (cdr oL) ) )
			(set! listFilters  (car (cdr (cdr oL) ) ) )
		)
		
	)
	; procure a list of all the layers
	(set! ii 0)
	(while (< ii num-layers) 
		; check the first word in the name of each layer, if it matches lgname, perform the action
		(set! layer (aref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		(if (eq? isFindRefLayer #t)
		
			(if (eq? isRefLayerByName #t) 
				(begin
					;...(set! layer-name (car (gimp-layer-get-name layer)))
					(set! layer-name-group (layerNameToSearchSize layer-name sRefLayerName)) ; added to allow for subgrouping ; compare the first part of the layer's name with lgname
					
					(if (equal? layer-name-group sRefLayerName) ; if we found a layergroup name that matches the reference layer name...
						(begin
							(set! iDestinationLayer ii)
							(set! isFindRefLayer #f)
						)
					)
				)
				;------- else isRefLayerByName
				(if (equal? layer iLayerID)
					(begin
						(set! iDestinationLayer ii)
						(set! isFindRefLayer #f)
					)
				)
			)
		)
		
		(if (=  (car(gimp-layer-get-linked layer)) TRUE)  ; we only collect linked layers
			(begin
				(set! isSelectThisLayer #t)
				(if (> iLenFilter 0) 
					(begin
						(if (isTrue isMultipleFilters); multiple filter strings separated either by "|" or "^"
							(begin
								(set! isSelectThisLayer (isSelectLayerByMultipleFilters layer-name listFilters isORFilters ) )
							)
							;---else just a simple filter
							(begin
								(set! isSelectThisLayer (isSelectLayerByFilter layer-name sFilter) )
							)
						)
					)
						
				)
				(if (isTrue isSelectThisLayer)
					(begin
						(set! lglist (cons layer lglist)) ; this adds the layers to the list in reverse order
						(if (isTrue isRefFirstLinkedLayer) ; is the first found linked layer to be considered the reference layer ? (used for reverse order of linked layers)
							(begin
								(set! iDestinationLayer ii)
								(set! isFindRefLayer #f)
								(set! isRefFirstLinkedLayer #f)
							)
						)
						
					)
					
				)
				
			)
		)
		(set! ii (+ ii 1))
	)
	; --------------------------------
	(set! lglist (cons iDestinationLayer lglist)) ; add the found destination layer to the top of the list

	)
)
;=====================================================================================================================
(define (script-fu-gimp-layers-linked-movebelow-bis image sFilter sRefLayerName iLayerID)  ; 
	(script-fu-gimp-layers-linked-movebelow image sFilter sRefLayerName iLayerID )
)

;=====================================================================================================================
(define (script-fu-gimp-layers-linked-movebelow image sFilter sRefLayerName iLayerID )  ; ^^^^^^^^^^^^@@@@@@@@@@@@@@@@@@@@^^^^^^^^^^^^^^
; @@@@@@@@ sFilter functionality to be implemented !
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(lglist '())
		
		(iDestinationLayer -1)
		(layer-movecount 0)
		;--------
		(jj 0)
		(isProceed #t)
		(iDeltaMove 0)
		(iLayer2Move 0)
		
		
		)
	
	(gimp-image-undo-disable image)
	(set! lglist (getListLinkedLayersAndRefLayer image sRefLayerName iLayerID sFilter))
	

	; ;---------------------------------
	(set! iDestinationLayer (car lglist) )
	(set! lglist (cdr lglist)) ; because top element in the list is the destination layer index
	;-----------------
	(set! lglist (reverse lglist));  reverse the order of the collected layer indexes because the last is on top of the list...
	; use this list to find each layer
	(set! num-layers (length lglist))
	
	(if (< iDestinationLayer 0) ; iDestinationLayer must be >=0 !!
		(set! isProceed #f) ; this could happen if the user chooses a layer in ANOTHER image as a reference layer!
	)
	(set! ii 0)
	(while (and (eq? isProceed #t) (< ii num-layers)) 
		
		(set! layer (list-ref lglist ii))
		(set! iLayer2Move (car (gimp-image-get-layer-position image layer)))
		
		;-------------
		; find out how many idividual moves we need to move this layer DOWNWARDS
		(set! iDeltaMove (- iDestinationLayer iLayer2Move) )
		(if (> iDeltaMove 0)  
			(begin  ; move downwards, the source layer is above the destination layer
				(set! jj 0)
				(while  (< jj iDeltaMove)
					(gimp-image-lower-layer image layer)
					(set! jj (+ jj 1))
				)

			)

		)
		
		(set! ii (+ ii 1))

	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
;----------------------------------------------------------
(define (script-fu-gimp-layers-linked-moveabove-bis image sFilter sRefLayerName iLayerID ) 
	(script-fu-gimp-layers-linked-moveabove image sRefLayerName iLayerID)
)
; (define (script-fu-gimp-layers-linked-moveabove image sRefLayerName iLayerID) 
	; (script-fu-gimp-layers-linked-moveabove image sRefLayerName iLayerID)
; )
;=====================================================================================================================
(define (script-fu-gimp-layers-linked-moveabove image sFilter sRefLayerName iLayerID )  ; ##################[ $%$%$%$%$$%$%$%$%$% ]######################3
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(lglist '())
		
		(iDestinationLayer -1)
		(layer-movecount 0)
		;--------
		(jj 0)
		(isProceed #t)
		(iDeltaMove 0)
		(iLayer2Move 0)
		)
	(gimp-image-undo-disable image)
	(set! lglist (getListLinkedLayersAndRefLayer image sRefLayerName iLayerID sFilter))

	; ;---------------------------------
	(set! iDestinationLayer (car lglist) )
	(set! lglist (cdr lglist)) ; remove top element because top element in the list is the destination layer index
	; we do not reverse the order of the list, because the last one should be the first to move upwards
	; use this list to find each layer
	(set! num-layers (length lglist))
	
	(if (< iDestinationLayer 0) ; iDestinationLayer must be >=0 !!
		(set! isProceed #f) ; this could happen if the user chooses a layer in ANOTHER image as a reference layer!
	)
	(set! ii 0)
	(while (and (eq? isProceed #t) (< ii num-layers)) 
		
		(set! layer (list-ref lglist ii))
		(set! iLayer2Move (car (gimp-image-get-layer-position image layer)))
		
		;-------------
		; find out how many idividual moves we need to move this layer UPWARDS
		(set! iDeltaMove (-  iLayer2Move iDestinationLayer ))
		(if (> iDeltaMove 0)  
			(begin  ; move upwards, the source layer is below the destination layer
				(set! jj 0)
				(while  (< jj iDeltaMove)
					(gimp-image-raise-layer image layer)
					(set! jj (+ jj 1))
				)
			)
			; if the iDeltaMove ==0 we do not move anything
		)
		
		(set! ii (+ ii 1))

	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
; #######################################################################################

;---------------------------------------------------------
(define (script-fu-gimp-layer-group-movebelow image lgname isOnlyLinkedLayers sFilter sREFLayer iREFLayerID)
	; @@@@@@@@@@@@@@@@@@@@@@@@@@@@ move a layer group BELOW a specific reference layer
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(iDestinationLayer -1)
			(iLayer2Move 0)
			(iDeltaMove 0)
			(ii 0)
			(jj 0)
			(layer 0)
		)

	(gimp-image-undo-disable image)
	(set! iDestinationLayer (locateReferenceLayer image sREFLayer iREFLayerID) )
	(if (>= iDestinationLayer 0)
		(begin
			;...(set! layer-array (reverse layer-array));  reverse the order of the collected layer indexes because the last is on top of the list...
			; we do not reverse the order of the list, because the last one should be the first to move upwards
			(set! ii 0)
			(while (< ii num-layers)
				
				(set! layer (list-ref layer-array ii))
				(set! iLayer2Move (car (gimp-image-get-layer-position image layer)))
				;-------------
				; find out how many idividual moves we need to move this layer DOWNWARDS
				(set! iDeltaMove (- iDestinationLayer iLayer2Move) )
				(if (> iDeltaMove 0)  
					(begin  ; move downwards, the source layer is above the destination layer
						(set! jj 0)
						(while  (< jj iDeltaMove)
							(gimp-image-lower-layer image layer)
							(set! jj (+ jj 1))
						)
					)
					; if the iDeltaMove ==0 we do not move anything
				)
				
				(set! ii (+ ii 1))
			)
		)
		;---else
		(begin
		)
	)
	

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
;--------------------------------------------------------------
(define (locateReferenceLayer image sRefLayerName iLayerID)
	(let*
		(
			(layers (gimp-image-get-layers image))
			(num-layers (car layers))
			(layer-array (cadr layers))
			(iLenRefLayerName (string-length (string-trim sRefLayerName) ) )
			(isRefLayerByName #f)
			(layer 0)
			(layer-name "")
			(layer-name-group "")
			(ii 0)
			
			(iDestinationLayer -1)
		)
		(if (> iLenRefLayerName 0)
			(set! isRefLayerByName #t)
		)
		(set! ii 0)
		(while (< ii num-layers) 
			
			(set! layer (aref layer-array ii))
			(set! layer-name (car (gimp-layer-get-name layer)))
			(if (eq? isRefLayerByName #t) 
				(begin
					;...(set! layer-name (car (gimp-layer-get-name layer)))
					(set! layer-name-group (layerNameToSearchSize layer-name sRefLayerName)) ; added to allow for subgrouping ; compare the first part of the layer's name with lgname
					
					(if (equal? layer-name-group sRefLayerName) ; if we found a layergroup name that matches the reference layer name...
						(begin
							(set! iDestinationLayer ii)
							(set! ii num-layers) ; to quit loop
							
						)
					)
				)
				;------- else isRefLayerByName
				(if (equal? layer iLayerID)
					(begin
						(set! iDestinationLayer ii)
						(set! ii num-layers) ; to quit loop
						
					)
				)
			)
			
			(set! ii (+ ii 1))
		)
		;-----return value
		iDestinationLayer 
	)
)
;======================================@@@@@@@@@@@@===============================================================================
(define (script-fu-gimp-layers-linked-reverse-order image sFilter )  ; ##################[ $%$%$%$%$$%$%$%$%$% ]######################3
; @@@ reverse the order of the current set of linked layers
; @@@ 
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(lglist '())
		
		(iDestinationLayer -1)
		(layer-movecount 0)
		;--------
		(jj 0)
		(isProceed #t)
		(iDeltaMove 0)
		(iLayer2Move 0)
		(iLayerID -99) ; indicate that the first linked layer will be considered the reference layer
		(sRefLayerName "")
		;-------
		
		)
	(gimp-image-undo-disable image)
	(set! lglist (getListLinkedLayersAndRefLayer image sRefLayerName iLayerID sFilter))

	
	;---------------------
	(set! lglist (cdr lglist)) ; remove top element because top element in the list is the destination layer index
	(set! lglist (reverse lglist));  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	
	; use this list to find each layer
	(set! num-layers (length lglist))
	
	
	(set! ii 0)
	(while (< ii num-layers) 
		
		(set! layer (list-ref lglist ii))
		
		
		(if (= ii 0) ; the first linked layer found of the list is the reference layer
			(begin
				(set! iDestinationLayer (car (gimp-image-get-layer-position image layer)))
				
			)
		)
		(set! iLayer2Move (car (gimp-image-get-layer-position image layer)))
		
		;-------------
		; find out how many idividual moves we need to move this layer UPWARDS
		(set! iDeltaMove (-  iLayer2Move iDestinationLayer ))
		(if (> iDeltaMove 0)  
			(begin  ; move upwards, the source layer is below the destination layer
				(set! jj 0)
				(while  (< jj iDeltaMove)
					(gimp-image-raise-layer image layer)
					(set! jj (+ jj 1))
				)
			)
			; if the iDeltaMove ==0 we do not move anything
		)
		
		(set! ii (+ ii 1))

	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
;---------------------------------------------------------
(define (script-fu-gimp-layer-group-reverse-order image lgname isOnlyLinkedLayers sFilter )
	; @@@@@@@@@@@@@@@@@@@@@@@@@@@@ move a layer group ABOVE a specific reference layer
	;... sREFLayer iREFLayerID
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(iDestinationLayer -1)
			(iLayer2Move 0)
			(iDeltaMove 0)
			(ii 0)
			(jj 0)
			(layer 0)
		)

	(gimp-image-undo-disable image)
	; reference layer is the first layer of the group!
	

	(set! ii 0)
	(while (< ii num-layers)
		
		(set! layer (list-ref layer-array ii))
		(if (= ii 0)
			(begin  ; reference layer is the first layer of the group!
				(set! iDestinationLayer (car (gimp-image-get-layer-position image layer)) )
			)
			;---else
			(begin
				(set! iLayer2Move (car (gimp-image-get-layer-position image layer)))
		
				;-------------
				; find out how many idividual moves we need to move this layer UPWARDS
				(set! iDeltaMove (-  iLayer2Move iDestinationLayer ))
				(if (> iDeltaMove 0)  
					(begin  ; move upwards, the source layer is below the destination layer
						(set! jj 0)
						(while  (< jj iDeltaMove)
							(gimp-image-raise-layer image layer)
							(set! jj (+ jj 1))
						)
					)
					; if the iDeltaMove ==0 we do not move anything
				)
				;..(gimp-image-remove-layer image layer);
			
			) ; end else
		)
		
		(set! ii (+ ii 1))
	)
		
	

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
;---------------------------------------------------------
(define (script-fu-gimp-layer-group-moveabove image lgname isOnlyLinkedLayers sFilter sREFLayer iREFLayerID)
	; @@@@@@@@@@@@@@@@@@@@@@@@@@@@ move a layer group ABOVE a specific reference layer
	(let* (
			(layers (gimp-image-get-layers image))
			( listSelectedLayers (collectListOfLayersBasedOnCriteria image lgname isOnlyLinkedLayers sFilter #f ) )
			(num-layers (car listSelectedLayers))
			(layer-array (cdr listSelectedLayers ))
			(iDestinationLayer -1)
			(iLayer2Move 0)
			(iDeltaMove 0)
			(ii 0)
			(jj 0)
			(layer 0)
		)

	(gimp-image-undo-disable image)
	(set! iDestinationLayer (locateReferenceLayer image sREFLayer iREFLayerID) )
	(if (>= iDestinationLayer 0)
		(begin
			(set! layer-array (reverse layer-array));  reverse the order of the collected layer indexes because the last is on top of the list...
			
			(set! ii 0)
			(while (< ii num-layers)
				
				(set! layer (list-ref layer-array ii))
				(set! iLayer2Move (car (gimp-image-get-layer-position image layer)))
		
				;-------------
				; find out how many idividual moves we need to move this layer UPWARDS
				(set! iDeltaMove (-  iLayer2Move iDestinationLayer ))
				(if (> iDeltaMove 0)  
					(begin  ; move upwards, the source layer is below the destination layer
						(set! jj 0)
						(while  (< jj iDeltaMove)
							(gimp-image-raise-layer image layer)
							(set! jj (+ jj 1))
						)
					)
					; if the iDeltaMove ==0 we do not move anything
				)
				;..(gimp-image-remove-layer image layer);
				(set! ii (+ ii 1))
			)
		)
		;---else
		(begin
		)
	)
	

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
;=====================================================================================================================
(define (script-fu-gimp-linked-layers-delete image sFilter  )  ; ##################[ $%$%$%$%$$%$%$%$%$% ]######################3
; @@@
; @@@
	(let* ((layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		(ii 0)
		(layer 0)
		(lglist '())
		
		(iDestinationLayer -1)
		(layer-movecount 0)
		;--------
		(jj 0)
		(isProceed #t)
		(iDeltaMove 0)
		(iLayer2Move 0)
		(sRefLayerName "") 
		(iLayerID -99)
		)
	(gimp-image-undo-disable image)
	(set! lglist (getListLinkedLayersAndRefLayer image sRefLayerName iLayerID sFilter))

	; ;---------------------------------
	(set! iDestinationLayer (car lglist) )
	(set! lglist (cdr lglist)) ; remove top element because top element in the list is the destination layer index
	
	(set! num-layers (length lglist))
	(set! ii 0)
	(while  (< ii num-layers)
		(set! layer (list-ref lglist ii))
		(gimp-image-remove-layer image layer);
		(set! ii (+ ii 1))
	)

	(gimp-displays-flush)
	(gimp-image-undo-enable image)
	)
)
; ===========================================
(define (generate-selectable-list-open-images )
; @@@@@@@@ generates an array of strings where each string contains the <Id>-><fileNameOfTheImage>. The <Id> is the integer
; specifying the image id in GIMP
	(let*
		(
			(oListOut '() )
			(oListImages '()  )
			(iCntImages 0)
			(aImage-ids '() )
			(ii 0)
			(iElem 0)
			(sElem "")
			(sInfo "")
			(sCurrImageEntry "0->[Current image]")
		)
		(set! oListImages (gimp-image-list) )
		(set! iCntImages (car oListImages))
		(set! aImage-ids (cadr oListImages) )
		(set! sInfo (number->string iCntImages))
		(while (< ii iCntImages)
			(set! iElem (aref aImage-ids ii) )
			(set! sElem (string-append (number->string iElem) "->" (string-trim (car (gimp-image-get-filename iElem)))) )
			;//(set! sElem (string-append sElem "^^^ " (string-LeftOf "->" "abcd->xyz")))
			(set! oListOut (cons sElem oListOut))
			(set! ii (+ ii 1))
		)
		(set! sCurrImageEntry (string-append sCurrImageEntry " | cnt images= " sInfo))
		(set! oListOut (cons sCurrImageEntry oListOut))

		oListOut


	);let
)

; =================================================
(define (script-fu-gui-get-layer-list image )
; generate a list of layers to be presented to the user via the GUI
	(let* (
		(layers (gimp-image-get-layers image))
		(num-layers (car layers))
		(layer-array (cadr layers))
		;///(layer-count 0)
		(ii 0)
		(layer 0)
		;///(iElem 0)
		(sElem "")
		(sInfo "")
		(oListOut '() )
		(layer-name "")
		(layer-name-group "")
		)

	(while (< ii num-layers)
		; ----------
		(set! layer (aref layer-array ii))
		(set! layer-name (car (gimp-layer-get-name layer)))
		(set! sElem (string-append (number->string ii) "->" (string-trim layer-name) ))
		(set! oListOut (cons sElem oListOut))

		(set! ii (+ ii 1))
	)
	(set! oListOut (reverse oListOut))
	oListOut

	)
)




; -------------------------------- GUI support ---------------------------------
; ===========================================
(define (script-fu-gui-force-large-width iWidth )
; @@@@@@@@ used as a dummy to force the GUI window to be larger than normal. A SF-OPTION command in combination with this
; call, generates a menu containing one line with the indicated iWidth chars (max 300), forcing the GUI window to be at least as large (for the values)
	(let*
		(
			(oListOut '() )
			(ss "....:....1....:....2....:....3....:....4....:....5....:....6....:....7....:....8....:....9....:....0....:....1....:....2....:....3....:....4....:....5....:....6....:....7....:....8....:....9....:....0....:....1....:....2....:....3....:....4....:....5....:....6....:....7....:....8....:....9....:....0")

		)

		(set! oListOut (cons (substring ss 0 iWidth) oListOut))
		oListOut
	);let
)


(define (script-fu-gui-help-info-txtlines iIndexHelp)

	(let*
		(
			(ii 0)
			(sOut "")
			(aList (script-fu-gui-help-info iIndexHelp #f ))
			(iCnt (length aList))
			(sSep "")
		)
		(while (< ii iCnt)
			(set! sSep (iif (> ii 0 ) "\n" "" ) )
			(set! sOut (string-append sOut sSep (list-ref aList ii) ) )
			(set! ii (+ ii 1) )
		)
		;-----return value
		sOut 
	)
)

(define (script-fu-gimp-layer-groups-help-overview image) ; dummy function, does nothing
	(let*
		(
			(sOut "")
		)
		;-----return value
		sOut 
	)
)
(define (script-fu-gimp-layer-groups-help-overview-2 image) ; dummy function, does nothing
	(let*
		(
			(sOut "")
		)
		;-----return value
		sOut 
	)
)
(define (script-fu-gimp-layer-groups-help-overview-3 image) ; dummy function, does nothing
	(let*
		(
			(sOut "")
		)
		;-----return value
		sOut 
	)
)
(define (script-fu-gimp-layer-groups-help-overview-4 image) ; dummy function, does nothing
	(let*
		(
			(sOut "")
		)
		;-----return value
		sOut 
	)
)
(define (@helpLines: aHelpLines . aText)
	(let*
		(
			(ii 0)
			(iCnt (length aText))
			(sOut "")
		)
		(while (< ii iCnt)
			(set! aHelpLines (cons (list-ref aText ii) aHelpLines))
			(set! ii (+ ii 1))
		)
		;-----return value
		aHelpLines 
	)
)

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(define @@guiLang@@ 1) ; language 1 = eng (english), language 2= fra (french)
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

(define (script-fu-gui-help-info iIndexHelp isMenu)
; @@@ Retrieves the help text for a particular language (for example french). The current 
; @@@ language is defined by the @@guiLang@@ global variable. 1=english, 2=french, 3=...whatever
; @@@ If a particular help item has not been defined for a language, then the english
; @@@ text will be automatically retrieved 
	(let*
		(
			
			(oListOut '() )
		)
		(cond
			((= @@guiLang@@ 1) ; ENGLISH
				(set! oListOut (script-fu-gui-help-info-eng iIndexHelp isMenu) )
			)
			((= @@guiLang@@ 2) ; FRENCH
				(set! oListOut (script-fu-gui-help-info-fra iIndexHelp isMenu) )
			)
		)
		
		;-----return value
		(if (< (length oListOut) 1 )
			
			;--- if some help item has not been defined in a particular language, return the english text
			(begin
				(set! oListOut (script-fu-gui-help-info-eng iIndexHelp isMenu) )
			)
		)
		oListOut 
	)
)

; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ FRENCH help info @@@@@@@@@@@@@@@@@@@@@@@@@@@

(define (script-fu-gui-help-info-fra iIndexHelp isMenu)
; @@@ This is an incomplete base script to be used for french users. Please let some french native speaker  
; @@@ complete this script, by adding (in french) the options that have been written in english in the (script-fu-gui-help-info-eng ) script 
	(let*
		(
			(oListOut '() )
		)
		(cond
			((= iIndexHelp 0)
				(set! oListOut (@helpLines: oListOut 			
					(string-append  "Generalites Groupement Calques du Gimp" (help-info-open-menu-prompt-fra isMenu) )
					"======================================="
					;0....:....1....:....2....:....3....:....4....:....5....:....      6....:....7....:....8....:....9....:....0....:....1....:....2
					
					"Layer Groups (groupes de calques) vous permet d'utiliser"
					"plusieurs fonctions manipulatives sur des groupes de calques"
					"de votre image, au lieux d'etre force' a apliquer ces"
					"fonctions individualement sur toutes les calques `a changer"
					"@@@ Qui vas faire la traduction integrale francaise ? @@@"
					)
				)
			
			)
			((= iIndexHelp 3) ; (script-fu-gui-help-info-txtlines 3)    (script-fu-gui-help-info 3 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "REMOUVEZ groupement de calques" (help-info-open-menu-prompt-fra isMenu) )
					"======================================="
					"REMOUVES tous les groupes de calques qui ont des nons"
					"qui comencent  avec un non de calc qui vous specifiez."
					"------------"
					;"If the optional FILTER text is mentioned,"
					;"only those members of the group which have that text in the"
					;"layer name will be selected for this activity"
					"Si le text optional FILTRE est mencione'"
					"les seules calques selectiones pour cette operation"
					"sont les calques qui contiennent ce texte dans le non"
					"------------" 
					;"If you only specify the FILTER (without a Layer group"
					;"selection), then you select all layers of the whole IMAGE"
					;"having the specified filter text ANYWHERE in the layer name"
					"Si vous specifiez le FILTRE (sans mentioner le groupe"
					"de calques) vous seletionez tous les calques de l'image"
					"qui contiennent le text FILTRE mentione' dans de non"
					"du calque (quelque parte)"
					)
				)
				
			)
			((= iIndexHelp 12) ; (script-fu-gui-help-info-txtlines 12) ;    (script-fu-gui-help-info 12 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "@@ FRENCH @@ Move LINKED Layers ABOVE a specific reference layer" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"@@ FRENCH @@  Move ALL relevant LINKED layers ABOVE another layer to be"
					"used as a reference. Only the linked layers which are"
					"*located BELOW* the reference layer will be moved."
					""
					"Ex: Assume layers 2, 3,  6 and 7 are LINKED"
					"are LINKED"
					"Layer sequence BEFORE the linked layer move:"
					"-----------------------------------------------------"
					"'layer-1'            <-- the top layer of the image"
					"'layer-2'              LINKED"
					"'layer-3'              LINKED"
					"'layer-4'"
					"'layer-5-reference'  <-- the reference layer"
					"'layer-6'              LINKED"
					"'layer-7'              LINKED"
					"'layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					""
					"AFTER the move, this will be the resulting sequence:"
					"------------------------------------------------------"
					"'layer-1'            <-- the top layer of the image"
					"'layer-2'              not moved (it was above reference)"
					"'layer-3'              not-moved (it was above reference)"
					"'layer-4'"
					"'layer-6'              MOVED (it was below reference)"
					"'layer-7'              MOVED (it was below reference)"
					"'layer-5-reference'  <--the reference layer"
					"'layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					
					)
				)
				(set! oListOut (append (script-fu-help-info-duplications-fra 2) oListOut ) ) ;
			)
		)
		;-----return value
		(if (> (length oListOut) 1)
			(begin
				(set! oListOut (reverse oListOut))
			)
			
		)
		
		oListOut 
	)
)
;-----------------------------------------------------
(define (script-fu-help-info-duplications-fra iIndexHelp)
	(let*
		(
			
			(oListOut '() )
		)
		(cond
			((= iIndexHelp 1)
				
				(set! oListOut (@helpLines: oListOut 
					"@@ FRENCH @@ Translate!"
					"If the 'Optional FILTER' is mentioned, only those members"
					"of the group which contain that text ANYWHERE in the layer"
					"name will be selected for this activity."
					""
					"If you only specify the FILTER (without a Layer group"
					"selection), then you select *all layers of the whole IMAGE*"
					"having the specified filter text ANYWHERE in the layer name"
					
					)
				)
			)
			((= iIndexHelp 2)
				
				(set! oListOut (@helpLines: oListOut 
					"@@ FRENCH @@ Translate!"
					"You can specify the REFERENCE layer in one of 2 ways:"
					"#(1) specify the name of the layer or a group name. If"
					"specified, then the first layer (counting from the TOP of"
					"the layers) that matches the specified layer name (or group"
					"name) will become the REFERENCE layer."
					"#(2) Choose the layer from a menu of all layers (of all"
					"images!)."
					"NOTE:If you choose a layer from another image than the"
					"current one, then no layer will me moved! "
					
					)
				)
			)
			((= iIndexHelp 3)
				
				(set! oListOut (@helpLines: oListOut 
					"@@ FRENCH @@ Translate!"
					"If you save the list in a memory slot that is occupied"
					"with another list, the old list is overwritten by the"
					"new one"
					
					)
				)
			)
			
		)
		;-----return value
		oListOut 
	)
)
;--------
(define (help-info-open-menu-prompt-fra isMenu)
	(let*
		(
			
			(sOut (iif (isTrue isMenu) "         [#] SVP, Ouvrez le menu pour lire..." ""))
		)
		;-----return value
		sOut 
	)
)
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ENGLISH help info @@@@@@@@@@@@@@@@@@@@@@@@@@@

; ===========================================
(define (script-fu-gui-help-info-eng iIndexHelp isMenu)
; @@@@@@@@ used as a dummy to force the GUI window to be larger than normal. A SF-OPTION command in combination with this
; call, generates a menu containing one line with the indicated iWidth chars (max 300), forcing the GUI window to be at least as large (for the values)
	(let*
		(
			(oListOut '() )
		)
		(cond
			((= iIndexHelp 0) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				(set! oListOut (@helpLines: oListOut 			
					(string-append  "Overview (1) Gimp LayerGroups" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					;0....:....1....:....2....:....3....:....4....:....5....:....      6....:....7....:....8....:....9....:....0....:....1....:....2
					
					"Layer Groups offers you an extensive set of functions"
					"allowing you to do certain changes to groups of layers instead"
					"of to individual layers one at a time."
					""
					"For example, assume you are designing screens for a"
					"future application. If your screens are based on tabs and "
					"those tabs can contain sub-tabs, you will want to be able"
					"to define your layer names in such a way that you can" 
					"later hide (or set visible) all the layers containing components"
					"(list boxes, radio buttons etc ) that belong to a particular tab"
					"or sub-tab."
					"Example of layer names in this scenario:" 
					""
					"Midarea.tabarea.tab1.Label"
					"Midarea.tabarea.tThiab1.Subtab1.Label"
					"Midarea.tabarea.tab1.Subtab1.Radio1"
					"Midarea.tabarea.tab1.Subtab1.Radio2"
					"Midarea.tabarea.tab1.Subtab2.Label"
					"Midarea.tabarea.tab1.Subtab2.Listbox1"
					"Midarea.tabarea.tab1.Subtab2.Radio1"
					"Midarea.tabarea.tab1.Subtab2.Radio2"
					"Midarea.tabarea.tab1.Subtab3.Label"
					"Midarea.tabarea.tab1.Subtab3.Radio2"
					""
					"Using functions of this Layer Groups system you can"
					"for example hide all the layers belonging to group"
					"'Midarea.tabarea.tab1.Subtab1' which in this case will"
					"hide 3 layers and you can set visible the group"
					"'Midarea.tabarea.tab1.Subtab2' which in this case will" 
					"set visible 4 layers simultaneously. But you can also hide the" 
					"group 'Midarea.tabarea.tab1', which will simultaneously hide"
					"10 layers."
					"                                ...continues at Overview (2)..."

					
					
					)
				)
				
			)
			((= iIndexHelp 100) ; (script-fu-gui-help-info-txtlines 3)    (script-fu-gui-help-info 3 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Overview (2) Gimp LayerGroups" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"                           ...continuation from Overview (1) ..."
					"Grouping and subgrouping (of layers) for a particular activity"
					"like hiding or'unhiding' layers can thus easily be achieved by"
					"simply mentioning a shorter or larger layer name prefix for your"
					"group. When, for example, you mention group"  
					"'Midarea.tabarea.tab1.Subtab1',"
					"this Layer Groups system is just selecting all layers"
					"of your image whose names begin with that particular prefix"
					"and then applying the operation to those selected layers"
					"collectively." 
					""
					"Available functions allow you to LINK or UNLINK groups of"
					"layers, RENAME groups of layers, DELETE groups of layers,"
					"DUPLICATE groups of layers (even into other images). You can" 
					"also MOVE groups of layers upwards or downwards in the layer"
					"stack ('raise' or 'lower' the layers). You can also change"
					"portions of the layer names of all layers, even layers that are"
					"not grouped as described before (ex: change 'Selected' into"
					"'selected' for all layers where 'Selected' occurs in the layer"
					"names). Another usefull option is taking a group of layers"
					"and generating images of a particular type (png, jpeg, gif...)"
					"for each of the individual layers. For example you could take"
					"a group of 50 layers and generate 50 png images on disk..."
					"                                 ...continues at Overview (3)..."
					
					)
				)
				
			)
			((= iIndexHelp 101) ; (script-fu-gui-help-info-txtlines 3)    (script-fu-gui-help-info 3 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Overview (3) Gimp LayerGroups" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"                     ...continuation from Overview (2) ..."
					
					"---- FILTERING options for selection groups ----"
					"Many Layer Groups functions allow you to select layers"
					"not only by mentioning the 'prefix' layer group name, but"
					"also by allowing you to filter that selected group for"
					"specific text strings contained in the layer group name."
					"For example, where a FILTER is allowed you could mention"
					"the text '.global.'. This would mean that only the layers of"
					"the specified layer group which contain the text '.global.' in"
					"the layer group name would be selected for the particular"
					"activity you are porsuing."
					"This filtering allows also an extra degree of flexibility:"
					"You can mention MULTIPLE filters of one of 2 possible kinds."
					"(1) To indicate that you want to select layers based on 'OR'"
					"criteria (for example '.global.' OR '.particular.', you separate"
					"the criteria with a vertical bar '|', like in '.global.|.particular.'"
					"(2) To indicate that you want to select layers based on 'AND'"
					"criteria (for example '.global.' AND '.particular.' you separate"
					"the criteria with a caret '^' char, like in '.global.^.particular.'"
					"This would mean that only a layer containing both text strings in"
					"the layer name would be selected, like for example the following"
					"layer: 'This.global.group.that.particular.subgroup.layer'"
					""
					"You may mention many 'OR' string patterns or many 'AND' string"
					"patterns in the FILTER (not only 2 as in the example above),"
					"but you may not intermix 'OR' with 'AND'. Thus you may use"
					"either the '|' symbol or the '^' symbol, but not both"
					"simultaneously in the same filter string."
					"Example 1: FILTER: 'Red|Blue|Yellow|Green': only layers of the"
					"group containing any of the color names in the layer name will"
					"be selected"
					"Example 2: FILTER: 'Tab1^Red^Panel5': only layers of the"
					"group containing ALL these text names in the layer name will"
					"be selected. The layer named 'Screen.Tab1.Panel5.RedButton'"
					"would be selected."
					"                                 ...continues at Overview (4)..."
					)
				)
				
			)
			((= iIndexHelp 102) ; (script-fu-gui-help-info-txtlines 3)    (script-fu-gui-help-info 3 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Overview (4) Gimp LayerGroups" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"                     ...continuation from Overview (3) ..."
					""
					"This GIMP LayerGroups (v. 1.0a) system has been developed"
					"in 2008/2009 by Miguel Oliveira (melocotone at gmail"
					"dot com) based on initial work done by Joseph Miller"
					"(josephcmiller2 at gmail dot com)"
					""
					"This built-in HELP system has been prepared for translation"
					"in other languages. The Script-Fu code contains an initial"
					"example showing how to convert the help system to French"
					"I hope other people will find this Layer Groups system"
					"usefull enough to merit a translation of the help"
					"system into other languages like French, German, Spanish,"
					"Portuguese, etc."
					""
					"The Script-Fu code of this Layer Groups system contains"
					"many usefull GENERIC functions that could be reused when"
					"developing other scripts for GIMP."
					""
					"-----"
					"This LayerGroups subsystem appears by default at the"
					"main menu of the GIMP Image window. If you prefer to have"
					"it under the 'Layer' menu of that window you only have"
					"to change ONE character at line 35 of the script."
					"Use a simple text editor to open the script file, locate"
					"line 35 (should begin with '#f'), change the 'f' to 't'"
					"(thus the line should begin with '#t' after the change)"
					"save the script file and from the GIMP menu choose"
					"'Filters' -> 'Script-Fu' -> 'Refresh Scripts'. That's all!" 
					""
					"Miguel Oliveira"
					"(The Netherlands)"
					"Email: melocotone at gmail dot com"
					"June 30, 2009"
					)
				)
				
			)
			((= iIndexHelp 1)
					
				(set! oListOut (@helpLines: oListOut 
					(string-append  "SAVE or RESTORE a set of visible layers" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"[#]SAVING: Saves the current status of VISIBILITY of layers"
					"(which are hidden, which are visible) in a file in the same"
					"directory as the current image."
					"This file is named the same as the current image except for"
					"an extra 'suffix' containing the name of the SET between"
					"'{' and '}'. The extension of the file is 'XCF^V'."
					"This file is a simple TEXT file containing the names of the"
					"layers and the indication 'visibility yes/no'"
					"Ex: image name='/myGimp/myImage.xcf', set name='mySet',"
					"filename where saved='/myGimp/myImage{mySet}.XCF^V'"
					"-------------"
					"[#]Image version OPTION: "
					"This option makes it possible to save/restore visibility settings"
					"that may be used with several versions of the same image"
					"For this option to work, the file name must contain - near the"
					"file extension - the pattern '@@' followed by some text"
					"specifying the version number. In this case the save/restore"
					"file name will consist of the text up to the '@@' pattern"
					"(not included) followed by the file extension."
					"Ex:  image name='/myGimp/myImage@@version3.2.xcf',"
					"set name='mySet', "
					"filename where saved='/myGimp/myImage{mySet}.XCF^V'"
					"-------------"
					"[#]RESTORING: Restores the original status of visibility of"
					"the layers of this image, this of course, only when there"
					"is a set with the given name saved for the current image."
					"The layers that cannot be found because they in the mean"
					"time have been renamed or deleted, will be ignored"
					"-------------"
					"[#]Remark: Be patient: restoring can take many seconds,"
					"even minutes if you have hundreds of layers! If you are"
					"impatient, try then setting the visibility layer by layer..."
					"For the technically inclined: string processing using script-fu"
					"is very slow because strings are lists of characters and"
					"lists are processed by 'peeling off' elements fom the "
					"beginning, repeatedly... we need some speedy string"
					"processing extension functions written in C..."
					"Reading/writing text files from/to disk from script-fu"
					"is also a painfully slow affair..."
					
					
					)
				)
			)	
			;--------
			((= iIndexHelp 2) ; (script-fu-gui-help-info-txtlines 2)    (script-fu-gui-help-info 2 #t)
								
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "DUPLICATE layers belonging to a Layer Group" (help-info-open-menu-prompt-eng isMenu) )
									"=========================================="
					"DUPLICATES all layers whose names *START* with the given"
					"'Selection Layer Group Name'. If a different image is"
					"specified, it duplicates those layers in that specified"
					"image. Unlinks all layers first, then links the duplicates."
					""
					"NOTE: to duplicate the layer group into ANOTHER IMAGE"
					"you must select the checkbox '<-- duplicate layers on"
					"another image', and select the destination image under"
					"'SELECT destination image', otherwise the layers"
					"will be duplicated in the current image!"
					""
					
					"Every duplicated layer is named <DestinationLayerGroupName>"
					"<LayerNameSuffix> where <LayerNameSuffix> represents the"
					"portion of the layer name that FOLLOWS the 'Selection"
					"Layer Group Name`"
					""					
					"Example: groupname 'AA.BBB', destination group name="
					"'AA.XXXX; layer 'AA.BBB.whatever.name' is duplicated to"
					"'AA.XXXX.whatever.name'."
					""
					"If group name and destination name are equal (in the same"
					"image!), '.DUPLICATED' is appended to the group name."
					""
					"You can duplicate a subset of the layers of the specified"
					"Layer Group, by specifying 'duplicate only members of the"
					"Layer Group which are LINKED'."
					""
					"If the optional FILTER text is mentioned, only those members"
					"of the group which have that text in the layer name will be"
					"duplicated."
					""
					"If you only specify the FILTER (without a Layer group"					
					"selection), then you duplicate all layers of the image"
					"having the specified filter text anywhere in the layer name"
					""
					"Duplicating layers has a side effect: All the layers of the"
					"destination image (the current one or another one) are"
					"UN-linked and the new added duplicated layers are all linked." 
					"This is to facilitate a common activity: you duplicate some"
					"existing group of layers and most probably you want to"
					"apply a common operation to them, like moving them inside"
					"the image..." 
					""
					"Duplication location: Before duplicating, make sure to"
					"set the current layer of the destination image. The duplicated"
					"layers will be added to the image ABOVE that current layer."
				
					)
				)
			)
		
			((= iIndexHelp 3) ; (script-fu-gui-help-info-txtlines 3)    (script-fu-gui-help-info 3 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "DELETE Layer Group" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"DELETE all layers whose names *START* with the given Layer"
					"Group Name."
					"------------"
					"If the optional FILTER text is mentioned,"
					"only those members of the group which have that text in the"
					"layer name will be selected for this activity"
					"-----------"
					"You can DELETE a subset of the layers of the specified"
					"Layer Group, by specifying 'delete only members of the"
					"Layer Group which are LINKED'."
					"------------" 
					"If you only specify the FILTER (without a Layer group"
					"selection), then you select all layers of the whole IMAGE"
					"having the specified filter text ANYWHERE in the layer name"
					 
					)
				)
				
			)
			((= iIndexHelp 4) ; (script-fu-gui-help-info-txtlines 4)    (script-fu-gui-help-info 4 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "RENAME Layer Group" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"RENAME all layers whose names *START* with the specified"
					"'Selection Layer Group Name' to a name belonging to the"
					"'NEW Layer Group Name."
					"------------"
					"If the optional FILTER text is mentioned,"
					"only those members of the group which have that text in the"
					"layer name will be selected for this activity"
					"-----------"
					"You can RENAME a subset of the layers of the specified"
					"Layer Group, by specifying 'rename only members of the"
					"Layer Group which are LINKED'."
					"------------" 
					"If you only specify the FILTER (without a Layer group"
					"selection), then you select all layers of the whole IMAGE"
					"having the specified filter text ANYWHERE in the layer name"
					"Each individual layer is renamed in such a way that it"
					"retains the portion of the name that follows the 'Selection'"
					"name." 
					"---------"
					"Example: Assume 'Selection' group 'AAA.BBB' and 'NEW' layer"
					"group 'AAA.NNNN'. An individual Layer named"
					"'AAA.BBB.thisLayer' will be renamed to 'AAA.NNNN.thisLayer';"
					"Another layer 'AAA.BBB.CCC.thatLayer' will be renamed to"
					"'AAA.NNNN.CCC.thatLayer' "
					
					)
				)
			)
			((= iIndexHelp 5) ; (script-fu-gui-help-info-txtlines 5)    (script-fu-gui-help-info 5 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "RENAME some portions of layer names of a Layer group" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"RENAME patterns inside layer names (substrings inside a"
					"layer name) of layers belonging to a Layer group."
					"------"
					"Example: Layer group 'AAA.BBB', pattern 'P1' to be renamed"
					"to pattern 'PBIGONE'; individual layers like"
					"'AAA.BBB.whatever.P1.T2',  are renamed to"
					"'AAA.BBB.whatever.PBIGONE.T2'."
					"-------"
					"If no Selection Group and no Filter is mentioned, then *ALL"
					"layers of the image* will be selected for this operation."
					"This would allow you to systematically change the names"
					"of all layers were for example the text 'yellow'"
					"occurs somewhere in a layer name to the text 'red'" 
					)
				)
				
			)
			((= iIndexHelp 6) ; (script-fu-gui-help-info-txtlines 6)    (script-fu-gui-help-info 6 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "LIST all Layer Names" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"GENERATE a text-layer containing the names of all layers"
					"of the image. The name of this text layer is"
					"'LIST-OF-ALL-LAYERS.' followed by the date and time of"
					"creation of the layer."
					""
					"This functionality gives you an overview of all the layers"
					"of the image (without having to scroll your way out through"
					"the layer dialog). It produces a text layer in your image"
					"at the X-Y location that you specify. If you open the Gimp"
					"text editor you can copy the whole text and paste it into"
					"a text editor for analysis and then delete the generated"
					"text layer."
					"You can enter a name for the list (as a title) and a timestamp"
					"will be added to that name. The image file name (full path)"
					"will also be added on the top of the list, before the"
					"individual text lines containing the layer names are listed."
					""
					"Each subsequent text line starts with a layer sequence number,"
					"followed by a marker (just a 'X' between '[' and ']')"
					"indicating whether the layer is VISIBLE, another marker"
					"indicating whether the layer is LINKED and then the layer"
					"name."
					""
					"The text layer will be created just above the current"
					"layer. It is advisable to set the top layer as current"
					"just before generating the list, otherwise the text layer"
					"can be invisible due to being hidden behind some other layer"
					
					)
				)
				
				
			)
			((= iIndexHelp 7) ; (script-fu-gui-help-info-txtlines 7)  ;  (script-fu-gui-help-info 7 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "SHOW Layer Group" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"SHOW all layers whose names *START* with the given"
					"'Selection Layer Group Name'."
					
					"-----------"
					"You can SHOW a subset of the layers of the specified"
					"Layer Group, by specifying 'show only members of the"
					"Layer Group which are LINKED'."
					"------------" 
					"If you only specify the FILTER (without a Layer group"
					"selection), then you select all layers of the whole IMAGE"
					"having the specified filter text ANYWHERE in the layer name"
					
					""
					"Ex: If your Selection layer group name is 'AA.BBB' then"
					"layer 'AA.BBB.thisLayer' and 'AA.BBB.thatLayer' will be" 
					"marked as VISIBLE but other layers like 'X.YYY.thisLayer' will"
					"not be affected."
					)
				)
				
			)
			((= iIndexHelp 8) ; (script-fu-gui-help-info-txtlines 8)  ;  (script-fu-gui-help-info 8 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "HIDE Layer Group" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"HIDE all layers whose names *START* with the given "
					"'Selection Layer Group Name'."
					
					"-----------"
					"You can HIDE a subset of the layers of the specified"
					"Layer Group, by specifying 'hide only members of the"
					"Layer Group which are LINKED'."
					"------------" 
					"If you only specify the FILTER (without a Layer group"
					"selection), then you select all layers of the whole IMAGE"
					"having the specified filter text ANYWHERE in the layer name"
					""
					"Ex: If your Selection layer group name is 'AA.BBB' then"
					"layer 'AA.BBB.thisLayer' and 'AA.BBB.thatLayer' will be" 
					"marked as HIDDEN but other layers like 'X.YYY.thisLayer' will"
					"not be affected."
					
					)
				)
			)
			((= iIndexHelp 9) ; (script-fu-gui-help-info-txtlines 8) ;    (script-fu-gui-help-info 8 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "LINK or UNLINK layer group" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"LINK or UNLINK all layers whose names *START* with the"
					"specified 'Selection Layer Group Name'"
					""
					"If the 'Optional FILTER' is specified, then only the layers"
					"which belong to the specified group *AND* contain the"
					"specified text in the layer name will be linked."
					""
					"If you do NOT specify the 'Selection Layer Group' but do "
					"specify the OPTIONAL filter, then all layers containing the"
					"text filter will be linked"
					""
					"If you want to UNLINK the specified layers instead of LINKing"
					"them, you must check the 'Reverse action' check box.  "
					
					)
				)
			)
			((= iIndexHelp 10) ; (script-fu-gui-help-info-txtlines 10) ;    (script-fu-gui-help-info 10 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Rename LINKED Layers to Group Name" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"RENAME all linked layers to a group name"
					"This assumes that the layers have un-organized names"
					"that you want to organize under a group name."
					""
					"Each individual layer will be named "
					"'<NewGroupName>.[LR].<oldName>' where '<NewGroupName>'"
					"represents the group name under which you want to"
					"group the linked layers, '.[LR].' is just a label"
					"indicating that the layers have been grouped via"
					"'(L)ink (R)ename' and <oldName> is the original name"
					"of the layer, before executing this operation."					
					""
					"Ex: New layer group name='AAA.BB',"
					"linked layer, original layer name='PPP.QQQ.thisLayer'"
					"Layer name AFTER rename= 'AAA.BB.[LR].PPP.QQQ.thisLayer'"
					
					)
				)
				
			)
			((= iIndexHelp 11) ; (script-fu-gui-help-info-txtlines 11) ;    (script-fu-gui-help-info 11 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Duplicate LINKED Layers to new Group" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"Creates a new group of layers by DUPLICATING all the"
					"currently linked layers. These duplicated layers"
					"will have names composed as follows:"
					"'<Layer_Group_Name>.[LD].<oldName>', where <Layer_Group_Name>"
					"represents the name you mention for the new layer group"
					"'.[LD].' is just a label indicating that the layer has"
					"been created by '(L)ink (D)uplication'"
					"and <oldName> the original full name of the layer,"
					"before the duplication operation took place."
					""
					"Ex: group name for the layers to be duplicated='AAA.BB'"
					"Original layer name of a linked layer='PPP.QQQ.thisLayer'"
					"Name of the DUPLICATED layer= 'AAA.BB.[LD].PPP.QQQ.thisLayer'"
					
					)
				)
				
			)
			((= iIndexHelp 12) ; (script-fu-gui-help-info-txtlines 12) ;    (script-fu-gui-help-info 12 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Move LINKED Layers ABOVE a specific reference layer" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"Move ALL relevant LINKED layers ABOVE another layer to be"
					"used as a reference. Only the linked layers which are"
					"*located BELOW* the reference layer will be moved."
					""
					"Ex: Assume layers 2, 3,  6 and 7 are LINKED"
					""
					"Layer sequence BEFORE the linked layer move:"
					"-----------------------------------------------------"
					"'layer-1'            <-- the top layer of the image"
					"'layer-2'              LINKED"
					"'layer-3'              LINKED"
					"'layer-4'"
					"'layer-5-reference'  <-- the reference layer"
					"'layer-6'              LINKED"
					"'layer-7'              LINKED"
					"'layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					""
					"AFTER the move, this will be the resulting sequence:"
					"------------------------------------------------------"
					"'layer-1'            <-- the top layer of the image"
					"'layer-2'              not moved (it was above reference)"
					"'layer-3'              not-moved (it was above reference)"
					"'layer-4'"
					"'layer-6'              MOVED (it was below reference)"
					"'layer-7'              MOVED (it was below reference)"
					"'layer-5-reference'  <--the reference layer"
					"'layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					
					)
				)
				(set! oListOut (append (script-fu-help-info-duplications-eng 2) oListOut ) ) ;
			)
			((= iIndexHelp 13) ; (script-fu-gui-help-info-txtlines 13) ;    (script-fu-gui-help-info 13 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Move LINKED Layers BELOW a specific reference layer" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"Move ALL relevant LINKED layers BELOW another reference"
					"layer. Only the linked layers *located ABOVE* the reference"
					"layer will be moved."
					""
					"Ex: Assume layers 2, 3, 6 and 7 are LINKED"
					""
					"Layer sequence BEFORE the linked layer move:"
					"-----------------------------------------------------"
					"'layer-1'            <-- the top layer of the image"
					"'layer-2'              LINKED"
					"'layer-3'              LINKED"
					"'layer-4-reference'  <-- the reference layer"
					"'layer-5'"
					"'layer-6'              LINKED"
					"'layer-7'              LINKED"
					"'layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					""
					"AFTER the move, this will be the resulting sequence:"
					"------------------------------------------------------"
					"'layer-1'            <-- the top layer of the image"
					"'layer-4-reference'  <--the reference layer"
					"'layer-2'              MOVED (it was above reference)"
					"'layer-3'              MOVED (it was above reference)"
					"'layer-5'"
					"'layer-6'              not-moved (it was below reference)"
					"'layer-7'              not-moved (it was below reference)"
					"'layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"--------------------------------------------------------"
					
					)
				)
				(set! oListOut (append (script-fu-help-info-duplications-eng 2) oListOut ) ) ;
			)
			; 
			((= iIndexHelp 14) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Save-Restore list of LINKED layers" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"(1) If SAVING, it saves the list of currently LINKED layers"
					"in one of 26 available 'memory slots', all of them"
					"reserved for saving the LINK status of layers."
					""
					"(2) If RESTORING, it restores (from the given 'memory slot')"
					"the LINK status of the layers that where linked at"
					"the time the list was saved in that memory slot"
					
					
					)
					
				)
				(set! oListOut (append (script-fu-help-info-duplications-eng 3) oListOut ) ) ;
				
			)
			
				
			; )
			((= iIndexHelp 15) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				(set! oListOut (@helpLines: oListOut 			
					(string-append  "Save-Restore list of VISIBLE layers" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"(1)If SAVING, it saves the list of currently VISIBLE layers"
					"in one of 26 available 'memory slots', all of them"
					"reserved for saving the VISIBILITY status of layers."
					""
					"(2)If RESTORING, it restores (from the given 'memory slot')"
					"the VISIBILITY of the layers that where VISIBLE at"
					"the time the list of visible layers was saved in that"
					"memory slot"
					
					)
				)
				(set! oListOut (append (script-fu-help-info-duplications-eng 3) oListOut ) ) ;
				
			)
			; (@helpLines: aHelpLines . aText)
			((= iIndexHelp 16) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Debug some data" (help-info-open-menu-prompt-eng isMenu) )
					"DEBUG some data by droping the contents of a list"
					"on a temporary multiline text layer"
					"--------------------"
					)
				)
			)
			((= iIndexHelp 17) ; (script-fu-gui-help-info-txtlines 17) ;    (script-fu-gui-help-info 17 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Move Layer Group ABOVE a specific reference layer" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					;0....:....1....:....2....:....3....:....4....:....5....:....      6....:....7....:....8....:....9....:....0....:....1....:....2
					"Move layers belonging to specified 'Selection Layer Group'"
					"ABOVE another layer to be used as a reference. Only the"
					"layers of the group which are *located BELOW* the reference"
					"layer will be moved."
					""
					"Ex: Assume you want to move Selection Group 'AAA.BB'"
					"above a layer named 'layer-5-reference'"
					""
					"Layer sequence BEFORE the Selection Layer Group move:"
					"-----------------------------------------------------"
					"'XX.YY.layer-1'      <-- the top layer of the image"
					"'AAA.BB.layer-2'"
					"'AAA.BB.layer-3'"
					"'XX.YY.layer-4'"
					"'layer-5-reference'  <-- the reference layer"
					"'AAA.BB.layer-6'"
					"'AAA.BB.layer-7'"
					"'XX.YY.layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					""
					"AFTER the move, this will be the resulting sequence:"
					"------------------------------------------------------"
					"'XX.YY.layer-1'      <-- the top layer of the image"
					"'AAA.BB.layer-2'        not moved (it was above reference)"
					"'AAA.BB.layer-3'        not-moved (it was above reference)"
					"'XX.YY.layer-4'"
					"'AAA.BB.layer-6'        MOVED (it was below reference)"
					"'AAA.BB.layer-7'        MOVED (it was below reference)"
					"'layer-5-reference'  <--the reference layer"
					"'XX.YY.layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					
					)
				)
				(set! oListOut (append (script-fu-help-info-duplications-eng 2) oListOut ) ) ;
			)
			((= iIndexHelp 18) ; (script-fu-gui-help-info-txtlines 18) ;    (script-fu-gui-help-info 18 #t)
				
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Move Layer Group BELOW a specific reference layer" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					;0....:....1....:....2....:....3....:....4....:....5....:....      6....:....7....:....8....:....9....:....0....:....1....:....2
					"Move layers belonging to specified 'Selection Layer Group'"
					"BELOW another layer to be used as a reference. Only the"
					"layers of the group which are *located ABOVE* the reference"
					"layer will be moved."
					""
					"Ex: Assume you want to move Selection Group 'AAA.BB'"
					"below a layer named 'layer-4-reference'"
					""
					"Layer sequence BEFORE the Selection Layer Group move:"
					"-----------------------------------------------------"
					"'XX.YY.layer-1'      <-- the top layer of the image"
					"'AAA.BB.layer-2'"
					"'AAA.BB.layer-3'"
					"'layer-4-reference'  <-- the reference layer"
					"'XX.YY.layer-5'"
					"'AAA.BB.layer-6'"
					"'AAA.BB.layer-7'"
					"'XX.YY.layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					""
					"AFTER the move, this will be the resulting sequence:"
					"------------------------------------------------------"
					"'XX.YY.layer-1'      <-- the top layer of the image"
					"'layer-4-reference'  <--the reference layer"
					"'AAA.BB.layer-2'        MOVED  (it was above reference)"
					"'AAA.BB.layer-3'        MOVED  (it was above reference)"
					"'XX.YY.layer-5'"
					"'AAA.BB.layer-6'        not-moved (it was below reference)"
					"'AAA.BB.layer-7'        not-moved (it was below reference)"
					"'XX.YY.layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					
					
					)
				)
				(set! oListOut (append (script-fu-help-info-duplications-eng 2) oListOut ) ) ;
			)
			((= iIndexHelp 19) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Drop shadows on layers of a layer group" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"Given layers belonging to the specified layer group"
					"creates new layers (with drop shadows) just below"
					"each individual layer of the group."
					""
					"Each created shadow layer will have the same name as"
					"the layer above it, but this name will have a added suffix"
					"'.[DShadow].nnn' where 'nnn' is a sequence number"
					""
					"Ex: Consider an image with the following layers:"
					"-----------------"
					"AAA.thisLayer"
					"AAA.thatLayer"
					"BBBB.thisLayer"
					"BBBB.thatLayer"
					"BBBB.anotherLayer"
					"CCC.thisLayer"
					"CCC.thatLayer"
					"Background"
					"-----------------"
					"If you drop shadows on group 'BBBB' you will get the"
					"following layers:"
					"-----------------"
					"AAA.thisLayer"
					"AAA.thatLayer"
					"BBBB.thisLayer"
					"BBBB.thisLayer.[DShadow].000"
					"BBBB.thatLayer"
					"BBBB.thatLayer.[DShadow].001"
					"BBBB.anotherLayer"
					"BBBB.anotherLayer.[DShadow].002"
					"CCC.thisLayer"
					"CCC.thatLayer"
					"Background"
					"-----------------"
					""
					"If the optional FILTER text is mentioned,"
					"only those members of the group which have that text in the"
					"layer name will be selected for this activity"
					"-----------"
					"You can DROP SHADOW to a subset of the layers of the"
					"specified Layer Group, by specifying 'shadow only"
					"members of the Layer Group which are LINKED'."
					"------------" 
					"If you only specify the FILTER (without a Layer group"
					"selection), then you select all layers of the whole IMAGE"
					"having the specified filter text ANYWHERE in the layer name"
					
					
					)
				)
			)
			((= iIndexHelp 20) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Create PERSPECTIVE shadows on layers of a layer group" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"Given layers belonging to the specified layer group"
					"creates new layers (with perspective shadows) just below"
					"each individual layer of the group."
					""
					"Each created shadow layer will have the same name as"
					"the layer above it, but this name will have a added suffix"
					"'.[PShadow].nnn' where 'nnn' is a sequence number"
					""
					"Ex: Consider an image with the following layers:"
					"-----------------"
					"AAA.thisLayer"
					"AAA.thatLayer"
					"BBBB.thisLayer"
					"BBBB.thatLayer"
					"BBBB.anotherLayer"
					"CCC.thisLayer"
					"CCC.thatLayer"
					"Background"
					"-----------------"
					"If you drop shadows on group 'BBBB' you will get the"
					"following layers (possibly):"
					"-----------------"
					"AAA.thisLayer"
					"AAA.thatLayer"
					"BBBB.thisLayer"
					"BBBB.thisLayer.[PShadow].000"
					"BBBB.thatLayer"
					"BBBB.thatLayer.[PShadow].001"
					"BBBB.anotherLayer"
					"BBBB.anotherLayer.[PShadow].002"
					"CCC.thisLayer"
					"CCC.thatLayer"
					"Background"
					"-----------------"
					""
					"If the optional FILTER text is mentioned,"
					"only those members of the group which have that text in the"
					"layer name will be selected for this activity"
					"-----------"
					"You can Perspective SHADOW to a subset of the layers of the"
					"specified Layer Group, by specifying 'shadow only"
					"members of the Layer Group which are LINKED'."
					"------------" 
					"If you only specify the FILTER (without a Layer group"
					"selection), then you select all layers of the whole IMAGE"
					"having the specified filter text ANYWHERE in the layer name"
					
					
					)
				)
			)
			((= iIndexHelp 21) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Create Transluced 3D effect on layers of a layer group" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"Experimental..."
					"Not yet working properly..."
					
					)
				)
			)
			((= iIndexHelp 22) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Save each layer of a layer group as an individual image" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"Given layers belonging to the specified layer group"
					"saves each of the layers to disk as an individual image file."
					"The default image file type is 'png' but you can choose"
					"other image types like 'jpeg', 'gif' etc, as long as it is"
					"an image file type supported by GIMP."
					""
					"The image files are - by default - saved in the directory"
					"of the current GIMP multilayer image, but you can specify"
					"a different existing directory by setting the corresponding"
					"checkbox and specifying the name of the directory."
					""
					"The file name of each saved file is the name of the layer"
					"followed by the file type extension (default='png')"
					"You can specify a PREFIX for each file name."
					""
					"Example: layers 'X.aaa' and 'X.bbb' are selected,"
					"the directory of the current image is '/myimages/',"
					"the prefix is 'MYLAYERS-' and no extension is specified."
					"The following files will be saved:"
					"/myimages/MYLAYERS-X.aaa.png"
					"/myimages/MYLAYERS-X.bbb.png"
					)
				)
			)
			((= iIndexHelp 23) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Reverse the stack order of a group of layers" (help-info-open-menu-prompt-eng isMenu) )
					
					"======================================="
					"Given a selected group of layers, reverse the order"
					"of those layers in the Gimp layer stack. Thus the"
					"lowest layer of the group will become the first of"
					"the group, the second lowest will become the second"
					"etc."
					
					""
					"Ex: Assume we want to reverse the order of the layers"
					"of layer group 'groupX'"
					""
					"Layer sequence BEFORE reversing the order of the layers:"
					"-----------------------------------------------------"
					"'layer-1'            <-- the top layer of the image"
					"'layer-2'      "
					"'groupX.layer-3'      "
					"'groupX.layer-4'"
					"'layer-5'  "
					"'groupX.layer-6'      "
					"'groupX.layer-7'             "
					"'layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					""
					"AFTER the move, this will be the resulting sequence:"
					"------------------------------------------------------"
					"'layer-1'            <-- the top layer of the image"
					"'layer-2'              "
					"'groupX.layer-7'       "
					"'groupX.layer-6'       "
					"'groupX.layer-4'       "
					"'groupX.layer-3'       "
					"'layer-5'          "
					"'layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					"Note that after the stack order reversal, the layer group"
					"will be stacked together, even if not stacked together"
					"previously ('layer-5' was stacked between 'groupX' layers)."
					"This is the result of moving the relative positions of the"
					"layer group layers. The first layer of the group acts"
					"as a reference layer for the moves: all the others will be"
					"stacked above it. This 'stacking together' can be used"
					"to stack together members of a group scattered through"
					"the image by calling this functionality twice in a row!"
					
					)
				)
			)
			((= iIndexHelp 24) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Reverse the stack order of all LINKED layers" (help-info-open-menu-prompt-eng isMenu) )
					
					"======================================="
					"Given the group of all currently linked layers of the image"
					"(possibly filtered for some substring in the layer name),"
					"reverse the order of those layers. The "
					"lowest layer of the group will become the first of"
					"the group, the second lowest will become the second"
					"etc."
					
					""
					"Ex: Assume we want to reverse the order of the layers"
					"which are currently LINKED"
					""
					"Layer sequence BEFORE reversing the layers:"
					"-----------------------------------------------------"
					"'layer-1'            <-- the top layer of the image"
					"'layer-2'      "
					"'layer-3'            LINKED"
					"'layer-4'            "
					"'layer-5'            "
					"'layer-6'            LINKED"
					"'layer-7'            LINKED"
					"'layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					""
					"AFTER the move, this will be the resulting sequence:"
					"------------------------------------------------------"
					"'layer-1'            <-- the top layer of the image"
					"'layer-2'              "
					"'layer-7'            LINKED"
					"'layer-6'            LINKED"
					"'layer-3'            LINKED"
					"'layer-4'            "
					"'layer-5'          "
					"'layer-8'"
					"'Background'         <-- the bottom layer of the image"
					"------------------------------------------------------"
					"Note that after the stack order reversal, the linked layers"
					"will be stacked together, even if not stacked together"
					"previously ('layer-4' and 'layer-5' were in-between)."
					"This is the result of moving the relative positions of the"
					"linked layers! The first linked layer acts as a reference"
					"layer for the moves: all the others will be stacked above it."
					)
				)
			)
			((= iIndexHelp 25) ; (script-fu-gui-help-info-txtlines 14) ;    (script-fu-gui-help-info 14 #t)
				(set! oListOut (@helpLines: oListOut 
					(string-append  "Delete LINKED layers" (help-info-open-menu-prompt-eng isMenu) )
					"======================================="
					"Delete all currently linked layers. You can"
					"filter the set of linked layers to delete"
					"by sepecifying a filter substring that must"
					"be present in the name of the linked layers."
					"Only the linked layers satisfying that condition"
					"will be deleted."
					
					)
				)
			)
			
		) ;cond
		(set! oListOut (reverse oListOut))

		oListOut
	);let
)
;-----------------------------------------------------
(define (script-fu-help-info-duplications-eng iIndexHelp)
	(let*
		(
			
			(oListOut '() )
		)
		(cond
			((= iIndexHelp 1)
				
				(set! oListOut (@helpLines: oListOut 
					""
					"If the 'Optional FILTER' is mentioned, only those members"
					"of the group which contain that text ANYWHERE in the layer"
					"name will be selected for this activity."
					""
					"If you only specify the FILTER (without a Layer group"
					"selection), then you select *all layers of the whole IMAGE*"
					"having the specified filter text ANYWHERE in the layer name"
					
					)
				)
			)
			((= iIndexHelp 2)
				
				(set! oListOut (@helpLines: oListOut 
					""
					"You may FILTER the list of linked layers to move, by"
					"specifying a filter string for the layer name: only "
					"linked layers having that string in the name will be moved."
					""
					"You can specify the REFERENCE layer in one of 2 ways:"
					"#(1) specify the name of the layer or a group name. If"
					"specified, then the first layer (counting from the TOP of"
					"the layers) that matches the specified layer name (or group"
					"name) will become the REFERENCE layer."
					"#(2) Choose the layer from a menu of all layers (of all"
					"images!)."
					"NOTE:If you choose a layer from another image than the"
					"current one, then no layer will me moved! "
					
					)
				)
			)
			((= iIndexHelp 3)
				
				(set! oListOut (@helpLines: oListOut 
					""
					"If you save the list in a 'memory slot' that is occupied"
					"with another list, the old list is overwritten by the"
					"new one"
					""
					"If you save a list from one image in a 'memory slot'"
					"and try to restore from that 'memory slot' into a DIFFERENT"
					"image, the action will be ignored (nothing will happen)"
					""
					"When you quit GIMP, the lists saved in the 'memory slots'"
					"wiil be lost"
					
					)
				)
			)
			
		)
		;-----return value
		oListOut 
	)
)

(define (help-info-open-menu-prompt-eng isMenu)
	(let*
		(
			
			(sOut (iif (isTrue isMenu) "         [#] Please open this menu to read..." ""))
		)
		;-----return value
		sOut 
	)
)
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(define (gui-list-memory-slots-names )
	(let*
		(
			(ii 0)
			(sOut "")
			(oListOut '())
		)
		(set! oListOut (@helpLines: oListOut 			
					"(A) - slot  1"
					"(B) - slot  2"
					"(C) - slot  3"
					"(D) - slot  4"
					"(E) - slot  5"
					"(F) - slot  6"
					"(G) - slot  7"
					"(H) - slot  8"
					"(I) - slot  9"
					"(J) - slot 10"
					"(K) - slot 11"
					"(L) - slot 12"
					"(M) - slot 13"
					"(N) - slot 14"
					"(O) - slot 15"
					"(P) - slot 16"
					"(Q) - slot 17"
					"(R) - slot 18"
					"(S) - slot 19"
					"(T) - slot 20"
					"(U) - slot 21"
					"(V) - slot 22"
					"(W) - slot 23"
					"(X) - slot 24"
					"(Y) - slot 25"
					"(Z) - slot 26"
					
					)
		)
		(set! oListOut (reverse oListOut))
		;-----return value
		oListOut 
	)
)
;=============================================================================
;...(define pub_layer-groups-in-GIMP-layer-menu #f) ; <<<<<<<<< change '#f' to '#t' if you want the layergroups subsystem to appear under the 'Layer' menu of GIMP
;=============================================================================

;=============================================================================
(define (@gimpMenuLocation sMenuOption)
	(let*
		(
			
			(sOut (string-append (iif pub_layer-groups-in-GIMP-layer-menu "<Image>/Layer" "<Image>") sMenuOption) )
		)
		;-----return value
		sOut 
	)
)
; ---------------GIMP registration functions -----------------------------------
;
; --------------------------------- EDIT ----------------------------------------

(script-fu-register
    "script-fu-gimp-layer-group-duplicate"
	(@gimpMenuLocation "/LayerGroups/2 - Edit/Edit: (1) Duplicate Layer Group")
    (script-fu-gui-help-info-txtlines 2) 
    "Miguel Oliveira <melocotone at gmail.com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "SELECTION Layer Group Name (layers to duplicate)" ""
	SF-TOGGLE "<-- duplicate only members of the specified Layer Group which are LINKED" FALSE
	SF-STRING "Optional selection FILTER: Text that must appear in each Layer name " ""
	SF-STRING "RESULT Layer Group Name (created duplicated layers)" ""
	
	SF-TOGGLE "<-- duplicate layers to ANOTHER image (if you select this, then please 'select DESTINATION image ->', next)" FALSE
	SF-IMAGE "Select DESTINATION image (*ignored* if box above not checked) ->" -1
	
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 2 #t) ; // (script-fu-gui-force-large-width 180 )

	)


(script-fu-register
    "script-fu-gimp-layer-group-delete"
    (@gimpMenuLocation "/LayerGroups/2 - Edit/Edit: (2) Delete Layer Group")
    (script-fu-gui-help-info-txtlines 3) ;
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group Name" ""
	SF-TOGGLE "<-- delete only members of the specified Layer Group which are LINKED" FALSE
	SF-STRING "Optional selection FILTER: Text that must appear in each Layer name " ""
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 3 #t) 
)
(script-fu-register
    "script-fu-gimp-linked-layers-delete"
    (@gimpMenuLocation "/LayerGroups/2 - Edit/Edit: (3) Delete LINKED layers")
    (script-fu-gui-help-info-txtlines 25) ;
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
	SF-STRING "Optional selection FILTER: Text that must appear in each Layer name " ""
    
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 25 #t) 
)
	
(script-fu-register
    "script-fu-gimp-layer-group-rename"
    (@gimpMenuLocation "/LayerGroups/2 - Edit/Edit: (4) RENAME Layer Group")
    (script-fu-gui-help-info-txtlines 4) ;
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group Name" ""
	SF-TOGGLE "<-- rename only members of the specified Layer Group which are LINKED" FALSE
	SF-STRING "Optional selection FILTER: Text that must appear in each Layer name " ""
	SF-STRING "      NEW Layer Group Name" ""
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 4 #t) ; 
		;(script-fu-gui-help-info-txtlines 3)    (script-fu-gui-help-info 3 #t)
)
	

(script-fu-register
    "script-fu-gimp-layer-group-pattern-rename"
    (@gimpMenuLocation "/LayerGroups/2 - Edit/Edit: (5) RENAME some portions of layer names of a Layer Group ")
    (script-fu-gui-help-info-txtlines 5) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group Name" ""
	SF-TOGGLE "<-- rename only in members of the specified Layer Group which are LINKED" FALSE
	SF-STRING "Optional FILTER: Text that must appear in the layer name for the layer to be selected" ""	
	SF-STRING "Pattern to search in each layer name..." ""
	SF-STRING "...to be replaced by this pattern" ""
	SF-TOGGLE "<-- Replace multiple occurrences of the pattern, not only the first found" FALSE
	 ; (script-fu-gui-help-info-txtlines 5)    (script-fu-gui-help-info 5 #t)
	
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 5 #t) ; 
)
(script-fu-register
    "script-fu-gimp-layer-group-save-to-individual-image-files"
    (@gimpMenuLocation "/LayerGroups/2 - Edit/Edit: (6) Create individual images (on disk) from layers of a Layer Group")
    (script-fu-gui-help-info-txtlines 22) ;
    "Miguel Oliveira <melocotone at gmail dot com>, inspired and adapted from code by Eric Anderton" ; http://uzebox.org/wiki/index.php?title=Layer_Export_GIMP_Script#sg-save-all-layers.scm
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group Name" ""
	SF-TOGGLE "<-- select only members of the specified Layer Group which are LINKED" FALSE
	SF-STRING "Optional selection FILTER: Text that must appear in each Layer name " ""
	SF-TOGGLE  "<-- Output: save images in a specific directory. (Default=current image directory)" FALSE
	SF-STRING  "    Output: Specific directory (full path!) where to save the images" ""
	SF-STRING  "    Output: PREFIX for each file name" ""
	SF-STRING  "    Output: Image file type EXTENSION (default='png')" ""
	
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 22 #t) ; 
		
	)
	
	
; ----------------------VIEW -----------------------------------------------------
(script-fu-register
    "script-fu-gimp-layers-group-list-of-all-layer-names"
    (@gimpMenuLocation "/LayerGroups/1 - View/View: (0) List all Layer names")
    (script-fu-gui-help-info-txtlines 6) ; 
    "Miguel Oliveira <melocotone at gmail.com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Title for the list" "List of all layers"
	SF-VALUE "x coordinate for top left corner of output text layer " "20"
	SF-VALUE "y coordinate for top left corner of output text layer " "10"
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 6 #t) ;
	; (script-fu-gui-help-info-txtlines 6)    (script-fu-gui-help-info 6 #t)
	)

(script-fu-register
    "script-fu-gimp-layer-group-show"
    (@gimpMenuLocation "/LayerGroups/1 - View/View: (1) Show Layer Group")
    (script-fu-gui-help-info-txtlines 7)  ;  
    "Joseph Miller <josephcmiller2@gmail.com>"
    "Licensed GPL V2.0, 2008, Joseph Miller/ 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group Name" ""
	SF-TOGGLE "<-- show only members of the specified Layer Group which are LINKED" FALSE
	SF-STRING "Optional selection FILTER: Text that must appear in each Layer name " ""
	; (script-fu-gui-help-info-txtlines 7)  ;  (script-fu-gui-help-info 7 #t)	
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 7 #t) ;
)

(script-fu-register
    "script-fu-gimp-layer-group-hide"
    (@gimpMenuLocation "/LayerGroups/1 - View/View: (2) Hide Layer Group")
    (script-fu-gui-help-info-txtlines 8); "
    "Joseph Miller <josephcmiller2@gmail.com>"
    "2008, Joseph Miller"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group Name" ""
	SF-TOGGLE "<-- hide only members of the specified Layer Group which are LINKED" FALSE
	SF-STRING "Optional selection FILTER: Text that must appear in each Layer name " ""	
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 8 #t) ;
)


(script-fu-register
    "script-fu-gimp-layer-group-showall"
    (@gimpMenuLocation "/LayerGroups/1 - View/View: (3) Show All Layers")
    "Show all layers, without exception"
    "Joseph Miller <josephcmiller2@gmail.com>"
    "2008, Joseph Miller"
    "Thu Mar 20 16:48:26 2008"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0)

(script-fu-register
    "script-fu-gimp-layer-group-hideall"
    (@gimpMenuLocation "/LayerGroups/1 - View/View: (4) Hide All Layers")
    "Hide all layers without exception"
    "Joseph Miller <josephcmiller2@gmail.com>"
    "2008, Joseph Miller"
    "Thu Mar 20 16:48:26 2008"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0)

(script-fu-register
    "script-fu-gimp-layer-group-showallbut"
    (@gimpMenuLocation "/LayerGroups/1 - View/View: (5) Show All Layers But Group")
    "SHOW all layers whose names *DO NOT START* with the given Layer Group Name"
    "Joseph Miller <josephcmiller2@gmail.com>"
    "2008, Joseph Miller"
    "Thu Mar 20 16:48:26 2008"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group Name" "")

(script-fu-register
    "script-fu-gimp-layer-group-hideallbut"
    (@gimpMenuLocation "/LayerGroups/1 - View/View: (6) Hide All Layers But Group")
    "HIDE all layers whose names *DO NOT START* with the given Layer Group Name"
    "Joseph Miller <josephcmiller2@gmail.com>"
    "2008, Joseph Miller"
    "Thu Mar 20 16:48:26 2008"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group Name" "")
(script-fu-register
    "script-fu-gimp-layer-group-hide-linked-layers"
    (@gimpMenuLocation "/LayerGroups/1 - View/View: (7) Hide All LINKED layers")
    "HIDE all LINKED layers"
    "Miguel Oliveira <melocotone at gmail.com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    ; ///SF-STRING "Layer Group Name" ""
	)
(script-fu-register
    "script-fu-gimp-layer-group-show-linked-layers"
    (@gimpMenuLocation "/LayerGroups/1 - View/View: (8) SHOW All LINKED Layers")
    "SHOW all LINKED layers"
    "Miguel Oliveira <melocotone at gmail.com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
	)


; ------------ group of LINK activities --------

(script-fu-register
    "script-fu-gimp-layer-group-link"
    (@gimpMenuLocation "/LayerGroups/3 - Link/Link: (1) Link or Unlink Layer Group")
    (script-fu-gui-help-info-txtlines 9 ) ; 
    "Joseph Miller <josephcmiller2@gmail.com> and Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2008, Joseph Miller/ 2009 Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group Name" ""
	SF-STRING "Optional FILTER: Text that must appear in each Layer name " ""
	SF-TOGGLE "<-- Reverse action: UNLINK the selection (instead of LINK the selection)" FALSE
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 9 #t) ;
)
; -------------------------
(script-fu-register
    "script-fu-gimp-layer-group-rename-linked"
    (@gimpMenuLocation "/LayerGroups/3 - Link/Link: (2) Rename Linked Layers to Group Name")
    (script-fu-gui-help-info-txtlines 10) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "NEW Layer Group Name" ""
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 10 #t) ;
	; (script-fu-gui-help-info-txtlines 10) ;    (script-fu-gui-help-info 10 #t)
	)


(script-fu-register
    "script-fu-gimp-layer-group-duplicate-linked"
    (@gimpMenuLocation "/LayerGroups/3 - Link/Link: (3) Duplicate Linked Layers to new Group")
    (script-fu-gui-help-info-txtlines 11) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "New Layer Group Name" ""
	SF-TOGGLE "<-- duplicate layers on ANOTHER image (if you select this, then please 'select DESTINATION image ->', next)" FALSE
	SF-IMAGE "Select DESTINATION image (*ignored* if box above not checked) ->" -1
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 11 #t) ;
	; (script-fu-gui-help-info-txtlines 11) ;    (script-fu-gui-help-info 11 #t)
	)

(script-fu-register
    "script-fu-gimp-layer-group-delete-linked"
    (@gimpMenuLocation "/LayerGroups/3 - Link/Link: (4) Delete all linked layers")
    "DELETE all LINKED layers"
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0)

(script-fu-register
    "script-fu-gimp-layer-group-move-to-top-linked-bis" ; the 'bis' is needed to be able to access the same code from 2 different GUI interface instances...
    (@gimpMenuLocation "/LayerGroups/3 - Link/Link: (5) Move all LINKED layers to the top")
    "MOVE all LINKED layers to the Top of the layer stack"
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0)
(script-fu-register
    "script-fu-gimp-layer-group-move-to-bottom-linked-bis"
    (@gimpMenuLocation "/LayerGroups/3 - Link/Link: (6) Move all LINKED layers to the bottom")
    "MOVE all LINKED layers to the Bottom of the layer stack"
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0)
(script-fu-register
    "script-fu-gimp-layers-linked-moveabove-bis"
    (@gimpMenuLocation "/LayerGroups/3 - Link/Link: (7) Move LINKED Layers ABOVE a specific reference layer")
    (script-fu-gui-help-info-txtlines 12) ;  
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2008, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
	SF-STRING "Optional FILTER: Text that must appear in each Layer name, in order to move it" ""
	SF-STRING "Name of the specific REFERENCE Layer..." ""
	SF-DRAWABLE "...OR choose it from a menu list" 0
	SF-STRING "Optional FILTER: Text that must appear in each Layer name " ""
    
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 12 #t) ;
	)
(script-fu-register
    "script-fu-gimp-layers-linked-movebelow-bis"
    (@gimpMenuLocation "/LayerGroups/3 - Link/Link: (8) Move LINKED Layers BELOW a specific reference layer")
    (script-fu-gui-help-info-txtlines 13) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
	SF-STRING "Optional FILTER: Text that must appear in each Layer name, in order to move it" ""
	SF-STRING "Name of the specific REFERENCE Layer..." ""
	SF-DRAWABLE "...OR choose it from a menu list" 0
	SF-STRING "Optional FILTER: Text that must appear in each Layer name " ""
   
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 13 #t) ;
)
(script-fu-register
    "script-fu-gimp-layer-group-unlinkall"
    (@gimpMenuLocation "/LayerGroups/3 - Link/Link: (9) Unlink All")
    "UNLINK all linked layers"
    "Joseph Miller <josephcmiller2@gmail.com>"
    "2008, Joseph Miller"
    "Thu Mar 20 16:48:26 2008"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0)

;------------------------------------- MOVE ----------------------------------------------



(script-fu-register
    "script-fu-gimp-layer-group-moveabove"
    (@gimpMenuLocation "/LayerGroups/4 - Move/Move: (1) Move Layer Group ABOVE a specific reference layer")
    (script-fu-gui-help-info-txtlines 17); 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group (name) to move ABOVE..." ""
	SF-TOGGLE "<-- move only members of the specified Layer Group which are LINKED" FALSE
	SF-STRING "Optional FILTER: Text that must appear in each Layer name " ""
	SF-STRING "Name of the specific REFERENCE Layer..." ""
	SF-DRAWABLE "...OR choose the REFERENCE layer from a list (CURRENT image only!)" 0
    
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 17 #t) ;
)

(script-fu-register
    "script-fu-gimp-layer-group-movebelow"
    (@gimpMenuLocation "/LayerGroups/4 - Move/Move: (2) Move Layer Group BELOW a specific reference layer")
    (script-fu-gui-help-info-txtlines 18); 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0  ;  0 (gimp-image-list)
    SF-STRING "Selection Layer Group (name) to move BELOW..." ""
	SF-TOGGLE "<-- move only members of the specified Layer Group which are LINKED" FALSE
	SF-STRING "Optional FILTER: Text that must appear in each Layer name " ""
	SF-STRING "Name of the specific REFERENCE Layer..." ""
	SF-DRAWABLE "...OR choose the REFERENCE layer from a list (CURRENT image only!)" 0
    
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 18 #t) ;
	; (script-fu-gui-help-info-txtlines 13) ;    (script-fu-gui-help-info 13 #t)
)
(script-fu-register
    "script-fu-gimp-layer-group-move-to-top-linked"
    (@gimpMenuLocation "/LayerGroups/4 - Move/Move: (3) Move all LINKED layers to the top")
    "MOVE all LINKED layers to the Top of the layer stack"
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2008, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0)
(script-fu-register
    "script-fu-gimp-layer-group-move-to-bottom-linked"
    (@gimpMenuLocation "/LayerGroups/4 - Move/Move: (4) Move all LINKED layers to the bottom")
    "MOVE all LINKED layers to the Bottom of the layer stack"
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2008, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0)
;// script-fu-gimp-layers-linked-moveabove
(script-fu-register
    "script-fu-gimp-layers-linked-moveabove"
    (@gimpMenuLocation "/LayerGroups/4 - Move/Move: (5) Move LINKED Layers ABOVE a specific reference layer")
    (script-fu-gui-help-info-txtlines 12) ;   
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
	SF-STRING "Optional FILTER: Text that must appear in each Layer name, in order to move it" ""
	SF-STRING "Name of the specific REFERENCE Layer" ""
	SF-DRAWABLE "...or choose it from a menu list" 0
    
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 12 #t) ;
	
	)
(script-fu-register
    "script-fu-gimp-layers-linked-movebelow"
    (@gimpMenuLocation "/LayerGroups/4 - Move/Move: (6) Move LINKED Layers BELOW a specific reference layer")
    (script-fu-gui-help-info-txtlines 13) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
	SF-STRING "Optional FILTER: Text that must appear in each Layer name, in order to move it" ""
	SF-STRING "Name of the specific REFERENCE Layer" ""
	SF-DRAWABLE "...OR choose it from a menu list" 0
    
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 13 #t) ;
	
)
(script-fu-register
    "script-fu-gimp-layer-group-reverse-order"
    (@gimpMenuLocation "/LayerGroups/4 - Move/Move: (7) REVERSE Layer Group order sequence")
    (script-fu-gui-help-info-txtlines 23); 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
    SF-STRING "Selection Layer Group (name) to move ABOVE..." ""
	SF-TOGGLE "<-- move only members of the specified Layer Group which are LINKED" FALSE
	SF-STRING "Optional FILTER: Text that must appear in each Layer name " ""
	
    
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 23 #t) ;
)
(script-fu-register
    "script-fu-gimp-layers-linked-reverse-order"
    (@gimpMenuLocation "/LayerGroups/4 - Move/Move: (8) REVERSE the order sequence of LINKED Layers")
    (script-fu-gui-help-info-txtlines 24) ;   
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
	SF-STRING "Optional FILTER: Text that must appear in each Layer name, in order to move it" ""
	
	
    
	SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 24 #t) ;
	
	)
	
	
; -------------------------------- SAVE RESTORE -------------	

(script-fu-register
    "script-fu-gimp-layer-group-visiblelayers-save-restore"
    (@gimpMenuLocation "/LayerGroups/5 - Save-Restore/Save-Restore: (1) Visible layers - Save or Restore a VISIBILITY SET from DISK")
    (script-fu-gui-help-info-txtlines 1) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
	SF-STRING "Name of the SET to either SAVE or RESTORE" ""
	SF-OPTION "Choose action: SAVE or RESTORE" '("SAVE the status of visibility of all layers of this image under the given SET name" "RESTORE the (previously) saved status of layer visibility from the given SET name"    )
	SF-OPTION "Help info" (script-fu-gui-help-info 1 #t)  ; (script-fu-gui-help-info-txtlines 1) ; /// 
)
(script-fu-register
    "script-fu-gimp-layer-group-linked-layers-save-restore"
    (@gimpMenuLocation "/LayerGroups/5 - Save-Restore/Save-Restore: (2) Linked layers - Save or Restore from MEMORY")
    (script-fu-gui-help-info-txtlines 14) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
	SF-OPTION "Choose memory SLOT to either SAVE or RESTORE" (gui-list-memory-slots-names ) ; '("(A) - slot 1" "(B) - slot 2" "(C) - slot 3" "(D) - slot 4" "(E) - slot 5" "(F) - slot 6" "(G) - slot 7" "(H) - slot 8" "(I) - slot 9" "(J) - slot 10")
	SF-OPTION "Choose action: SAVE or RESTORE" '("SAVE the current list of LINKED layers" "RESTORE the (previously) saved list of linked layers"    )
	SF-OPTION "Help info" (script-fu-gui-help-info 14 #t)  ; (script-fu-gui-help-info-txtlines 1) ; /// 
)
(script-fu-register
    "script-fu-gimp-layer-group-visible-layers-save-restore"
    (@gimpMenuLocation "/LayerGroups/5 - Save-Restore/Save-Restore: (3) Visible layers - Save or Restore from MEMORY")
    (script-fu-gui-help-info-txtlines 15) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
	SF-OPTION "Choose memory SLOT to either SAVE or RESTORE" (gui-list-memory-slots-names ) ; '("(A) - slot 1" "(B) - slot 2" "(C) - slot 3" "(D) - slot 4" "(E) - slot 5" "(F) - slot 6" "(G) - slot 7" "(H) - slot 8" "(I) - slot 9" "(J) - slot 10")
	SF-OPTION "Choose action: SAVE or RESTORE" '("SAVE the current list of VISIBLE layers" "RESTORE the (previously) saved list of visible layers"    )
	SF-OPTION "Help info" (script-fu-gui-help-info 15 #t)  ; (script-fu-gui-help-info-txtlines 1) ; /// 
)
; -------------------------- GENERAL OVERVIEW -------------
(script-fu-register
    "script-fu-gimp-layer-groups-help-overview"
    (@gimpMenuLocation "/LayerGroups/LayerGroups (v. 1.0a) Overview (1)")
    (script-fu-gui-help-info-txtlines 0) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
   
)
(script-fu-register
    "script-fu-gimp-layer-groups-help-overview-2"
    (@gimpMenuLocation "/LayerGroups/LayerGroups (v. 1.0a) Overview (2)")
    (script-fu-gui-help-info-txtlines 100) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
   
)
(script-fu-register
    "script-fu-gimp-layer-groups-help-overview-3"
    (@gimpMenuLocation "/LayerGroups/LayerGroups (v. 1.0a) Overview (3)")
    (script-fu-gui-help-info-txtlines 101) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
   
)
(script-fu-register
    "script-fu-gimp-layer-groups-help-overview-4"
    (@gimpMenuLocation "/LayerGroups/LayerGroups (v. 1.0a) Overview (4)")
    (script-fu-gui-help-info-txtlines 102) ; 
    "Miguel Oliveira <melocotone at gmail dot com>"
    "Licensed GPL V2.0, 2009, Miguel Oliveira"
    "June 4, 2009"
    "RGBA RGB INDEXED*"
    SF-IMAGE "Image" 0
   
)

(script-fu-register "script-fu-gimp-layer-group-drop-shadow"
  (@gimpMenuLocation "/LayerGroups/6 - Effects/Effects: (1) Drop Shadow on Layer Group ")
  (script-fu-gui-help-info-txtlines 19) ; "Add a drop shadow to the selected region (or alpha)"
  "Sven Neumann <sven@gimp.org>, integrated for grouping 2009-04-28 by Miguel Oliveira <melocotone at gmail dot com>"
  "Sven Neumann"
  "1999/12/21"
  "RGB* GRAY*"
  SF-IMAGE      "Image"           0
  SF-STRING "Selection Layer Group Name" ""
  SF-TOGGLE "<-- 'shadow' only members of the specified Layer Group which are LINKED" FALSE
  SF-STRING "Optional selection FILTER: Text that must appear in each Layer name " ""
  SF-ADJUSTMENT _"Offset X"       '(8 -4096 4096 1 10 0 1)
  SF-ADJUSTMENT _"Offset Y"       '(8 -4096 4096 1 10 0 1)
  SF-ADJUSTMENT _"Blur radius"    '(15 0 1024 1 10 0 1)
  SF-COLOR      _"Color"          "black"
  SF-ADJUSTMENT _"Opacity"        '(80 0 100 1 10 0 0)
  SF-TOGGLE     _"Allow resizing" TRUE
  SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 19 #t)  ; (script-fu-gui-help-info-txtlines 1) ; /// 
)
(script-fu-register "script-fu-gimp-layer-group-perspective-shadow"
  (@gimpMenuLocation "/LayerGroups/6 - Effects/Effects: (2) Perspective Shadow on Layer Group")
  (script-fu-gui-help-info-txtlines 20) ; "Add a perspective shadow to the selected region (or alpha)"
  "Sven Neumann <sven@gimp.org>, integrated for grouping 2009-04-28 by Miguel Oliveira <melocotone at gmail dot com>"
  "Sven Neumann"
  "1999/12/21"
  "RGB* GRAY*"
  SF-IMAGE      "Image"           0
  SF-STRING "Selection Layer Group Name" ""
  SF-TOGGLE "<-- 'shadow' only members of the specified Layer Group which are LINKED" FALSE
  SF-STRING "Optional selection FILTER: Text that must appear in each Layer name " ""
  SF-ADJUSTMENT _"Angle"                        '(45 0 180 1 10 1 0)
  SF-ADJUSTMENT _"Relative distance of horizon" '(5 0.1 24.1 0.1 1 1 1)
  SF-ADJUSTMENT _"Relative length of shadow"    '(1 0.1 24   0.1 1 1 1)
  SF-ADJUSTMENT _"Blur radius"                  '(3 0 1024 1 10 0 0)
  SF-COLOR      _"Color"                        '(0 0 0)
  SF-ADJUSTMENT _"Opacity"                      '(80 0 100 1 10 0 0)
  SF-ENUM       _"Interpolation"                '("InterpolationType" "linear")
  SF-TOGGLE     _"Allow resizing"               FALSE
  SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 20 #t)  ; (script-fu-gui-help-info-txtlines 1) ; /// 
)
(script-fu-register "script-fu-gimp-layer-group-translucent-3D-effect"
  (@gimpMenuLocation "/LayerGroups/6 - Effects/Effects: (3) Translucent 3D effect on Layer Group")
  (script-fu-gui-help-info-txtlines 21) ; "Add a subtle translucent 3D effect to the selected region (or alpha)"
  "Adrian Likins <adrian@gimp.org>, integrated for grouping 2009-04-29 by Miguel Oliveira <melocotone at gmail dot com>"
  "Adrian Likins"
  "9/28/97"
  "RGB* GRAY*"
  SF-IMAGE      "Image"           0
  SF-STRING "Selection Layer Group Name" ""
  SF-TOGGLE "<-- affect only members of the specified Layer Group which are LINKED" FALSE
  SF-STRING "Optional selection FILTER: Text that must appear in each Layer name " ""
  SF-ADJUSTMENT _"Highlight X offset"      '(-1 -100 100 1 10 0 1)
  SF-ADJUSTMENT _"Highlight Y offset"      '(-1 -100 100 1 10 0 1)
  SF-COLOR      _"Highlight color"         "white"
  SF-ADJUSTMENT _"Highlight opacity"       '(66 0 255 1 10 0 0)
  SF-COLOR      _"Drop shadow color"       "black"
  SF-ADJUSTMENT _"Drop shadow opacity"     '(100 0 100 1 10 0 0)
  SF-ADJUSTMENT _"Drop shadow blur radius" '(12 0 255 1 10 0 1)
  SF-ADJUSTMENT _"Drop shadow X offset"    '(5 0 255 1 10 0 1)
  SF-ADJUSTMENT _"Drop shadow Y offset"    '(5 0 255 1 10 0 1)
  SF-TOGGLE     _"Keep selection"          TRUE
  SF-OPTION "Help info (open menu to read)" (script-fu-gui-help-info 21 #t)  ; (script-fu-gui-help-info-txtlines 1) ; /// 
)


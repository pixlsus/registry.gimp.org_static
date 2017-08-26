; backup_working.scm
; by Rob Antonishen
; http://ffaat.pointclark.net

; Version 1.2 (20120301)

; Changes:
; 1.1 changes to use xcfgz compressed files
; 1.2 added option to limit the number of files backed up.  
; This has changed the naming convention to include the original extension

; Description
; Saves a backup copy as [imagename-ext]-YYYY-MM-DD-HH-MM.XCFGZ
; It will appear in the File menu.  and can easily accessed with Alt-F, B
; License:
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
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (backup-working img inLayer)
  (let* 
    (
      (timestamp (unbreakupstr 
                   (append (list (number->string (+ 1900 (car (time))))) ; year
                           (map (lambda (x) (string-append (make-string (- 2 (string-length (number->string x))) #\0) (number->string x))) ; pad to two characters
                             (append (list  (+ 1  (cadr (time)))) (butlast (cddr (time)))))) "-")) ; month is 0 referenced when (time) returns it
      (filename "")
      (extension "")
      (dirname "")
      (dupimage (car (gimp-image-duplicate img)))
      (maxbackups 5)  ; Change this to define the number of backup files to keep!!!
      (filelist "")
	)
	
    ;;helper defines
    (define split
      (lambda (ls)
        (letrec ((split-h (lambda (ls ls1 ls2)
                            (cond
                              ((or (null? ls) (null? (cdr ls)))
                               (cons (reverse ls2) ls1))
                              (else (split-h (cddr ls)
                                      (cdr ls1) (cons (car ls1) ls2)))))))
          (split-h ls ls '()))))
          
    (define merge
      (lambda (pred ls1 ls2)
        (cond
          ((null? ls1) ls2)
          ((null? ls2) ls1)
          ((pred (car ls1) (car ls2))
           (cons (car ls1) (merge pred (cdr ls1) ls2)))
          (else (cons (car ls2) (merge pred ls1 (cdr ls2)))))))

    ;pred is the comparison, i.e. <= for an ascending numeric list, or 
    ;string<=? for a case sensitive alphabetical sort, 
    ;string-ci<=? for a case insensitive alphabetical sort, 
    (define merge-sort
      (lambda (pred ls)
        (cond
          ((null? ls) ls)
          ((null? (cdr ls)) ls)
          (else (let ((splits (split ls)))
                  (merge pred
                    (merge-sort pred (car splits))
                    (merge-sort pred (cdr splits))))))))
                    
    (define get-n-items
      (lambda (lst num)
        (if (> num 0)
          (cons (car lst) (get-n-items (cdr lst) (- num 1)))
          '()))) ;'

   (define slice
     (lambda (lst start count)
        (if (> start 1)
          (slice (cdr lst) (- start 1) count)
          (get-n-items lst count))))
    
    ; It Starts Here....
    ;---------------------
    (if (= (string-length (car (gimp-image-get-filename img))) 0)
      (gimp-message "The file must be saved before a backup can be made.") 
      (begin
        (set! filename (unbreakupstr (butlast (strbreakup (car (gimp-image-get-name img)) ".")) "."))
        (set! extension (car (last (strbreakup (car (gimp-image-get-name img)) "."))))
        (set! dirname (unbreakupstr (butlast (strbreakup (car (gimp-image-get-filename img)) DIR-SEPARATOR)) DIR-SEPARATOR))

        ;update progressbar text
        (gimp-progress-set-text (string-append "Backing up as: " filename "-" extension "-" timestamp ".xcfgz"))
        
        ;save file
        (gimp-xcf-save 0 dupimage (car (gimp-image-get-active-drawable dupimage))
          (string-append dirname DIR-SEPARATOR filename "-" extension "-" timestamp ".xcfgz")
          (string-append filename "-" extension "-" timestamp ".xcfgz"))
      )
    )
            
    ;clean up
    (gimp-image-delete dupimage) 
    
    ;delete extra backups
    (when (> maxbackups 0)
      (set! filelist (merge-sort string<=? (cadr (file-glob (string-append dirname DIR-SEPARATOR filename "-" extension "-*.xcfgz") 0))))
      (set! filelist (slice filelist 0 (max (- (length filelist) maxbackups) 0)))
      (map (lambda (x) (file-delete x)) filelist)
    )    
  )
)

(script-fu-register "backup-working"
        		    "<Image>/File/Save a _Backup"
					"Saves a backup copy as [imagename-ext]-YYYY-MM-DD-HH-MM.XCFGZ"
					"Rob Antonishen"
					"Rob Antonishen"
					"March 2012"
					"*"
					SF-IMAGE      "image"      0
					SF-DRAWABLE   "drawable"   0
)				
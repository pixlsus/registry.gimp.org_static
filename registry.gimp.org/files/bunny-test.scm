; Bunny-Unit 
 (define (test code) 
   (list 'bunny-test 
         code 
         'bunny-test:un-tested)) 
  
 (define (bunny-test? t) 
   (and (list? t) 
        (eq? (car t) 'bunny-test))) 
  
 (define (get-code t) 
   (if (bunny-test? t) 
       (cadr t))) 
  
 (define (get-test-result t) 
   (if (bunny-test? t) 
       (begin 
         (if (not (test-execed? t)) 
             (exec-test t)) 
         (caddr t)))) 
  
 (define (test-execed? t) 
   (not (equal? 'bunny-test:un-tested 
                (caddr t)))) 
  
 (define (exec-test t) 
   (set-car! (cddr t) 
             (eval (get-code t)))) 
  
 (define (test-failed? t) 
   (if (bunny-test? t) 
       (not (get-test-result t)))) 
  
 (define (test-passed? t) 
   (and (bunny-test? t) 
        (get-test-result t))) 
  
 (define (test-suite name . tests) 
   (list 'bunny-suite 
         name 
         tests)) 
  
 (define (bunny-suite? b) 
   (and (list? b) 
        (eq? 'bunny-suite (car b)))) 
  
 (define (get-suite-name b) 
   (if (bunny-suite? b) 
       (cadr b))) 
  
 (define (get-suite-tests b) 
   (if (bunny-suite? b) 
       (caddr b))) 
  
 (define (display-suite s) 
   (newline) 
   (display "Suite: ") 
   (display (get-suite-name s)) 
   (for-each (lambda (t) (test-report t)) 
            (get-suite-tests s))) 
  
 (define (display-passed-test t) 
   (display ".")) 
  
 (define (display-failed-test t) 
   (newline) 
   (display "Failed: ") 
   (display (get-code t)) 
   (newline)) 
  
 (define (test-report item . depth) 
   (cond 
     ((bunny-suite? item) 
      (display-suite item)) 
     ((and (bunny-test? item) 
           (test-failed? item)) 
      (display-failed-test item)) 
     ((and (bunny-test? item) 
           (test-passed? item)) 
      (display-passed-test item))   
     (else 
      (error "No test or suite passed to report!")))) 
  
 (define (test-tests) 
   (test-suite 'Test-Tests 
     (test '(bunny-test? (test '#t))) 
     (test '(not(bunny-test? #f))) 
     (test '(equal? (get-code (test 'some-code)) 'some-code)) 
     (test '(get-test-result (test #t))) 
     (test '(not(get-test-result (test #f)))) 
     (let ((t (test '#t))) 
       (test (not (test-execed? t))) 
       (test (get-test-result t)) 
       (test (test-execed? t))))) 
  
 (define (suite-tests) 
   (test-suite 'Suite-Tests 
     (test '(bunny-suite? (test-suite 'dummy (test #t) (test #f)))) 
     (test '(equal? (get-suite-name (test-suite 'dummy (test #t))) 'dummy)) 
     (test '(bunny-test? (car (get-suite-tests (test-suite 'dummy (test #t)))))) 
     (test '(bunny-suite? (cadr (get-suite-tests (test-suite 'dummy (test #t) (test-suite 'inner-dummy)))))))) 
  
 (define (all-tests) 
   (test-suite 'All-Tests 
     (test-tests) 
     (suite-tests))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simple test system wich tests both for an expected run output and
;;; return value. A call to run-tests will run all the registered
;;; tests, or a single test can be called by calling its name ex: (test).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (to-string-val e . es)
  (define val (gensym 'val))
  (define out (gensym 'out))
  `(lambda ()
     (let ((,val (with-output-to-file "test-dump" (lambda () ,e ,@es)))
           (,out (to-string ,e ,@es)))
       (delete-file "test-dump")
       (values ,val ,out))))

(define-macro (define-test name expected-out expected-retval e . es)
  (define test-out (gensym 'test-out))
  (define test-ret (gensym 'test-ret))
  (define output (gensym 'output))
  (define retval (gensym 'retval))
  
  `(begin
     (define (,name)
       (show ',name ": ")
       (call-with-values (to-string-val ,e ,@es)
         (lambda (,retval ,output)
           (let* ((,test-out (string=? ,output ,expected-out))
                  (,test-ret (equal? ,retval ,expected-retval)))
             (if (or (not ,test-ret) (not ,test-out))
                 (display 'failed!)
                 (display 'passed))
             (if (not ,test-out)
                 (show " expected output " ,expected-out " got " ,output))
             (if (not ,test-ret)
                 (show " expected return value " ,expected-retval
                       " got " ,retval))
             (newline)))))
     (set! tests (cons ,name tests))))

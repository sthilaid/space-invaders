;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simple test system wich tests both for an expected run output and
;;; return value. A call to run-tests will run all the registered
;;; tests, or a single test can be called by calling its name ex: (test).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tests '())
(define (run-tests) (for-each (lambda (t) (t)) (reverse tests)))


(include "scm-lib.scm")

(define-type corout id kont)

(define (new-corout id thunk)
  (make-corout id (lambda (dummy) (thunk))))

(define (yield-corout)
  (call/cc
   (lambda (k)
     (corout-kont-set! (current-coroutine) k)
     (corout-scheduler))))

(define (terminate-corout)
  (current-coroutine #f)
  (corout-scheduler))

(define current-coroutine (make-parameter #f))
(define corout-q (make-parameter #f))
(define root-k (make-parameter #f))

(define (corout-boot c1 . cs)
  (call/cc
   (lambda (k)
     (parameterize ((root-k k)
                    (corout-q (new-queue)))
       (for-each (lambda (c) (enqueue! (corout-q) c))
                 (cons c1 cs))
       (corout-scheduler)))))

(define (corout-scheduler)
  ;; if the current corout is not finished, enqueue it
  (if (current-coroutine)
      (enqueue! (corout-q) (current-coroutine)))
  ;; Get the next coroutine, if one is available
  (with-exception-catcher
   (lambda (e) (case e ((empty-q) #f) (else (raise e))))
   (lambda () (current-coroutine (dequeue! (corout-q)))))
  ;; if there is one coroutine, run it, else stop the coroutine
  ;; scheduler.
  (if (current-coroutine)
      ((corout-kont (current-coroutine)) 'go)
      ((root-k) #t)))
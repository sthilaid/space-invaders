(include "scm-lib.scm")

(define-type corout id kont)

(define (new-corout id thunk)
  (make-corout id (lambda (dummy) (thunk))))

(define (yield-corout)
  (call/cc
   (lambda (k)
     (corout-kont-set! (current-coroutine) k)
     (corout-scheduler))))

(define (terminate-corout exit-val)
  (current-coroutine exit-val)
  (corout-scheduler))

(define current-coroutine (make-parameter #f))
(define corout-q (make-parameter #f))
(define root-k (make-parameter #f))
(define return-value-handler (make-parameter #f))
(define return-value (make-parameter #f))

(define (corout-simple-boot c1 . cs)
  (apply corout-boot (lambda (acc val) val) c1 cs))

(define (corout-boot return-handler c1 . cs)
  (call/cc
   (lambda (k)
     (parameterize ((root-k k)
                    (corout-q (new-queue))
                    (return-value-handler return-handler)
                    (return-value #f))
       (for-each (lambda (c) (enqueue! (corout-q) c))
                 (cons c1 cs))
       (corout-scheduler)))))

(define (corout-scheduler)
  
  ;; if the current corout is not finished, enqueue it. Else use the
  ;; return value handler to store the return value of the finished
  ;; corout.
  (if (corout? (current-coroutine))
      (enqueue! (corout-q) (current-coroutine))
      (return-value
       (if (not (return-value))
           (current-coroutine)
           ((return-value-handler) (return-value) (current-coroutine)))))
  
  ;; Get the next coroutine, if one is available
  (with-exception-catcher
   (lambda (e) (case e ((empty-q) #f) (else (raise e))))
   (lambda () (current-coroutine (dequeue! (corout-q)))))
  
  ;; if there is one coroutine, run it, else stop the coroutine
  ;; scheduler.
  (if (corout? (current-coroutine))
      ((corout-kont (current-coroutine)) 'go)
      ((root-k) (return-value))))
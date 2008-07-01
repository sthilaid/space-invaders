(load "scm-lib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-type thrd id kont mailbox)

(define current-thrd (make-parameter #f))
(define q (make-parameter #f))
(define root-k (make-parameter #f))
(define return-value-handler (make-parameter #f))
(define return-value (make-parameter #f))


(define (corout-scheduler)
  (if (thrd? (current-thrd))
      (enqueue! (q) (current-thrd))
      (return-value
       (if (not (return-value))
           (current-thrd)
           ((return-value-handler) (return-value) (current-thrd)))))

  ;; Get the next coroutine, if one is available
  (with-exception-catcher
   (lambda (e) (case e ((empty-q) #f) (else (raise e))))
   (lambda () (current-thrd (dequeue! (q)))))
  
  ;; if there is one coroutine, run it, else stop the coroutine
  ;; corout-scheduler.
  (if (thrd? (current-thrd))
      ((thrd-kont (current-thrd)) 'go)
      ((root-k) (return-value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-thrd id thunk)
  (make-thrd id (lambda (dummy) (thunk)) (new-queue)))


(define (empty-mailbox?)
  (empty-queue? (thrd-mailbox (current-thrd))))


(define (?)
  (define mail (thrd-mailbox (current-thrd)))
  (if (empty-queue? mail)
      (begin (corout-yield)
             (?))
      (dequeue! mail)))


(define (! dest-thrd msg)
  (enqueue! (thrd-mailbox dest-thrd) msg))


(define (corout-yield)
  (call/cc
   (lambda (k)
     (thrd-kont-set! (current-thrd) k)
     (corout-scheduler))))



(define (terminate-corout exit-val)
  (current-thrd exit-val)
  (corout-scheduler))

(define (simple-corout-boot c1 . cs)
  (apply corout-boot (lambda (acc val) val) c1 cs))

(define (corout-boot return-handler c1 . cs)
  (call/cc
   (lambda (k)
     (parameterize ((root-k k)
                    (current-thrd #f)
                    (q (new-queue))
                    (return-value-handler return-handler)
                    (return-value #f))
       (for-each (lambda (c) (enqueue! (q) c))
                 (cons c1 cs))
       (corout-scheduler)))))

(define (kill-all!)
  ((root-k) 'reset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simple-test)
  (let ((c1 (new-thrd 'c1 (lambda ()
                              (pp 'A)
                              (corout-yield)
                              (pp 'B)
                              (terminate-corout 'C))))
        (c2 (new-thrd 'c2 (lambda ()
                              (pp 1)
                              (corout-yield)
                              (pp 2)
                              (terminate-corout 3)))))
    (simple-corout-boot c1 c2)))


(define (test-ret-val-handler)
  (let ((c1 (new-thrd 'c1 (lambda ()
                              (pp 'A)
                              (corout-yield)
                              (pp 'B)
                              (terminate-corout '1))))
        (c2 (new-thrd 'c2 (lambda ()
                              (pp 1)
                              (corout-yield)
                              (pp 2)
                              (terminate-corout 2))))
        (c3 (new-thrd 'c3 (lambda ()
                              (pp #\a)
                              (corout-yield)
                              (pp #\b)
                              (terminate-corout 3)))))
    (pp `(return value (expecting 6) =
                 ,(corout-boot (lambda (acc v) (+ acc v))
                               c1 c2 c3)))))

(define (test-mailboxes)
  (letrec ((c1 (new-thrd 'c1 (lambda ()
                                 (pp `(c1 received ,(?)))
                                 (pp `(c1 received ,(?)))
                                 (terminate-corout 'done))))
           (c2 (new-thrd 'c2 (lambda ()
                                 (pp 'sending-data-from-c2)
                                 (! c1 'allo)
                                 (terminate-corout 'do))))
           (c3 (new-thrd 'c3 (lambda ()
                                 (pp 'sending-data-from-c3)
                                 (! c1 'salut)
                                 (terminate-corout 'ne)))))
    (simple-corout-boot c1 c2 c3)))

(define (tests)
  (simple-test)
  (newline)
  (test-ret-val-handler)
  (newline)
  (test-mailboxes))
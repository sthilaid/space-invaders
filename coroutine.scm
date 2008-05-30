(define-type corout id kont mailbox)

(define (new-corout id thunk)
  (make-corout id (lambda (dummy) (thunk)) (new-queue)))

(define (corout-empty-mailbox?)
  (empty-queue? (corout-mailbox (current-coroutine))))

(define (?)
  (define mail (corout-mailbox (current-coroutine)))
  (if (empty-queue? mail)
      (begin (yield-corout)
             (?))
      (dequeue! mail)))

(define (! dest-corout msg)
  (enqueue! (corout-mailbox dest-corout) msg))

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
                    (current-coroutine #f)
                    (corout-q (new-queue))
                    (return-value-handler return-handler)
                    (return-value #f))
       (for-each (lambda (c) (enqueue! (corout-q) c))
                 (cons c1 cs))
       (corout-scheduler)))))

(define (corout-kill-all!)
  ((root-k) 'reset))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; simple test: expected output
(define (simple-test)
  (let ((c1 (new-corout 'c1 (lambda ()
                              (pp 'A)
                              (yield-corout)
                              (pp 'B)
                              (terminate-corout 'C))))
        (c2 (new-corout 'c2 (lambda ()
                              (pp 1)
                              (yield-corout)
                              (pp 2)
                              (terminate-corout 3)))))
    (corout-simple-boot c1 c2)))

;; Return handling test
(define (test-ret-val-handler)
  (let ((c1 (new-corout 'c1 (lambda ()
                              (pp 'A)
                              (yield-corout)
                              (pp 'B)
                              (terminate-corout '1))))
        (c2 (new-corout 'c2 (lambda ()
                              (pp 1)
                              (yield-corout)
                              (pp 2)
                              (terminate-corout 2))))
        (c3 (new-corout 'c3 (lambda ()
                              (pp #\a)
                              (yield-corout)
                              (pp #\b)
                              (terminate-corout 3)))))
    (pp `(return value (expecting 6) =
                 ,(corout-boot (lambda (acc v) (+ acc v))
                               c1 c2 c3)))))

(define (test-mailboxes)
  (letrec ((c1 (new-corout 'c1 (lambda ()
                                 (pp `(c1 received ,(?)))
                                 (pp `(c1 received ,(?)))
                                 (terminate-corout 'done))))
           (c2 (new-corout 'c2 (lambda ()
                                 (pp 'sending-data-from-c2)
                                 (! c1 'allo)
                                 (terminate-corout 'do))))
           (c3 (new-corout 'c3 (lambda ()
                                 (pp 'sending-data-from-c3)
                                 (! c1 'salut)
                                 (terminate-corout 'ne)))))
    (corout-simple-boot c1 c2 c3)))

(define (corout-tests)
  (simple-test)
  (newline)
  (test-ret-val-handler)
  (newline)
  (test-mailboxes))
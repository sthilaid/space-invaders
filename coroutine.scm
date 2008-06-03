;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: coroutine.scm
;;
;; description: A simple coroutine scheduler implementation. A
;; coroutine as defined here is an execution thread that can only
;; explicitelly yield the execution thread to the next (fifo)
;; available coroutine.
;;
;; Coroutines can also communicate together using a simple mailing
;; system, where some scheme data can be sent to another coroutine.
;;
;; Also, there is a return value handler that can be provided to
;; mangage the values returned by each coroutine and produce a final
;; return value return at the end of the scheduler's execution.
;;
;; Usages exemples are provided in the test section.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Coroutine data type definition
(define-type corout id kont mailbox)

;; Dynamically scoped control variables
(define current-coroutine (make-parameter #f))
(define corout-q (make-parameter #f))
(define root-k (make-parameter #f))
(define return-value-handler (make-parameter #f))
(define return-value (make-parameter #f))

;; Coroutine scheduler function. Used internally to schedule the next
;; coroutine in the fifo and manage the return values of the finished
;; coroutines.
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
;; External definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Coroutine constructor
(define (new-corout id thunk)
  (make-corout id (lambda (dummy) (thunk)) (new-queue)))

;; Verify if the current coroutine's mailbox is empty or if a message
;; is available.
(define (corout-empty-mailbox?)
  (empty-queue? (corout-mailbox (current-coroutine))))

;; Receive a message into the current coroutine. If no messages are
;; available, the coroutine will hang in a semi-active wait loop to
;; get the message, else the received message is returned.
(define (?)
  (define mail (corout-mailbox (current-coroutine)))
  (if (empty-queue? mail)
      (begin (yield-corout)
             (?))
      (dequeue! mail)))

;; Send a message to the given coroutine
(define (! dest-corout msg)
  (enqueue! (corout-mailbox dest-corout) msg))

;; Yield the execution to the next coroutine
(define (yield-corout)
  (call/cc
   (lambda (k)
     (corout-kont-set! (current-coroutine) k)
     (corout-scheduler))))

;; Kill the exeuction of the current coroutine and have it return with
;; the specified exit value.
(define (terminate-corout exit-val)
  (current-coroutine exit-val)
  (corout-scheduler))

;; Will boot the scheduler with all the specified coroutines with the
;; default return-value handler. This handler will be such that the
;; value of the last coroutine to terminate will be returned.
(define (corout-simple-boot c1 . cs)
  (apply corout-boot (lambda (acc val) val) c1 cs))

;; Boot the scheduler with the provided coroutines and use
;; return-handler to manage the return values. The return-handler
;; function must have the following form:
;;
;; (define (my-return-val-handler (acc val) ...))
;;
;; Where acc is the so far accumulated return values resulted by the
;; handler and val is the currently returned value. The first returned
;; value will internally be accumulated by the scheduler such that the
;; first call the the return-handler will occur when the second
;; coroutine is terminated.
;;
;; The coroutines must be terminated by the terminate-corout function
;; for the result to be compiled by the return-handler.
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

;; Terminate the scheduler, resulting in stoping all the executing
;; coroutines. This must be called from within a coroutine.
(define (corout-kill-all!)
  ((root-k) 'reset))



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
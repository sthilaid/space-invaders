(include "statprof.scm")
(include "scm-lib-macro.scm")
(load "scm-lib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-type thrd id kont mailbox state)

(define current-thrd (make-parameter 'unbound))
(define q (make-parameter 'unbound))
(define root-k (make-parameter 'unbound))
(define return-value-handler (make-parameter 'unbound))
(define return-value (make-parameter 'unbound))
(define parent-state (make-parameter 'unbound))
(define return-to-sched (make-parameter 'unbound))

(define (unbound? v) (eq? v 'unbound))

(define (save-state!)
  (let ((current-state (save-state)))
    (thrd-parent-state-set! (current-thrd) current-state)))

(define (save-state)
  (if (not (or (unbound? (current-thrd))
               (unbound? (q))
               (unbound? (root-k))
               (unbound? (return-value-handler))
               (unbound? (return-value))))
      
      (begin
        (pp 'saved-state)
        (vector (current-thrd)
                (q)
                (root-k)
                (return-value-handler)
                (return-value)
                (parent-state)
                (return-to-sched)))
      (begin
        (pp 'did-not-saved-state)
        #f)))

(define (restore-state state)
  (pp 'restoring-state)
  (if state
      (begin
        (current-thrd         (vector-ref state 0))
        (q                    (vector-ref state 1))
        (root-k               (vector-ref state 2))
        (return-value-handler (vector-ref state 3))
        (return-value         (vector-ref state 4))
        (parent-state         (vector-ref state 5))
        (return-to-sched      (vector-ref state 6)))))


(define (corout-scheduler)
  (let loop ()
    (pp `(test-scheduler
        current:
        ,(if (thrd? (current-thrd))
             (thrd-id (current-thrd))
             `(value: ,(current-thrd)))
        parent? ,(not (not (parent-state)))))


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
        ;; execute the thread
        (begin (call/cc (lambda (k)
                          (return-to-sched k)
                          (if (thrd-state (current-thrd))
                              (let ((state (save-state)))
                                (restore-state (thrd-state (current-thrd)))
                                (parent-state state)))
                          ((thrd-kont (current-thrd)) 'go)))

               ;; temporary testing?
               ;; can't work here, inf loop
               (special-yield)

               (loop))
        ;; finish up the scheduling process
        (let ((finish-scheduling (root-k)))
          (pp 'ending-sheduling)
          (restore-state (parent-state))
          (finish-scheduling (return-value))))
    ))


(define (resume-scheduling)
  ((return-to-sched) 'back-in-sched))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-thrd id thunk)
  (make-thrd id (lambda (dummy) (thunk)) (new-queue) #f))


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


;;TODO??? Sauver parent-state dans variable dyn, et current-state ds
;; corout.  evaluer dans parameterize a l'interieur du sched la
;; partie de code appartement a la coroutine du sched.

(define (corout-yield)
  (call/cc
   (lambda (k)
     (thrd-kont-set! (current-thrd) k)
     (resume-scheduling))))


;; If current-thrd is not set, then the yield should have occured
;; within the scheduler.
(define (special-yield)
  (pp 'test-special-yield)
  (call/cc
   (lambda (k)
     (if (parent-state)
         (let ((state (save-state)))
           (restore-state (parent-state))
           ;; current-thrd should be restored on the parent's
           ;; level. It just thus be defined as the new coroutine
           ;; system's coroutine.
           (thrd-state-set! (current-thrd) state)
           (thrd-kont-set! (current-thrd) k)
           (resume-scheduling))))))


(define (terminate-corout exit-val)
  (current-thrd exit-val)
  (resume-scheduling))


(define (simple-corout-boot c1 . cs)
  (apply corout-boot (lambda (acc val) val) c1 cs))

(define (corout-boot return-handler c1 . cs)
  (call/cc
   (lambda (k)
     (parameterize ((parent-state (save-state))
                    (root-k k)
                    (current-thrd #f)
                    (q (new-queue))
                    (return-value-handler return-handler)
                    (return-value #f))
       (for-each (lambda (c) (enqueue! (q) c))
                 (cons c1 cs))
       (corout-scheduler))))
  (pp 'boot-finished))

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

(define (test-recursive-sim)
  (let* ((sA (new-thrd 'sA (lambda () (for i 0 (< i 3)
                                           (begin (pp 'A)
                                                  (corout-yield)))
                                   (terminate-corout 'sA))))
         (sB (new-thrd 'sB (lambda () (for i 0 (< i 3)
                                           (begin (pp 'B)
                                                  (corout-yield)))
                                   (terminate-corout 'sB))))
         (s1 (new-thrd 's1 (lambda () (for i 0 (< i 3)
                                           (begin (pp 1)
                                                  (corout-yield)))
                                   (pp 's1-finished)
                                   (terminate-corout 's1))))
         (s2 (new-thrd 's2 (lambda () (for i 0 (< i 3)
                                           (begin (pp 2)
                                                  (corout-yield)))
                                   (pp 's2-finished)
                                   (terminate-corout 's2))))
         (meta-AB
          (new-thrd 'meta-AB
                    (lambda ()
                      (simple-corout-boot sA sB)
                      (terminate-corout 'meta-AB-done!))))
         (meta-12
          (new-thrd 'meta-12
                    (lambda ()
                      (simple-corout-boot s1 s2)
                      (terminate-corout 'meta-12-done!)))))
    (simple-corout-boot meta-AB meta-12)))

(define (test-prof)
  (profile-start!)
  (let ((exit-test (make-parameter #f)))
    (call/cc
     (lambda (k)
       (exit-test (lambda () (k 'fini)))
       (test-recursive-sim))))
  (profile-stop!)
  (write-profile-report "thread-profiling"))

(define (tests)
  (simple-test)
  (newline)
  (test-ret-val-handler)
  (newline)
  (test-mailboxes))
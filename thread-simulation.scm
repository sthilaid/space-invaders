(include "test-macro.scm")
(include "scm-lib-macro.scm")
(load "scm-lib")
(load "test")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-type corout id kont mailbox (state unprintable:))
(define-type state
  current-corout q root-k return-value-handler
  return-value return-to-sched
  (parent-state unprintable:))

(define current-corout (make-parameter 'unbound))
(define q (make-parameter 'unbound))
(define root-k (make-parameter 'unbound))
(define return-value-handler (make-parameter 'unbound))
(define return-value (make-parameter 'unbound))
(define parent-state (make-parameter 'unbound))
(define return-to-sched (make-parameter 'unbound))

(define unbound 'unbound)
(define (unbound? v) (eq? v unbound))

(define (save-state!)
  (let ((current-state (save-state)))
    (corout-parent-state-set! (current-corout) current-state)))

(define (save-state)
  (if (not (or (unbound? (current-corout))
               (unbound? (q))
               (unbound? (root-k))
               (unbound? (return-value-handler))
               (unbound? (return-value))))
      
      (begin
        #;
        (pp 'saved-state)
        (make-state (current-corout)
                    (q)
                    (root-k)
                    (return-value-handler)
                    (return-value)
                    (return-to-sched)
                    (parent-state)))
      (begin
        #;
        (pp 'did-not-saved-state)
        #f)))

(define (restore-state state)
  #;
  (pp `(restoring-state ,state))
  (if state
      (begin
        (current-corout       (state-current-corout state))
        (q                    (state-q state))
        (root-k               (state-root-k state))
        (return-value-handler (state-return-value-handler state))
        (return-value         (state-return-value state))
        (return-to-sched      (state-return-to-sched state))
        (parent-state         (state-parent-state state)))
      ;; un-initializing the global simulation state
      (begin
        (current-corout       unbound)
        (q                    unbound)
        (root-k               unbound)
        (return-value-handler unbound)
        (return-value         unbound)
        (return-to-sched      unbound)
        (parent-state         unbound))))



(define (corout-scheduler)
  #;
  (pp `(test-scheduler
  current:
  ,(if (corout? (current-corout))
  (corout-id (current-corout))
  `(value: ,(current-corout)))
  parent? ,(not (not (parent-state)))))


  (if (corout? (current-corout))
      (enqueue! (q) (current-corout))
      (return-value
       (if (not (return-value))
           (current-corout)
           ((return-value-handler) (return-value) (current-corout)))))

  ;; Get the next coroutine, if one is available
  (with-exception-catcher
   (lambda (e) (case e ((empty-q) #f) (else (raise e))))
   (lambda () (current-corout (dequeue! (q)))))
  
  ;; if there is one coroutine, run it, else stop the coroutine
  ;; corout-scheduler.
  (if (corout? (current-corout))
      ;; execute the thread
      (begin (call/cc (lambda (k)
                        (return-to-sched k)
                        (let ((kontinuation (corout-kont (current-corout))))
                          (if (corout-state (current-corout))
                              (let ((state (save-state)))
                                (restore-state (corout-state (current-corout)))
                                (parent-state state)))
                          (kontinuation 'go))))
             
             (corout-scheduler))
      ;; finish up the scheduling process
      (let ((finish-scheduling (root-k))
            (ret-val (return-value)))
        #;
        (pp `(ending-sheduling with ,ret-val))
        (restore-state (parent-state))
        (finish-scheduling ret-val))))


(define (resume-scheduling)
  ((return-to-sched) 'back-in-sched))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-corout id thunk)
  (make-corout id
               ;; here the terminate-corout call is added to ensure that
               ;; the thread will terminate cleanly.
               (lambda (dummy) (terminate-corout (thunk)))
               (new-queue) #f))


(define (empty-mailbox?)
  (empty-queue? (corout-mailbox (current-corout))))


(define (?)
  (define mail (corout-mailbox (current-corout)))
  (if (empty-queue? mail)
      (begin (corout-yield)
             (?))
      (dequeue! mail)))


(define (! dest-corout msg)
  (enqueue! (corout-mailbox dest-corout) msg))


;;TODO??? Sauver parent-state dans variable dyn, et current-state ds
;; corout.  evaluer dans parameterize a l'interieur du sched la
;; partie de code appartement a la coroutine du sched.

(define (corout-yield)
  (call/cc
   (lambda (k)
     (corout-kont-set! (current-corout) k)
     (resume-scheduling))))


;; If current-corout is not set, then the yield should have occured
;; within the scheduler.
(define (super-yield)
  #;
  (pp 'test-super-yield)
  (call/cc
   (lambda (k)
     (if (parent-state)
         (let ((state (save-state)))
           (restore-state (parent-state))
           ;; current-corout should be restored on the parent's
           ;; level. It just thus be defined as the new coroutine
           ;; system's coroutine.
           (corout-state-set! (current-corout) state)
           (corout-kont-set! (current-corout) k)
           (resume-scheduling))
         #;
         (pp 'ignored)))))


(define (terminate-corout exit-val)
  (current-corout exit-val)
  (resume-scheduling))


(define (simple-corout-boot c1 . cs)
  (apply corout-boot (lambda (acc val) val) c1 cs))

(define (corout-boot return-handler c1 . cs)
  (call/cc
   (lambda (k)
     (let ((fresh-start? (unbound? (current-corout))))
       (if (not fresh-start?) (parent-state (save-state)))
       (root-k k)
       (current-corout #f)
       (q (new-queue))
       (return-value-handler return-handler)
       (return-value #f)
       (if fresh-start? (parent-state #f)))
     (for-each (lambda (c) (enqueue! (q) c))
               (cons c1 cs))
     (corout-scheduler))))

(define (kill-all!)
  (let ((finish-scheduling (root-k))
        (ret-val (return-value)))
    (restore-state (parent-state))
    (finish-scheduling 'killed-all)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test simple-test "A1B2" 3
  (let ((c1 (new-corout 'c1 (lambda ()
                              (display 'A)
                              (corout-yield)
                              (display 'B)
                              (terminate-corout 'C))))
        (c2 (new-corout 'c2 (lambda ()
                              (display 1)
                              (corout-yield)
                              (display 2)
                              (terminate-corout 3)))))
    (simple-corout-boot c1 c2)))

(define-test test-kill-all "12" 'killed-all
  (let ((c1 (new-corout 'c1 (lambda ()
                              (for i 0 (< i 3) (begin (if (= i 1)
                                                          (kill-all!))
                                                      (display 1)
                                                      (corout-yield))))))
        (c2 (new-corout 'c2 (lambda ()
                              (for i 0 (< i 3) (begin (display 2)
                                                      (corout-yield)))))))
    (simple-corout-boot c1 c2)))

(define-test test-ret-val-handler "A1aB2" 6
  (let ((c1 (new-corout 'c1 (lambda () (begin (display 'A)
                                              (corout-yield)
                                              (display 'B)
                                              1))))
        (c2 (new-corout 'c2 (lambda () (begin (display 1)
                                              (corout-yield)
                                              (display 2)
                                              2))))
        ;; here terminate-corout is used to shortcircuit the flow of
        ;; the thunk and finish earlier this thread.
        (c3 (new-corout 'c3 (lambda () (begin (display #\a)
                                              (terminate-corout 3)
                                              (corout-yield)
                                              (display #\b))))))
    (corout-boot (lambda (acc v) (+ acc v)) c1 c2 c3)))


(define-test test-mailboxes
  (string-append "sending-data-from-c2\n"
                 "sending-data-from-c3\n"
                 "(c1 received allo)\n"
                 "(c1 received salut)\n")
  'done
  (letrec ((c1 (new-corout 'c1 (lambda ()
                                 (pretty-print `(c1 received ,(?)))
                                 (pretty-print `(c1 received ,(?)))
                                 'done)))
           (c2 (new-corout 'c2 (lambda ()
                                 (pretty-print 'sending-data-from-c2)
                                 (! c1 'allo))))
           (c3 (new-corout 'c3 (lambda ()
                                 (pretty-print 'sending-data-from-c3)
                                 (! c1 'salut)))))
    (simple-corout-boot c1 c2 c3)))


(define-test test-recursive-sim "A12BA12BA12B" 'meta-12-done!
  (let* ((sA (new-corout 'sA (lambda () (for i 0 (< i 3)
                                             (begin (display 'A)
                                                    (super-yield)
                                                    (corout-yield))))))
         (sB (new-corout 'sB (lambda () (for i 0 (< i 3)
                                             (begin (display 'B)
                                                    (corout-yield)))
                                     (terminate-corout 'sB))))
         (s1 (new-corout 's1 (lambda () (for i 0 (< i 3)
                                             (begin (display 1)
                                                    (corout-yield))))))
         (s2 (new-corout 's2 (lambda () (for i 0 (< i 3)
                                             (begin (display 2)
                                                    (super-yield)
                                                    (corout-yield))))))
         (meta-AB
          (new-corout 'meta-AB
                      (lambda ()
                        (simple-corout-boot sA sB)
                        'meta-AB-done!)))
         (meta-12
          (new-corout 'meta-12
                      (lambda ()
                        (simple-corout-boot s1 s2)
                        'meta-12-done!))))
    (simple-corout-boot meta-AB meta-12)))

(define-test test-kill-all-rec "A1B2ABAB" 'meta-AB-done!
  (let* ((sA (new-corout 'sA (lambda () (for i 0 (< i 3)
                                             (begin (display 'A)
                                                    (super-yield)
                                                    (corout-yield))))))
         (sB (new-corout 'sB (lambda () (for i 0 (< i 3)
                                             (begin (display 'B)
                                                    (super-yield)
                                                    (corout-yield))))))
         (s1 (new-corout 's1 (lambda () (for i 0 (< i 3)
                                             (begin (if (= i 1) (kill-all!))
                                                    (display 1)
                                                    (super-yield)
                                                    (corout-yield))))))
         (s2 (new-corout 's2 (lambda () (for i 0 (< i 3)
                                             (begin (display 2)
                                                    (super-yield)
                                                    (corout-yield))))))
         (meta-AB
          (new-corout 'meta-AB
                      (lambda ()
                        (simple-corout-boot sA sB)
                        'meta-AB-done!)))
         (meta-12
          (new-corout 'meta-12
                      (lambda ()
                        (simple-corout-boot s1 s2)
                        'meta-12-done!))))
    (simple-corout-boot meta-AB meta-12)))


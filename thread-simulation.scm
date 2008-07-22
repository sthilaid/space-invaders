(include "test-macro.scm")
(include "scm-lib-macro.scm")
(include "thread-simulation-macro.scm")
(load "scm-lib")
(load "test")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data type definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type corout id kont mailbox (state unprintable:) prioritize?)

(define-type state
  current-corout q sleep-q root-k return-value-handler
  return-value return-to-sched
  (parent-state unprintable:))

(define-type sem value wait-queue)

;; Must be used to enqueue a coroutine in the coroutine active
;; queue. The sleep queue *must not* use otherwise, it will result in
;; an infinite loop if a coroutine is to be prioritized.
(define (corout-enqueue! q corout)
  (if (corout-prioritize? corout)
      (queue-push! q corout)
      (enqueue! q corout)))

;; A prioritized coroutine will be scheduled as the next coroutine to
;; run, if scheduled with corout-enqueue!.
(define (prioritize! c)
  (corout-prioritize?-set! c #t))
(define (unprioritize! c)
  (corout-prioritize?-set! c #f))

(define (make-sleep-q-el cond corout) (cons cond corout))
(define (sleep-q-el-condition? sleep-q-el)
  ((car sleep-q-el)))
(define sleep-q-el-coroutine cdr)

(define unbound '!---unbound---!)
(define (unbound? v) (eq? v unbound))

(define sleeping '!---sleeping---!)
(define (sleeping? v) (eq? v unbound))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduling state global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-corout (make-parameter unbound))
(define q (make-parameter unbound))
(define sleep-q (make-parameter unbound))
(define root-k (make-parameter unbound))
(define return-value-handler (make-parameter unbound))
(define return-value (make-parameter unbound))
(define parent-state (make-parameter unbound))
(define return-to-sched (make-parameter unbound))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State management functionnalities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Saves the current scheduler's state into an object
(define (save-state)
  (if (not (or (unbound? (current-corout))
               (unbound? (q))
               (unbound? (sleep-q))
               (unbound? (root-k))
               (unbound? (return-value-handler))
               (unbound? (return-value))))
      
      (make-state (current-corout)
                  (q)
                  (sleep-q)
                  (root-k)
                  (return-value-handler)
                  (return-value)
                  (return-to-sched)
                  (parent-state))
      #f))

;; Restores the givent state object into the environment
(define (restore-state state)
  (if state
      (begin
        (current-corout       (state-current-corout state))
        (q                    (state-q state))
        (sleep-q              (state-sleep-q state))
        (root-k               (state-root-k state))
        (return-value-handler (state-return-value-handler state))
        (return-value         (state-return-value state))
        (return-to-sched      (state-return-to-sched state))
        (parent-state         (state-parent-state state)))

      ;; un-initializing the global simulation state
      (begin
        (current-corout       unbound)
        (q                    unbound)
        (sleep-q              unbound)
        (root-k               unbound)
        (return-value-handler unbound)
        (return-value         unbound)
        (return-to-sched      unbound)
        (parent-state         unbound))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduler's internals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If the current-corout is not a corout, it means that the last
;; coroutine finished, and the value of current-corout is the return
;; value of that coroutine. Exception, when current-corout is
;; unbound, this happens when the q is empty and no coroutine in the
;; sleep-q can be awaken yet.
(define (manage-return-value)
  (cond
   ((corout? (current-corout)) (corout-enqueue! (q) (current-corout)))
   ((not (sleeping? (current-corout)))
    (return-value
     (if (not (return-value))
         (current-corout)
         ((return-value-handler) (return-value) (current-corout)))))))

(define (wake-up-sleepers)
  (let loop ((sleeping-el (dequeue!? (sleep-q))) (still-sleeping '()))
    (if sleeping-el
        (if (sleep-q-el-condition? sleeping-el)
            (begin
              (corout-enqueue! (q) (sleep-q-el-coroutine sleeping-el))
              (loop (dequeue!? (sleep-q)) still-sleeping))
            (loop (dequeue!? (sleep-q))
                  (cons sleeping-el still-sleeping)))
            (for-each (lambda (el) (enqueue! (sleep-q) el))
                      still-sleeping))))

(define (resume-coroutine)
  (call/cc (lambda (k)
               (return-to-sched k)
               (let ((kontinuation (corout-kont (current-corout))))
                 ;; if it is a statefull couroutine
                 ;; (executing a scheduler) then restore its
                 ;; environnement
                 (if (corout-state (current-corout))
                     (let ((state (save-state)))
                       (restore-state (corout-state (current-corout)))
                       (parent-state state)))
                 ;; run the coroutine
                 (kontinuation 'go)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the main coroutine scheduler. Dispatches coroutine one
;; after another, manages the coroutine's return values and returns to
;; it's super scheduer (if there is one) when all the coroutines
;; finishes.
(define (corout-scheduler)
  #;
  (pp `(corout-scheduler cur: ,(if (corout? (current-corout))
                                   (corout-id (current-corout))
                                   (current-corout))
                         q: ,(map corout-id (queue-list (q)))))
  
  ;; Verify for a return value
  (manage-return-value)
  
  ;; Verify if the sleeping elements can be woke up
  (wake-up-sleepers)

  ;; Get the next coroutine, if one is available
  (current-corout (dequeue!? (q)))

  #;
  (pp `(now booting on ,(if (corout? current-corout)
                            (corout-id current-corout)
                            current-corout)
            q: ,(map corout-id (queue-list q))
            sleep-q: ,(map corout-id (queue-list sleep-q))))

  ;; if there is one coroutine, run it, else stop the coroutine
  ;; scheduler.
  (cond
   ((corout? (current-corout))
    (begin
      (resume-coroutine)
      ;; loop the scheduler, if the coroutine finished
      (corout-scheduler)))

   ;; If only sleeping coroutines are left, sleep for a while
   ((not (empty-queue? (sleep-q)))
    (begin
      (thread-sleep! 0.0001) ; TODO: change this?
      (current-corout sleeping)
      (corout-scheduler)))
      
   ;; finish up the scheduling process by restoring the super
   ;; environnement and end this scheduler
   (else
    (let ((finish-scheduling (root-k))
          (ret-val (return-value)))
      (restore-state (parent-state))
      (finish-scheduling ret-val)))))


;; Simple abstraction that just resumes the scheduler's calculation
(define (resume-scheduling)
  ((return-to-sched) 'back-in-sched))

;; Semaphore's internals 
(define (sem-increase! sem) (sem-value-set! sem (+ (sem-value sem) 1)))
(define (sem-decrease! sem) (sem-value-set! sem (- (sem-value sem) 1)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creation of a new coroutine object with id as it's identifier
;; (mostly used for debugging and the given thunk as its body.
(define (new-corout id thunk)
  (make-corout id
               ;; here the terminate-corout call is added to ensure that
               ;; the thread will terminate cleanly.
               (lambda (dummy) (terminate-corout (thunk)))
               (new-queue) #f #f))

;; Querry the current-coroutine's mailbox to see if its empty
(define (empty-mailbox?)
  (empty-queue? (corout-mailbox (current-corout))))

;; Gets the next message in the mailbox queue. If no message is
;; available, the coroutine will sleep until a message is received.
(define (?)
  (define mail (corout-mailbox (current-corout)))
  (if (empty-queue? mail)
      (begin (corout-yield)
             (?))
      (dequeue! mail)))


;; Send a message to the givent destination coroutine object.
(define (! dest-corout msg)
  (enqueue! (corout-mailbox dest-corout) msg))

;; Yields the current coroutine's calculation. The calculation will
;; resume where it left once the scheduler decides to resume it.
(define (corout-yield)
  (call/cc
   (lambda (k)
     (corout-kont-set! (current-corout) k)
     (resume-scheduling))))


;; This will yield the work of the scheduler itselft, assuming that
;; the scheduler runs inside a coroutine too, i.e. that we are in a
;; child recursive coroutine.
(define (super-yield)
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
           (resume-scheduling))))))

;; Terminates early the calculation of the current coroutine and
;; returns with the givent ret-val.
(define (terminate-corout exit-val)
  (current-corout exit-val)
  (resume-scheduling))

;; Starts the scheduling of the passed coroutines. This call will
;; return when all the coroutine will have terminated. It will use the
;; default return value handler, thus will return the value returned
;; by the last coroutine to finish.
(define (simple-corout-boot c1 . cs)
  (apply corout-boot (lambda (acc val) val) c1 cs))

;; Starts the scheduling of the givent coroutines with a specific
;; return value handler. The return value handler must have the
;; following format: (lambda (acc val) ...) where acc is the
;; accumulated return value and val is the last returned value by a
;; coroutine. The accumulated value will be return when the scheduling
;; process finishes.
(define (corout-boot return-handler c1 . cs)
  (call/cc
   (lambda (k)
     (let ((fresh-start? (unbound? (current-corout))))
       (if (not fresh-start?) (parent-state (save-state)))
       (root-k               k)
       (current-corout       #f)
       (q                    (new-queue))
       (sleep-q              (new-queue))
       (return-value-handler return-handler)
       (return-value         #f)
       (if fresh-start? (parent-state #f)))
     (for-each (lambda (c) (corout-enqueue! (q) c))
               (cons c1 cs))
     (corout-scheduler))))

;; Kills all the currently executing coroutines. This will operate on
;; only 1 level of scheduling, killing thus the currently executing
;; scheduler.
(define (kill-all! exit-val)
  (let ((finish-scheduling (root-k))
        #; (ret-val (return-value)))
    (restore-state (parent-state))
    (finish-scheduling exit-val)))

;; Will spawn the brother coroutine into the scheduler of the
;; currently executing coroutine
(define (corout-spawn-brother corout)
  (enqueue! (q) corout))

;; Spawns an anonymous brother coroutine.
(define (corout-spawn-brother-thunk id thunk)
  (enqueue! (q) (new-corout id thunk)))


;; Will put the coroutine into sleep, until the condition-thunk
;; returns #t.
(define (sleep-until condition-thunk)
  (if (not (condition-thunk))
      (call/cc
       (lambda (k)
         (corout-kont-set! (current-corout) k)
         (enqueue! (sleep-q) (make-sleep-q-el condition-thunk (current-corout)))
         (current-corout sleeping)
         ((return-to-sched) 'asleep)))))

;; Will put the current-corout to sleep for about "secs" seconds
(define (sleep-for secs)
  (let ((alarm (+ (time->seconds (current-time)) secs)))
    (sleep-until (lambda () (> (time->seconds (current-time)) alarm)))))


;; Create a new semaphore or mutex object
(define (new-semaphore init-value) (make-sem init-value '()))
(define (new-mutex) (new-semaphore 1))

;; Return #t if the semaphore is locked or #f if not
(define (sem-locked? sem) (< (sem-value sem) 1))

;; Usual lock and unlock (P/V, take/release, etc...) implementation
;; where the executed event will be put into a sleep queue if it tries
;; to take an unavailable semaphore, and the first enqueued (fifo)
;; event will be resumed when an unlock is performed.
(define (sem-lock! sem)
  (if (< (sem-value sem) 0)
      (sleep-until (lambda ()(not (sem-locked? sem))))
      (sem-decrease! sem)))
(define (sem-unlock! sem)
  (sem-increase! sem))




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
                              (for i 0 (< i 3)
                                   (begin (if (= i 1)
                                              (kill-all! 'killed-all))
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
                                             (begin (if (= i 1)
                                                        (kill-all! 'over))
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


;; We are here expecting that running 5 times inside the scheduler
;; should occur faster than 2 secs... (should take about 5*0.1 secs)
(define-test test-sleep
  (string-append "bon-matin\n"
                 "bonne-aprem\n"
                 "bonne-nuit\n")
  'dont-care
  (let ((c1 (new-corout
             'c1
             (lambda ()
               (let ((counter 0))
                 (sleep-until (lambda ()
                                (set! counter (+ counter 2))
                                (> counter 5))))
               (pretty-print 'bonne-aprem))))
        (c2 (new-corout 'c2 (lambda () (pretty-print 'bon-matin))))
        (c3 (new-corout
             'c3
             (lambda ()
               (sleep-for 1)
               (pretty-print 'bonne-nuit)))))
    (simple-corout-boot c1 c2 c3)
    'dont-care))

(define-test test-mutex "4123" 'done
  (let* ((mut (new-mutex))
         (c1 (new-corout 'c1 (lambda () (critical-section! mut
                                                           (corout-yield)
                                                           (display "1")))))
         (c2 (new-corout 'c2 (lambda () (critical-section! mut
                                                           (corout-yield)
                                                           (display "2")))))
         (c3 (new-corout 'c3 (lambda () (critical-section! mut
                                                           (corout-yield)
                                                           (display "3")))))
         (c4 (new-corout 'c4 (lambda () (display "4")))))
    (simple-corout-boot c1 c2 c3 c4)
    'done))

(define-test test-continuation "21" 'done
  (let* ((mut (new-mutex))
         (c1 (new-corout 'c1 (lambda () (display "1"))))
         (c2 (new-corout 'c2 (lambda () (display "2")
                               (corout-continuation c1)))))
    (simple-corout-boot c2)
    'done))

(define-test test-prioritized-cont "2431" 'done
  (let* ((mut (new-mutex))
         (c1 (new-corout 'c1 (lambda () (display "1"))))
         (c2 (new-corout 'c2 (lambda () (display "2")
                                     (corout-continuation c1))))
         (c3 (new-corout 'c3 (lambda () (display "3"))))
         (c4 (new-corout 'c4 (lambda () (display "4")
                                     (prioritized-continuation c3)))))
    (simple-corout-boot c2 c4)
    'done))

(define-test test-thunk-composition "123" 'ok
  (let* ((t1 (lambda () (display "1")))
         (t2 (lambda () (display "2")))
         (t3 (lambda () (display "3") 'ok))
         (c1 (new-corout
              'c1 (compose-thunks t1 t2 t3))))
    (simple-corout-boot c1)))

(define-test test-spawning "CABD" 'done
  (let* ((c1 (new-corout 'c1 (lambda () (display 'A))))
         (t2 (lambda () (display 'B) (corout-yield) 'done))
         (c3 (new-corout 'c3 (lambda () (display 'C)
                                     (corout-spawn-brother c1)
                                     (corout-spawn-brother-thunk t2)
                                     (corout-yield)
                                     (display 'D)))))
    (simple-corout-boot c3)))
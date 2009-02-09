(include "test-macro.scm")
(include "scm-lib-macro.scm")
(include "thread-simulation-macro.scm")

;; FIXME: Load calls should be removed in the final compiled version
(load "rbtree.scm")
(load "scm-lib")
(load "test")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type timer time freq paused? thread)
(define (start-timer! freq)
  (let* ((timer (make-timer 0 freq #f #f))
         (thread (make-thread
                  (lambda ()
                    (let loop ()
                      #;(pp `(time is at ,(timer-time timer)))
                      (if (not (timer-paused? timer))
                          (timer-time-set! timer (+ (timer-time timer) 1)))
                      (thread-sleep! (timer-freq timer))
                      (loop))))))
    (timer-thread-set! timer thread)
    (thread-start! thread)
    timer))

(define (stop-timer! timer)
  (thread-terminate! (timer-thread timer)))

;; Naive implementation, assuming here that the timer is *not* taking
;; alot of cpu time. Also not really thread safe, but should be ok on
;; the long run.
(define (pause-timer! timer)
  (timer-paused?-set! timer (not (timer-paused? timer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data type definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type corout id kont mailbox (state unprintable:) prioritize?
  sleeping? delta-t)

(define (mailbox-enqueue thrd msg)
  (enqueue! (corout-mailbox thrd) msg))
(define (mailbox-dequeue thrd)
  (dequeue! (corout-mailbox thrd)))

(define-type state
  current-corout q timer time-sleep-q root-k return-value-handler
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; time related sleep queue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (make-time-sleep-q )
(define make-time-sleep-q sleep-queue-create)
;; (make-time-sleep-q wake-time corout)
(define make-time-sleep-q-el sleep-queue-node-create)
(define time-sleep-q-el-wake-time sleep-queue-wake-time)
(define time-sleep-q-el-corout sleep-queue-corout)
(define time-sleep-q-empty? sleep-queue-empty?)
(define (time-sleep-q-peek? q)
  (and (not (sleep-queue-empty? q))
       (sleep-queue-leftmost q)))
(define time-sleep-q-dequeue! sleep-queue-retrieve-top!)
(define time-sleep-q-insert! sleep-queue-insert!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define unbound '!---unbound---!)
(define (unbound? v) (eq? v unbound))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduling state global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-corout       (make-parameter unbound))
(define q                    (make-parameter unbound))
(define timer                (make-parameter unbound))
(define time-sleep-q         (make-parameter unbound))
(define root-k               (make-parameter unbound))
(define return-value-handler (make-parameter unbound))
(define return-value         (make-parameter unbound))
(define parent-state         (make-parameter unbound))
(define return-to-sched      (make-parameter unbound))

(define (current-sim-time)
  (cond ((timer? (timer))
         (timer-time (timer)))
        (else (error "could not retrieve the simulation time"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State management functionnalities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Saves the current scheduler's state into an object
(define (save-state)
  (if (not (or (unbound? (current-corout))
               (unbound? (q))
               (unbound? (timer))
               (unbound? (time-sleep-q))
               (unbound? (root-k))
               (unbound? (return-value-handler))
               (unbound? (return-value))))
      
      (make-state (current-corout)
                  (q)
                  (timer)
                  (time-sleep-q)
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
        (timer                (state-timer state))
        (time-sleep-q         (state-time-sleep-q state))
        (root-k               (state-root-k state))
        (return-value-handler (state-return-value-handler state))
        (return-value         (state-return-value state))
        (return-to-sched      (state-return-to-sched state))
        (parent-state         (state-parent-state state)))

      ;; un-initializing the global simulation state
      (begin
        (current-corout       unbound)
        (q                    unbound)
        (timer                unbound)
        (time-sleep-q         unbound)
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
   ((and (corout? (current-corout))
         (not (corout-sleeping? (current-corout))))
    (corout-enqueue! (q) (current-corout)))
   ((not (corout? (current-corout)))
    (return-value
     (if (not (return-value))
         (current-corout)
         ((return-value-handler) (return-value) (current-corout)))))))

(define (wake-up-sleepers)
  (let ((now (current-sim-time)))
    (while (and (not (time-sleep-q-empty? (time-sleep-q)))
                (<= (time-sleep-q-el-wake-time
                     (time-sleep-q-peek? (time-sleep-q)))
                    now))
           (let ((corout-to-wake (time-sleep-q-el-corout
                                  (time-sleep-q-dequeue! (time-sleep-q)))))
             (corout-sleeping?-set! corout-to-wake #f)
             (corout-enqueue! (q) corout-to-wake)))))

(define (resume-coroutine)
  (continuation-capture
   (lambda (k)
     (return-to-sched k)
     (let ((kontinuation (corout-kont (current-corout))))
       ;; if it is a statefull coroutine
       ;; (executing a scheduler) then restore its
       ;; environnement
       (if (corout-state (current-corout))
           (let ((state (save-state)))
             (restore-state (corout-state (current-corout)))
             (parent-state state)))
       ;; run the coroutine
       (if (procedure? kontinuation)
           (kontinuation 'go)
           (continuation-return kontinuation 'go))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the main coroutine scheduler. Dispatches coroutine one
;; after another, manages the coroutine's return values and returns to
;; it's super scheduer (if there is one) when all the coroutines
;; finishes.
(define (corout-scheduler)

  ;; Verify for a return value
  (manage-return-value)
  
  ;; Verify if the sleeping elements can be woke up
  (wake-up-sleepers)

  ;; Get the next coroutine, if one is available
  (current-corout (dequeue! (q)))

  ;; if there is one coroutine, run it, else stop the coroutine
  ;; scheduler.
  (cond
   ((corout? (current-corout))

    (resume-coroutine)

    ;; loop the scheduler, if the coroutine finished
    (corout-scheduler))

   ;; If only sleeping coroutines are left, sleep for a while
   ((not (time-sleep-q-empty? (time-sleep-q)))
    (begin
      (let* ((next-wake-time
              (time-sleep-q-el-wake-time (time-sleep-q-peek? (time-sleep-q)))))
        (thread-sleep! (- next-wake-time (current-sim-time))))
      (corout-scheduler)))
      
   ;; finish up the scheduling process by restoring the super
   ;; environnement and end this scheduler
   (else
    (let ((finish-scheduling (root-k))
          (ret-val (return-value)))
      (restore-state (parent-state))
      (continuation-return finish-scheduling ret-val)))))


;; Simple abstraction that just resumes the scheduler's calculation
(define (resume-scheduling)
  (continuation-return (return-to-sched) 'back-in-sched))

;; Semaphore's internals 
(define (sem-increase! sem) (sem-value-set! sem (+ (sem-value sem) 1)))
(define (sem-decrease! sem) (sem-value-set! sem (- (sem-value sem) 1)))


(define (internal-yield)
  (continuation-capture
   (lambda (k)
     (corout-kont-set! (current-corout) k)
     (resume-scheduling))))


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
               (new-queue) #f #f
               #f ; not sleeping
               #f))


;;; Mailbox system

;; Querry the current-coroutine's mailbox to see if its empty
(define (empty-mailbox?)
  (empty-queue? (corout-mailbox (current-corout))))

(define (?)
  (define mailbox (corout-mailbox (current-corout)))
  (while (empty-queue? mailbox)
         (continuation-capture
          (lambda (k)
            (let ((corout (current-corout)))
              (corout-kont-set! corout k)
              (corout-sleeping?-set! corout #t)
              (resume-scheduling)))))
  (dequeue! mailbox))


;; Send a message to the givent destination coroutine object.
(define (! dest-corout msg)
  (enqueue! (corout-mailbox dest-corout) msg)
  (if (corout-sleeping? dest-corout)
      (begin
        (corout-sleeping?-set! dest-corout #f)
        (corout-enqueue! (q) dest-corout))))

(define (?? pred)
  (define mailbox (corout-mailbox (current-corout)))
  (let loop ()
    (cond ((queue-find-and-remove! pred mailbox)
           => (lambda (val) val))
          (else
           (continuation-capture
            (lambda (k)
              (let ((corout (current-corout)))
                (corout-kont-set! corout k)
                (corout-sleeping?-set! corout #t)
                (resume-scheduling))))
           (loop)))))


;;; Thread control

;; Yields the current coroutine's calculation. The calculation will
;; resume where it left once the scheduler decides to resume it.
(define (yield)
  (internal-yield))

;; This will yield the work of the scheduler itselft, assuming that
;; the scheduler runs inside a coroutine too, i.e. that we are in a
;; child recursive coroutine.
(define (super-yield)
  (continuation-capture
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
  ;; first do all the requester actions on exit
  (current-corout exit-val)
  (resume-scheduling))

;; Starts the scheduling of the passed coroutines. This call will
;; return when all the coroutine will have terminated. It will use the
;; default return value handler, thus will return the value returned
;; by the last coroutine to finish.
(define (simple-boot c1 . cs)
  (define default-freq 0.001)
  (let* ((used-timer (start-timer! default-freq))
         (result     (apply boot used-timer (lambda (acc val) val) c1 cs)))
    (stop-timer! used-timer)
    result))

;; Starts the scheduling of the givent coroutines with a specific
;; return value handler. The return value handler must have the
;; following format: (lambda (acc val) ...) where acc is the
;; accumulated return value and val is the last returned value by a
;; coroutine. The accumulated value will be return when the scheduling
;; process finishes.
(define (boot used-timer return-handler c1 . cs)
  (continuation-capture
   (lambda (k)
     (begin
       (let ((fresh-start? (unbound? (current-corout))))
         (if (not fresh-start?) (parent-state (save-state)))
         (root-k               k)
         (current-corout       #f)
         (q                    (new-queue))
         (timer                used-timer)
         (time-sleep-q         (make-time-sleep-q))
         (return-value-handler return-handler)
         (return-value         #f)
         (if fresh-start? (parent-state #f)))
       (for-each (lambda (c) (corout-enqueue! (q) c))
                 (cons c1 cs))
       (corout-scheduler)))))


;; Kills all the currently executing coroutines. This will operate on
;; only 1 level of scheduling, killing thus the currently executing
;; scheduler.
(define (kill-all! exit-val)
  ;; first do all the requester actions on exit
  (let ((finish-scheduling (root-k))
        #; (ret-val (return-value)))
    (restore-state (parent-state))
    (continuation-return finish-scheduling exit-val)))

;; Kills all schedulers in the currently running scheduler tree and
;; return exit-val as the exit value of the primordial scheduler.
(define (super-kill-all! exit-val)
  ;; first do all the requester actions on exit
  (while (parent-state)
    (restore-state (parent-state)))
  (kill-all! exit-val))



;; Will spawn the brother coroutine into the scheduler of the
;; currently executing coroutine
(define (spawn-brother corout)
  (enqueue! (q) corout))

;; Spawns an anonymous brother coroutine.
(define (spawn-brother-thunk id thunk)
  (enqueue! (q) (new-corout id thunk)))


;; Will put the current-corout to sleep for about "secs" seconds
(define (sleep-for secs)
  (if (>= secs 0)
      (continuation-capture
       (lambda (k)
         (let ((corout (current-corout))
               (wake-time (+ (current-sim-time) secs)))
           (corout-kont-set! corout k)
           (time-sleep-q-insert! (time-sleep-q)
                                 (make-time-sleep-q-el wake-time corout))
           (corout-sleeping?-set! corout #t)
           #; (pp `(now is ,(current-sim-time) sleeping until ,wake-time))
           (resume-scheduling))))
      (yield)))


;; Create a new semaphore or mutex object
(define (new-semaphore init-value) (make-sem init-value (new-queue)))
(define (new-mutex) (new-semaphore 1))

;; Return #t if the semaphore is locked or #f if not
(define (sem-locked? sem) (< (sem-value sem) 1))

;; Usual lock and unlock (P/V, take/release, etc...) implementation
;; where the executed event will be put into a sleep queue if it tries
;; to take an unavailable semaphore, and the first enqueued (fifo)
;; event will be resumed when an unlock is performed.
(define (sem-lock! sem)
  (while (sem-locked? sem)
         #; (pretty-print `(sem-locked putting ,(corout-id (current-corout)) to sleep))
         (continuation-capture
          (lambda (k)
            (let ((corout (current-corout)))
              (corout-kont-set! corout k)
              (enqueue! (sem-wait-queue sem) corout)
              (corout-sleeping?-set! corout #t)
              (resume-scheduling)))))
  #; (pretty-print `(,(corout-id (current-corout)) took the lock!))
  ;; should be unqueued by the unlock call...
  (sem-decrease! sem)
  #; (pretty-print `(mutex-locked val: ,(sem-value sem))))

(define (sem-unlock! sem)
  #;(pretty-print `(locked released by ,(corout-id (current-corout))))
  (if (not (empty-queue? (sem-wait-queue sem)))
      (let ((corout-to-wake (dequeue! (sem-wait-queue sem))))
        #;(pretty-print `(waking up ,(corout-id corout-to-wake)))
        (corout-sleeping?-set! corout-to-wake #f)
        (corout-enqueue! (q) corout-to-wake)))
  (sem-increase! sem)
  #; (pretty-print `(mutex-unlocked val: ,(sem-value sem))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coroutine tracing system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define trace-coroutines? #t)
(define trace-corout-table (make-table test: eq?))

(define (output-corout-tracing-results)
  ;; Print in milliseconds the time used by each threads
  (with-output-to-file "histo-threads.csv"
    (lambda () (generate-histogram
                "corout-threads" 30
                (##table-foldl rcons
                               '()
                               (lambda (k v)
                                 (cons k (map (lambda (x) (* x 1000)) v)))
                               trace-corout-table)
                0.01)))
  #;
  (with-output-to-file "thread-tracing.html"
    (display "<html>\n")
    (display
     "\t<head><title>Thread simulation tracing results</title></head>\n")
    (display "\t<body>\n")
    (##table-for-each
                (lambda (id values)
                  (pp `(,id avg: ,(average values)
                            stdev: ,(standard-deviation values))))
                trace-corout-table)
    (display "\t</body>\n")
    (display "</html>\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test simple-test "A1B2" 3
  (let ((c1 (new-corout 'c1 (lambda ()
                              (display 'A)
                              (yield)
                              (display 'B)
                              (terminate-corout 'C))))
        (c2 (new-corout 'c2 (lambda ()
                              (display 1)
                              (yield)
                              (display 2)
                              (terminate-corout 3)))))
    (simple-boot c1 c2)))

(define-test test-kill-all "12" 'killed-all
  (let ((c1 (new-corout 'c1 (lambda ()
                              (for i 0 (< i 3)
                                   (begin (if (= i 1)
                                              (kill-all! 'killed-all))
                                          (display 1)
                                          (yield))))))
        (c2 (new-corout 'c2 (lambda ()
                              (for i 0 (< i 3) (begin (display 2)
                                                      (yield)))))))
    (simple-boot c1 c2)))

(define-test test-ret-val-handler "A1aB2" 6
  (let ((c1 (new-corout 'c1 (lambda () (begin (display 'A)
                                              (yield)
                                              (display 'B)
                                              1))))
        (c2 (new-corout 'c2 (lambda () (begin (display 1)
                                              (yield)
                                              (display 2)
                                              2))))
        ;; here terminate-corout is used to shortcircuit the flow of
        ;; the thunk and finish earlier this thread.
        (c3 (new-corout 'c3 (lambda () (begin (display #\a)
                                              (terminate-corout 3)
                                              (yield)
                                              (display #\b)))))
        (timer (start-timer! 0.001)))
    (let ((result (boot timer (lambda (acc v) (+ acc v)) c1 c2 c3)))
      (stop-timer! timer)
      result)))


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
    (simple-boot c1 c2 c3)))


(define-test test-recursive-sim "A12BA12BA12B" 'meta-12-done!
  (let* ((sA (new-corout 'sA (lambda () (for i 0 (< i 3)
                                             (begin (display 'A)
                                                    (super-yield)
                                                    (yield))))))
         (sB (new-corout 'sB (lambda () (for i 0 (< i 3)
                                             (begin (display 'B)
                                                    (yield)))
                                     (terminate-corout 'sB))))
         (s1 (new-corout 's1 (lambda () (for i 0 (< i 3)
                                             (begin (display 1)
                                                    (yield))))))
         (s2 (new-corout 's2 (lambda () (for i 0 (< i 3)
                                             (begin (display 2)
                                                    (super-yield)
                                                    (yield))))))
         (meta-AB
          (new-corout 'meta-AB
                      (lambda ()
                        (simple-boot sA sB)
                        'meta-AB-done!)))
         (meta-12
          (new-corout 'meta-12
                      (lambda ()
                        (simple-boot s1 s2)
                        'meta-12-done!))))
    (simple-boot meta-AB meta-12)))

(define-test test-kill-all-rec "A1B2ABAB" 'meta-AB-done!
  (let* ((sA (new-corout 'sA (lambda () (for i 0 (< i 3)
                                             (begin (display 'A)
                                                    (super-yield)
                                                    (yield))))))
         (sB (new-corout 'sB (lambda () (for i 0 (< i 3)
                                             (begin (display 'B)
                                                    (super-yield)
                                                    (yield))))))
         (s1 (new-corout 's1 (lambda () (for i 0 (< i 3)
                                             (begin (if (= i 1)
                                                        (kill-all! 'over))
                                                    (display 1)
                                                    (super-yield)
                                                    (yield))))))
         (s2 (new-corout 's2 (lambda () (for i 0 (< i 3)
                                             (begin (display 2)
                                                    (super-yield)
                                                    (yield))))))
         (meta-AB
          (new-corout 'meta-AB
                      (lambda ()
                        (simple-boot sA sB)
                        'meta-AB-done!)))
         (meta-12
          (new-corout 'meta-12
                      (lambda ()
                        (simple-boot s1 s2)
                        'meta-12-done!))))
    (simple-boot meta-AB meta-12)))


;; We are here expecting that running 5 times inside the scheduler
;; should occur faster than 2 secs... (should take about 5*0.1 secs)
(define-test test-sleep
  (string-append "bon-matin\n"
                 "bonne-aprem\n"
                 "bonne-nuit\n")
  'dont-care
  (let ((c1 (new-corout 'c1 (lambda ()
                              (sleep-for 0.2)
                              (pretty-print 'bonne-nuit))))
        (c2 (new-corout 'c2 (lambda () (pretty-print 'bon-matin))))
        (c3 (new-corout 'c3 (lambda ()
                              (sleep-for 0.1)
                              (pretty-print 'bonne-aprem)))))
    (simple-boot c1 c2 c3)
    'dont-care))

(define-test test-mutex "4123" 'done
  (let* ((mut (new-mutex))
         (c1 (new-corout 'c1 (lambda () (critical-section! mut
                                                           (yield)
                                                           (display "1")))))
         (c2 (new-corout 'c2 (lambda () (critical-section! mut
                                                           (yield)
                                                           (display "2")))))
         (c3 (new-corout 'c3 (lambda () (critical-section! mut
                                                           (yield)
                                                           (display "3")))))
         (c4 (new-corout 'c4 (lambda () (display "4")))))
    (simple-boot c1 c2 c3 c4)
    'done))

(define-test test-continuation "210" 'done
  (let* ((c1 (new-corout 'c1 (lambda () (display "1")
                                     (continue-with-thunk!
                                      (lambda () (display "0") 'done)))))
         (c2 (new-corout 'c2 (lambda () (display "2")
                                     (continue-with c1)))))
    (simple-boot c2)))

(define-test test-prioritized-cont "2431" 'done
  (let* ((c1 (new-corout 'c1 (lambda () (display "1"))))
         (c2 (new-corout 'c2 (lambda () (display "2")
                                     (continue-with c1))))
         (c3 (new-corout 'c3 (lambda () (display "3"))))
         (c4 (new-corout 'c4 (lambda () (display "4")
                                     (prioritized-continuation c3)))))
    (simple-boot c2 c4)
    'done))

(define-test test-thunk-composition "123" 'ok
  (let* ((t1 (lambda () (display "1")))
         (t2 (lambda () (display "2")))
         (t3 (lambda () (display "3") 'ok))
         (c1 (new-corout
              'c1 (compose-thunks t1 t2 t3))))
    (simple-boot c1)))

(define-test test-spawning "CABD" 'done
  (let* ((c1 (new-corout 'c1 (lambda () (display 'A))))
         (t2 (lambda () (display 'B) (yield) 'done))
         (c3 (new-corout 'c3 (lambda () (display 'C)
                                     (spawn-brother c1)
                                     (spawn-brother-thunk 'c2 t2)
                                     (yield)
                                     (display 'D)))))
    (simple-boot c3)))

(define-test test-extended-mailboxes
  (string-append "c2-sent-data\n"
                 "c3-sleeps\n"
                 "c3-sent-data\n"
                 "3\n")
  2
  (let* ((c1 (new-corout 'c1 (lambda ()
                               (pretty-print (?? odd?))
                               (?))))
         (c2 (new-corout 'c2 (lambda ()
                               (! c1 2)
                               (pretty-print 'c2-sent-data))))
         (c3 (new-corout 'c3 (lambda ()
                               (pretty-print 'c3-sleeps)
                               (sleep-for 0.2)
                               (! c1 3)
                               (pretty-print 'c3-sent-data)))))
    (simple-boot c1 c2 c3)))
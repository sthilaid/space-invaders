
;; FIXME: Load calls should be removed in the final compiled version

;; (include "class.scm")
;; (load "rbtree.scm")
;; (load "scm-lib")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class timer ()
  (slot: time)
  (slot: freq)
  (slot: paused?)
  (slot: thread)
  (slot: time-multiplier))

;; the timer uses an integer time value so that it will enable the use
;; of bignums for precise long simulations. That's why the freq
;; multiplier is applied to each current-sim-time calls. The
;; time-multiplier is applied as a multiplier of the current
;; simulation time. Thus a time-multiplier of 2 will have a simulation
;; run 2 times faster.
(define (start-timer! freq #!key (time-multiplier 1))
  (let* ((timer (make-timer 0 freq #f #f time-multiplier))
         (thread (make-thread
                  (lambda ()
                    (let loop ()
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

(define-class corout ()
  (slot: id)
  (slot: kont)
  (slot: mailbox)
  (slot: state-env) ;; should be unprintable
  (slot: prioritize?)
  (slot: sleeping?)
  (slot: delta-t)
  (slot: msg-lists)
  (constructor: (lambda (obj id thunk)
                  (set-fields! obj corout
                    ((id          id)
                     (kont        (lambda (dummy) (terminate-corout (thunk))))
                     (mailbox     (new-queue))
                     (state-env       #f)
                     (prioritize? #f)
                     (sleeping?   #f)
                     (delta-t     #f)
                     (msg-lists   (empty-set)))))))

(define (sleeping-on-mutex)    
  'sleeping-on-mutex)
(define (sleeping-on-msg)    
  'sleeping-on-msg)
(define (sleeping-over-time node interruptible?)
  (cons node interruptible?))
(define (sleeping-on-msg? c)
  (eq? (corout-sleeping? c) 'sleeping-on-msg))
(define (sleeping-over-time? c)
  (let ((sleeping? (corout-sleeping? c)))
    (and (pair? sleeping?)
         (time-sleep-q-node? (car sleeping?)))))
(define (interruptible? c)
  (let ((sleeping? (corout-sleeping? c)))
    (and (pair? sleeping?) (cdr sleeping?))))
(define (sleeping-over-time?->node c)
  (let ((sleeping? (corout-sleeping? c)))
    (and (pair? sleeping?) (car sleeping?))))
(define (sleeping-on-mutex? c)    
  (eq? (corout-sleeping? c) 'sleeping-on-mutex))



(define (mailbox-enqueue thrd msg)
  (enqueue! (corout-mailbox thrd) msg))
(define (mailbox-dequeue thrd)
  (dequeue! (corout-mailbox thrd)))

(define-class state ()
  (slot: current-corout)
  (slot: q)
  (slot: timer)
  (slot: time-sleep-q)
  (slot: root-k)
  (slot: return-value-handler)
  (slot: return-value)
  (slot: return-to-sched)
  (slot: parent-state) ;unprintable:
  (slot: dynamic-handlers)) 


(define-class sem () (slot: value) (slot: wait-queue))

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
(define time-sleep-q-remove! sleep-queue-remove!)
(define time-sleep-q-node? sleep-queue-node?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define unbound '!---unbound---!)
(define (unbound? v) (eq? v unbound))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduling state global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-corout       (make-parameter unbound))
(define self                 current-corout) ; variable alias
(define q                    (make-parameter unbound))
(define timer                (make-parameter unbound))
(define time-sleep-q         (make-parameter unbound))
(define root-k               (make-parameter unbound))
(define return-value-handler (make-parameter unbound))
(define return-value         (make-parameter unbound))
(define parent-state         (make-parameter unbound))
(define return-to-sched      (make-parameter unbound))
(define dynamic-handlers     (make-parameter unbound))

(define (current-sim-time)
  (let ((t (timer)))
   (cond ((timer? t)
          (* (timer-time t) (timer-freq t) (timer-time-multiplier t)))
         (else (error "could not retrieve the simulation time")))))

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
               (unbound? (return-value))
               (unbound? (dynamic-handlers))))
      
      (make-state (current-corout)
                  (q)
                  (timer)
                  (time-sleep-q)
                  (root-k)
                  (return-value-handler)
                  (return-value)
                  (return-to-sched)
                  (parent-state)
                  (dynamic-handlers))
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
        (parent-state         (state-parent-state state))
        (dynamic-handlers     (state-dynamic-handlers state)))

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
        (parent-state         unbound)
        (dynamic-handlers     unbound))))


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
       (if (corout-state-env (current-corout))
           (let ((state (save-state)))
             (restore-state (corout-state-env (current-corout)))
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
        ;; sleeping for a scaled period of time
        (thread-sleep! (/ (- next-wake-time (current-sim-time))
                          (timer-time-multiplier (timer)))))
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
  (init-corout! (make-corout #f #f #f #f #f #f #f) id thunk))


;;; Mailbox system

(define mailbox-timeout-exception 'mailbox-timeout-exception)

;; Querry the current-coroutine's mailbox to see if its empty
(define (empty-mailbox?)
  (empty-queue? (corout-mailbox (current-corout))))

;; Send a message to the givent destination coroutine object.
(define (! dest-corout msg)
  (enqueue! (corout-mailbox dest-corout) msg)
  (cond ((sleeping-on-msg? dest-corout)
         (corout-sleeping?-set! dest-corout #f)
         (corout-enqueue! (q) dest-corout))
        ((and (sleeping-over-time? dest-corout)
              (interruptible? dest-corout))
         (time-sleep-q-remove! (sleeping-over-time?->node dest-corout))
         (corout-sleeping?-set! dest-corout #f)
         (corout-enqueue! (q) dest-corout))))

(define (? #!key (timeout 'infinity))
  (define mailbox (corout-mailbox (current-corout)))
  (if (empty-queue? mailbox)
      (if (number? timeout)
          (sleep-for timeout interruptible?: #t)
          (continuation-capture
           (lambda (k)
             (let ((corout (current-corout)))
               (corout-kont-set! corout k)
               (corout-sleeping?-set! corout (sleeping-on-msg))
               (resume-scheduling))))))
  (if (empty-queue? mailbox)
      (raise mailbox-timeout-exception)
      (dequeue! mailbox)))

(define (?? pred #!key (timeout 'infinity))
  (define mailbox (corout-mailbox (current-corout)))
  (let loop ()
    (cond ((queue-find-and-remove! pred mailbox)
           => (lambda (val) val))
          (else
           (if (number? timeout)
               (let ((msg-q-size (queue-size mailbox)))
                 (sleep-for timeout interruptible?: #t)
                 ;; might be awoken either from a (!) or timeout
                 (if (= (queue-size mailbox) msg-q-size)
                     (raise mailbox-timeout-exception)
                     (loop)))
               (begin (continuation-capture
                       (lambda (k)
                         (let ((corout (current-corout)))
                           (corout-kont-set! corout k)
                           (corout-sleeping?-set! corout (sleeping-on-msg))
                           (resume-scheduling))))
                      (loop)))))))



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
           (corout-state-env-set! (current-corout) state)
           (corout-kont-set! (current-corout) k)
           (resume-scheduling))))))

;; Terminates early the calculation of the current coroutine and
;; returns with the givent ret-val.
(define (terminate-corout exit-val)
  (for-each (lambda (msg-list) (unsubscribe msg-list (self)))
            (corout-msg-lists (self)))
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
         (dynamic-handlers     '())
         (if fresh-start? (parent-state #f)))
       (for-each (lambda (c) (corout-enqueue! (q) c))
                 (cons c1 cs))
       (corout-scheduler)))))


;; Kills all the currently executing coroutines. This will operate on
;; only 1 level of scheduling, killing thus the currently executing
;; scheduler.
(define (kill-all! exit-val)
  ;; first do all the requester actions on exit
  (let ((finish-scheduling (root-k)))
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
  (enqueue! (q) corout)
  corout)

;; Spawns an anonymous brother coroutine.
(define (spawn-brother-thunk id thunk)
  (let ((c (new corout id thunk)))
    (enqueue! (q) (new corout id thunk))
    c))


;; Will put the current-corout to sleep for about "secs" seconds
(define (sleep-for secs #!key (interruptible? #f))
  (if (>= secs 0)
      (continuation-capture
       (lambda (k)
         (let* ((corout (current-corout))
                (wake-time (+ (current-sim-time) secs))
                (sleep-queue-node (make-time-sleep-q-el wake-time corout)))
           (corout-kont-set! corout k)
           (time-sleep-q-insert! (time-sleep-q) sleep-queue-node)
           ;; Here setting the node inside the sleeping? slot (for
           ;; possible removal by msging system)...
           ;; FIXME: here, this will make possible to wakeup a
           ;; sleeping thread by sending it a msg...
           (corout-sleeping?-set! corout
                                  (sleeping-over-time sleep-queue-node
                                                      interruptible?))
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
         (continuation-capture
          (lambda (k)
            (let ((corout (current-corout)))
              (corout-kont-set! corout k)
              (enqueue! (sem-wait-queue sem) corout)
              (corout-sleeping?-set! corout (sleeping-on-mutex))
              (resume-scheduling)))))
  ;; should be unqueued by the unlock call...
  (sem-decrease! sem))

(define (sem-unlock! sem)
  (if (not (empty-queue? (sem-wait-queue sem)))
      (let ((corout-to-wake (dequeue! (sem-wait-queue sem))))
        (corout-sleeping?-set! corout-to-wake #f)
        (corout-enqueue! (q) corout-to-wake)))
  (sem-increase! sem))



;;; Messaging lists

(define messaging-lists (make-table test: equal?))
(define (get-msg-list list-id)
  (table-ref messaging-lists list-id #f))
(define (subscribe list-id agent)
  (cond ((table-ref messaging-lists list-id #f)
         => (lambda (lst)
              (table-set! messaging-lists list-id (set-add eq? agent lst))))
        (else (table-set! messaging-lists list-id (make-set agent))))
  (update! agent corout msg-lists
           (lambda (lists) (set-add eq? list-id lists))))
(define (unsubscribe list-id agent)
  (let ((msg-list (get-msg-list list-id)))
    (and msg-list
         (update! agent corout msg-lists
                  (lambda (lists) (set-remove eq? list-id lists)))
         (table-set! messaging-lists list-id
                     (set-remove eq? agent msg-list)))))
(define (broadcast list-id msg)
  (let ((msg-list (get-msg-list list-id)))
    (and msg-list
         (for-each (lambda (subscriber)
                     (! subscriber msg))
                   msg-list))))


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


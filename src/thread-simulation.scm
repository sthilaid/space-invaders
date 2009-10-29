
;; FIXME: Load calls should be removed in the final compiled version

(include "scm-lib_.scm")
;;(include "thread-simulation_.scm")
;; (load "rbtree.scm")
;; (load "scm-lib")

;; taken from the object system since its compatible and usefull :)
(define-macro (update! obj class field f)
  (define (symbol-append s1 . ss)
    (string->symbol (apply string-append
                           (symbol->string s1)
                           (map symbol->string ss))))
  (define (gen-accessor-name class-name var)
    (symbol-append class-name '- var))
  (define (gen-setter-name class-name var)
    (symbol-append class-name '- var '-set!))
  (let ((objval (gensym 'objval)))
   `(let ((,objval ,obj))
      (,(gen-setter-name class field) ,objval
       (,f (,(gen-accessor-name class field) ,objval))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type timer time freq paused? thread time-multiplier)

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
                      (thread-sleep! (timer-freq timer))
                      (if (not (timer-paused? timer))
                          (update! timer timer time (flip + 1)))
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
;;; Internal definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data type definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type corout id kont mailbox state-env
                    sleeping? delta-t msg-lists
                    result)

(define corout-unbound-result (gensym 'corout-unbound-result))
(define (new-corout id thunk)
  (let ((kont (lambda (dummy) (terminate-corout (thunk))))
        (mailbox (new-queue))
        (state-env #f)
        (sleeping? #f)
        (delta-t #f)
        (msg-lists (empty-set))
        (result corout-unbound-result))
   (make-corout id kont mailbox state-env 
                sleeping? delta-t msg-lists
                corout-unbound-result)))

(define (corout-get-result c)
  (if (eq? (corout-result c) corout-unbound-result)
      (raise 'coroutine-not-terminated-exception)
      (corout-result c)))

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

(define (corout-set-sleeping-mode! corout mode)
  (begin
    (corout-sleeping?-set! corout mode)
    (if mode
        (sleeping-coroutines (+ (sleeping-coroutines) 1))
        (sleeping-coroutines (- (sleeping-coroutines) 1)))))

(define (mailbox-enqueue thrd msg)
  (enqueue! (corout-mailbox thrd) msg))
(define (mailbox-dequeue thrd)
  (dequeue! (corout-mailbox thrd)))

(define-type state current-corout q timer time-sleep-q root-k
                   parent-state dynamic-handlers sleeping-coroutines)

(define (make-fresh-state)
  (make-state unbound unbound unbound unbound unbound
              unbound unbound unbound))
(define-type sem value wait-queue)

;; Must be used to enqueue a coroutine in the coroutine active
;; queue. 
(define (corout-enqueue! q corout)
  (enqueue! q corout))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; time related sleep queue
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
;;; Internal symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define unbound '!---unbound---!)
(define (unbound? v) (eq? v unbound))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheduling state global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ___internal-state___ unbound)

(define (current-corout . new-val)
  (if (pair? new-val)
      (state-current-corout-set! ___internal-state___ (car new-val))
      (state-current-corout ___internal-state___)))

(define self current-corout) ;alias

(define (q . new-val)
  (if (pair? new-val)
      (state-q-set! ___internal-state___ (car new-val))
      (state-q ___internal-state___)))

(define (timer . new-val)
  (if (pair? new-val)
      (state-timer-set! ___internal-state___ (car new-val))
      (state-timer ___internal-state___)))

(define (time-sleep-q . new-val)
  (if (pair? new-val)
      (state-time-sleep-q-set! ___internal-state___ (car new-val))
      (state-time-sleep-q ___internal-state___)))

(define (root-k . new-val)
  (if (pair? new-val)
      (state-root-k-set! ___internal-state___ (car new-val))
      (state-root-k ___internal-state___)))

(define (parent-state . new-val)
  (if (pair? new-val)
      (state-parent-state-set! ___internal-state___ (car new-val))
      (state-parent-state ___internal-state___)))

(define (dynamic-handlers . new-val)
  (if (pair? new-val)
      (state-dynamic-handlers-set! ___internal-state___ (car new-val))
      (state-dynamic-handlers ___internal-state___)))

(define (sleeping-coroutines . new-val)
  (if (pair? new-val)
      (state-sleeping-coroutines-set! ___internal-state___ (car new-val))
      (state-sleeping-coroutines ___internal-state___)))

(define (current-sim-time)
  (let ((t (timer)))
   (cond ((timer? t)
          (* (timer-time t) (timer-freq t) (timer-time-multiplier t)))
         (else (error "could not retrieve the simulation time")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State management functionnalities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Saves the current scheduler's state into an object
(define (save-state)
  (if (not (or (unbound? ___internal-state___)
               (unbound? (current-corout))
               (unbound? (q))
               (unbound? (timer))
               (unbound? (time-sleep-q))
               (unbound? (root-k))
               (unbound? (dynamic-handlers))
               (unbound? (sleeping-coroutines))))
      
      (make-state (current-corout)
                  (q)
                  (timer)
                  (time-sleep-q)
                  (root-k)
                  (parent-state)
                  (dynamic-handlers)
                  (sleeping-coroutines))
      #f))

;; Restores the givent state object into the environment
(define (restore-state state)
  (if state
      (set! ___internal-state___ state)
      (begin
        ;; un-initializing the global simulation state
        (set! ___internal-state___ unbound))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheduler's internals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If the current-corout is not a corout, it means that the last
;; coroutine finished, and the value of current-corout is the return
;; value of that coroutine. Exception, when current-corout is
;; unbound, this happens when the q is empty and no coroutine in the
;; sleep-q can be awaken yet.
(define ___scheduler-is-speeping___ (gensym))
(define ___coroutine-was-paused___ (gensym))
(define (manage-return-value)
  (let ((current-corout-val (current-corout)))
    (cond
     ((and (corout? current-corout-val)
           (not (corout-sleeping? current-corout-val)))
      (corout-enqueue! (q) current-corout-val)))))

(define (wake-up-sleepers)
  (let ((now (current-sim-time)))
    (while (and (not (time-sleep-q-empty? (time-sleep-q)))
                (<= (time-sleep-q-el-wake-time
                     (time-sleep-q-peek? (time-sleep-q)))
                    now))
           (let ((corout-to-wake (time-sleep-q-el-corout
                                  (time-sleep-q-dequeue! (time-sleep-q)))))
             (corout-set-sleeping-mode! corout-to-wake #f)
             (corout-enqueue! (q) corout-to-wake)))))

(define (resume-coroutine)
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
        (continuation-return kontinuation 'go))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheduler
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

  (cond

   ;; if there is one coroutine, run it
   ((corout? (current-corout)) (resume-coroutine))

   ;; If only sleeping coroutines are left, sleep for a while
   ((not (time-sleep-q-empty? (time-sleep-q)))
    (begin
      (let* ((next-wake-time
              (time-sleep-q-el-wake-time (time-sleep-q-peek? (time-sleep-q)))))
        ;; sleeping for a scaled period of time
        (thread-sleep! (/ (- next-wake-time (current-sim-time))
                          (timer-time-multiplier (timer)))))
      ;; Make sure that result value manager knows that the
      ;; current-corout is not a return value...
      (current-corout ___scheduler-is-speeping___)
      ;; continue scheduling
      (corout-scheduler)))
      
   ;; finish up the scheduling process by restoring the super
   ;; environnement and end this scheduler
   (else
    (let ((finish-scheduling (root-k))
          (ret-val (void)))
      (if (> (sleeping-coroutines) 0)
          (error "Deadlock detected in coroutine system..."))
      (restore-state (parent-state))
      (continuation-return finish-scheduling ret-val)))))


;; Semaphore's internals 
(define (sem-increase! sem) (sem-value-set! sem (+ (sem-value sem) 1)))
(define (sem-decrease! sem) (sem-value-set! sem (- (sem-value sem) 1)))


(define (internal-yield)
  (continuation-capture
   (lambda (k)
     (corout-kont-set! (current-corout) k)
     (corout-scheduler))))

;; Pause takes the current coroutine out of the running queue. Thus
;; the coroutine will have to be re-inserted back with, for example,
;; spawn-brother.
(define (corout-pause)
  (continuation-capture
   (lambda (k)
     (corout-kont-set! (current-corout) k)
     (current-corout ___coroutine-was-paused___)
     (corout-scheduler))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Mailbox system

(define mailbox-timeout-exception 'mailbox-timeout-exception)

;; Querry the current-coroutine's mailbox to see if its empty
(define (empty-mailbox?)
  (empty-queue? (corout-mailbox (current-corout))))

(define (trash-mailbox!)
  (corout-mailbox-set! (current-corout) (new-queue)))

;; Send a message to the givent destination coroutine object.
(define (! dest-corout msg)
  (enqueue! (corout-mailbox dest-corout) msg)
  (cond ((sleeping-on-msg? dest-corout)
         (corout-set-sleeping-mode! dest-corout #f)
         (corout-enqueue! (q) dest-corout))
        ((and (sleeping-over-time? dest-corout)
              (interruptible? dest-corout))
         (time-sleep-q-remove! (sleeping-over-time?->node dest-corout))
         (corout-set-sleeping-mode! dest-corout #f)
         (corout-enqueue! (q) dest-corout))))

(define undefined-timeout-val (gensym))

(define (? #!key (timeout 'infinity) (timeout-val undefined-timeout-val))
  (let ((mailbox (corout-mailbox (current-corout))))
    (if (empty-queue? mailbox)
        (if (not (eq? timeout 'infinity))
            (sleep-for timeout interruptible?: #t)
            (continuation-capture
             (lambda (k)
               (let ((corout (current-corout)))
                 (corout-kont-set! corout k)
                 (corout-set-sleeping-mode! corout (sleeping-on-msg))
                 (corout-scheduler))))))
    (if (empty-queue? mailbox)
        (if (eq? timeout-val undefined-timeout-val)
            (raise mailbox-timeout-exception)
            timeout-val)
        (dequeue! mailbox))))

(define (?? pred #!key (timeout 'infinity) (timeout-val undefined-timeout-val))
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
                     (if (eq? timeout-val undefined-timeout-val)
                         (raise mailbox-timeout-exception)
                         timeout-val)
                     (loop)))
               (begin (continuation-capture
                       (lambda (k)
                         (let ((corout (current-corout)))
                           (corout-kont-set! corout k)
                           (corout-set-sleeping-mode! corout (sleeping-on-msg))
                           (corout-scheduler))))
                      (loop)))))))



;;; Thread control

;; Yields the current coroutine's calculation. The calculation will
;; resume where it left once the scheduler decides to resume it.
(define (yield)
  (internal-yield))

;; Warning: resolution for the timeout can't be smaller than 0.01 sec..
(define (yield-to corout)
  (if (not (corout-sleeping? corout))
      (begin
        (queue-find-and-remove! (lambda (x) (eq? x corout)) (q))
        (queue-push! (q) corout) ; corout is put on top of the ready queue
        (yield))))

;; This will yield the work of the scheduler itselft, assuming that
;; the scheduler runs inside a coroutine too, i.e. that we are in a
;; child recursive coroutine.
(define (super-yield)
  (continuation-capture
   (lambda (k)
     (let ((parent (parent-state)))
       (if parent
          (let ((state (save-state)))
            (restore-state parent)
            ;; current-corout should be restored on the parent's
            ;; level. It just thus be defined as the new coroutine
            ;; system's coroutine.
            (let ((current-c (current-corout)))
              (corout-state-env-set! current-c state)
              (corout-kont-set! current-c k))
            (corout-scheduler)))))))

;; Terminates early the calculation of the current coroutine and
;; returns with the givent ret-val.
(define (terminate-corout exit-val)
  (for-each (lambda (msg-list) (unsubscribe msg-list (self)))
            (corout-msg-lists (self)))
  (corout-result-set! (self) exit-val)
  (current-corout exit-val)
  (corout-scheduler))

;; Starts the scheduling of the givent coroutines with a specific
;; return value handler.  The used timer optionnal parameter is only
;; significant for the primordial system. In recursive cases the
;; primodordial timer will be shared to children systems.
(define (boot coroutines
              #!optional (used-timer (start-timer! 0.001)))
  (let ((result
         (continuation-capture
          (lambda (k)
            (begin
              (let ((fresh-start? (unbound? ___internal-state___)))
                (if fresh-start?
                    (begin
                      (set! ___internal-state___ (make-fresh-state))
                      (parent-state #f)
                      (timer used-timer))
                    (begin
                      (parent-state (save-state))
                      (timer (timer))))
                (root-k               k)
                (current-corout       #f)
                (q                    (new-queue))
                (time-sleep-q         (make-time-sleep-q))
                (dynamic-handlers     '())
                (sleeping-coroutines  0))
              (for-each (curry2 corout-enqueue! (q)) coroutines)
              (corout-scheduler))))))
    (stop-timer! used-timer)
    result))


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
  (let ((c (new-corout id thunk)))
    (enqueue! (q) (new-corout id thunk))
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
           (corout-set-sleeping-mode! corout
                                      (sleeping-over-time sleep-queue-node
                                                          interruptible?))
           #; (pp `(now is ,(current-sim-time) sleeping until ,wake-time))
           (corout-scheduler))))
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
              (corout-set-sleeping-mode! corout (sleeping-on-mutex))
              (corout-scheduler)))))
  ;; should be unqueued by the unlock call...
  (sem-decrease! sem))

(define (sem-unlock! sem)
  (if (not (empty-queue? (sem-wait-queue sem)))
      (let ((corout-to-wake (dequeue! (sem-wait-queue sem))))
        (corout-set-sleeping-mode! corout-to-wake #f)
        (corout-enqueue! (q) corout-to-wake)))
  (sem-increase! sem))




;;; Messaging lists

(define messaging-lists-debug #f)
(define messaging-lists (make-table test: equal?))
(define (get-msg-list list-id)
  (table-ref messaging-lists list-id #f))
(define (msg-list-size list-id)
  (cond
   ((get-msg-list list-id) => length)
   (else 0)))

(define (subscribe list-id agent)
  (cond ((table-ref messaging-lists list-id #f)
         => (lambda (lst)
              (table-set! messaging-lists list-id (set-add eq? agent lst))))
        (else (table-set! messaging-lists list-id (make-set agent))))
  (update! agent corout msg-lists
           (lambda (lists) (set-add equal? list-id lists))))
(define (unsubscribe list-id agent)
  (if messaging-lists-debug
      (pp `(unsubscribe ,list-id
                        ,(corout-id agent) (by: ,(corout-id (self))))))
  (let ((msg-list (get-msg-list list-id)))
    (and msg-list
         (update! agent corout msg-lists
                  (lambda (lists) (set-remove equal? list-id lists)))
         (table-set! messaging-lists list-id
                     (set-remove eq? agent msg-list)))))
(define (broadcast list-id msg)
  (let ((msg-list (get-msg-list list-id)))
    (if (not msg-list)
        (error (to-string
                (show "could not broadcast message to list " list-id
                      ": list does not exists")))
        (if (not (> (length msg-list) 0))
            (error (to-string
                    (show "could not broadcast message to list " list-id
                          ": list is empty!")))
            (for-each (lambda (subscriber)
                        (if messaging-lists-debug
                            (pp `(sending ,msg
                                          to ,(corout-id subscriber)
                                          from ,(corout-id (self)))))
                        (! subscriber msg))
                      msg-list)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coroutine tracing system
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: event-simulation.scm
;;
;; description: A simple discrete event simulator implementation. The
;; behaviour of the simulator is straightforward: First some events
;; must be registered into a simulation object (generated with a call
;; to create-simulation) with the schedule-event! function. Once all
;; the initial events are scheduled, the simulation can be started
;; with start-simulation! function. All the time values units are
;; seconds. To create an infinite simulation, +inf.0 can be used as
;; the simulation horizon.
;;
;; In a running simulation, the next event to be ran will be either
;; the most late event, or the event that should be ran the closer to
;; the current simulation time.
;;
;; Some utilitary functions that should be used inside events are also
;; provided and macro are present in the event-simulation-macro.scm
;; file. Some exemples are also shown below.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(include "event-simulation-macro.scm")

;; (define-type event-simulation event-queue)
;; (define-type event label actions)

;; (define (create-event-simulation)
;;   (make-event-simulation (event-heap-create)))

(define create-simulation event-heap-create)

;; takes 2 arguments, occuring time value and actions thunk
(define create-event event-heap-node-create)

;; Dynamically scoped "hidden" variables used by the simulator to
;; perform "tricks" such as pause the simulation, etc...
(define !!-event-queue-!! (make-parameter #f))
(define !!-current-time-!! (make-parameter #f))
(define !!-simulation-start-time-!! (make-parameter #f))
(define !!-event-continuation-!! (make-parameter #f))
(define !!-exit-simulation-!! (make-parameter #f))

;; Here the event ev, is a thunk to be executed in dt seconds once the
;; simulation is started.
(define (schedule-event! sim dt ev)
  (event-heap-insert! sim (event-heap-node-create dt ev)))

;; This will start the simulator on the given simulation and will stop
;; the simulation after horizon seconds.
(define (start-simulation! sim horizon)
  ;; Record the initial time
  (define simulation-start-time (time->seconds (current-time)))
  (define stop-simulation #f)
  ;; Event that will occur at horizon seconds
  (define end-of-simulation-event
    (lambda ()
      (stop-simulation
       (string-append "Simulation finished normally at time: "
                      (number->string (!!-current-time-!!))))))

  ;; Simulator main loop
  (define (run-simulation)
    ;; Preschedule the end of simulation event
    (schedule-event! sim horizon end-of-simulation-event)
    (let iterate ()
      ;; the heap should never be empty, as it should at least contain
      ;; the end-of-simulation event
      (if (event-heap-empty? sim)
          (error "No more events available...")
          (let* ((top-node (event-heap-retrieve-top! sim))
                 (current-event-time (event-heap-time top-node))
                 (current-actions (event-heap-actions top-node))
                 (wake-time (- (time->seconds (current-time))
                               simulation-start-time)))
            ;; Sleep while waiting for the next event to pass by
            (let ((sleep-time (- current-event-time wake-time)))
;;               (if (< sleep-time 0)
;;                   (show "Warning: Simulation is getting late by "
;;                         sleep-time " secs.\n"))
              (thread-sleep! sleep-time))

            ;; perform the event's actions inside a dynamic scoping of
            ;; several control variables used by the utilistaries.
            (call/cc
             (lambda (k)
               (parameterize ((!!-event-queue-!! sim)
                              (!!-simulation-start-time-!!
                               simulation-start-time)
                              (!!-current-time-!! current-event-time)
                              (!!-event-continuation-!! k)
                              (!!-exit-simulation-!! stop-simulation))
              (current-actions))))
            (iterate)))))

  ;; Start the simulation
  (call/cc (lambda (k)
             (set! stop-simulation
                   (lambda (msg) (k msg)))
             (run-simulation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilitaries
;;
;; Note: All the given utilitaries should be used *inside* a
;; simulation event.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some Time definitions that should "short-circuit" the simulator to
;; execute an event very soon.
(define NOW! -10)
(define RIGHT-NOW! -inf.0)

;;;;;;;;;;;; Semaphores and critical sections ;;;;;;;;;;

;; Internal data type def
(define-type sem value wait-queue)

;;;;; Internal control functions ;;;;
(define (sem-increase! sem) (sem-value-set! sem (+ (sem-value sem) 1)))
(define (sem-decrease! sem) (sem-value-set! sem (- (sem-value sem) 1)))

;; innefficient queue implementation...
(define (sem-enqueue-k! sem k)
  (sem-wait-queue-set! sem (cons k (sem-wait-queue sem))))
(define (sem-dequeue-k! sem)
  (let ((k (car (take-right (sem-wait-queue sem) 1))))
    (sem-wait-queue-set! sem (drop-right (sem-wait-queue sem) 1))
    k))

;;;; External Semaphores functions ;;;;

;; Constructors
(define (new-semaphore init-value) (make-sem init-value '()))
(define (new-mutex) (new-semaphore 1))

(define (sem-locked? sem) (< (sem-value sem) 1))

;; Usual lock and unlock (P/V, take/release, etc...) implementation
;; where the executed event will be put into a sleep queue if it tries
;; to take an unavailable semaphore, and the first enqueued (fifo)
;; event will be resumed when an unlock is performed.
(define (sem-lock! sem)
  (call/cc (lambda (k)
             (sem-decrease! sem)
             (if (< (sem-value sem) 0)
                 (begin
                   (sem-enqueue-k! sem k)
                   ((!!-event-continuation-!!) 'dummy))))))

(define (sem-unlock! sem)
  (sem-increase! sem)
  (if (sem-locked? sem)
      (let ((k (sem-dequeue-k! sem)))
        (in 0 (lambda () (k 'dummy))))))

;;;; Simulation abrupt stop. Will result in having the
;;;; start-simulation! function to return return-val.
(define (exit-simulation return-val) ((!!-exit-simulation-!!) return-val))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (event-sim-test)
  (define sim (create-simulation))
  (define (make-test-actions i)
    (lambda ()
      (pp `(,i : now it is ,(!!-current-time-!!)))
      (in 1 (make-test-actions (+ i 1)))))

  ;; Cannot do (in 12.54321 (make-test-actions 0)) because the current
  ;; flow of control is not yet in the simulation.
  (schedule-event! sim 0.75 (make-test-actions 0))
  (display (start-simulation! sim 6)) (newline))

(define (event-sim-synchro-test)
  (define sim (create-simulation))
  (define sem (new-semaphore 0))
  
  (define (consumerA)
    (sem-lock! sem)
    (pp '(consumerA took 1 ressource))
    (in 0 consumerA))

  (define (consumerB)
    (sem-lock! sem)
    (pp '(consumerB took 1 ressource))
    (in 0 consumerB))

  (define (producer)
    (sem-unlock! sem)
    (pp `(produced 1 ressource))
    (in 0.5 producer))

  (schedule-event! sim 0 consumerA)
  (schedule-event! sim 0 consumerB)
  (schedule-event! sim 0 producer)
  (display (start-simulation! sim 5.6)) (newline))

(define (event-sim-exit-test)
  (define (infinite-loop-ev)
    (if (> (!!-current-time-!!) 5)
        (exit-simulation "exited simulation successfully")
        (pp `(it is now ,(!!-current-time-!!))))
    (in 1 infinite-loop-ev))
  (define sim (create-simulation))
  (schedule-event! sim 0 infinite-loop-ev)
  (display (start-simulation! sim +inf.0)) (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On stand-by stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Simulation performed in single stepping, no simulation end event is
;; automatically added and should be added manually (using a call/cc).

;; (define (simulation-step! sim)
;;   (if (event-heap-empty? sim)
;;       (error "No more events available...")
;;       (let* ((top-node (event-heap-retrieve-top! sim))
;;              (current-time (event-heap-time top-node))
;;              (current-actions (event-heap-actions top-node)))
;;         (parameterize ((!!-event-queue-!! sim)
;;                        (!!-current-time-!! current-time))
;;                       (current-actions)))))


;; (define-macro (in delta arg1 . args)
;;   `(schedule-event! (!!-event-queue-!!)
;;                     (+ (!!-current-time-!!) ,delta)
;;                     (lambda () ,arg1 ,@args)))


;; (define (event-sim-step-test)
;;   (define sim (create-simulation))
;;   (define (make-test-actions i)
;;     (lambda ()
;;       (pp `(,i : now it is ,(!!-current-time-!!)))
;;       (if (< i 10)
;;           (in 4 (make-test-actions (+ i 1))))))
;;   (schedule-event! sim 12.5421 (make-test-actions 0))
;;   (let loop ((i 0))
;;     (if (< i 10)
;;         (begin (simulation-step! sim)
;;                (loop (+ i 1)))
;;         (pp `(completed  ,i  iterations successfully.)))))

(include "rbtree.scm")

;; (define-type event-simulation event-queue)
;; (define-type event label actions)

;; (define (create-event-simulation)
;;   (make-event-simulation (event-heap-create)))

(define create-simulation event-heap-create)

;; takes 2 arguments, occuring time value and actions thunk
(define create-event event-heap-node-create)

(define !!-event-queue-!! (make-parameter #f))
(define !!-current-time-!! (make-parameter #f))

;; Here the event ev, is a thunk to be executed when the simulation is
;; performed.
(define (schedule-event! sim time ev)
  (event-heap-insert! sim (event-heap-node-create time ev)))

(define (start-simulation! sim horizon)
  (define simulation-start-time (time->seconds (current-time)))
  (define stop-simulation #f)
  (define end-of-simulation-event
    (lambda ()
      (stop-simulation
       (string-append "Simulation finished normally at time: "
                      (number->string (!!-current-time-!!))))))
  
  (define (run-simulation)
    (schedule-event! sim horizon end-of-simulation-event)
    (let iterate ()
      (if (event-heap-empty? sim)
          (error "No more events available...")
          (let* ((top-node (event-heap-retrieve-top! sim))
                 (current-event-time (event-heap-time top-node))
                 (current-actions (event-heap-actions top-node))
                 (wake-time (- (time->seconds (current-time))
                               simulation-start-time)))
            (pp `(sleeping for ,(- current-event-time wake-time)))
            (thread-sleep! (- current-event-time wake-time))

            (parameterize ((!!-event-queue-!! sim)
                           (!!-current-time-!! current-event-time))
              (current-actions))
            (iterate)))))

  (call/cc (lambda (k)
             (set! stop-simulation
                   (lambda (msg) (pp msg) (k #t)))
             (run-simulation))))


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

(define-macro (in delta thunk)
  `(schedule-event! (!!-event-queue-!!)
                    (+ (!!-current-time-!!) ,delta)
                    ,thunk))

(define (event-sim-test)
  (define sim (create-simulation))
  (define (make-test-actions i)
    (lambda ()
      (pp `(,i : now it is ,(!!-current-time-!!)))
      (if (< i 10)
          (in 4 (make-test-actions (+ i 1))))))

  ;(in 12.54321 (make-test-actions 0))
  (schedule-event! sim 12.5421 (make-test-actions 0))
  (start-simulation! sim 50))


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

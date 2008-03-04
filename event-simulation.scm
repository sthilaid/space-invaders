(include "rbtree.scm")

;; (define-type event-simulation event-queue)
;; (define-type event label actions)

;; (define (create-event-simulation)
;;   (make-event-simulation (event-heap-create)))

(define create-simulation event-heap-create)

;; takes 2 arguments, occuring time value and actions thunk
(define create-event event-heap-node-create)

;; (define !!-event-queue-!! (make-parameter #f))
;; (define !!-current-time-!! (make-parameter #f))

(define schedule-event! event-heap-insert!)

(define (start-simulation! sim horizon)
  (define stop-simulation #f)
  (define end-of-simulation-event
    (create-event horizon
                  (lambda (sim current-time)
                    (stop-simulation
                     (string-append "Simulation finished normally at time: "
                                    (number->string current-time))))))
  
  (define (run-simulation)
    (schedule-event! sim end-of-simulation-event)
    (let iterate ()
      (if (event-heap-empty? sim)
          (error "No more events available...")
          (let* ((top-node (event-heap-retrieve-top! sim))
                 (current-time (event-heap-time top-node))
                 (current-actions (event-heap-actions top-node)))
            (current-actions sim current-time)
            (iterate)))))

  (call/cc (lambda (k)
             (set! stop-simulation
                   (lambda (msg) (pp msg) (k #t)))
             (run-simulation))))

;; (define-macro (in delta arg1 . args)
;;   `(schedule-event! sim (create-event 
             
(define (test)
  (define sim (create-simulation))
  (define (make-test-actions i)
    (lambda (sim current-time)
      (pp `(,i : now it is ,current-time))
      (if (< i 10)
          (schedule-event! sim (create-event
                                (+ current-time 1)
                                (make-test-actions (+ i 1)))))))
  
  (schedule-event! sim (create-event 12.5421 (make-test-actions 0)))
  (start-simulation! sim 50))

;; (in 4.3
;;     (pp 'allo)
;;     (in 2.2 (pp 'coucpo)))

;; (after 4.3 
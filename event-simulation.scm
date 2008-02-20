(include "rbtree.scm")

(define-type event-simulation event-queue)
(define-type event label actions)

(define (create-event-simulation)
  (make-event-simulation (heap-create)))

(define (create-event label actions)
  (make-event label actions))

(define (schedule-event! sim ev absolute-time)
;  (pp `(now scheduling new event at ,absolute-time))
  (heap-insert! (event-simulation-event-queue sim)
                (heap-node-create absolute-time ev))
  'void)


(define (start-simulation! sim horizon)
  (define stop-simulation #f)
  (define end-of-simulation-event
    (create-event
     'end-of-simulation
     (lambda (sim time)
       (stop-simulation
        (string-append "Simulation finished normally at time: "
                       (number->string time))))))
  
  (define (run-simulation)
    (schedule-event! sim end-of-simulation-event horizon)
    (let iterate ((time 0))
      (if (heap-empty? (event-simulation-event-queue sim))
          (stop-simulation "No more events available...")
          (let* ((top-node (heap-retrieve-top!
                            (event-simulation-event-queue sim)))
                 (current-time (heap-value top-node))
                 (current-event (heap-content top-node)))
            ((event-actions current-event) sim current-time)
            (iterate current-time)))))

  (call/cc (lambda (k)
             (set! stop-simulation
                   (lambda (msg) (pp msg) (k #t)))
             (run-simulation))))
             

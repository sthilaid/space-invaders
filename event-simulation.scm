(include "rbtree.scm")

(define-type event-simulation event-queue)
(define-type event label actions)

(define (create-event-simulation)
  (make-event-simulation (heap-create)))

(define (create-event label actions)
  (make-event label actions))

(define (schedule-event! sim ev absolute-time)
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
          (error "No more events available...")
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
             
(define (test)
  (define sim (create-event-simulation))
  (define (make-test-ev i)
    (create-event
     'test-ev (lambda (sim time)
                (pp `(,i : now it is ,time))
                (if (< i 10)
                    (schedule-event! sim (make-test-ev (+ i 1)) (+ time 1))))))
  
  (schedule-event! sim (make-test-ev 0) 12.5421)
  (start-simulation! sim 50))
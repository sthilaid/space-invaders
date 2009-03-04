(include "thread-simulation-macro.scm")
(include "test-macro.scm")
(include "class.scm")
(include "match.scm")

;; Usage of load is better but cannot because of the oo system. Here
;; copied the thread-simulation package load requirements and include it...
(load "rbtree.scm")
(load "scm-lib")
(include "scm-lib-macro.scm")
(include "thread-simulation.scm")

;;(load "thread-simulation.scm")
(load "test")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thread Simulation Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test simple-test "A1B2" 3
  (let ((c1 (new corout 'c1 (lambda ()
                              (display 'A)
                              (yield)
                              (display 'B)
                              (terminate-corout 'C))))
        (c2 (new corout 'c2 (lambda ()
                              (display 1)
                              (yield)
                              (display 2)
                              (terminate-corout 3)))))
    (simple-boot c1 c2)))

(define-test test-kill-all "12" 'killed-all
  (let ((c1 (new corout 'c1 (lambda ()
                              (for i 0 (< i 3)
                                   (begin (if (= i 1)
                                              (kill-all! 'killed-all))
                                          (display 1)
                                          (yield))))))
        (c2 (new corout 'c2 (lambda ()
                              (for i 0 (< i 3) (begin (display 2)
                                                      (yield)))))))
    (simple-boot c1 c2)))

(define-test test-ret-val-handler "A1aB2" 6
  (let ((c1 (new corout 'c1 (lambda () (begin (display 'A)
                                              (yield)
                                              (display 'B)
                                              1))))
        (c2 (new corout 'c2 (lambda () (begin (display 1)
                                              (yield)
                                              (display 2)
                                              2))))
        ;; here terminate-corout is used to shortcircuit the flow of
        ;; the thunk and finish earlier this thread.
        (c3 (new corout 'c3 (lambda () (begin (display #\a)
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
  (letrec ((c1 (new corout 'c1 (lambda ()
                                 (pretty-print `(c1 received ,(?)))
                                 (pretty-print `(c1 received ,(?)))
                                 'done)))
           (c2 (new corout 'c2 (lambda ()
                                 (pretty-print 'sending-data-from-c2)
                                 (! c1 'allo))))
           (c3 (new corout 'c3 (lambda ()
                                 (pretty-print 'sending-data-from-c3)
                                 (! c1 'salut)))))
    (simple-boot c1 c2 c3)))


(define-test test-recursive-sim "A12BA12BA12B" 'meta-12-done!
  (let* ((sA (new corout 'sA (lambda () (for i 0 (< i 3)
                                             (begin (display 'A)
                                                    (super-yield)
                                                    (yield))))))
         (sB (new corout 'sB (lambda () (for i 0 (< i 3)
                                             (begin (display 'B)
                                                    (yield)))
                                     (terminate-corout 'sB))))
         (s1 (new corout 's1 (lambda () (for i 0 (< i 3)
                                             (begin (display 1)
                                                    (yield))))))
         (s2 (new corout 's2 (lambda () (for i 0 (< i 3)
                                             (begin (display 2)
                                                    (super-yield)
                                                    (yield))))))
         (meta-AB
          (new corout 'meta-AB
                      (lambda ()
                        (simple-boot sA sB)
                        'meta-AB-done!)))
         (meta-12
          (new corout 'meta-12
                      (lambda ()
                        (simple-boot s1 s2)
                        'meta-12-done!))))
    (simple-boot meta-AB meta-12)))

(define-test test-kill-all-rec "A1B2ABAB" 'meta-AB-done!
  (let* ((sA (new corout 'sA (lambda () (for i 0 (< i 3)
                                             (begin (display 'A)
                                                    (super-yield)
                                                    (yield))))))
         (sB (new corout 'sB (lambda () (for i 0 (< i 3)
                                             (begin (display 'B)
                                                    (super-yield)
                                                    (yield))))))
         (s1 (new corout 's1 (lambda () (for i 0 (< i 3)
                                             (begin (if (= i 1)
                                                        (kill-all! 'over))
                                                    (display 1)
                                                    (super-yield)
                                                    (yield))))))
         (s2 (new corout 's2 (lambda () (for i 0 (< i 3)
                                             (begin (display 2)
                                                    (super-yield)
                                                    (yield))))))
         (meta-AB
          (new corout 'meta-AB
                      (lambda ()
                        (simple-boot sA sB)
                        'meta-AB-done!)))
         (meta-12
          (new corout 'meta-12
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
  (let ((c1 (new corout 'c1 (lambda ()
                              (sleep-for 0.2)
                              (pretty-print 'bonne-nuit))))
        (c2 (new corout 'c2 (lambda () (pretty-print 'bon-matin))))
        (c3 (new corout 'c3 (lambda ()
                              (sleep-for 0.1)
                              (pretty-print 'bonne-aprem)))))
    (simple-boot c1 c2 c3)
    'dont-care))

(define-test test-mutex "4123" 'done
  (let* ((mut (new-mutex))
         (c1 (new corout 'c1 (lambda () (critical-section! mut
                                                           (yield)
                                                           (display "1")))))
         (c2 (new corout 'c2 (lambda () (critical-section! mut
                                                           (yield)
                                                           (display "2")))))
         (c3 (new corout 'c3 (lambda () (critical-section! mut
                                                           (yield)
                                                           (display "3")))))
         (c4 (new corout 'c4 (lambda () (display "4")))))
    (simple-boot c1 c2 c3 c4)
    'done))

(define-test test-continuation "210" 'done
  (let* ((c1 (new corout 'c1 (lambda () (display "1")
                                     (continue-with-thunk!
                                      (lambda () (display "0") 'done)))))
         (c2 (new corout 'c2 (lambda () (display "2")
                                     (continue-with c1)))))
    (simple-boot c2)))

(define-test test-prioritized-cont "2431" 'done
  (let* ((c1 (new corout 'c1 (lambda () (display "1"))))
         (c2 (new corout 'c2 (lambda () (display "2")
                                     (continue-with c1))))
         (c3 (new corout 'c3 (lambda () (display "3"))))
         (c4 (new corout 'c4 (lambda () (display "4")
                                     (prioritized-continuation c3)))))
    (simple-boot c2 c4)
    'done))

(define-test test-thunk-composition "123" 'ok
  (let* ((t1 (lambda () (display "1")))
         (t2 (lambda () (display "2")))
         (t3 (lambda () (display "3") 'ok))
         (c1 (new corout
              'c1 (compose-thunks t1 t2 t3))))
    (simple-boot c1)))

(define-test test-spawning "CABD" 'done
  (let* ((c1 (new corout 'c1 (lambda () (display 'A))))
         (t2 (lambda () (display 'B) (yield) 'done))
         (c3 (new corout 'c3 (lambda () (display 'C)
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
  (let* ((c1 (new corout 'c1 (lambda ()
                               (pretty-print (?? odd?))
                               (?))))
         (c2 (new corout 'c2 (lambda ()
                               (! c1 2)
                               (pretty-print 'c2-sent-data))))
         (c3 (new corout 'c3 (lambda ()
                               (pretty-print 'c3-sleeps)
                               (sleep-for 0.2)
                               (! c1 3)
                               (pretty-print 'c3-sent-data)))))
    (simple-boot c1 c2 c3)))

(define-test test-timeout-? "1ok" 'done
  (let* ((c1 (new corout 'c1 (lambda () (with-exception-catcher
                                         (lambda (e) (display 'ok))
                                         (lambda ()
                                           (display (?))
                                           (display (? timeout: 0.1))))
                                     'done)))
         (c2 (new corout 'c2 (lambda () (! c1 1)))))
    (simple-boot c1 c2)))

(define-test test-timeout-?? "3ok" 'done
  (let* ((c1 (new corout 'c1 (lambda () (with-exception-catcher
                                         (lambda (e) (display 'ok))
                                         (lambda ()
                                           (display (?? odd? timeout: 1))
                                           (display (?? odd? timeout: 0.2))))
                                     'done)))
         (c2 (new corout 'c2 (lambda () (! c1 2) (yield) (! c1 3)))))
    (simple-boot c1 c2)))

(define-test test-timeout-extended "allogot-nothing" 'done
  (let* ((c1 (new corout 'c1 (lambda ()
                               (cond ((timeout? #f (? timeout: 0))
                                      => (lambda (x) (display x)))
                                     (else (display 'got-nothing)))
                               (cond ((timeout? #f (? timeout: 0.2))
                                      => (lambda (x) (display x)))
                                     (else (display 'got-nothing)))
                               'done)))
         (c2 (new corout 'c2 (lambda () (! c1 'allo)))))
    (simple-boot c1 c2)))

(define-test test-recv "pongpingfinished!" 'done
  (let* ((c1 (new corout 'c1 (lambda ()
                               (let loop ()
                                 (recv
                                  (ping (display 'ping) (loop))
                                  (pong (display 'pong) (loop))
                                  (after 0.1 (display 'finished!) 'done))))))
         (c2 (new corout 'c2 (lambda () (! c1 'pong))))
         (c3 (new corout 'c3 (lambda () (! c1 'ping)))))
    (simple-boot c1 c2 c3)))

(define-test test-recv-ext "pong1pong2pong" 'ok
  (let* ((c1 (new corout 'c1 (lambda ()
                               (let loop ()
                                 (recv ((,from ping)
                                        (where (corout? from))
                                        (! from 'pong)))
                                 (loop)))))
         (c2 (new corout 'c2 (lambda ()
                               (let loop ((n 1))
                                 (recv ((,from inc)
                                        (if (< n 3)
                                            (begin (! from `(ack ,n))
                                                   (loop (+ n 1)))
                                            (kill-all! 'ok))))))))
         (c3 (new corout 'c3 (lambda ()
                               (let loop ()
                                 (! c1 `(,(current-corout) ping))
                                 (recv (pong (! c2 `(,(current-corout) inc))))
                                 (display 'pong)
                                 (recv ((ack ,n) (display n)))
                                 (loop))))))
    (simple-boot c1 c2 c3)))

(define-test test-msg-lists "alloallosalut#tallo" 'salut
  (let ((c1 (new corout 'c1 (lambda ()
                              (subscribe 'toto (current-corout))
                              (recv (,x (display x)))
                              (unsubscribe 'toto (current-corout))
                              (recv (,x (display x))))))
        (c2 (new corout 'c2 (lambda ()
                              (subscribe 'toto (current-corout))
                              (recv (,x (display x)))
                              (recv (,x (display x))))))
        (c4 (new corout 'c4 (lambda ()
                              (subscribe 'toto (current-corout))
                              ;; ensure not woken up
                              (let ((t (current-sim-time)))
                                (sleep-for 0.5)
                                (display (>= (- (current-sim-time) t) 0.4))
                                (display (?))
                                (?)))))
        (c3 (new corout 'c3 (lambda ()
                              (broadcast 'toto 'allo)
                              (yield)
                              (broadcast 'toto 'salut)
                              (yield)))))
    (simple-boot c1 c2 c4 c3)))

(define-test test-dynamic-handlers "allonotutu" 'ok
  (let ((c1 (new corout 'c1 (lambda ()
                                  (! (self) 'allo)
                                  (! (self) 'allo)
                                  (! (self) 'tutu)
                                  (with-dynamic-handlers
                                   ((allo (display 'allo)))
                                   (recv (toto (display 'toto))
                                         (after 0.02 (display 'no))))
                                  (recv (toto (display 'toto))
                                        (after 0.02 (display 'no)))
                                  (with-dynamic-handlers
                                   ((tutu (display 'allo)))
                                   (recv (tutu (display 'tutu))
                                         (after 0.02 (display 'no))))
                                   'ok))))
        (simple-boot c1)))

(define-test test-msg-box-cleaning "allo" 3
  (let ((c1 (new corout 'c1 (lambda ()
                              (subscribe 'toto (current-corout))
                              (recv (allo (display 'allo)))
                              (let ((x (clean-mailbox allo)))
                                (recv (allo (display 'no))
                                      (after 0 x))))))
        (c2 (new corout 'c2 (lambda ()
                              (broadcast 'toto 'allo)
                              (broadcast 'toto 'allo)
                              (broadcast 'toto 'allo)
                              (broadcast 'toto 'allo)))))
    (simple-boot c1 c2)))

(define-test test-after0 "noallo" 'done
  (let ((c1 (new corout 'c1 (lambda ()
                              (! (self) 'salut)
                              (recv (allo (display 'allo))
                                    (after 0 (display 'no)))
                              (! (self) 'allo)
                              (recv (allo (display 'allo))
                                    (after 0 (display 'no)))
                              'done))))
    (simple-boot c1)))

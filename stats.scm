(include "scm-lib-macro.scm")

(define (generate-histogram interval sample)
  (define (inc bars index)
    (vector-set! bars index (+ (vector-ref bars index) 1)))
  (let* ((simplified-sample
          (map (lambda (x) (quotient (inexact->exact (round x))
                                     interval))
               sample))
         (size (+ (apply max simplified-sample) 1))
         (bars (make-vector size 0)))
    (for-each (lambda (s) (inc bars s)) simplified-sample)

    (display "range,occurences\n")
    (for i 0 (< i size)
         (begin
           (display (* i interval))
           (display "-")
           (display (- (* (+ i 1) interval) 1))
           (display ",")
           (display (vector-ref bars i))
               (display "\n")))))



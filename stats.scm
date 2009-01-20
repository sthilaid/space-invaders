(include "scm-lib-macro.scm")

(define (generate-histogram nb-of-divisions sample
                            #!optional (range-precision 1))
  (define (fix-precision nb precision)
    (/ (round (* nb (/ 1 precision))) (/ 1 precision)))
  (define (inc bars index)
    (vector-set! bars index (+ (vector-ref bars index) 1)))
  (let* ((min-value (apply min sample))
         (max-value (apply max sample))
         (interval  (/ (- max-value min-value) nb-of-divisions))
         (simplified-sample
          (map (lambda (x) (inexact->exact
                            (floor (/ (- x min-value) interval))))
               sample))
         (size (+ (apply max simplified-sample) 1))
         (bars (make-vector (+ nb-of-divisions 1) 0)))
    (for-each (lambda (s) (inc bars s)) simplified-sample)

    (display "range,occurences\n")
    (for i 0 (< i size)
         (begin
           (display (fix-precision (+ (* i interval) min-value)
                                   range-precision))
           (display "-")
           (display (fix-precision (+ (* (+ i 1) interval) min-value)
                                   range-precision))
           (display ",")
           (display (exact->inexact (vector-ref bars i)))
               (display "\n")))))



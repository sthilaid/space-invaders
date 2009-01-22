(include "scm-lib-macro.scm")

(define (generate-histogram title nb-of-divisions samples-alist
                            #!optional (range-precision 1))
  (define filename (string-append "histo-" title))
  (define (fix-precision nb precision)
    (/ (round (* nb (/ 1 precision))) (/ 1 precision)))
  (define (inc bars index)
    (vector-set! bars index (+ (vector-ref bars index) 1)))
  (let* ((samples (map cdr samples-alist))
         (min-value (fold-l (lambda (acc sample) (apply min acc sample))
                            +inf.0
                            samples))
         (max-value (fold-l (lambda (acc sample) (apply max acc sample))
                            -inf.0
                            samples))
         (interval  (/ (- max-value min-value) nb-of-divisions))
         (simplified-samples
          (map (lambda (sample)
                 (let* ((bars (make-vector (+ nb-of-divisions 1) 0))
                        (simple-sample
                         (map (lambda (x)
                                (inexact->exact
                                 (floor (/ (- x min-value) interval))))
                              sample)))
                   (for-each (lambda (s) (inc bars s)) simple-sample)
                   bars))
               samples)))

    (pp `(,title : min: ,min-value max: ,max-value int: ,interval))
    (for-each (lambda (el) (pp `(,(car el) ,(apply max (cdr el)))))
              samples-alist)

    (with-output-to-file (string-append filename  ".csv")
     (lambda ()
       ;; Header printout
       (display "range")
       (let loop ((samples-alist samples-alist))
         (if (pair? samples-alist)
             (begin (display ",") (display (caar samples-alist))
                    (loop (cdr samples-alist)))))
       (display "\n")
       ;; content
       (for i 0 (<= i nb-of-divisions)
            (begin
              (display (fix-precision (+ (* i interval) min-value)
                                      range-precision))
              (display "-")
              (display (fix-precision (+ (* (+ i 1) interval) min-value)
                                      range-precision))
              (let loop ((simplified-samples simplified-samples))
                (if (pair? simplified-samples)
                    (begin (display ",")
                           (display (exact->inexact
                                     (vector-ref (car simplified-samples) i)))
                           (loop (cdr simplified-samples)))))
              (display "\n")))))
    
    (with-output-to-file
     (string-append filename  ".gn")
     (lambda ()
       (show
        "set terminal png\n"
        "set output \"" filename ".png\"\n"
        "set datafile separator ','\n"
        "set title 'Histogram'\n"
        "set auto x\n"
        "set auto y\n"
        "set style data histogram\n"
        "set style fill solid border -1\n"
        "set boxwidth 2\n"
        "set xtic rotate by 90\n"
        "plot [] [0:] \"" filename ".csv\" using 2:xtic(1) ti col")
       (for i 0 (< i (- (length samples-alist) 1))
            (show ", \"\" using " (+ i 3) " ti col"))
       (display "\n\n")))))



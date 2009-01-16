(include "scm-lib-macro.scm")

(define (sum numbers)
  (if (not (pair? numbers)) (error "sum: must sum a list of arguments"))
  (let loop ((numbers numbers) (acc 0))
    (if (not (pair? numbers))
        acc
        (loop (cdr numbers) (+ acc (car numbers))))))
      
(define (average sample)
  (if (not (pair? sample)) (error "Average sample must be a list."))
  (/ (sum sample) (length sample)))

(define (variance sample)
  (define mean (average sample))
  (define N (length sample))
  (if (not (pair? sample)) (error "Variance sample must be a list."))
  (/ (fold-l (lambda (acc n) (+ (expt (- n mean) 2) acc))
             0
             sample)
     N))

(define (standard-deviation sample) (sqrt (variance sample)))

(define (create-simple-moving-avg)
  (define n 0)
  (define SMA 0)
  (define current-mode #f)
  
  (define (gather-init-data new-val)
    (set! SMA new-val)
    (set! n 1)
    (set! current-mode sma-calculator))
  
  (define (sma-calculator new-val)
    (let ((new-n (+ n 1)))
      (set! SMA (+ (* SMA (/ n new-n))
                   (* new-val (/ 1 new-n))))
      (set! n new-n))
    SMA)
  
  (define (dispatcher . arg)
    (if (not (pair? arg))
        (if (eq? current-mode sma-calculator)
            SMA
            'N/A)
        (current-mode (car arg))))

  (set! current-mode gather-init-data)
  dispatcher)

(define (create-bounded-simple-moving-avg bound)
  (define data '())
  (define SMA 'N/A)
  (define current-mode #f)
  (define max -inf.0)
  (define min +inf.0)

  (define (check-extremum! val)
    (cond ((< val min) (set! min val))
          ((> val max) (set! max val))))
  
  (define (gather-init-data new-val)
    (check-extremum! new-val)
    (set! data (cons new-val data))
    (set! SMA (average data))
    (if (>= (length data) bound)
        (set! current-mode sma-calculator))
    SMA)
  
  (define (sma-calculator new-val)
    (check-extremum! new-val)
    (set! data (cons new-val (drop-right data 1)))
    (set! SMA (average data))
    SMA)
  
  (define (dispatcher . arg)
    (cond
     ((not (pair? arg))   SMA)
     ((number? (car arg)) (current-mode (car arg)))
     ((symbol? (car arg)) (case (car arg)
                            ((min) min)
                            ((max) max)
                            ((avg) SMA)
                            (else SMA)))))
  
  (set! current-mode gather-init-data)
  dispatcher)

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



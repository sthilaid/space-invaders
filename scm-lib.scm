;;                       General Scheme library 
;;                         by David St-Hilaire
;;
;;
;; This library source file contains various well known and less well
;; known functions used in functionnal programming. Authors referrence
;; will not be cited but most of the code here was not invented by the
;; author of this file. Also, these functions will not be documented
;; because names and uses of these are trivial for any functionnal
;; programmer.


; Compiler declarations for optimizations
;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block)
;;          (not safe)
;;          (fixnum))

;;;;;;;;;;;;;;;;;;;;;;; Display and Debug display ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ___scm-lib-debug___ #t)

(define (show . args)
  (for-each (lambda (x) (if (string? x) (display x) (write x))) args))

(define (debug-show . args)
  (if (and ___scm-lib-debug___ (not (null? args)))
      (apply show args)))

(define (pipe p c)
  (receive (out in) (open-string-pipe '(direction: output))
    (let ((t
           (thread-start!
            (make-thread
             (lambda ()
               (with-output-to-port out p))))))
      (with-input-from-port in c)))) 
  

;;;;;;;;;;;;;;;;;;;;;;; list operations ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-member comparator el list)
  (cond
   ((not (pair? list)) #f)
   ((comparator el (car list)) list)
   (else (generic-member comparator el (cdr list)))))

(define (find-first-index comp list)
  (cond
   ((not (pair? list)) (error "object not found in the list"))
   ((comp (car list)) 0)
   (else (+ 1 (find-first-index comp (cdr list))))))

(define (list-remove comparator el list)
  (let loop ((list list)
             (acc '()))
    (if (not (pair? list))
        (reverse acc)
        (if (comparator (car list) el)
            (loop (cdr list) acc)
            (loop (cdr list) (cons (car list) acc))))))

(define (filter pred list)
  (cond
   ((not (pair? list)) '())
   ((pred (car list)) (cons (car list) (filter pred (cdr list))))
   (else (filter pred (cdr list)))))

(define (exists pred list)
  (cond
   ((not (pair? list)) #f)
   ((pred (car list)) (car list))
   (else  (exists pred (cdr list)))))

(define (forall pred list)
  (cond
   ((not (pair? list)) #t)
   ((not (pred (car list))) #f)
   (else (forall pred (cdr list)))))

(define (fold-l f acc list)
  (if (not (pair? list))
      acc
      (fold-l f (f acc (car list)) (cdr list))))

(define (cleanse lst)
  (cond
   ((not (pair? lst)) '())
   ((null? (car lst)) (cleanse (cdr lst)))
   (else (cons (car lst) (cleanse (cdr lst))))))

(define (union l1 l2)
  (let loop ((l1 l1) (acc l2))
    (if (not (pair? l1))
        acc
        (if (member (car l1) l2)
            (loop (cdr l1) acc)
            (loop (cdr l1) (cons (car l1) acc))))))

(define (take lst n)
  (if (< n 1)
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (or (< n 1) (not (pair? lst)))
      lst
      (drop (cdr lst) (- n 1))))

(define (take-right lst n)
  (let lp ((lag lst)  (lead (drop lst n)))
    (if (pair? lead)
        (lp (cdr lag) (cdr lead))
        lag)))

(define (drop-right lst n)
  (let recur ((lag lst) (lead (drop lst n)))
    (if (pair? lead)
        (cons (car lag) (recur (cdr lag) (cdr lead)))
        '())))


(define (quick-sort smaller? equal? greater? lst)
  (if (or (not (pair? lst))
          (null? (cdr lst)))
      lst
      (let ((pivot (car lst)))
        (append (quick-sort smaller? equal? greater?
                            (filter (lambda (x) (smaller? x pivot)) lst))
                (filter (lambda (x) (equal? x pivot)) lst)
                (quick-sort smaller? equal? greater?
                            (filter (lambda (x) (greater? x pivot)) lst))))))

(define-macro (extremum-fonction comparator opposite-extremum)
  (let ((lst-sym (gensym 'lst-sym))
        (extremum-sym (gensym 'extremum-sym))
        (loop-sym (gensym 'loop-sym)))
    `(lambda (,lst-sym) 
       (let ,loop-sym ((,lst-sym ,lst-sym)
                       (,extremum-sym ,opposite-extremum))
            (cond
             ((not (pair? ,lst-sym)) ,extremum-sym)
             ((,comparator (car ,lst-sym) ,extremum-sym)
              (,loop-sym (cdr ,lst-sym) (car ,lst-sym)))
             (else
              (,loop-sym (cdr ,lst-sym) ,extremum-sym)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Math stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-power-of-2 n)
  (let ((current-power (expt 2 (- (integer-length n) 1))))
    (if (= n current-power)
        n
        (* current-power 2))))

(define maximum (extremum-fonction > -inf.0))
(define minimum (extremum-fonction < +inf.0))

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

(define (complex-conjugate z)
  (make-rectangular (real-part z) (- (imag-part z))))

(define pi (* 2 (asin 1)))

;;;;;;;;;;;;;;;;;;;;;; 2d-position vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define-type pos2d x y)
(define make-pos2d cons)
(define (pos2d? x) (and (pair? x) (number? (car x)) (number? (cdr x))))
(define pos2d-x car)
(define pos2d-y cdr)
(define pos2d-x-set! set-car!)
(define pos2d-y-set! set-cdr!)
(define (pos2d-add p1 p2) (make-pos2d (+ (pos2d-x p1) (pos2d-x p2))
                                      (+ (pos2d-y p1) (pos2d-y p2))))
(define (pos2d-sub p1 p2) (make-pos2d (- (pos2d-x p1) (pos2d-x p2))
                                      (- (pos2d-y p1) (pos2d-y p2))))
(define (pos2d-scalar-prod p1 p2)
  (+ (* (pos2d-x p1) (pos2d-x p2)) (* (pos2d-y p1) (pos2d-y p2))))
(define (pos2d-cartesian-distance p1 p2)
  (sqrt (+ (expt (- (pos2d-x p1) (pos2d-x p2)) 2)
           (expt (- (pos2d-y p1) (pos2d-y p2)) 2))))

(define (pos2d-complexify p) (make-rectangular (pos2d-x p) (pos2d-y p)))

(define (pos2d= p1 p2)
  (and (= (pos2d-x p1) (pos2d-x p2))
       (= (pos2d-y p1) (pos2d-y p2))))



;;;;;;;;;;;;;;;;;;;;;; Boolean operation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define-macro (xor b1 b2) `(not (eq? ,b1 ,b2)))
  

;;;;;;;;;;;;;;;;;;;;;;;; Data Structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (push element stack) -> new stack
;; (peek stack)         -> top stack element
;; (pop stack)          -> new stack
;; (define make-stack '())
;; (define push cons)   
;; (define peek car)    
;; (define (pop stack)  
;;   (if (null? stack)
;;       (error "cannot pop from empty stack")
;;       (cdr stack)))


;; Queue implementation
(define (new-queue) (cons '() '()))
(define queue-list car)
(define queue-list-set! set-car!)

(define (enqueue! queue val)
  (queue-list-set! queue (cons val (queue-list queue))))

(define (dequeue! queue)
  (define list (queue-list queue))
  (if (empty-queue? queue)
      (raise 'empty-q)
      (let ((queue-head (car (take-right list 1))))
        (queue-list-set! queue (drop-right list 1))
        queue-head)))

(define (empty-queue? q) (not (pair? (queue-list q))))

(define (queue-find-obj? q predicate)
  (exists predicate (queue-list q)))





;; Randomize current mrg's seed
(random-source-randomize! default-random-source)



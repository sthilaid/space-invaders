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

;;;;;;;;;;;;;;;;;;;;;;; symbol operations ;;;;;;;;;;;;;;;;;;;;;;;;

(define (symbol-append s1 . ss)
  (string->symbol (apply string-append
                         (symbol->string s1)
                         (map symbol->string ss))))



;;;;;;;;;;;;;;;;;;;;;;; High Order Functions ;;;;;;;;;;;;;;;;;;;;;;;;

(define (flip f x)
  (lambda (y) (f y x)))

(define (curry-flip f)
  (lambda (x) (lambda (y) ((f y) x))))

;;;;;;;;;;;;;;;;;;;;;;; list operations ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-list number thunk)
  (let loop ((n number) (acc '()))
    (if (> n 0)
        (loop (- n 1) (cons (thunk) acc))
        acc)))

(define (create-vector number thunk)
  (create-vector-param number (lambda (_) (thunk))))

(define (create-vector-param number fn)
  (define v (make-vector number #f))
  (let loop ((n (- number 1)))
    (if (>= n 0)
        (begin
          (vector-set! v n (fn n))
          (loop (- n 1)))
        v)))

(define (rcons x y) (cons y x))

(define (reverse-append l1 . ls)
  (fold-l (lambda (acc l) (append l acc))
          '()
          (cons l1 ls)))

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

;; Loosy error check. Assumes all list have same size and that at
;; least 1 list is used.
(define (multi-fold-l f acc . lists)
  (if (not (pair? (car lists)))
      acc
      (apply multi-fold-l f (apply f acc (map car lists)) (map cdr lists))))

(define (cleanse lst)
  (cond
   ((not (pair? lst)) '())
   ((null? (car lst)) (cleanse (cdr lst)))
   (else (cons (car lst) (cleanse (cdr lst))))))

(define (generic-union comparator l1 l2)
  (let loop ((l1 l1) (acc l2))
    (if (not (pair? l1))
        acc
        (if (generic-member comparator (car l1) l2)
            (loop (cdr l1) acc)
            (loop (cdr l1) (cons (car l1) acc))))))

(define (generic-multi-union comp . ls)
  (fold-l (lambda (acc-set set) (generic-union comp acc-set set))
          '()
          ls))

(define (generic-intersection comparator l1 l2)
  (let loop ((l1 l1) (acc '()))
    (if (not (pair? l1))
        acc
        (if (generic-member comparator (car l1) l2)
            (loop (cdr l1) (cons (car l1 acc)))
            (loop (cdr l1) acc)))))

;; Returns the cartesian products of multiple sets.
;; eg: (cartesian-product '(a b) '(c d) '(e f))
;;     => ((b d e) (b d f) (b c e) (b c f) (a d e) (a d f) (a c e) (a c f))
(define (cartesian-product set1 . sets)
    (if (pair? sets)
        (fold-l (lambda (acc el1)
                  (append (map (lambda (els) (cons el1 els))
                               (apply cartesian-product sets))
                          acc))
                '()
                set1)
        (map list set1)))

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

;; Will return lists of the values resulting in the application of f
;; to each of the lists members
;; eg: (map-values (lambda (x y) (values (+ x y) (* x y))) '(1 2 3) '(4 5 6))
(define (map-values f l . ls)
    (if (not (pair? l))
        'dummy
        (receive (v1 . vs) (apply f (map car (cons l ls)))
          (receive (v1s . vss) (apply map-values f (map cdr (cons l ls)))
            (apply values (map cons (cons v1 vs)
                               (if (pair? (cdr l))
                                   (cons v1s vss)
                                   (map (lambda (x) '()) (cons v1 vs)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Math stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exactisize n)
  (inexact->exact (floor n)))

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


;;; Stack
(define (new-stack) (cons '() #f))
(define stack->list car)
(define (empty-stack? stack) (null? (stack->list stack)))

;; (push! element stack) -> new stack
(define (push! el stack)
  (set-car! stack (cons el (stack->list stack))))

;; (peek stack)         -> top stack element
(define (peek stack) (car (stack->list stack)))

;; (pop! stack)          -> Top stack element and modifies the stack data
(define (pop! stack)
  (if (empty-stack? stack)
      (raise 'empty-stack-exception)
      (let ((top (peek stack)))
        (set-car! stack (cdr (stack->list stack)))
        top)))

;; (pop!? stack          -> Top stack element and modifies the stack data
(define (pop!? stack)
  (with-exception-catcher
   (lambda (e) (case e ((empty-stack-exception) #f) (else (raise e))))
   (lambda () (pop! stack))))



;;; Queue implementation
(define (new-queue) (cons '() '()))
(define queue-list car)
(define queue-list-set! set-car!)
(define (queue-size queue)
  (length (queue-list queue)))

(define (enqueue! queue val)
  (queue-list-set! queue (cons val (queue-list queue))))

(define (dequeue! queue)
  (define list (queue-list queue))
  (if (empty-queue? queue)
      (raise 'empty-q)
      (let ((queue-head (car (take-right list 1))))
        (queue-list-set! queue (drop-right list 1))
        queue-head)))

;; Will return #f if the queue is empty
(define (dequeue!? queue)
  (with-exception-catcher
   (lambda (e) (case e ((empty-q) #f) (else (raise e))))
   (lambda () (dequeue! queue))))

(define (queue-push! queue val)
  (queue-list-set! queue (append (queue-list queue) (list val))))

(define (empty-queue? q) (not (pair? (queue-list q))))

(define (queue-find-obj? q predicate)
  (exists predicate (queue-list q)))



;;; Sets
(define (empty-set) '())
(define (empty-set? set) (eq? set '()))
(define (set-add comp el set)
  (if (generic-member comp el set)
      set
      (cons el set)))
(define (set-remove comp el set)
  (list-remove comp el set))

(define (set-union comp set1 set2)
  (generic-union comp set1 set2))

(define (set-intersection comp set1 set2)
  (generic-intersection comp set1 set2))

(define (set-substract comp set1 set2)
  (fold-l (lambda (acc x) (set-remove comp x acc))
          set1
          set2))

(define (set-element? comp elem set)
  (generic-member comp elem set))

(define (list->set comp list)
  (fold-l (lambda (set el) (set-add comp el set))
          (empty-set)
          list))

;; Randomize current mrg's seed
(random-source-randomize! default-random-source)



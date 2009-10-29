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

(define (curry2 f x)
  (lambda (y) (f x y)))

(define (curry2* f x)
  (lambda y (##apply f (cons x y))))

(define (curry3 f x y)
  (lambda (z) (f x y z)))

(define (curry3* f x y)
  (lambda z (##apply f (cons x (cons y z)))))

(define (flip f x)
  (lambda (y) (f y x)))

(define (curry-flip f x)
  (lambda (y) ((f y) x)))

(define (identity x) x)

(define (compose f . gs)
  (lambda (x) (fold-l (lambda (acc f) (f acc))
                      x
                      (reverse (cons f gs)))))

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

(define (exists pred list . lists)
  (cond
   ((or (not (pair? list))
        (pair? (filter null? lists))) #f)
   ((apply pred (car list) (map car lists))
    (apply values (car list) (map car lists)))
   (else  (apply exists pred (cdr list) (map cdr lists)))))

;; returns the first result of the application of pred to a list
;; element that is not #f.
(define (find-value pred list)
  (cond
   ((not (pair? list)) #f)
   ((pred (car list)) => (lambda (x) x))
   (else  (find-value pred (cdr list)))))

(define (forall pred list . lists)
  (cond
   ((not (pair? list)) #t)
   ((not (apply pred (car list) (map car lists))) #f)
   (else (apply forall pred (cdr list) (map cdr lists)))))

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


(define (quick-sort smaller? equal? greater? lst
                    #!key (accessor identity))
  (let loop ((lst lst))
   (if (or (not (pair? lst))
           (null? (cdr lst)))
       lst
       (let ((pivot (accessor (car lst))))
         (append
          (loop (filter (lambda (x) (smaller? (accessor x) pivot)) lst))
          (filter (lambda (x) (equal? (accessor x) pivot)) lst)
          (loop (filter (lambda (x) (greater? (accessor x) pivot)) lst)))))))

(define (insert-in-ordered-list smaller obj lst #!key (accessor identity))
  (let ((obj-value (accessor obj)))
    (let loop ((lst lst) (acc '()))
      (cond ((not (pair? lst))
             (reverse (cons obj acc)))
            ((smaller obj-value (accessor (car lst)))
             (append (reverse acc) (cons obj lst)))
            (else
             (loop (cdr lst) (cons (car lst) acc)))))))

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

(define (map-with-index f l . ls)
  (let loop ((i 0) (l l) (ls ls) (acc '()))
    (if (not (pair? l))
        (reverse acc)
        (loop (+ i 1) (cdr l) (map cdr ls)
              (cons (apply f i (car l) (map car ls)) acc)))))

(define (for-each-with-index f l . ls)
  (let loop ((i 0) (l l) (ls ls))
    (if (pair? l)
        (begin (apply f i (car l) (map car ls))
               (loop (+ i 1) (cdr l) (map cdr ls))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Math stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fixnum->flonum x)
  (cond ((##fixnum? x) (##fixnum->flonum x))
        ((flonum?   x) x)
        (else       (error "Cannot convert number to flonum: " x))))

(define (1+ x) (+ x 1))
(define (1- x) (- x 1))
(define (one? x) (eq? x 1))

(define (clamp-floor epsilon min-value n)
  (if (>= (- n epsilon) min-value)
      n
      min-value))
(define (clamp-ceil epsilon max-value n)
  (if (<= (+ n epsilon) max-value)
      n
      max-value))
(define (clamp min-value max-value n #!optional (epsilon 0))
  (clamp-ceil epsilon max-value (clamp-floor epsilon min-value n)))

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

(define (create-bounded-simple-moving-avg bound #!key init-value)
  (define data '())
  (define SMA (if init-value init-value 'N/A))
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
(define-type queue id: queue head tail size)
(define-type queue-elem id: queue-elem prev next value)
(define (new-queue) (make-queue #f #f 0))

(define (enqueue! q val)
  (if (empty-queue? q)
      (let ((elem (make-queue-elem 'q-tail 'q-head val)))
        (queue-head-set! q elem)
        (queue-tail-set! q elem))
      (let* ((old-tail (queue-tail q))
             (elem (make-queue-elem 'q-tail old-tail val)))
        (queue-elem-prev-set! old-tail elem)
        (queue-tail-set! q elem)))
  (queue-size-set! q (1+ (queue-size q))))

(define (dequeue! q)
  (let ((qsize (queue-size q)))
    (cond
     ((zero? qsize) #f)
     (else
      (let* ((val (queue-elem-value (queue-head q))))
        (if (one? qsize)
            ;; if getting empty reset the queue
            (begin (queue-head-set! q #f)
                   (queue-tail-set! q #f))
            (let ((new-head (queue-elem-prev (queue-head q))))
              (queue-elem-next-set! new-head 'queue-head)
              (queue-head-set! q new-head)))
        (queue-size-set! q (1- (queue-size q)))
        val)))))

(define (queue-peek q)
  (if (zero? (queue-size q))
      #f
      (queue-elem-value (queue-head q))))


(define (queue-push! q val)
  (if (empty-queue? q)
      (enqueue! q val)
      (let* ((old-head (queue-head q))
             (elem (make-queue-elem old-head 'q-head val)))
        (queue-elem-next-set! old-head elem)
        (queue-head-set! q elem)
        (queue-size-set! q (1+ (queue-size q))))))

(define (empty-queue? q) (zero? (queue-size q)))

(define (queue-abstract-foldl it f acc el)
  (if (not (queue-elem? el))
      acc
      (queue-abstract-foldl it f (f acc (queue-elem-value el)) (it el))))
(define (queue-foldl f acc q)
  (queue-abstract-foldl queue-elem-prev f acc (queue-head q)))
(define (queue-rfoldl f acc q)
  (queue-abstract-foldl queue-elem-next f acc (queue-tail q)))

(define (queue->list q) (queue-foldl rcons '() q))

(define (queue-find-and-remove! pred q)
  (define (find-n-rem it pred el)
   (cond
    ((not (queue-elem? el)) #f)
    ((pred (queue-elem-value el))
     (let ((val  (queue-elem-value el))
           (prev (queue-elem-prev el))
           (next (queue-elem-next el)))
       (if (queue-elem? prev)
           (queue-elem-next-set! prev next)
           (queue-tail-set! q next))
       (if (queue-elem? next)
           (queue-elem-prev-set! next prev)
           (queue-head-set! q prev))
       (queue-size-set! q (1- (queue-size q)))
       val))
    (else (find-n-rem it pred (it el)))))

  (if (> (queue-size q) 0)
      (find-n-rem queue-elem-prev pred (queue-head q))
      #f))




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

(define (make-set . values)
  (list->set eq? values))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using the usual terminology: m rows by n columns
(define (make-matrix2d-with-index m n #!optional (init-value (lambda (i j) 0)))
  (include "scm-lib_.scm")
  (let ((row-container (make-vector m)))
    (for i 0 (< i m)
         (let ((row-vector (make-vector n)))
           (for j 0 (< j n) (vector-set! row-vector j (init-value i j)))
           (vector-set! row-container i row-vector)))
    row-container))

(define (make-matrix2d m n #!optional (init-value 0))
  (make-matrix2d-with-index m n (lambda (i j) init-value)))

(define (matrix2d-set! mat i j value)
  (vector-set! (vector-ref mat i) j value))

(define (matrix2d-get mat i j)
  (vector-ref (vector-ref mat i) j))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RNG seed randomization, !! must be at the end of this file !!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Randomize current rng's seed
(random-source-randomize! default-random-source)

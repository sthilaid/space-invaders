;;               Scheme library used for Assignement #1
;;                      for the course comp-521
;;
;;                        by David St-Hilaire
;;                            winter 2008
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


;;;;;;;;;;;;;;;;;;;;;;; list operations ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-member comparator el list)
  (cond
   ((not (pair? list)) #f)
   ((comparator el (car list)) list)
   (else (generic-member comparator el (cdr list)))))

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
  (let loop ((list list) (acc #f))
    (if (not (pair? list))
        acc
        (loop (cdr list) (or acc (pred (car list)))))))

(define (forall pred list)
  (let loop ((list list) (acc #t))
    (if (not (pair? list))
        acc
        (loop (cdr list) (and acc (pred (car list)))))))

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

(define maximum (extremum-fonction > -inf.0))
(define minimum (extremum-fonction < +inf.0))

(define (average sample)
  (/ (apply + sample) (length sample)))

(define (variance sample)
  (define mean (average sample))
  (define N (length sample))
  (/ (fold-l (lambda (acc n) (+ (expt (- n mean) 2) acc))
             0
             sample)
     N))

(define (standard-deviation sample) (sqrt (variance sample)))

(define (complex-conjugate z)
  (make-rectangular (real-part z) (- (imag-part z))))

(define pi (* 2 (asin 1)))


;;;;;;;;;;;;;;;;;;;;;; Boolean operation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (xor b1 b2) `(not (eq? ,b1 ,b2)))
  


;;;;;;;;;;;;;;;;;;;;;;;; Simple macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (for var init-val condition true . false)
  (let ((loop (gensym 'loop)))
    `(let ,loop ((,var ,init-val))
          (if ,condition
              (begin ,true (,loop (+ ,var 1)))
              ,(if (not (null? false))
                   false)))))

(define-macro (make-vector2d height width . init-val)
  (let ((vector-sym (gensym 'vector-sym))
        (row-sym (gensym 'row-sym)))
    `(let ((,vector-sym (make-vector ,height #f)))
       (for ,row-sym 0 (< ,row-sym ,height)
            (vector-set! ,vector-sym ,row-sym
                         (make-vector ,width ,(if (pair? init-val)
                                                  (car init-val)
                                                  #f))))
       ,vector-sym)))

(define-macro (vector2d-ref vector row col)
  `(vector-ref (vector-ref ,vector ,row) ,col))

(define-macro (vector2d-set! vector row col val)
  `(vector-set! (vector-ref ,vector ,row) ,col ,val))

(define-macro (make-vector3d i-length j-length k-length . init-val)
  (let ((vector-sym (gensym 'vector-sym))
        (i-sym (gensym 'i-sym))
        (j-sym (gensym 'j-sym)))
    `(let ((,vector-sym (make-vector2d ,i-length ,j-length #f)))
       (for ,i-sym 0 (< ,i-sym ,i-length)
            (for ,j-sym 0 (< ,j-sym ,j-length)
                 (vector2d-set! ,vector-sym ,i-sym ,j-sym
                                (make-vector ,k-length ,(if (pair? init-val)
                                                            (car init-val)
                                                            #f)))))
       ,vector-sym)))

(define-macro (vector3d-ref vector i j k)
  `(vector-ref (vector2d-ref ,vector ,i ,j) ,k))

(define-macro (vector3d-set! vector i j k val)
  `(vector-set! (vector2d-ref ,vector ,i ,j) ,k ,val))

; Randomize current mrg's seed
(random-source-randomize! default-random-source)
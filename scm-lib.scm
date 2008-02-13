(define (minimum a b) (if (<= a b) a b))

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
   ((pred (car list)) #t)
   (else (exists pred (cdr list)))))

(define (forall pred list)
  (cond
   ((not (pair? list)) #t)
   ((pred (car list)) (forall pred (cdr list)))
   (else #f)))

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
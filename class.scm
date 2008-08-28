
(define-macro (init)
  (eval '(define index 1))
  (eval '(define class-table (make-table)))
  (eval '(define (next-index) (set! index (+ index 1)) index))
  (eval '(define (make-class-descriptor super fields-indices)
           (cons super fields-indices)))
  `(begin
     (define runtime-class-table (make-table))))

(define-macro (define-class name supers fields)
  (define obj (gensym 'obj))
  (define val (gensym 'val))
  (define (gen-accessor-name class-name var)
    (symbol-append class-name '- var))
  (define (gen-setter-name class-name var)
    (symbol-append class-name '- var '-set!))


  (define (gen-accessors field-indices)
    (if (not (pair? field-indices))
        '()
        (let ((field (caar field-indices))
              (index (cdar field-indices)))
         (cons `(define (,(gen-accessor-name name field) ,obj)
                  (vector-ref ,obj
                              (vector-ref (vector-ref ,obj 0) ,index)))
               (gen-accessors (cdr field-indices))))))
  
  (define (gen-setters field-indices)
    (if (not (pair? field-indices))
        '()
        (let ((field (caar field-indices))
              (index (cdar field-indices)))
          (cons `(define (,(gen-setter-name name field) ,obj ,val)
                   (vector-set! ,obj
                                (vector-ref (vector-ref ,obj 0) ,index)
                                ,val))
               (gen-setters (cdr field-indices))))))

  (define (gen-descriptor field-indices)
    (define-macro (instance-index++)
      (define i (gensym 'i))
      `(let ((,i instance-index))
         (set! instance-index (+ instance-index 1))
         ,i))
    (let ((instance-index 1)
          (desc (make-vector
                 (+ (cdar (take-right ordered-lst 1)) 1)
                 'absent-field)))
      (for-each (lambda (i) (vector-set! desc i (instance-index++)))
                (map cdr ordered-lst))
      desc))
  
  (define (gen-instantiator field-indices)
    (define obj (gensym 'obj))
    `(define (,(symbol-append 'make- name) ,@(map car ordered-lst))
       #;
       (let ((,obj (make-vector
       ,(let ((class-desc (table-ref class-table name)))
       ;; must add one pointer in the instance to
       ;; point on the runtime class descriptor
       (+ (vector-ref class-desc
       (- (vector-length class-desc) 1))
       1))
       'unbound)))
       ;; setup the class-desc pointer into the instance
       (vector-set! ,obj 0 ',(table-ref class-table name))
       ,@(map (lambda (var) `(,(gen-setter-name name var) ,obj ,var))
       (map car field-indices))
       ,obj)
       (vector ',(table-ref class-table name)
               'TODOOOOOOOOOOOOOOOO))))

  (define-macro (indice-comp op)
      (let ((x (gensym 'x)) (y (gensym 'y)))
        `(lambda (,x ,y) (,op (cdr ,x) (cdr ,y)))))

  (define (sort-field-indices field-indices)
    (quick-sort (indice-comp <) (indice-comp =) (indice-comp >)
                field-indices))
  
  (include "scm-lib.scm")

  (let* ((field-indices
          (sort-field-indices (map (lambda (field) (cons field (next-index)))
                                   fields)))
         (class-desc (gen-descriptor field-indices)))
    (table-set! class-table name class-desc)
    `(begin ,@(gen-accessors field-indices)
            ,@(gen-setters field-indices)
            ,(gen-instantiator field-indices)
            (table-set! runtime-class-table ',class-desc))))

(init)
(pp (lambda () (define-class A (B C) (x y z)) 'toto))
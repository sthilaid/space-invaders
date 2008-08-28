
(define-macro (init)
  (eval '(define desc-index -1))
  (eval '(define class-table (make-table)))
  (eval '(define (next-desc-index)
           (set! desc-index (+ desc-index 1)) desc-index))
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

  ;; field-indices are expected to be sorted from lower index to
  ;; highest index
  (define (gen-descriptor field-indices)
    (define-macro (instance-index++)
      (define i (gensym 'i))
      `(let ((,i instance-index))
         (set! instance-index (+ instance-index 1))
         ,i))
    (let ((instance-index 1)
          (desc (make-vector
                 (+ (cdar (take-right field-indices 1)) 1)
                 'absent-field)))
      (for-each (lambda (i) (vector-set! desc i (instance-index++)))
                (map cdr field-indices))
      desc))

  ;; field-indices are expected to be sorted from lower index to
  ;; highest index
  (define (gen-instantiator field-indices)
    (define obj (gensym 'obj))
    `(define (,(symbol-append 'make- name) ,@(map car field-indices))
       (vector ',(table-ref class-table name)
               ,@(map car field-indices))))

  (define (sort-field-indices field-indices)
    (define-macro (indice-comp op)
      (let ((x (gensym 'x)) (y (gensym 'y)))
        `(lambda (,x ,y) (,op (cdr ,x) (cdr ,y)))))
    (quick-sort (indice-comp <) (indice-comp =) (indice-comp >)
                field-indices))
  
  (include "scm-lib.scm")

  (let* ((field-indices
          (sort-field-indices
           (map (lambda (field) (cons field (next-desc-index)))
                fields)))
         (class-desc (gen-descriptor field-indices)))
    (table-set! class-table name class-desc)
    `(begin ,@(gen-accessors field-indices)
            ,@(gen-setters field-indices)
            ,(gen-instantiator field-indices)
            (table-set! runtime-class-table ',class-desc))))

(init)
(pp (lambda () (define-class A () (a))))
(pp (lambda () (define-class B (A) (b))))
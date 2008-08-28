(include "scm-lib-macro.scm")

(define-macro (init)
  (eval
   '(begin (define desc-index -1)
           (define class-table (make-table test: eq?))
           (define (next-desc-index)
             (set! desc-index (+ desc-index 1))
             desc-index)
           (define (make-class-info field-indices descriptor)
             (vector field-indices descriptor))
           (define (class-info-fi info) (vector-ref info 0))
           (define (class-info-desc info) (vector-ref info 1)))))

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
       (vector ',(class-info-desc (table-ref class-table name))
               ,@(map car field-indices))))

  (define (sort-field-indices field-indices)
    (define-macro (indice-comp op)
      (let ((x (gensym 'x)) (y (gensym 'y)))
        `(lambda (,x ,y) (,op (cdr ,x) (cdr ,y)))))
    (quick-sort (indice-comp <) (indice-comp =) (indice-comp >)
                field-indices))
  
  (include "scm-lib.scm")

  (let ((temp-field-table (make-table test: eq?)))
    (for-each
     (lambda (super)
       (let ((super-field-indices
              (cond
               ((table-ref class-table super #f) =>
                (lambda (i) (class-info-fi i)))
               (else (error (to-string
                             (show "Inexistant super class: " super)))))))
         (for-each
          (lambda (field-index)
            (if (not (table-ref temp-field-table (car field-index) #f))
                (table-set! temp-field-table
                            (car field-index) (cdr field-index))
                (error
                 (to-string
                  (show "Field already defined: " (car field-index))))))
          super-field-indices)))
     supers)

    (for-each (lambda (field)
                ;; If a field is already provided by a super class
                ;; then the super class's is used ...
                (if (not (table-ref temp-field-table field #f))
                    (table-set! temp-field-table field (next-desc-index))))
              fields)

    (let* ((field-indices (sort-field-indices (table->list temp-field-table)))
           (class-desc (gen-descriptor field-indices)))

      (table-set! class-table name (make-class-info field-indices class-desc))
      `(begin ,@(gen-accessors field-indices)
              ,@(gen-setters field-indices)
              ,(gen-instantiator field-indices)))))


(init)

(define-class A () (a))
(define-class B (A) (b))
(define-class C () (c))
(define-class D () (d))
(define-class E (B C D) (e))

(let ((obj (make-E 1 2 3 4 5)))
  (pp (A-a obj))
  (pp (E-a obj))
  (pp (E-e obj)))

(define-class A () (a))
(define-class B (A) (b))
(define-class C () (c))
(define-class D (B C) (a))


(let ((obj (make-D 1 2 3)))
  (pp (A-a obj))
  (pp (D-a obj))
  (pp (B-a obj)))
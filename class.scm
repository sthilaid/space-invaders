
;; Initializes the global define-class macro-expension-time
;; environnment. This macro must be called
(define-macro (init)
  (eval
   '(begin
      ;; starts to 2 because 0/1 are reserved for the class id and supers
      (define desc-index 1) 
      (define class-table (make-table test: eq?))
      (define meth-table (make-table test: eq?))
      (define (next-desc-index)
        (set! desc-index (+ desc-index 1))
        desc-index)
      (define (make-class-info field-indices descriptor)
        (vector field-indices descriptor))
      (define (class-info-fi info) (vector-ref info 0))
      (define (class-info-desc info) (vector-ref info 1))
      (define (meth-name sign) (if (not (list? sign))
                                   (error 'bad-signature-syntax)
                                   (car sign)))
      (define any-type 'any-type)

      (define (symbol-append s1 . ss)
          (string->symbol (apply string-append
                                 (symbol->string s1)
                                 (map symbol->string ss))))
      (define (gen-method-desc-name sign)
        (symbol-append (meth-name sign) '-meth-desc))

      (define (gen-method-table-name name)
        (symbol-append name '-meth-table))

      (define (make-class-desc id supers num-fields)
        (let ((desc (make-vector (+ num-fields 2) 'unknown-slot)))
          (vector-set! desc 0 id)
          (vector-set! desc 1 supers)
          desc))
      (define (class-desc-id desc) (vector-ref desc 0))
      (define (class-desc-supers desc) (vector-ref desc 1))
      (define (class-desc-indices-vect desc) (vector-ref desc 2))

      (define make-method vector) ; (make-method id types body)
      (define (method-id meth) (vector-ref meth 0))
      (define (method-types meth) (vector-ref meth 1))
      (define (method-body meth) (vector-ref meth 2)))))

(define-macro (define-class name supers . fields) 
  (define temp-field-table (make-table test: eq?))
  
  (define obj (gensym 'obj))
  (define val (gensym 'val))
  (define (gen-accessor-name class-name var)
    (symbol-append class-name '- var))
  (define (gen-setter-name class-name var)
    (symbol-append class-name '- var '-set!))
  (define (class-desc-name)
    (symbol-append name '-class-descriptor))

  (define (make-slot type index) (cons type index))
  (define (is-class-slot? slot-info)
    (and (pair? slot-info)
         (eq? (car slot-info) class:)))
  (define (is-instance-slot? slot-info)
    (and (pair? slot-info)
         (eq? (car slot-info) instance:)))
  (define (slot-index slot-info)
    (cdr slot-info))

  ;; todo
  ;; puts the fields into the temp table. The class fields MUST be
  ;; processed AFTER that the super class's fields where processed.
  (define (process-field! field)
    (cond
     ((and (list? field)         ; FIXME: should this test be removed?
           (= (length field) 2)
           (eq? (car field) slot:))
      (let ((slot-name (cadr field)))
        ;; If a field is already provided by a super class
        ;; then the super class's is used ...
        (if (not (table-ref temp-field-table slot-name #f))
            (table-set! temp-field-table
                        slot-name
                        (make-slot instance: (next-desc-index))))))
     ((and (list? field)         ; FIXME: should this test be removed?
           (= (length field) 2)
           (eq? (car field) class-slot:))
      (let ((slot-name (cadr field)))
        (if (not (table-ref temp-field-table slot-name #f))
            (table-set! temp-field-table
                        slot-name
                        (make-slot class: (next-desc-index))))))))

  (define (gen-accessors field-indices)
    (define (gen-accessor field slot-info)
      (cond
       ((is-class-slot? slot-info)
        (let ((index (slot-index slot-info)))
          `(define (,(gen-accessor-name name field))
             (vector-ref ,(class-desc-name) ,index))))

       ((is-instance-slot? slot-info)
        (let ((index (slot-index slot-info)))
          `(define (,(gen-accessor-name name field) ,obj)
             (vector-ref ,obj
                         (vector-ref (vector-ref ,obj 0) ,index)))))))
    ;; Generate a list of all the accesssors
    (if (not (pair? field-indices))
        '()
        (let ((field (caar field-indices))
              (index (cdar field-indices)))
          (cons (gen-accessor field index)
                (gen-accessors (cdr field-indices))))))
  
  (define (gen-setters field-indices)
    (define (gen-setter field slot-info)
      (cond
       ((is-class-slot? slot-info)
        (let ((index (slot-index slot-info)))
          `(define (,(gen-setter-name name field) ,val)
             (vector-set! ,(class-desc-name) ,index ,val))))
       
       ((is-instance-slot? slot-info)
        (let ((index (slot-index slot-info)))
          `(define (,(gen-setter-name name field) ,obj ,val)
             (vector-set! ,obj
                          (vector-ref (vector-ref ,obj 0) ,index)
                          ,val))))))
    ;; Generate a list of all the setters
    (if (not (pair? field-indices))
        '()
        (let ((field (caar field-indices))
              (index (cdar field-indices)))
          (cons (gen-setter field index)
                (gen-setters (cdr field-indices))))))

  ;; field-indices are expected to be sorted from lower index to
  ;; highest index
  (define (gen-descriptor field-indices)
    (define-macro (instance-index++)
      (define i (gensym 'i))
      `(let ((,i instance-index))
         (set! instance-index (+ instance-index 1))
         ,i))
    (let* ((instance-index 1)
           (desc (make-class-desc (gensym name) supers
                                  (+ (slot-index
                                      (cdar (take-right field-indices 1)))
                                     1))))
      (for-each (lambda (i) (vector-set! desc i (instance-index++)))
                (map (lambda (field-index) (slot-index (cdr field-index)))
                     field-indices))
      desc))

  ;; field-indices are expected to be sorted from lower index to
  ;; highest index
  (define (gen-instantiator field-indices)
    (define obj (gensym 'obj))
    (define instance-field-indices
      (filter (lambda (field-index)
                (is-instance-slot? (cdr field-index)))
              field-indices))
    `(begin
       ;; Class descriptor is put in a global var
       (define ,(class-desc-name)
         ',(class-info-desc (table-ref class-table name)))
       (define (,(symbol-append 'make- name) ,@(map car instance-field-indices))
         (vector ,(class-desc-name)
                 ,@(map car instance-field-indices)))))

  (define (sort-field-indices field-indices)
    (define-macro (indice-comp op)
      (let ((x (gensym 'x)) (y (gensym 'y)))
        `(lambda (,x ,y) (,op (slot-index (cdr ,x))
                              (slot-index (cdr ,y))))))
    (quick-sort (indice-comp <) (indice-comp =) (indice-comp >)
                field-indices))

  (include "scm-lib-macro.scm")
  (include "scm-lib.scm")

  ;; Process the super classes' slots
  (for-each
   (lambda (super)
     (let ((super-field-indices
            (cond
             ((table-ref class-table super #f) =>
              (lambda (desc) (class-info-fi desc)))
             (else (error
                    (to-string
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

  ;; Process this class's slots
  (for-each (lambda (field)
              ;; If a field is already provided by a super class
              ;; then the super class's is used ...
              (process-field! field))
            fields)
  (let* ((field-indices (sort-field-indices (table->list temp-field-table)))
         (class-desc (gen-descriptor field-indices)))

    (table-set! class-table name (make-class-info field-indices class-desc))
    `(begin ,@(gen-accessors field-indices)
            ,@(gen-setters field-indices)
            ,(gen-instantiator field-indices))))

(define-macro (define-generic signature)
  (define name (meth-name signature))
  (define (parse-arg arg)
    (cond ((and (list? arg) (symbol? (car arg)) (symbol? (cadr arg)))
           (car arg))
          (else arg)))
  (define (args) (map parse-arg (cdr signature)))
  (table-set! meth-table name '()) ; <- wouldnt work in lisp hehe
  `(begin
     (define ,(gen-method-table-name name) (make-table test: equal?))
     (define (,name ,@(args))
       (define (get-types arg)
         (class-desc-id (vector-ref arg 0)))
       (let ((types (map get-types (list ,@(args)))))
         ((table-ref ,(gen-method-table-name name) types)
          ,@(args))))))

;; FIXME: VERY BAD object verification..
;; (define-macro (get-class obj)
;;   `(if (and (vector? obj) (vector? (vector-ref obj 0)))
;;        (vector-ref obj 0)
;;        any-type))

(define-macro (define-method signature bod . bods)
  (define (name) (meth-name signature))
  (define unknown-meth-error 'unknown-meth)
  (define (parse-arg arg)
    (cond ((and (list? arg) (symbol? (car arg)) (symbol? (cadr arg)))
           (let ((var (car arg))
                 (type (class-desc-id
                        (class-info-desc
                         (table-ref class-table (cadr arg))))))
             (values var type)))
          (else (values arg any-type))))
  ;; Returns 2 values: the ordrered list of arguments and the ordered
  ;; list of their types.
  (define (parse-args args) (map-values parse-arg args))
  (include "scm-lib-macro.scm")
  (include "scm-lib.scm")
  
  (with-exception-catcher
   (lambda (e)
     (pp `(received error: ,e))
     (if (eq? e unknown-meth-error)
         (error (to-string (show "Generic method was not defined: " (name))))
         (raise e)))
   (lambda ()
     (cond
      ((table-ref meth-table (name) #f) =>
       (lambda (current-meth-data)
         (receive (args types) (parse-args (cdr signature))
          (table-set! meth-table
                      (name)
                      (cons (make-method (name) types
                                         `(lambda ,args ,bod ,@bods))
                            current-meth-data))
          `(table-set! ,(gen-method-table-name (name))
                       ',types
                       (lambda ,args ,bod ,@bods)))))
           (else (raise unknown-meth-error))))))

(define-macro (polymorphize-methods!)
  (define (is-subclass? class-id super-id)
    (or (eq? class-id super-id)
        (exists (lambda (class-super) (is-subclass? class-super super-id))
                (class-super (class-info-desc
                              (table-ref class-table class-id))))))
  (define (find-sub-classes class-id)
    ))

(init)



;;; Runtime stuff ;;;
(include "scm-lib-macro.scm")
(include "test-macro.scm")
(load "scm-lib.scm")
(load "test.scm")



(define-test simple-instance-slots "aaabbccdde" 'ok
  (define-class A ()      (slot: a))
  (define-class B (A)     (slot: b))
  (define-class C ()      (slot: c))
  (define-class D ()      (slot: d))
  (define-class E (B C D) (slot: e))
  (let ((obj (make-E 'a 'b 'c 'd 'e)))
    (display (A-a obj))
    (display (B-a obj))
    (display (E-a obj))
    (display (B-b obj))
    (display (E-b obj))
    (display (C-c obj))
    (display (E-c obj))
    (display (D-d obj))
    (display (E-d obj))
    (display (E-e obj)))
  'ok)

(define-test simple-class-slots "123344" 'ok
  (define-class A () (slot: a) (class-slot: b))
  (let ((obj1 (make-A 1))
        (obj2 (make-A 2)))
    (A-b-set! 3)
    (display (A-a obj1))
    (display (A-a obj2))
    (display (A-b))
    (display (A-b))
    (A-b-set! 4)
    (display (A-b))
    (display (A-b)))
  'ok)

(define-test test-generic-simple "10dix" 'ok
  (define-class A () (slot: a))
  (define-class B () (slot: b))
  (define-generic (test obj))
  (define-method (test (a A)) (number->string (A-a a)))
  (define-method (test (b B)) (symbol->string (B-b b)))

  (display (test (make-A 10)))
  (display (test (make-B 'dix)))
  'ok)

;; (pp (lambda () (define-class A () (slot: a) (class-slot: b)) 'blu))
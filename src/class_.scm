;; utilitary macros for the object system


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro expansion time env
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initializes the global define-class macro-expension-time
;; environnment. This macro must be called

(define-macro (init)
  ;; macro exp time librairy
  (eval
   '(begin
      (include "include/scm-lib_.scm")
      (load "src/scm-lib.scm") ; to avoid mult load of the lib
      
      ;; method expansion mode
      (define mode 'iterative) ; iterative as default
      
      ;; starts to 2 because 0/1 are reserved for the class id and supers
      (define desc-index 1) 
      (define mt-class-table (make-table test: eq?))
      (define mt-meth-table (make-table test: eq?))
      (define (next-desc-index)
        (set! desc-index (+ desc-index 1))
        desc-index)
      
      (define (meth-name sign) (if (not (list? sign))
                                   (error 'bad-signature-syntax)
                                   (car sign)))

      (define any-type '*)
      (define (any-type? ty) (eq? ty any-type))
      (define (external-object? obj) (not (instance-object? obj)))

      (define match-type match-value:)
      (define (make-match-type val) (list match-type val))
      (define (match-type? obj) (and (list? obj) (eq? (car obj) match-type)))
      (define match-type-value cadr)

      (define match-member match-member:)
      (define (make-match-mem-type class member value)
        (list 'list
              match-member:
              `(quote ,class)
              `(lambda (obj)
                 (equal? (,(gen-accessor-name class member) obj)
                         (quote ,value)))))
      (define (match-member-type? ty) (and (list? ty)
                                           (eq? (car ty) match-member)))
      (define (match-member-class ty) (cadr ty))
      (define (match-member-predicate ty) (caddr ty))

      (define or-type 'or)
      (define (make-or-type . types) (cons or-type types))
      (define (or-type? ty) (and (list? ty) (eq? (car ty) or-type)))
      (define or-type-types cdr)

      (define and-type 'and)
      (define (make-and-type . types) (cons and-type types))
      (define (and-type? ty) (and (list? ty) (eq? (car ty) and-type)))
      (define and-type-types cdr)

      (define (symbol-append s1 . ss)
          (string->symbol (apply string-append
                                 (symbol->string s1)
                                 (map symbol->string ss))))

      ;;;;;;;;;;;;;;; Naming convention abstractions ;;;;;;;;;;;;;;;
      (define (gen-accessor-name class-name var)
        (symbol-append class-name '- var))
      (define (gen-setter-name class-name var)
        (symbol-append class-name '- var '-set!))
      (define (gen-predicate-name class-name)
        (symbol-append class-name '?))
      (define (gen-instantiator-name name)
        (symbol-append 'make- name '-instance))

      (define (class-desc-name  class-name)
        (symbol-append class-name '-class-descriptor))
      
      (define (gen-method-desc-name sign)
        (symbol-append (meth-name sign) '-meth-desc))

      (define (gen-method-table-name name)
        (symbol-append name '-meth-table))
      

      ;;;;;;;;;;;;;;; Data structure used ;;;;;;;;;;;;;;;

      (define (make-class-info field-indices descriptor)
        (vector field-indices descriptor))
      (define (class-info-fi info) (vector-ref info 0))
      (define (class-info-desc info) (vector-ref info 1))
      
      (define (make-class-desc id supers num-fields)
        ;; add 2 to include place holders for the id and supers
        (let ((desc (make-vector (+ num-fields 2) 'unknown-slot))
              (all-supers
               (apply generic-multi-union eq?
                      supers
                      (map (lambda (s) (class-desc-supers
                                        (class-info-desc
                                         (table-ref mt-class-table s))))
                           supers))))
          (vector-set! desc 0 id)
          (vector-set! desc 1 all-supers)
          desc))
      (define (class-desc-id desc) (vector-ref desc 0))
      (define (class-desc-supers desc) (vector-ref desc 1))
      (define (class-desc-indices-vect desc) (vector-ref desc 2))


      (define (make-slot type index options inherited?)
        (vector type index options inherited?))
      (define (is-class-slot? slot-info)
        (and (vector? slot-info)
             (eq? (slot-type slot-info) class-slot:)))
      (define (is-instance-slot? slot-info)
        (and (vector? slot-info)
             (eq? (slot-type slot-info) slot:)))
      (define (slot-type slot-info)
        (vector-ref slot-info 0))
      (define (slot-index slot-info)
        (vector-ref slot-info 1))
      (define (slot-options slot-info)
        (vector-ref slot-info 2))
      (define (slot-inherited? slot-info)
        (vector-ref slot-info 3))

      ;; returns the slot hooks, if any is present.
      (define (slot-read-hooks? slot-info)
        (let* ((options (slot-options slot-info))
               (hooks (assq read-hooks: options)))
          (if hooks (cdr hooks) #f)))
      (define (slot-write-hooks? slot-info)
        (let* ((options (slot-options slot-info))
               (hooks (assq write-hooks: options)))
          (if hooks (cdr hooks) #f)))
        
      (define (make-mt-generic-function name args)
        (vector name args (make-table test: equal?)))
      (define (mt-generic-function-name gf) (vector-ref gf 0))
      (define (mt-generic-function-args gf) (vector-ref gf 1))
      (define (mt-generic-function-instances gf) (vector-ref gf 2))
      (define (mt-generic-function-instances-add! gf instance)
        (table-set! (mt-generic-function-instances gf)
                    (method-types instance)
                    instance))
      (define (mt-generic-function-instances-list gf)
        (table->list (mt-generic-function-instances gf)))
      (define (mt-generic-function-get-instance gf types)
        (table-ref (mt-generic-function-instances gf) types #f))
      (define (mt-generic-function-instances-number gf)
        (table-length (mt-generic-function-instances gf)))
      
      
      (define make-method vector) ; (make-method id types body)
      (define (method-id meth) (vector-ref meth 0))
      (define (method-types meth) (vector-ref meth 1))
      (define (method-body meth) (vector-ref meth 2)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic constructors (new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (new class-name . params)
  `(init! (,(gen-instantiator-name class-name)) ,@params))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (update! obj class field f)
  (let ((objval (gensym 'objval))
        (newval (gensym 'newval)))
   `(let* ((,objval ,obj)
           (,newval (,f (,(gen-accessor-name class field) ,objval))))
      (,(gen-setter-name class field) ,objval ,newval)
      ,newval)))

(define-macro (set-fields! obj class field-val-list)
  (let ((obj-ptr (gensym 'obj)))
    `(let ((,obj-ptr ,obj))
       ,@(map (lambda (field-val)
                (if (not (and (list? field-val)
                              (= (length field-val) 2)))
                    (error "invalid set-fields! field syntax"))
                (let ((field-name (car field-val))
                      (val        (cadr field-val)))
                  `(,(gen-setter-name class field-name) ,obj-ptr ,val)))
              field-val-list)
       ,obj-ptr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro exp time env setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(init)
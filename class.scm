;; Very simple object system which focuses on runtime speed.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro expansion time env
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initializes the global define-class macro-expension-time
;; environnment. This macro must be called
(define-macro (init)
  ;; macro exp time librairy
  (eval
   '(begin
      (include "scm-lib-macro.scm")
      (include "scm-lib.scm")
      
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
      (define any-type 'any-type)

      (define (symbol-append s1 . ss)
          (string->symbol (apply string-append
                                 (symbol->string s1)
                                 (map symbol->string ss))))

      ;; Returns the super-class id, if found.
      (define (macro-is-subclass? class-id super-id)
        ;; Warning: This might return true even if its not a subclass if
        ;; an old superclass as been redefined...
        (and (not (eq? class-id 'any-type))
             (or (and (eq? class-id super-id) class-id)
                 (memq super-id
                       (class-desc-supers
                        (class-info-desc
                         (table-ref mt-class-table class-id))))
                 (eq? super-id any-type))))


      ;;;;;;;;;;;;;;; Naming convention abstractions ;;;;;;;;;;;;;;;
      (define (gen-accessor-name class-name var)
        (symbol-append class-name '- var))
      (define (gen-setter-name class-name var)
        (symbol-append class-name '- var '-set!))
      (define (gen-predicate-name class-name)
        (symbol-append class-name '?))
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


      (define (make-slot type index options) (vector type index options))
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
;; define-class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-class name supers . fields) 
  (define temp-field-table (make-table test: eq?))
  
  (define obj (gensym 'obj))
  (define val (gensym 'val))

  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  (define-macro (to-string e1 . es)
    `(with-output-to-string "" (lambda () ,e1 ,@es)))

  ;; puts the fields into the temp table. The class fields MUST be
  ;; processed AFTER that the super class's fields where processed.
  (define (process-field! field)
    (cond
     ((and (list? field)         ; FIXME: should this test be removed?
           (>= (length field) 2))
      (let ((slot-type (case (car field)
                         ((slot: class-slot:) (car field))
                         (else (error (to-string (show "Bad slot type: "
                                                       (car field)))))))
            (slot-name (cadr field))
            (slot-options (cddr field)))
        ;; If a field is already provided by a super class
        ;; then the super class's is used ...
        (if (not (table-ref temp-field-table slot-name #f))
            (table-set! temp-field-table
                        slot-name
                        (make-slot slot-type
                                   (next-desc-index)
                                   slot-options)))))
     (else (error (to-string (show "ill-formed slot declaration: "
                                   field))))))

  (define (gen-accessors field-indices)
    (define (gen-accessor field slot-info)
      (let* ((index (slot-index slot-info))
             (read-hooks (slot-read-hooks? slot-info)))
        (cond ((is-class-slot? slot-info)
               (let ((hook-call `(hook slot-value))
                     (direct-slot-access
                      `(vector-ref ,(class-desc-name name) ,index))
                     (indirect-slot-access
                            `(vector-ref (instance-class-descriptor ,obj)
                                         ,index)))
                 ;; An optional argument which must be an instance can
                 ;; be given. If this argument is present, then a
                 ;; dynamic fetch of the class field will be made.
                 `(define (,(gen-accessor-name name field) #!optional (,obj #f))
                    (let ((slot-access
                           (if ,obj ,indirect-slot-access ,direct-slot-access)))
                     ,(if read-hooks
                          `(let ((slot-value slot-access))
                             (for-each (lambda (hook) ,hook-call)
                                       (list ,@read-hooks))
                             slot-value)
                          'slot-access)))))
              ((is-instance-slot? slot-info)
               (let ((hook-call `(hook ,obj slot-value))
                     (slot-access
                      `(vector-ref ,obj
                                   (vector-ref (instance-class-descriptor ,obj)
                                               ,index))))
                 `(define (,(gen-accessor-name name field) ,obj)
                    ,(if read-hooks
                         `(let ((slot-value ,slot-access))
                            (for-each (lambda (hook) ,hook-call)
                                      (list ,@read-hooks))
                            slot-value)
                         slot-access)))))))
    ;; Generate a list of all the accesssors
    (if (not (pair? field-indices))
        '()
        (let ((field (caar field-indices))
              (index (cdar field-indices)))
          (cons (gen-accessor field index)
                (gen-accessors (cdr field-indices))))))
  
  (define (gen-setters field-indices)
    (define (gen-setter field slot-info)
      (let* ((index (slot-index slot-info))
             (write-hooks (slot-write-hooks? slot-info)))
        (cond ((is-class-slot? slot-info)
               (let ((hook-call `(hook ,val))
                     (direct-slot-set!
                      `(vector-set! ,(class-desc-name name) ,index ,val))
                     (indirect-slot-set!
                      `(vector-set! (instance-class-descriptor ,obj)
                                    ,index ,val)))
                 `(define (,(gen-setter-name name field)
                           ,obj
                           #!optional (val? #f))
                    (let* ((,val (if val? val? ,obj))
                           (slot-set! (if val?
                                          ,indirect-slot-set!
                                          ,direct-slot-set!)))
                      ,(if write-hooks
                           `(begin (for-each (lambda (hook) ,hook-call)
                                             (list ,@write-hooks))
                                   slot-set!)
                           'slot-set!)))))
              ((is-instance-slot? slot-info)
               (let ((hook-call `(hook ,obj ,val))
                     (slot-set!
                      `(vector-set!
                        ,obj
                        (vector-ref (instance-class-descriptor ,obj) ,index)
                        ,val)))
                 `(define (,(gen-setter-name name field) ,obj ,val)
                    ,(if write-hooks
                         `(begin (for-each (lambda (hook) ,hook-call)
                                           (list ,@write-hooks))
                                 ,slot-set!)
                         slot-set!))))
              (else (error "gen-setters: unknown class slot")))))
    
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
    (let* ((instance-index 1) ; 0 -> class-desc
           (desc (make-class-desc
                  name supers
                  (if (pair? field-indices)
                      ;; must substract 1 here because the index
                      ;; includes the clas id and supers' place
                      ;; holders (but id has index 0)
                      (- (slot-index
                          (cdar (take-right field-indices 1)))
                         1)
                      0))))
      ;; For each field, insert the instance index if it is an
      ;; instance slot or, simply add an unbound notice for a class
      ;; slot into the descriptor
      (for-each
       (lambda (fi)
         (let ((index (slot-index fi)))
           (cond ((is-instance-slot? fi)
                  (vector-set! desc index (instance-index++)))
                 ((is-class-slot? fi)
                  (vector-set! desc index 'unbound-class-slot))
                 (else
                  (error "Unknown slot type")))))
       (map cdr field-indices))
      desc))

  ;; field-indices are expected to be sorted from lower index to
  ;; highest index
  (define (gen-instantiator field-indices)
    (define obj (gensym 'obj))
    (define instance-field-indices
      (filter (lambda (field-index)
                (is-instance-slot? (cdr field-index)))
              field-indices))
    ;; Transforms a macro time vector to a vector runtime declaration
    (define (vector->vector v)
      (let ((code '())
            (len  (vector-length v)))
       (let loop ((i (- (vector-length v) 1))
                  (code '()))
         (if (>= i 0)
             (loop (- i 1) (cons `(quote ,(vector-ref v i)) code))
             (cons 'vector code)))))
    `(begin
       ;; Class descriptor is put in a global var
       (define ,(class-desc-name name)
         ,(vector->vector (class-info-desc (table-ref mt-class-table name))))
       (define (,(symbol-append 'make- name) ,@(map car instance-field-indices))
         (vector ,(class-desc-name name)
                 ,@(map car instance-field-indices)))))

  (define (gen-predicate)
    (define obj (gensym 'obj))
    `(begin
       ;; Class descriptor is put in a global var
       (define (,(gen-predicate-name name) ,obj)
         (and (instance-object? ,obj)
              (is-subclass? (class-desc-id (instance-class-descriptor ,obj))
                            ',name)))))

  (define (gen-printfun field-indices)
    ;; not clean hehe, obj uses a gensym but not the rest of the code...
    ;; i dont believe that the codes need hygiene here anyways...
    (define obj (gensym 'obj))
    (define (field->list f)
      (let ((fname (car f))
            (slot-info (cdr f)))
        (if (is-instance-slot? slot-info)
            (list 'list slot: `',fname ''=
              (list (gen-accessor-name name fname) obj))
            (list 'list class-slot: `',fname ''=
              (list (gen-accessor-name name fname))))))
    
    `(define-method (describe (,obj ,name))
       (list ,@(map field->list field-indices))))
  
  (define (sort-field-indices field-indices)
    (define-macro (indice-comp op)
      (let ((x (gensym 'x)) (y (gensym 'y)))
        `(lambda (,x ,y) (,op (slot-index (cdr ,x))
                              (slot-index (cdr ,y))))))
    (quick-sort (indice-comp <) (indice-comp =) (indice-comp >)
                field-indices))

  ;; Process the super classes' slots
  (for-each
   (lambda (super)
     (let ((super-field-indices
            (cond
             ((table-ref mt-class-table super #f) =>
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
  (for-each process-field! fields)

  (let* ((field-indices (sort-field-indices (table->list temp-field-table)))
         (class-desc (gen-descriptor field-indices)))

    (table-set! mt-class-table name (make-class-info field-indices class-desc))
    `(begin ,@(gen-accessors field-indices)
            ,@(gen-setters field-indices)
            ,(gen-predicate)
            ,(gen-instantiator field-indices)
            ;; here the class descriptor is put in a global table
            ;; but it is also available via its variable ,(class-desc-name name)
            (table-set! rt-class-table
                        ',name
                        ,(class-desc-name name))
            ;; Create a generi print utility (must be at the end)
            ,(gen-printfun field-indices))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-generic signature)
  (define name (meth-name signature))
  (define (parse-arg arg)
    (cond ((and (list? arg) (symbol? (car arg)) (symbol? (cadr arg)))
           (car arg))
          (else arg)))

  (let ((args (map parse-arg (cdr signature))))
    (table-set! mt-meth-table name (make-mt-generic-function name args))
    `(begin
       (define ,(gen-method-table-name name)
         (make-generic-function ',name ',args))
       (define (,name ,@args #!key (cast #f))
         (let ((types
                (if cast
                    (assert-cast (##list ,@args) cast)
                    (##list ,@(map (lambda (arg) `(get-class-id ,arg))
                                   args)))))
           (cond
            ((or (generic-function-get-instance ,(gen-method-table-name name)
                                                types)
                 (find-polymorphic-instance? ,(gen-method-table-name name)
                                             types))
             => (lambda (method)
                  ((method-body method) ,@args)))
            (else
             (error (string-append
                     "Unknown method: "
                     (with-output-to-string
                       ""
                       (lambda ()
                         (pretty-print `(,',name ,@types)))))))))))))



(define-macro (define-method signature bod . bods)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  (define-macro (to-string e1 . es)
    `(with-output-to-string "" (lambda () ,e1 ,@es)))
  (define (name) (meth-name signature))
  (define unknown-meth-error 'unknown-meth)
  (define (parse-arg arg)
    (cond ((and (list? arg) (symbol? (car arg)) (symbol? (cadr arg)))
           (let ((var (car arg))
                 (type (cadr arg)))
             (values var type)))
          (else (values arg any-type))))
  ;; Returns 2 values: the ordrered list of arguments and the ordered
  ;; list of their types.
  (define (parse-args args) (map-values parse-arg args))
  (with-exception-catcher
   (lambda (e)
     (pp `(received error: ,e))
     (if (eq? e unknown-meth-error)
         (error (to-string (show "Generic method was not defined: " (name))))
         (raise e)))
   ;; TODO: Add arity verification
   (lambda ()
     (cond
      ((table-ref mt-meth-table (name) #f) =>
       (lambda (gen-fun)
         (receive (args types) (parse-args (cdr signature))
                  (mt-generic-function-instances-add!
                   gen-fun
                   (make-method (name) types `(lambda ,args ,bod ,@bods)))
                  `(generic-function-instances-add!
                    ,(gen-method-table-name (name))
                    (make-method ',(name) ',types
                                 (lambda ,args ,bod ,@bods))))))
      (else (raise unknown-meth-error))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (update! obj class field f)
  (let ((objval (gensym 'objval)))
   `(let ((,objval ,obj))
      (,(gen-setter-name class field) ,objval
       (,f (,(gen-accessor-name class field) ,objval))))))


;;;;;;;;;;;;;;;;;;;;;;;; Runtime stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loading of the macro time lib
(init)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime lib
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Warning: The class descriptor data structure should not be
;; modified, or if so, some code in the generation of the descriptor
;; will need to be modified (because the rest of the other vector
;; fields are used by the indexes of all the fields of the objects.
(define (instance-class-descriptor obj) (vector-ref obj 0))

(define (class-desc-id desc)           (vector-ref desc 0))
(define (class-desc-supers desc)       (vector-ref desc 1))
(define (class-desc-indices-vect desc) (vector-ref desc 2))

(define (make-generic-function name args)
  (vector name args (make-table test: equal?) '()))
(define (generic-function-name gf)             (vector-ref gf 0))
(define (generic-function-args gf)             (vector-ref gf 1))
(define (generic-function-instances gf)        (vector-ref gf 2))
(define (generic-function-sorted-instances gf) (vector-ref gf 3))
(define (generic-function-instances-list gf)
  (##table-foldl rcons '() (lambda (k v) v)
                 (generic-function-instances gf)))
(define (generic-function-get-instance gf types)
  (table-ref (generic-function-instances gf) types #f))
(define (generic-function-instances-add! gf instance)
  (let ((new-method? (table-ref (generic-function-instances gf)
                                (method-types instance)
                                #f)))
    (table-set! (generic-function-instances gf)
                (method-types instance)
                instance)
    ;; If a method with the same types was not previously used, we can
    ;; re-use the same sorted-list, but we can't if its not the
    ;; case...
    (vector-set! gf 3
                 (sort-methods
                  (if new-method?
                      (cons instance
                            (generic-function-sorted-instances gf))
                      (generic-function-instances-list gf)))))
  
  ;; Update the sorted list of
  )
(define (generic-function-instances-number gf)
  (table-length (generic-function-instances gf)))

(define make-method vector)          ; (make-method id types body)
(define (method-id meth) (vector-ref meth 0))
(define (method-types meth) (vector-ref meth 1))
(define (method-body meth) (vector-ref meth 2))


;; Runtime class table
(define rt-class-table (make-table test: eq?))

(define (find-class? id)
  (table-ref rt-class-table id #f))

(define (instance-object? obj)
  (and (vector? obj)
       (vector? (instance-class-descriptor obj))
       (symbol? (class-desc-id (instance-class-descriptor obj)))))

(define (assert-cast args types)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  (define-macro (to-string e1 . es)
    `(with-output-to-string "" (lambda () ,e1 ,@es)))

  (for-each
   (lambda (arg type)
     (let ((class-id (get-class-id arg)))
       (if (not (is-subclass? class-id type))
           (error
            (to-string (show "Cannot perform cast: "
                             class-id " is not a sublclass of " type))))))
   args
   types)
  types)

;; FIXME: VERY BAD object verification..
(define (get-class-id obj)
  (cond
   ((instance-object? obj) (class-desc-id (instance-class-descriptor obj)))
   (else 'any-type)))

;; This produces a "light" copy because the fiels are simply
;; copied by value, not deeply replicated. Thus a pointer to a
;; data structure will be copied as a pointer to the same data
;; structure.
(define (object-light-copy obj)
  (let* ((len (vector-length obj))
         (new-obj (make-vector len #f)))
    (let loop ((i 0))
      (if (< i len)
          (begin (vector-set! new-obj i (vector-ref obj i))
                 (loop (+ i 1)))))
    new-obj))

(define (instance-of? obj class-id)
  (eq? (instance-class-descriptor obj)
       (table-ref rt-class-table class-id (gensym))))

(define (is-subclass? class-id super-id)
  (and (not (eq? class-id 'any-type))
       ;; (or (and (eq? class-id super-id) class-id)
       ;;  before, but not sure why... eh
       (or (eq? class-id super-id)
           (eq? super-id 'any-type)
           (and (memq super-id
                      (class-desc-supers
                       (table-ref rt-class-table class-id)))
                #t))))

(define (get-super-numbers type)
    (if (eq? type 'any-type)
        0
        (length (class-desc-supers (find-class? type)))))

(define (sort-methods method-lst)
    (define (method-comparator fun)
      (lambda (m1 m2)
        (fun (apply + (map get-super-numbers (method-types m1)))
             (apply + (map get-super-numbers (method-types m2))))))

    ;; Note the reversed order of comparators to get the most specific
    ;; gf instances first.
    (quick-sort (method-comparator >)
                (method-comparator =)
                (method-comparator <)
                method-lst))

;; Will find the "best" or most specific instance of the generic
;; function genfun that corresponds to the actual parameter's types
(define (find-polymorphic-instance? genfun types)
  (define (equivalent-types? instance-types param-types)
    (if (pair? instance-types)
        (and (is-subclass? (car param-types) (car instance-types))
             (equivalent-types? (cdr instance-types) (cdr param-types)))
        #t))

  (let ((sorted-instances (generic-function-sorted-instances genfun)))
    (exists (lambda (method) (equivalent-types? (method-types method)
                                                types))
            sorted-instances)))


(define-generic (describe obj))

;; A usefull function provided by Marc :)
(define (add-pp-method! type-predicate transformer)
  (let* ((old-wr
          ##wr)
         (new-wr
          (lambda (we obj)
            (old-wr we
                    (if (type-predicate obj)
                        (transformer obj)
                        obj)))))
    (set! ##wr new-wr)))


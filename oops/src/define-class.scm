;;; FILE: "define-class.scm"
;;; IMPLEMENTS: Code to support the define-class macro for Oops
;;; LANGUAGE: Gambit Scheme (v4 beta12)
;;; AUTHOR: Ken Dickey
;;;
;;; COPYRIGHT (c) 2005 by Kenneth Alan Dickey
;;;
;;;   Licensed under the Apache License, Version 2.0 (the "License");
;;;   you may not use this file except in compliance with the License.
;;;   You may obtain a copy of the License at
;;;
;;;       http://www.apache.org/licenses/LICENSE-2.0
;;;
;;;   Unless required by applicable law or agreed to in writing, software
;;;   distributed under the License is distributed on an "AS IS" BASIS,
;;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;   See the License for the specific language governing permissions and
;;;   limitations under the License.
;

;==============================================================;
(include "gambit-namespace.scm")
(##declare (standard-bindings) (extended-bindings) (fixnum) (separate))
;; top level names
(##namespace
 (""
  <value>
  %moby-define-class	; exported
  any?
  class-direct-slots
  class-direct-supers
  class-name
  class-precedence-list
  class-slots
  class?
  format
  member?
  slot-info?
  slot-info-name
  slot-info-allocation
  slot-info-slot-ref
  slot-info-slot-set!
  slot-info-init-value
  slot-info-init-fun
  slot-info-type
  type?

  debug-oops
))

; local names
(##namespace
 ("oops#"
  class-slot-info?	
  get-class-cell	; exported
  indexed-slot-info?	
  instance-slot-info?
  really-make-slot-info
  remove-duplicates
  remove-useless-supers
  slot-info->source
  thing-direct-slots
  thing-direct-supers
  thing-name
  thing-precedence-list
  un-quote
  virtual-slot-info?
))

;=============================================================================
(include "common-macros.scm")
;=============================================================================
;; Support for fancy macro to move as much processing as possible
;; to compile time...
;
;;; (define-class <name> ( <super>* ) ( <slot-option>* ) . init-fun )
;;;     where init-fun, if supplied, is (lambda (new-inst) ...)

; Example:
; (define-class <point> 
; 	      <value>
; 	      ((x init-value: 0)
; 	       (y init-value: 0)))
;
; expands to ==>
;
; (define <point>
;   (letrec ((initialize-instance (lambda (x) x))
;            (this-class
;             (<class> name:
;                      '<point>
;                      direct-supers:
;                      (list <value>)
;                      direct-slots:
;                      '(#<slot-info #12
;                           name: x
;                           allocation: instance:
;                           slot-ref: #f
;                           slot-set!: #f
;                           use-init-keyword: not-set-by-user:
;                           init-value: 0
;                           init-fun: #f
;                           type: #f>
;                        #<slot-info #13
;                           name: y
;                           allocation: instance:
;                           slot-ref: #f
;                           slot-set!: #f
;                           use-init-keyword: not-set-by-user:
;                           init-value: 0
;                           init-fun: #f
;                           type: #f>)
;                      class-slots:
;                      (list)
;                      instance-init-source:
;                      initialize-instance
;                      procedure:
;                      (lambda (#!key (x 0) (y 0))
;                        (let ((instance (oops#make-instance-plus 2)))
;                          (oops#instance-class-set! instance <point>)
;                          (begin (bind-slot! instance 0 x) (bind-slot! instance 1 y))
;                          (begin (initialize-instance instance) instance))))))
;     (begin
;       (oops#ensure-slot-set! this-class 'x 0 kind: indexed: value-class: <value>)
;       (oops#ensure-slot-set! this-class 'y 1 kind: indexed: value-class: <value>))
;     (begin
;       (oops#ensure-slot-ref this-class 'x 0 kind: indexed:)
;       (oops#ensure-slot-ref this-class 'y 1 kind: indexed:))
;     this-class))

; (define p (<point> x: 3)) ;;=> <point x: 3 y: 0>

; SLOT_OPTIONS:
;   allocation:
;  { class: | instance: | constant: | virtual: | each-subclass: | override: }
; 	       -- default: instance:
;     When allocation is override: the instance slot def is replaced.
;     When allocation is virtual: slot-set!: and slot-ref: are required
;       slot-set!: (lambda (instance new-value) form..)
;       slot-ref:  (lambda (instance) form..)
;     When allocation of a direct-slot is class: add a cell slot to this class
;       else reference cell in originating class
;     When allocation is each-subclass: always add slot to this class.
;   use-init-keyword: { #t | #f } -- default #t
; ;; chose one of { init-value: | init-function: }
;   init-value: <scheme-value>  -- default is slot-unbound-value
;   init-function: (lambda () ...) -- no default
;   type: a class/type

; For class slots (allocation: is class: or each-subclass:) a cell is made
; and used by getters/setters for all instances.

;=======================================================================
(define (slot-info->source slot-info)
  `(oops#really-make-slot-info
    ',(slot-info-name slot-info)
    ,(slot-info-allocation slot-info)
    ,(slot-info-slot-ref slot-info)
    ,(slot-info-slot-set! slot-info)
    ,(slot-info-use-init-keyword slot-info)
    ,(let ( (val (slot-info-init-value slot-info)) )
       (if (eq? val #!unbound)
           val
           (list 'quote val)))
    ,(slot-info-init-fun slot-info)
    ',(slot-info-type slot-info)
) )

;=======================================================================
;;; %MOBY-DEFINE-CLASS

;; Macro calls this bottleneck function (useful for testing)
(define (%moby-define-class name super-names slots virtual-class? instance-init-fun)

  (define (check-name name)
    (unless (symbol? name)
      (call-error "name must be a symbol" 'define-class name)))
  
  (define (check-supers super-names)
    (let ( (names
            (cond ((null?   super-names) '(<value>))
                  ((symbol? super-names) (list super-names))
                  ((pair?   super-names) super-names)
                  (else
                   (call-error "super-classes must be classes"
                               'define-class
                               super-names))))
         )
    (map (lambda (s-name)
           (let ( (super
                   (or (name->class-info s-name)
                       (and (bound? s-name)
                            (env-lookup s-name))))
                )
             (unless (and super (or (class-info? super)
                                    (class? super)))
               (call-error "super-classes must be classes"
                           'define-class
                           name))
             super))
         names)
  ) )

  (define (check-slots slot-defs)
    ;; NB: more checks in process-slot code
    (define (find-duplicates list-of-name)  ; find a duplicate in a list or #f
      (let loop ( (dups '()) (names list-of-name) )
        (cond
         ((or (null? names) (null? (cdr names))) dups)
         ((memq (car names) (cdr names))
          (loop (cons (car names) dups) (cdr names))
          )
         (else (loop dups (cdr names))))
    ) )

    (let ( (dups (find-duplicates (map slot-info-name slot-defs))) )
      (if (and (not (null? dups))
               (not (any? (lambda (name)
                            (any? (lambda (info)
                                    (and (eq? (slot-info-name info) name)
                                         (memq (slot-info-allocation info)
                                               '(override: each-subclass:))))
                                  slot-defs))
                          dups)))
          (call-error "duplicate slot in class"
                      'define-class
                      dups)))
   )

  (define (check-init instance-init-fun)
    (if (not (or (null? instance-init-fun)
                 (symbol? (car instance-init-fun)) ;; name of function
                 ;; expect: '((lambda (inst) ...))
                 (and (eq? 'lambda (caar instance-init-fun))
                      (list? (cadar instance-init-fun)) ;; arg exists
                      (= 1 (length (cadar instance-init-fun))) ;; 1 arg
                      (< 0 (length (cddar instance-init-fun)))) ;; body exists
        )   )
        (call-error "badly formed instance init function" 'define-class (car instance-init-fun))))

  ;; Take a list of slot-info structures
  ;;  and return a list ( (0 . info) (1 . info) ..) of instance slots
  (define (index-the-slots slot-defs)
    (let loop ( (index 0) (result '()) (defs slot-defs) )
      (cond
       ((null? defs)
        (reverse result)
        )
       ((instance-slot-info? (car defs))
        (loop (+ index 1) (cons (cons index (car defs)) result) (cdr defs))
        )
       (else ;; skip non-instance slots
        (loop index result (cdr defs))))))

  (define (un-quote-if-bound val)
    (if (eq? val #!unbound)
        val
        (un-quote val)))

  (define (make-lambda-list all-slots)
    (let loop ( (result '()) (slots all-slots) )
      (if (null? slots)
          (if (null? result) '()
              (cons '#!key (reverse result)))
          (let* ( (slot-info (car slots))
                  (slot-name (slot-info-name slot-info))
                )
            (cond
             ((eq? (slot-info-allocation slot-info) class:)
              (loop result (cdr slots)) ;; skip class slots
              )
             ((and (eq? (slot-info-allocation slot-info) each-subclass:)
                   (not (eq? #t (slot-info-use-init-keyword slot-info))))
              ;; Skip each-subclass: slots unless the user
              ;; explicitly asked for init keyword to be used. [rare]
              (loop result (cdr slots))
              )
             ((not (slot-info-use-init-keyword slot-info))
              (loop result (cdr slots)) ;; skip non-keyword slots
              )
             ((memq (slot-info-allocation slot-info)
                    ;; ok to init constant
                    '(instance: constant: override: each-subclass:))
              (cond
               ((slot-info-init-fun slot-info)
                => (lambda (quoted-form)
                     ;; (quote whatever) -> whatever
                     (let ( (init-form (un-quote quoted-form)) ) 
                       (if (or (null? init-form)
                               (not (or (symbol? init-form) ;; e.g. 'gensym
                                        ;; else expect a thunk: '(lambda () ..)
                                        (and (eq? 'lambda (car init-form))
                                             (> (length init-form) 2)
                                             (null? (cadr init-form))))))
                           (call-error "badly formed slot initialization"
                                       'define-class slot-name init-form)
                           (loop (cons (list slot-name (list init-form))
                                       result)
                                 (cdr slots))))
                ))
               ;; OK for slots to be unbound if not initialized..
               (else
                (loop (cons (list slot-name
                                  (un-quote-if-bound (slot-info-init-value slot-info)))
                            result)
                      (cdr slots))))
              )
             (else
              (loop result (cdr slots))))))))

  (define (initial-slot-value slot-def)
    (let ( (init-fun (slot-info-init-fun slot-def)) )
      (if init-fun
          ;; (qoute (lambda () . body))
          (cons 'begin (cddadr init-fun)) ; (begin . body)
          (un-quote-if-bound (slot-info-init-value slot-def)))
  ) )

  (define (make-slot-inits indexed-slots)
    (let loop ( (result '()) (slots indexed-slots) )
      (if (null? slots)
          (if (null? result) #f
              (cons 'begin (reverse result)))
          (let* ( (index      (caar slots))
                  (slot-info  (cdar slots))
                  (name       (slot-info-name slot-info))
                  (allocation (slot-info-allocation slot-info))
                )
            (cond
             ((not (slot-info-use-init-keyword slot-info))
              ;; just set the value from the init
              (if (memq allocation '(instance: constant: override:))
                  (loop (cons `(bind-slot! instance
                                           ,index
                                           ,(initial-slot-value slot-info))
                              result)
                        (cdr slots)
                        )
                  (loop result (cdr slots))) ;; else skip
              )
             ;; Set value from the keyword argument
             ((memq allocation '(instance: constant: override:)) ;; ok to init constant
              (loop (cons `(bind-slot! instance ,index ,name) result)
                    (cdr slots))
              )
             (else
              (call-error "unknown allocation"
                          'define-class
                          allocation
                          name)))))))

  (define (make-instance-init instance-init-fun)
    (if (null? instance-init-fun)
        #f
        (car instance-init-fun)))

  (define (direct-supers super-names)
    (cond
     ((null? super-names)   '(list <value>))	  ;; '()
     ((symbol? super-names) `(list ,super-names)) ;; e.g. <point>
     (else
      (cons 'list super-names)))) ;; e.g. (<point> <velocity>)

  (define (get-inherited-slots supers)
    (if (null? supers)
        '()
        (let loop ( (supers-seen '()) (supers-remaining supers) )
          (if (null? supers-remaining)
	      (map-append thing-direct-slots (reverse supers-seen))
              (let ( (supers-to-add
                      (filter (lambda (class) (not (memq class supers-seen)))
                              (thing-precedence-list (car supers-remaining))))
                   )
                (loop (append supers-to-add supers-seen) (cdr supers-remaining))))
   ) ) )

  (define (class-slot-defs slot-defs)
    (filter
     (lambda (info) (memq (slot-info-allocation info) '(class: each-subclass:)))
     slot-defs))

  (define (find-slot-def slot-name class)
    (let loop ( (slots (thing-direct-slots class))
                (class class)
                (left-to-try '())
              )
      (cond
       ((null? slots)
        (let ( (supers (thing-direct-supers class)) )
          (if (null? supers)
              (call-error "Can't find inherited class slot"
                          'inherited-class-slots
                          slot-name)
              (loop (thing-direct-slots (car supers))
                    (car supers)
                    (remove-duplicates (append (cdr supers) left-to-try) thing-name))))
        )
       ((eq? slot-name (slot-info-name (car slots)))
        (car slots)
        )
       (else
        (loop (cdr slots) class left-to-try)))
  ) )
  
  ;; Get each-subclass? inherited class slots for this class.
  ;;  (list (slot-name . class-name) ..)
  ;; Slots must exist in direct super-classes.

  (define (class-slots->defs local-class-slots     ;; (slot-info.. )
                             inherited-class-cells ;; ((slot-name cell class).. )
                             each-subclass-cells)  ;; ((slot-name cell class).. )
    (define (make-init-value info)
      (let* ( (val          (un-quote-if-bound (slot-info-init-value info)))
              (init-fun     (slot-info-init-fun   info))
              (val-unbound? (eq? val #!unbound))
            )
        (if val-unbound?
            (if init-fun init-fun #!unbound)
            val)
    ) )
    (define (local->form info) ;; new local cell
      ;; slot-info -> (name . (make-cell init-val))
      `(cons ',(slot-info-name info)
             (oops#make-cell ,(make-init-value info)))
     )
    (define (inherited->form list) ;; share the cell
      ;; (slot-name cell class) -> (name . (get-class-cell class name))
      `(cons ',(car list)
             (oops#get-class-cell ,(thing-name (caddr list))
                                  ',(car list)))
     )
    (define (each-sub->form list) ;; shadowed cell, use old value
      ;; (slot-name cell class) -> (slot-name . (make-cell old-cell-value))
      `(cons ',(car list)
             (oops#make-cell
              (oops#cell-ref
               (oops#get-class-cell ,(thing-name (caddr list))
                                  ',(car list)))))
     )

    (append '(list)
            (map local->form     local-class-slots)
            (map inherited->form inherited-class-cells)
            (map each-sub->form  each-subclass-cells))
    )
     
  (define (indexed-slot->setter pair)
    (let ( (index (car pair))
           (info  (cdr pair))
         )
    `(oops#ensure-slot-set! this-class
                       ',(slot-info-name info)
                       ,index
                       kind: indexed:
                       value-class: ,(info->type info))
  ) )

  (define (indexed-slot->getter pair)
    (let ( (index (car pair))
           (info  (cdr pair))
         )
    `(oops#ensure-slot-ref this-class
                      ',(slot-info-name info)
                      ,index
                      kind: indexed:)
  ) )

  (define (info->type info)
    (cond ((slot-info-type info)
           => (lambda (x) x))
          (else '<value>))
  )
                          
  (define (virtual-slot->setter def)
    `(oops#ensure-slot-set! this-class
                       ',(slot-info-name def)
                       ,(slot-info-slot-set! def)
                       kind: virtual:
                       value-class: ,(info->type def))
   )

  (define (virtual-slot->getter def)
    `(oops#ensure-slot-ref this-class
                       ',(slot-info-name def)
                       ,(slot-info-slot-ref def)
                       kind: virtual:))

  (define (class-slot->setter def)
    (if (slot-info? def)
        `(oops#ensure-slot-set! this-class
                           ',(slot-info-name def)
                           (oops#get-class-cell this-class ',(slot-info-name def))
                           kind: cell:
                           value-class: ,(info->type def))
        `(oops#ensure-slot-set! this-class
                           ',(car def)
                           ,(cdr def)
                           kind: cell:
                           value-class: <value>) ;;@@FIXME:
   ) )

  (define (class-slot->getter def)
    (if (slot-info? def)
        `(oops#ensure-slot-ref this-class
                          ',(slot-info-name def)
                          (oops#get-class-cell this-class ',(slot-info-name def))
                          kind: cell:)
        `(oops#ensure-slot-ref this-class
                          ',(car def)
                           ,(cdr def)
                           kind: cell:)
   ) )

  (define (subclass-slot->setter slot-stuff) ;; (name cell class)
    (let* ( (slot-name (car   slot-stuff))
            (class     (caddr slot-stuff))
            (type      (info->type (find-slot-def slot-name class)))
          )
    `(oops#ensure-slot-set! this-class
                           ',slot-name
                           (oops#get-class-cell this-class ',slot-name)
                           kind: cell:
                           value-class: ,type)
   ) )

  (define (subclass-slot->getter slot-stuff) ;; (name cell class)
    (let ( (slot-name (car slot-stuff)) )
      `(oops#ensure-slot-ref this-class
                             ',slot-name
                             (oops#get-class-cell this-class ',slot-name)
                             kind: cell:)
   ) )

  (define (constant-slot? slot-info)
    (eq? constant: (slot-info-allocation slot-info)))

  (define (ensure-setters indexed-slots virtual-slots class-slots subclassed-cells)
    (define (mutable-slot? pair)
      (not (constant-slot? (cdr pair)))
    )
    (append '(begin)
            (map indexed-slot->setter  (filter mutable-slot? indexed-slots))
            (map virtual-slot->setter  virtual-slots)
            (map class-slot->setter    class-slots)  ;;@@FIXME: constant slots (?)
            (map subclass-slot->setter subclassed-cells)
   ) ) 

  (define (ensure-getters indexed-slots virtual-slots class-slots subclassed-cells)
    (append '(begin)
            (map indexed-slot->getter  indexed-slots)
            (map virtual-slot->getter  virtual-slots)
            (map class-slot->getter    class-slots)
            (map subclass-slot->getter subclassed-cells)
  ) )

  ;;======== Processing ========;;
  ;; Syntax checks can error out
  (check-name   name)
  (check-init   instance-init-fun)

  (let* ( (supers            (check-supers super-names))
          (direct-slots      (map process-slot slots))
          ;; An allocation: each-subclass: or allocation: override: 
          ;; slot will substitute a new initialization for an inherited slot.
          (slot-overrides    (filter
                                (lambda (info)
                                  (eq? (slot-info-allocation info) override:))
                                direct-slots))
          (override-names    (map slot-info-name slot-overrides)) ;; indexed slots
          (inherited-slots   (filter
                                (lambda (info)
                                  (not (member? (slot-info-name info) override-names)))
				(remove-duplicates (get-inherited-slots supers) slot-info-name)))
          (each-subclass-names ;; inherited class slots
            (map slot-info-name
                 (filter
                    (lambda (info) (eq? (slot-info-allocation info) each-subclass:))
                    (get-inherited-slots supers))))
          (all-slots         (append inherited-slots direct-slots))
          (indexed-slots     (index-the-slots (filter indexed-slot-info? all-slots)))
          (virtual-slots     (filter virtual-slot-info? direct-slots))
          (num-indexed-slots (length indexed-slots))
          (local-class-slots (filter class-slot-info? direct-slots))
          (super-class-cells (remove-duplicates
                                (map-append (lambda (super)
                                                (map
                                                   (lambda (pair)
                                                     (list (car pair) ; slot-name
                                                           (cdr pair) ; value cell
                                                           super))    ; class
                                                   (thing-class-slots super)))
                                            supers)
                                car))
          (inherited-class-cells ;; exclusive of allocation: each-subclass:
                             (filter
                                (lambda (pair) (not (member? (car pair) each-subclass-names)))
                                super-class-cells))
	  (subclass-override-names
	   		     (map slot-info-name local-class-slots))
          (each-subclass-cells
                             (filter
                                (lambda (pair) 
				  (and (member? (car pair) each-subclass-names)
				       (not (member? (car pair) subclass-override-names))))
                                super-class-cells))
          (initialize-instance (make-instance-init instance-init-fun))
        )

    (check-slots all-slots)

    ;; Update expand-time data base
    (let ( (new-info
            (make-class-info name
                             (remove-useless-supers supers)
                             direct-slots
                             (append (map (lambda (info)
                                            `(,(slot-info-name info) .
                                              (oops#get-class-cell ,name
                                                                   ',(slot-info-name info))))
                                          local-class-slots)
                                     (map (lambda (list) (cons (car list) (cadr list)))
                                          super-class-cells))
                             '()))
         )
      (class-info-precedence-list-set! new-info (compute-cpl new-info))
      (add-class-info new-info) ;; Add to compile-time database
      )
    ;; Return the class definition to be compiled
    `(define ,name
       (letrec
          ( (initialize-instance ,initialize-instance)
            (this-class
             (<class> name: ',name
                      direct-supers: ,(direct-supers super-names)
                      direct-slots: ,(cons 'list (map oops#slot-info->source direct-slots))
                      class-slots: ,(class-slots->defs local-class-slots
                                                       inherited-class-cells
                                                       each-subclass-cells)
                      instance-init-source: initialize-instance
                      procedure:
                      (lambda ,(make-lambda-list all-slots)
                        (let ( (instance (oops#make-instance-plus ,num-indexed-slots)) )
                          (oops#instance-class-set! instance ,name)
                          ,(make-slot-inits indexed-slots)
                          ,(cond
                              (virtual-class?
                               '(initialize-instance instance)
                               )
                              (initialize-instance
                               '(begin (initialize-instance instance) instance)
                               )
                              (else 'instance)))))
            )) ;; end letrec
       ,(ensure-setters indexed-slots
                        virtual-slots
                        local-class-slots
                        each-subclass-cells)

       ,(ensure-getters indexed-slots
                        virtual-slots
                        local-class-slots
                        each-subclass-cells)
       this-class)
) ) )

(define (get-class-cell class slot-name)
  (cond ((assq slot-name (thing-class-slots class)) => cdr)
        (else (call-error "Unable to find class slot"
                          get-class-cell
                          class
                          slot-name))
 ) )

;=============================================================================


(when debug-oops
      (format #t "~%Oops: loaded DEFINE-CLASS code"))


;===========================E=O=F==============================;

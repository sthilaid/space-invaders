;;; FILE: "class-sierarchy.scm"
;;; IMPLEMENTS: Class hierarchy bootstrap for Oops
;;; LANGUAGE: Gambit Scheme (v4 beta15)
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

;==============================================================;
(include "gambit-namespace.scm")
(##declare (standard-bindings) (extended-bindings) (fixnum) (separate))
;; names exported to top level
(##namespace
 (""
  <boolean>		; export
  <box>			; export
  <character>		; export
  <class>		; export
  <complex>		; export
  <foreign>		; export
  <function>		; export
  <generic>		; export
  <hash-table>		; export
  <input-port>		; export
  <integer>		; export
  <keyword>		; export
  <meroon>		; export
  <method>		; export
  <null>		; export
  <number>		; export
  <one-of>		; export
  <output-port>		; export
  <pair>		; export
  <rational>		; export
  <real>		; export
  <record-type>		; export
  <record>		; export
  <singleton>		; export
  <string>		; export
  <symbol>		; export
  <type>		; export
  <value>		; export [redef]
  <vector>		; export
  <f32vector>
  <f64vector>
  <s8vector>
  <s16vector>
  <s32vector>
  <s64vector>
  <u8vector>
  <u16vector>
  <u32vector>
  <u64vector>
  any?
  bind-slot!
  class-direct-subclasses
  class-direct-subtypes
  class-of
  class-precedence-list
  class?
  every?
  format
  generic?
  instance-class
  instance?
  is-a?
  member?
  method?
  name
  one-of?
  precedence-list	; export
  precedence-list-set!	; export
  singleton?		; export
  subclass-of
  subclass?
  subtype?
  superclass
  superclass-set!
  test-for		; export
  test-for-set!		; export
  type
  type-set
  type?			; export
  value			; export

  debug-oops  
))

;; local names
(##namespace
 ("oops#"
  add-subclass
  class-direct-subtypes-set!
  define-primitive-class
  id1
  make-slot-ref
  make-slot-set!
  mark
  remove-duplicates
))

;=============================================================================
(include "common-macros.scm")
;==============================================================;
;; Elide loader warnings.
(define type-set    'bootstrap)
(define <string>    '<string>)
(define <vector>    '<vector>)
(define value       'bootstrap)
(define subclass-of 'bootstrap)
(define superclass  'bootstrap)
(define name        'bootstrap)
(define type        'bootstrap)
(define <f32vector> 'bootstrap) ;; defined in "limited-types.scm"
(define <f64vector> 'bootstrap) ;; defined in "limited-types.scm"
(define <s8vector>  'bootstrap) ;; defined in "limited-types.scm"
(define <s16vector> 'bootstrap) ;; defined in "limited-types.scm"
(define <s32vector> 'bootstrap) ;; defined in "limited-types.scm"
(define <s64vector> 'bootstrap) ;; defined in "limited-types.scm"
(define <u8vector>  'bootstrap) ;; defined in "limited-types.scm"
(define <u16vector> 'bootstrap) ;; defined in "limited-types.scm"
(define <u32vector> 'bootstrap) ;; defined in "limited-types.scm"
(define <u64vector> 'bootstrap) ;; defined in "limited-types.scm"

;==============================================================;
;; NB: I prefer <value> to <top>.  Everything is-a <value>.
;
;                   --<value>-----------------------
;                  /     |    \            \        \  ...fundamental values
;         <function>   <type>  <character> <symbol>  <record>
;        /     |    \   /  \----------------------------------------------------
; <method> <generic> \ /    \          \          \             \               \
;                  <class> <singleton> <one-of> <limited-range> <subclass-type> <record-type>
;
;==============================================================;
; COMPLETE THE <CLASS> BOOTSTRAP

(set! <value>
  (<class>
    name: '<value>
    procedure: (lambda igore (error '<value> "Can't make values"))
) )

; (define-class <type> <value>
;   ( (precedence-list type: <list>  init-value: '())
;     (superclass      type: <class> init-value: <value>)
;     (test            type: <function> allocation: each-subclass:)
;  )

(define %type-slots%
  (map process-slot
       '( (precedence-list type: <list>  init-value: '())
          (superclass      type: <class> init-value: <value>)
          (test-for        type: <function>)
;                           init-value:
;                           (lambda ignore (error "Type Error")))
        )
 ) )

;;; <TYPE>
(define <type> 
  (<class>
    name: '<type>
    direct-supers: `( ,<value> )
    direct-slots: %type-slots%
    procedure: (lambda igore (error '<type> "Can't make values"))
) )

 (ensure-slot-ref  <type> 'precedence-list 0 kind: indexed:)
 (ensure-slot-set! <type> 'precedence-list 0 kind: indexed:)
 (ensure-slot-ref  <type> 'superclass      1 kind: indexed:)
 (ensure-slot-set! <type> 'superclass      1 kind: indexed:)
 (ensure-slot-ref  <type> 'test-for        2 kind: indexed:)
 (ensure-slot-set! <type> 'test-for        2 kind: indexed:)

;; bootstrap (reset below)
(set! type? (lambda (thing) (subclass? thing <type>)))

;;; <FUNCTION>
(define <function> 
  (<class>
    name: '<function>
    direct-supers: `( ,<value> )
    procedure: (lambda igore (error '<function> "Can't make values; use lambda"))
) )

(class-direct-subclasses-set! <value>    (list <type> <function>))
(class-direct-subclasses-set! <type>     (list <class> ))
(class-direct-subclasses-set! <function> (list <class> ))

(class-precedence-list-set!   <value>    (compute-cpl <value>))   ;; (list <value>))
(class-precedence-list-set!   <type>     (compute-cpl <type>))    ;; (list <type>     <value>))
(class-precedence-list-set!   <function> (compute-cpl <function>));; (list <function> <value>))

(class-direct-supers-set!     <class>  (list <function> <type>))
(class-precedence-list-set!   <class>  (list <class> <function> <type> <value>))

(class-depth-set! <type>     1)
(class-depth-set! <function> 1)
(class-depth-set! <class>    2)

;;; Primitive Classes

(define-macro (define-primitive-class name . direct-supers)
  `(begin
     (define ,name
       (<class>
         name: ',name
         direct-supers: (list ,@direct-supers)
         procedure: (lambda igore (error ',name "Can't create values"))))
     ;; make-cpl
     (map
      (lambda (super)
        (class-direct-subclasses-set!
         super
         (cons ,name (class-direct-subclasses super))))
      (list ,@direct-supers))
     ,name
     )
)


(define-primitive-class <number>   <value>)
(define-primitive-class <complex>  <number>)
(define-primitive-class <real>     <complex>)
(define-primitive-class <rational> <real>)
(define-primitive-class <integer>  <rational>)

(define-primitive-class <list>      <value>)  ; re-defined in "collection-classes.scm"
(define-primitive-class <null>      <list>)   ; re-defined in "collection-classes.scm"
(define-primitive-class <pair>      <list>)   ; re-defined in "collection-classes.scm"
(define-primitive-class <symbol>    <value>)
(define-primitive-class <character> <value>)
(define-primitive-class <boolean>   <value>) ;; Bootstrap: set! below

;;(define-primitive-class <eof-object>  <value>) ;; <singleton>
(define-primitive-class <record>      <value>)
(define-primitive-class <record-type> <type>)
(define-primitive-class <input-port>  <value>) ;;@@FIXME:
(define-primitive-class <output-port> <value>) ;;@@FIXME:
;;@@ string ports et al
(define-primitive-class <hash-table>  <value>) ;; bootstrap
(define-primitive-class <box>         <value>) ;;@@FIXME:
(define-primitive-class <meroon>      <value>) ;;@@FIXME:
;;(define-primitive-class <unbound>     <value>) ;; <singleton>
(define-primitive-class <keyword>     <value>)
;;(define-primitive-class <void>        <value>) ;; <singleton>

(define-primitive-class <foreign>     <value>) ;; foreign objects [xenoids]


;==============================================================;
;;; Gambit native <RECORD> & <RECORD-TYPE> support
;; DEFINE-STRUCTURE defines a #type-type instance
;; with a record constructor.  Instances retirned by
;; such a constructor are of class <record> and
;; their type is an inctance of <record-type>.
;;
;; In general, one uses a specific record-type to
;; dispatch on (rather than <record>).
;;
;; It is probably best to think of <record-type>
;; as a meta-class of <record>s.

(define (record? thing)
  (subclass? (class-of thing) <record>))

(define (record-type? thing)
  (subclass? (class-of thing) <record-type>))

;; Add <record> accessors
(ensure-slot-ref <record>
                 'type
                 '(lambda (inst) ;; UNCLECKED!!
                    (##structure-type inst))
                 kind: virtual:)

(ensure-slot-ref <value>
                 'type
                 '(lambda (inst)
                    (class-of inst))
                 kind: virtual:)

;; Override <type> accessors
(ensure-slot-ref <record-type>
                 'superclass
                 ;; Instances of <record-type>s are <record>s.
                 '(lambda (ignored-instance) <record>)
                 kind: virtual:)

(ensure-slot-ref <record-type>
                 'precedence-list
                 '(lambda (inst)
                    ;;@@FIXME: structure inheritance
                    (cons (##structure-type inst)
                          (class-precedence-list <record>)))
                 kind: virtual:)


(ensure-slot-ref <record-type>
                 'test-for
                 '(lambda (type)
                    (lambda (obj)
                      (##structure-instance-of?
                         obj
                         (##type-id type))))
                 kind: virtual:)

;; Add accessors for <record-type>
(ensure-slot-ref <record-type>
                 'name
                 '(lambda (obj)
                    (and (##type? obj)
                         (##type-name obj)))
                 kind: virtual:)

;;@@@ field-names field-ref field-set!


;==============================================================;
;;; <SINGLETON>

;; Singletons are used in gf dispatching  on unique objects.
;; A singleton on the same object always returns the same singleton.
;;  (eq? (<singleton> #!eof) (<singleton> #!eof)) ==> #t
;;  (value (<singleton> #!key))  ==> #!key
;;  (class-of #t) ==> <singleton>
;;  (class-of (<singleton> 3)) ==> <singleton>

(define <singleton> 
  (let ( (singletons (make-table weak-keys: #t test: eq?))
         (not-found-marker '"not found")
         (slot-defs
          (append %type-slots%
                  (map process-slot
                       '( (value
                           allocation: constant:
                           use-init-keyword: #f)
                         )
         )))
       )
    (letrec ( (singleton-class ;; build this class "by hand".
               (<class>
                name: '<singleton>
                direct-supers: (list <type>)
                direct-slots:  slot-defs
                procedure:
                (lambda (value)
                  (let ( (probe
                          (table-ref singletons value not-found-marker))
                       )
                  (cond
                   ((eq? probe not-found-marker)
                    (if (null? value)
                        <null> ;; NOT a singleton! [but simple error]
                        ;; Build a new instance "by hand"
                        (let ( (inst (make-instance-plus 4)) )
                          (instance-class-set! inst singleton-class)
                          (table-set! singletons value inst)
                          (let ( (superclass (class-of value)) )
                            (bind-slot! inst
                                        0 ; precedence-list
                                        (cons inst
                                              (class-precedence-list superclass)))
                            (bind-slot! inst 1 superclass) ; superclass of this class
                            (bind-slot! inst 2 ;; type test for singletons
                                 (lambda (whatever) (eq? whatever value)))
                            (bind-slot! inst 3 value)


                            ;;@@FIXME: use weak cells
                            (class-direct-subtypes-set!
                               superclass
                               (cons inst
                                     (class-direct-subtypes superclass)))
                            inst)))
                    )
                   (else probe))))
                ))
              )

      (class-direct-subclasses-set! <type>
                                    (cons singleton-class
                                          (class-direct-subclasses <type>)))
      singleton-class
) ) )

; 0 precedence-list
; 1 superclass
; 2 test-for
(ensure-slot-ref <singleton> 'value 3 kind: indexed:)
;; value is constant  => no getter

(define (singleton? obj) ;; NB: used by IS-A?
  (and (instance? obj) (eq? (class-of obj) <singleton>))) ;; else true for '(), etc.

;==============================================================;
;;; <ONE-OF>
;; <one-of>s allow dispatch on a join of Singetons.
;; E.g. (define <vowels> (apply <one-of> (string->list "aeiouAEIOU")))

(define <one-of>
  (let ( (slot-defs
          (append %type-slots%
                  (map process-slot
                       '( (value-list
                           allocation: constant:
                           use-init-keyword: #f)
                          ))))
       )
    (letrec ( (one-of-class
               (<class>
                name: '<one-of>
                direct-supers: (list <type>)
                direct-slots:  slot-defs
                procedure:
                (lambda value-list
                  (unless (< 1 (length value-list))
                    (call-error "Expected two or more values"
                                '<one-of> value-list))
                  (let ( (common-class (class-of (car value-list))) )
                    (unless (every? (lambda (v) (is-a? v common-class))
                                    value-list)
                      (call-error "Expected all values to be of the same class"
                                '<one-of> value-list))
                    (let ( (inst (make-instance-plus 4)) )
                      (instance-class-set! inst one-of-class)
                      (bind-slot! inst
                                  0  ; precedence-list
                                  (cons inst (class-precedence-list common-class)))
                      (bind-slot! inst 1 common-class) ; superclass of this class
                      (bind-slot! inst 2 ;; type test for one-of
                          (lambda (whatever) (member? whatever value-list)))
                      (bind-slot! inst 3 value-list)
                      (class-direct-subtypes-set!
                         common-class
                         (cons inst (class-direct-subtypes common-class)))
                      inst)))
                ))
              )
      (class-direct-subclasses-set!
         <type>
         (cons one-of-class (class-direct-subclasses <type>)))

      one-of-class
) ) )

; 0 precedence-list
; 1 superclass
; 2 test-for
(ensure-slot-ref <one-of> 'value-list 3 kind: indexed:)
;; value-list is constant  => no setter


(define (one-of? obj) ;; NB: used by IS-A?
  (eq? (class-of obj) <one-of>))


;;; See also "limited-types.scm"

;==============================================================;
;; BOOTSTRAP

(define add-subclass really-add-subclass)

;==============================================================;
;;; CLASS-OF

;; Magic from
;; (include "~~/lib/header.scm")

;; Bootstrap <method> & <generic>
(define <method>  #f)
(define <generic> #f)
(define (method? thing)  #f)
(define (generic? thing) #f)

;;@@FIXME: other Gambit things:errors, read-table, promise, u16vector, etc.

;;; CLASS-OF  -- Nota Bene: ReReDefined in "collection-classes.scm"
(set! class-of 
   (lambda (thing)
     (if (not (##subtyped? thing))
         (case (##type thing)
           ((0) <integer>) ;; fixnum
           ((3) <pair>)
           ((2) (cond  ;; #!eof #t ...
                 ((char? thing) <character>)
                 ((null? thing) <null>)
                 (else <singleton>))
            )
           (else
            (call-error "Don't understand type" class-of (##type thing) thing))
           )
         (case (##subtype thing)
           ;;((macro-subtype-vector)       0)
           ((0) <vector>)
           ;;((macro-subtype-pair)         1)
           ((1) <pair>)
           ;;((macro-subtype-ratnum)       2)
           ((2) <rational>)
           ;;((macro-subtype-cpxnum)       3)
           ((3) <complex>)
           ;;((macro-subtype-structure)    4)
           ((4) (cond
                 ((instance? thing) (instance-class thing))
                 ((##type? thing)   <record-type>)
                 ((table? thing)    <hash-table>)
                 (else  <record>))) ;; a.k.a. structure
           ;;((macro-subtype-boxvalues)    5)
           ((5) <box>)
           ;;((macro-subtype-meroon)       6)
           ((6) <meroon>)
           ;;((macro-subtype-symbol)       8)
           ((8) <symbol>)
           ;;((macro-subtype-keyword)      9)
           ((9) <keyword>)
           ;;((macro-subtype-frame)        10)
           ((10) <value>) ;;@@
           ;;((macro-subtype-contthread)   11)
           ((11) <value>) ;;@@
           ;;((macro-subtype-promise)      12)
           ((12) <value>) ;;@@
           ;;((macro-subtype-willtable)    13)
           ((13) <value>) ;;@@
           ;;((macro-subtype-procedure)    14)
           ((14) (cond
                  ((class?   thing) <class>)
                 ; ((method?  thing) <method>)  ;; Not a class
                 ; ((generic? thing) <generic>) ;; Not a class
                  (else <function>))
            )
           ;;((macro-subtype-return)       15)
           ((15) <value>) ;;@@
           ;;((macro-subtype-foreign)      18)
           ((18) <foreign>)
           ;;((macro-subtype-string)       19)
           ((19) <string>)
           ;;((macro-subtype-s8vector)     20)
           ((20) <s8vector>)
           ;;((macro-subtype-u8vector)     21)
           ((21) <u8vector>)
           ;;((macro-subtype-s16vector)    22)
           ((22) <s16vector>)
           ;;((macro-subtype-u16vector)    23)
           ((23) <u16vector>)
           ;;((macro-subtype-s32vector)    24)
           ((24) <s32vector>) 
           ;;((macro-subtype-u32vector)    25)
           ((25) <u32vector>)
           ;;((macro-subtype-f32vector)    26)
           ((26) <f32vector>)
           ;;((macro-subtype-s64vector)    27)
           ((27) <s64vector>)
           ;;((macro-subtype-u64vector)    28)
           ((28) <u64vector>)
           ;;((macro-subtype-f64vector)    29)
           ((29) <f64vector>)
           ;;((macro-subtype-flonum)       30)
           ((30) <real>)
           ;;((macro-subtype-bignum)       31)
           ((31) <integer>)
           (else
            (call-error "Don't understand subtype" class-of (##subtype thing) thing))
) ) ) )


;;; SUBTYPE?  -- Nota Bene: redefined in "limited-types.scm"
(set! subtype?
 (lambda (sub super)
  (cond
   ((eq? sub super)
    ;; #t
    )
   ((and (class? sub) (class? super))
    (or (eq? super <class>)
        (subclass? sub super))
    )
   ((type? sub)
    (subclass? (superclass sub) super)
    )
   (else #f)
) ) )


;;; IS-A?  -- set! in "limited-types.scm"
(define (is-a? obj class-or-type)
  (let ( (obj-class (class-of obj)) )
    (cond
     ((eq? obj-class class-or-type) ;; #t
      )
     ((class? class-or-type) ;; Types are NOT Classes.
      ;; (member? class (class-precedence-list (class-of obj)))))
      (subclass? obj-class class-or-type)
      )
     ((singleton? class-or-type)
      (eq? (value class-or-type)
           (if (singleton? obj) (value obj) obj))
      )
     ((one-of? class-or-type)
      (and (memq (if (singleton? obj) (value obj) obj)
                 (value class-or-type))
           #t)
      )
     (else #f))))

;;; TYPE?
(set! type?    ;; NB: types are distinct from classes
      (lambda (thing) (subclass? (class-of thing) <type>)))

;; We can fix <boolean> now...
(set! <boolean> (<one-of> #t #f))


(when debug-oops
      (format #t "~%Oops: loaded CLASS-HIERARCHY bootstrap code"))


;===========================E=O=F==============================;

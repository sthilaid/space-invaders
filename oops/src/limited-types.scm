;;; FILE: "limited-types.scm"
;;; IMPLEMENTS: Limited Types for Oops
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

;==============================================================;
(include "gambit-namespace.scm")
(##declare (standard-bindings) (extended-bindings) (fixnum) (separate))
;; top level names
(##namespace
 (""
  <class>
  <function>
  <integer>
  <limited-range>		; export
  <number>
  <predicate-type>		; export
  <range>			; export
  <s16int>			; export
  <s32int>			; export
  <s8int>			; export
  <float32>			; export
  <float64>			; export
  <subclass-type>		; export
  <type>
  <u16int>			; export
  <u32int>			; export
  <u8int>			; export
  <value>
  <real>
  any?
  bind-slot!
  class-of
  class?
  class
  class-direct-subclasses
  class-direct-subtypes
  class-precedence-list
  depth
  ensure-getters
  ensure-setters
  every?
  format
  instance?
  is-a?				; REDEF
  limited-range?		; export
  make-ref
  make-set!
  max-value			; export
  member?
  min-value			; export
  one-of?
  pred<=?			; export
  precedence-list
  precedence-list-set!
  singleton?
  subclass-of
  subclass-type?
  subclass?
  subtype?			; REDEF
  superclass
  superclass-set!
  test-for
  test-for-set!
  the-class-of			; export
  type?
  type-set
  value
  value-list

  debug-oops
))

;; local names
(##namespace
 ("oops#"
  list-find-first
))

;==============================================================;
(include "common-macros.scm")
(include "oops-macros.scm")
;==============================================================;

(define max-value    'bootstrap) ;; elide compiler warnings
(define min-value    'bootstrap)
(define pred<=?      'bootstrap)

;;; <RANGE>
(define-class <range> <value> ;; NB: not a supported <type>
  ((min-value type: <number>)
   (max-value type: <number>)
   (pred<=?   type: <function>
              init-value: <=)
   )
)


;;; <LIMITED-RANGE>
(define-class <limited-range> (<type> <range>) ()
  ;; instance initialization
  (lambda (inst)
    (let ( (super (superclass inst))
           (<=? (pred<=?   inst))
           (min (min-value inst))
           (max (max-value inst))
         )
      (precedence-list-set! inst
         (cons inst (class-precedence-list super))
         )
      (test-for-set! inst
         (lambda (whatever)
           (and (is-a? whatever super)
                (<=? min whatever max)))
         )
      ;;@@FIXME: use weak cells
      (class-direct-subtypes-set! super
         (cons inst (class-direct-subtypes super)))
  ) )
)

(define (limited-range? thing)
  (subclass? (class-of thing) <limited-range>))


;;; <SUBCLASS-TYPE>
(define-class <subclass-type> <type>
  ((subclass-of type: <class> init-value: <value>))
  ;; instance initialization
  (lambda (inst)
    (let* ( (super <class>)
            (sub-of (subclass-of inst))
            (probe (list-find-first
                    (lambda (elt)
                      (and (subclass-type? elt)
                           (eq? super (subclass-of elt))))
                    (class-direct-subtypes super)))
          )
      ;; e.g. (eq? (<subclass-type> subclass-of: <integer>)
      ;;           (<subclass-type> subclass-of: <integer>)) ==> #t
      (if probe
          probe
          (begin
            (precedence-list-set! inst
             (cons inst (class-precedence-list super))
             )
            (test-for-set! inst
              (lambda (whatever) 
                (subclass? whatever sub-of))
              )
            (superclass-set! inst super) ;; super == <class>
            ;;@@FIXME: use weak cells
            (class-direct-subtypes-set! sub-of
             (cons inst (class-direct-subtypes sub-of)))
            ))
  ) )
)

(define (subclass-type? thing)
  (subclass? (class-of thing) <subclass-type>))

;==============================================================;
;;; <PREDICATE-TYPE>
(define-class <predicate-type> <type> ()
  (lambda (inst)
    (precedence-list-set! inst (cons inst (class-precedence-list (class-of inst))))
    (let ( (inst-class    (superclass inst))
           (original-test (test-for   inst))
         )
      (test-for-set! inst
                    (lambda (thing)
                        (and (is-a? thing inst-class) ;; add trivial guard
                             (original-test thing))))
) ) )

;; E.g. (define <even?> (<predicate-type> superclass: <integer> test-for: even?))


;==============================================================;
;;;REDEFINITION of IS-A?
(set! is-a?
  (lambda (obj class-or-type)
    (cond
     ((class? class-or-type)
      (subclass? (class-of obj) class-or-type)
      )
     ((type? class-or-type)
      ((test-for class-or-type) obj)
      )
     (else #f))
) )

;==============================================================;
;;; REDEFINITION of SUBTYPE?
(set! subtype?
  (lambda (sub super)
    (cond
     ((eq? sub super)
      ;; #t
      )
     ((class? super)
      (cond
       ((class? sub)
        (or (eq? super <class>)
            (subclass? sub super)))
       ((subclass-type? sub)
        (subtype? (subclass-of sub) super)
        )
       ((type? sub)
        (subtype? (superclass sub) super))
       (else #f))
      )
     ;; specialized <type>s
     ((limited-range? super)
      (and (limited-range? sub)
           (subclass? (superclass sub) (superclass super))
           (eq? (pred<=? sub) (pred<=? super))
           ((pred<=? sub)
              (min-value super)
              (min-value sub)
              (max-value sub)
              (max-value super)))
      )
     ((one-of? super)
      (and (one-of? sub)
           (subclass? (superclass sub)
                      (superclass super))
           (let ( (super-things (value-list super)) )
             (every? (lambda (thing)
                       (member? thing super-things))
                     (value-list sub))))
      )
     ((subclass-type? super)
      (subtype? sub (subclass-of super))
      )
     (else #f)
) ) )


;==============================================================;
;; For limited-vector types:
(define <u8int>
  (<limited-range> superclass: <integer>
                   min-value: 0
                   max-value: #xFF))

(define <u16int>
  (<limited-range> superclass: <integer>
                   min-value: 0
                   max-value: #xFFFF))

(define <u32int>
  (<limited-range> superclass: <integer>
                   min-value: 0
                   max-value: #xFFFFFFFF))

(define <u64int>
  (<limited-range> superclass: <integer>
                   min-value: 0
                   max-value: #xFFFFFFFFFFFFFFFF))

(define <s8int>
  (<limited-range> superclass: <integer>
                   min-value: #x-FE
                   max-value: #xFF))

(define <s16int>
  (<limited-range> superclass: <integer>
                   min-value: #x-FFFE
                   max-value: #xFFFF))

(define <s32int>
  (<limited-range> superclass: <integer>
                   min-value: #x-FFFFFFFE
                   max-value: #xFFFFFFFF))

(define <s64int>
  (<limited-range> superclass: <integer>
                   min-value: #x-FFFFFFFFFFFFFFFE
                   max-value: #xFFFFFFFFFFFFFFFF))

(define <float32>
  (<limited-range> superclass: <real>
                   min-value: (exact->inexact #x-FFFFFFFE)
                   max-value: (exact->inexact #xFFFFFFFF)))
(define <float64>
  (<limited-range> superclass: <real>
                   min-value: (exact->inexact #x-FFFFFFFFFFFFFFFE)
                   max-value: (exact->inexact #xFFFFFFFFFFFFFFFF)))

;==============================================================;

(when debug-oops
      (format #t "~%Oops: loaded LIMITED TYPES code"))

;===========================E=O=F==============================;

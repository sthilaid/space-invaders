;;; FILE: "generics.scm"
;;; IMPLEMENTS: Generic Functions for Oops
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
;; The method dispatch algorithms used here are based on the papers:
;;
;; "Simple and Efficient Subclass Tests"
;; by Jonathan Bachrach jrb@ai.mit.edu
;; http://people.csail.mit.edu/people/jrb/pve/pve.htm
;; [Note local file "subclass.scm"]
;;
;; "Multiple-Dispatching Based on Automata",
;; by Chen and Turau
;; Journal of Theory and Practice of Object Systems, 1(1), 1995.
;;
;; and
;;
;; "Fast Algorithm for Creating Space Efficient Dispatching Tables
;;  with Application to Multi-Dispatching"
;; by Zibin & Gil
;; OOPSLA '92
;;
;==============================================================;
; The basic idea is to build a lookup automaton as per Chen and Turau,
; use the information to construct dispatching based on binary search
; as outlined by Zibin & Gil, using (sub)class numbering as per Bachrach.
;
; The major advantage of using the LUA code is to avoid
; the combinatorial case of having to look at all types/classes
; in order to build a gf's dispatch table.
;
; Rather than indexing through state objects, we build a form which
; implements a tail recursive state mechine.
;
; Note that I am not yet trying to be optimal here. I am starting
; with a goal of "good" performance using algorithms which I
; can understand.
;
; The next phase is to tune & improve based on measured performance.
;==============================================================;

(include "gambit-namespace.scm")
(##declare (standard-bindings) (extended-bindings) (fixnum) (separate))
;; top level names
(##namespace
 (""
  %define-generic		; export
  %method
  <class>
  <method>
  <generic>			; export
  <value>
  any?
  class?
  class-of
  class-name
  class-direct-subclasses
  class-precedence-list
  describe
  ensure-generic		; export
  every?
  format
  generic?			; export
  generic-args-info		; export
  generic-methods		; export
  generic-name			; export
  generic-slot-bind	       	; export
  instance?
  limited-range?
  member?
  method-applicable?
  method-args-congruent?
  method-args-info
  method-name
  method<=?
  min-value
  max-value
  no-next-method		; export
  one-of?
  precedence-list
  singleton?
  subclass?
  subclass-of
  subclass-type?
  subtype?
  superclass
  test-for
  type?
  value
  value-list

  debug-oops
))

;; local names
(##namespace
 ("oops#"
  %%generic-tag%%
  %%num-standard-generic-slots%%
  %%the-slots-of-generic%%
  alist-cons
  add-method
  class-info-precedence-list
  class-more-specific?
  filter
  filter+sort-applicable-methods
  generic-dispatch
  generic-methods-set!
  id1
  iota
  list-find-first
  list-pos
  list-remove
  list-remove!
  list-sort
  invalid-subclass-generation 
  make-assq
  make-generic-ref
  make-generic-set!
  proc->generic
  remove-duplicates
  thing-name
  thing-precedence-list

  debug-generics ;; debug
))

;==============================================================;
(include "common-macros.scm")
;==============================================================;
;; Elide compiler warnings
(define limited-range? 'bootstrap)
(define min-value      'bootstrap)
(define max-value      'bootstrap)
(define describe       'bootstrap)
(define subclass-type? 'bootstrap)
;==============================================================;
  
(define debug-generics #f)


;; <generic>

(define %%the-slots-of-generic%%
  '(name		 ;0
    args-info
    methods		 ;2
    subclass-generation	 
    dispatch-source-code ;4
    dispatch-fun	 
    refresh-fun          ;6
    ))

(define (flush-dispatch-cache gf)
  (generic-subclass-generation-set! gf invalid-subclass-generation)
 ; (generic-dispatch-fun-set! gf #f)
 ; (generic-dispatch-source-code-set! gf #f)
)

(define %%num-standard-generic-slots%%
  (length %%the-slots-of-generic%%) )

(define %%generic-tag%% '("generic")) ;; not eq? to anything else

;; (define-values (proc->generic generic? make-generic-ref make-generic-set! make-generic-bind!)
;;     (proc->callable %%generic-tag%% %%num-standard-generic-slots%%))
(define proc->generic     #f)
;(define generic?          #f)
(define make-generic-ref  #f)
(define make-generic-set! #f)
(define generic-slot-bind #f)
(call-with-values
  (lambda ()
    (proc->callable %%generic-tag%% %%num-standard-generic-slots%%))
  (lambda (construct pred? mk-ref mk-set! mk-bind!)
    (set! proc->generic     construct)
    (set! generic?          pred?)
    (set! make-generic-ref  mk-ref)
    (set! make-generic-set! mk-set!)
    (set! generic-slot-bind mk-bind!)))


(define generic-name
  (make-generic-ref  "name" 0))

(define generic-args-info
  (make-generic-ref  "args-info" 1))

(define generic-methods
  (make-generic-ref  "methods" 2))

(define generic-methods-set!
  (make-generic-set! "methods" 2))

(define generic-subclass-generation
  (make-generic-ref  "subclass-generation" 3))

(define generic-subclass-generation-set!
  (make-generic-set! "subclass-generation" 3))

(define generic-dispatch-source-code
  (make-generic-ref  "dispatch-source-code" 4))

(define generic-dispatch-source-code-set!
  (make-generic-set! "dispatch-source-code" 4))

(define generic-dispatch-fun
  (make-generic-ref  "dispatch-fun" 5))

(define generic-dispatch-fun-set!
  (make-generic-set! "dispatch-fun" 5))

(define generic-refresh-fun
  (make-generic-ref  "refresh-fun" 6))

(define generic-refresh-fun-set!
  (make-generic-set! "refresh-fun" 6))

(set! <generic>
  (lambda (#!key name (methods '()) args-info default-proc)
    (unless (and (symbol? name)
                 (method-args? args-info)
                 (or (procedure? default-proc)
                     (eq? default-proc #f)))
      (call-error "Required arguments missing/broken"
                  <generic> name methods args-info default-proc))
  
    (letrec ( (new-generic
               (proc->generic
                (lambda args (generic-dispatch new-generic args))))
            )
      (generic-slot-bind new-generic 0 name)
      (generic-slot-bind new-generic 1 args-info)
      (generic-slot-bind new-generic 2 (if default-proc
                                           (list default-proc)
                                           methods))
      (generic-slot-bind new-generic 3 invalid-subclass-generation)
      ;; %%subclass-generation%% is def'ed in "subclass.scm"
      (generic-slot-bind new-generic 4 #f)  ;; dispatch-source-code
      (generic-slot-bind new-generic 5 #f)  ;; dispatch-fun
      (generic-slot-bind new-generic 6 #f)  ;; refresh-fun
      new-generic
) ) )


;==== Define-generic ==========================================;

;; Called by define-generic macro
(define (%define-generic name body)
  ;; name is a symbol and body is (method lambda-list form*)
  ;; name is (name lambda-list) and body is (form*)
  (let ( (name (if (symbol? name) name (car name)))
         (args (if (list? name) (cdr name) (cadar body)))
         (body (if (null? body)
                   '()
                   (if (list? name) body (cddr body))))
       )
    (let ( (proc (if (null? body)
                      #f
                      (%method (cons name args) body)))
          )
;;   (when proc (format #t "~&%define-generic: ~&~y" proc)) ;;@@DEBUG
    `(define ,name
       (let ( (check ;; value unused
               (if (and (oops#bound? ',name)
                        (generic? (##global-var-ref ',name)))
                   ;; @@FIXME: signal a continuable exception..
                   (format (current-error-port)
                           "***WARNING: Redefining Generic Function named: ~a~%"
                           ',name)))
            )
         (<generic> name: ',name
                    args-info: (oops#parse-method-args ,(list 'quote args))
                    default-proc: ,proc))
) ) ) )

;==== Ensure-generic ==========================================;

(define (ensure-generic method)
  (let* ( (name (method-name method))
          (global-val
           (and (bound? name) (##global-var-ref name)))
        )
    (if (and global-val (generic? global-val))
        (add-method (##global-var-ref name) method)
        (begin
          ;; @@FIXME: signal a continuable exception..
          (unless (memq global-val '(#!unbound bootstrap #f))
            (warn "*** Redefining to generic:" name)
            (when debug-oops   ;;@@@DEBUG@@
              (display " was: ")
              (oops#dump global-val))
          )
          (env-define! name
                       (<generic>
                        name: name
                        methods: (list method)
                        args-info: (method-args->implicit-generic-function-args
                                    (method-args-info method))
                        default-proc: #f)))
 ) ) )

;==== Add-method ==============================================;

(define (add-method gf method)
  (define (insert-or-replace m args-info methods)
    (cond
     ((null? methods)
      ;; only need to flush dispatch cache if ADD a new method.
      (flush-dispatch-cache gf) 
      (cons m methods)
      )
     ((equal? (method-args-specializers args-info)
              (method-args-specializers
                (method-args-info (car methods))))
      (cons m (cdr methods))
      ;; No need to update dispatch cache
      )
     (else (cons (car methods)
                 (insert-or-replace m args-info (cdr methods)))))
    )
  (if (method-args-congruent? (method-args-info method)
                              (generic-args-info gf))
      (generic-methods-set!
         gf
         (insert-or-replace method
                            (method-args-info method)
                            (generic-methods gf)))
      (call-error "Method arguments not congruent with <generic>"
                  add-method
                  (generic-name gf)
                  " expected "
                  (method-args-form (generic-args-info gf))
                  " got "
                  (method-args-form (method-args-info method))
                  ))
)


;==============================================================;
;;; GENERIC-DISPATCH
(define (generic-dispatch gf actual-args)
  (if (not (= %%subclass-generation%% (generic-subclass-generation gf)))
      (begin
        (recalc-generic-dispatch-fun gf)   ;; re-calc/re-cache
        (generic-dispatch gf actual-args)) ;; force recheck
      (apply (generic-dispatch-fun gf) actual-args) ;; else dispatch
) )

(define (recalc-generic-dispatch-fun gf)
  (let ( (gf-dispatch-source-code
            (lua->dispatch-code
               (simplify-lua-states
                  (construct-lookup-automaton gf))))
       )
    (when debug-generics
      (generic-dispatch-source-code-set!
         gf
         gf-dispatch-source-code))
    (generic-subclass-generation-set!
       gf
       %%subclass-generation%%)
    (generic-dispatch-fun-set!
       gf ;; closure-compile [Gambit]
       (eval gf-dispatch-source-code))
) )


(define no-next-method
  (lambda whatever (error "No next-method")))


(define no-applicable-method
  (lambda whatever (error "No applicable-method")))

;==============================================================;
;;; Sets implemented as lists.
;; NB: Sets are considered immutable [unchecked]
;;     Set operations must maintain original ordering.

(define make-set list)

(define the-empty-set '())

(define set-empty? null?)

(define (set-not-empty? set) (not (set-empty? set)))

(define (set-contains? elt set) (and (memq elt set) #t))

(define set-length length)

(define (set-equal? s1 s2)
  (and (= (set-length s1) (set-length s2))
       (every? (lambda (elt) (set-contains? elt s2)) s1)))
       
(define (set-union s #!rest sets)
  ;; Note: order preserving
  (let outer ( (set (reverse s)) (sets sets) )
    (if (null? sets)
        (reverse set)
        (let inner ( (elts set) (others (car sets)) )
          (cond
           ((null? others) (outer elts (cdr sets)))
           ((memq (car others) elts) (inner elts (cdr others)))
           (else (inner (cons (car others) elts) (cdr others)))
           ) )
        )
) )

(define (set-intersection s1 s2) ;; Order preserving
  (cond ((set-empty? s1) '())
        ((memq (car s1) s2)
         (cons (car s1) (set-intersection (cdr s1) s2)))
        (else
         (set-intersection (cdr s1) s2)))
 )

(define (set-remove set elts) ;; NB: returns a NEW set
  ;; remove list of elts from set
  (let loop ( (result '()) (set set) )
    (cond
     ((null? set) result)
     ((memq (car set) elts)
      (loop result (cdr set)))
     (else (loop (cons (car set) result) (cdr set))))
) )

(define (set-difference s1 s2)
  (set-remove (append s1 s2)
              (set-intersection s1 s2))
)

(define (subset? possible-subset super-set)
  (every?
     (lambda (elt) (set-contains? elt super-set))
     possible-subset))

(define set-for-each for-each)

(define set-filter filter)

(define set-map map)

(define (set-fold f seed set)
  (define (fold-left value list)
    (if (null? list)
        value
        (fold-left (f (car list) value)
                   (cdr list)))
    )
  (reverse (fold-left seed set)) ;; maintain order
)

(define (set-add set elt) ;; NB: returns a NEW set
  (if (memq elt set)
      set
      (cons elt set)))

(define (set-sub set elt)
  (cond
   ((set-empty? set) the-empty-set)
   ((eq? elt (car set))  (set-sub (cdr set) elt))
   (else (cons (car set) (set-sub (cdr set) elt)))
) )

(define set-first car)

(define set-rest  cdr)

(define (set->list set)  (set-fold cons '() set)) ;; Return a copy.

(define (list->set list)
  (set-fold set-add list the-empty-set)) ;; Return a copy.


;==============================================================;
;;; THING-PRECEDENCE-LIST redefined to include instances of
;;; subclasses of <type>.

(set! thing-precedence-list ;; REDEF!
  (lambda (thing)
    (cond
     ((class? thing)
      (class-precedence-list thing)
      )
     ((class-info? thing)
      (class-info-precedence-list thing)
      )
     ((type? thing)
      (precedence-list thing)
      )
     (else
      (error "thing-precedence-list called with unhandled" thing))
) ) )


(define (class-precedence-more-specific? c1 c2 class)
  ;; PRECONDITION: both c1 and c2 are in cpl of class.
  ;; True iff c1 comes before c2 in class-precedence-list of class
  (let loop ( (cpl (thing-precedence-list class)) )
    (cond
     ((null? cpl) #f)
     ((eq? c1 (car cpl)) (memq c2 (cdr cpl)))
     (else (loop (cdr cpl))))))
  


;;; GLB: greatest-lower-bound
(define (greatest-lower-bound s t) ;;-> set
  ;; The GLB of two types is the set of direct subtypes which have
  ;; both (super) types as ancestors but have no ancestors which 
  ;; satisfy the same criterion
  (cond
   ((subtype? s t) (make-set s)) ; degenerate case
   ((subtype? t s) (make-set t)) ; degenerate case
   (else
    (let loop ( (results       '())
                (left-to-check (make-set s t))
              )
      (cond
       ((null? left-to-check)
        (remove-duplicates results id1)
        )
       ((let ( (type (car left-to-check)) )
          (and (subtype? type s)
               (subtype? type t)))
        (loop (cons (car left-to-check) results) ;; add class/type, but
              (cdr left-to-check)) ;; don't add kids
        )
       (else
        (loop results  ;; don't add class/type, but do check kids
              (if (class? (car left-to-check))
                  (append (cdr left-to-check)
                          (class-direct-subclasses (car left-to-check)))
                  (cdr left-to-check)) ;; ..if any
              ))))))
)

;==============================================================;
;;; TYPE-CLOSURE
;; The (type-)closure of a Set of types S is the union of the GLB's of
;; all its elements.
(define (type-closure type-set)
  (list-sort ;; Return types sorted by TYPE-MORE-SPECIFIC?.
   (remove-duplicates
    (apply append
           (cons type-set
                 (map
                  (lambda (s)
                    (apply append
                           (map
                            (lambda (t) (greatest-lower-bound s t))
                            (set-sub type-set s))))
                  type-set)))
    id1)
   type-more-specific?)
)


(define (type-more-specific? t1 t2)
  ;; This is best if a total ordering
  (or (and (type? t1) (type? t2))
      (error "type-more-specific?: args must be types or classes"
                t1 t2))
  (cond
   ((and (class? t1) (class? t2))
    (class-more-specific? t1 t2)
    )
   ((instance? t1)
    (if (instance? t2)
        ;; Types may be more specific based on the
        ;; specificity of their superclasses.
        (or (class-more-specific?
               (superclass t1)
               (superclass t2))
            ;; or they may just be more specific..
            (subtype? t1 t2))
        ;; types are more specific than classes
        #t)
    )
   (else ;; (and (instance? t2) (not (instance? t1)))
    #f)) ;; classes are less specific than types
)


;;======================================================;;
;;; DOMAIN
(define (domain factors) ;; factors is a list of sets
  (apply set-union factors))

;;; FACTOR-CROSS-INTERSECTION
;;  _
;; | | A B is ( (a1 ^ b1) (a1 ^ b2) .. (a2 ^ b1) (a2 ^ b2) ..
;;     with empty sets removed.  NB: Ordering must be preserved!
(define (factor-cross-intersect A B)
  ;; list-of-sets X list-of-sets -> list-of-sets
  (filter set-not-empty?
          (apply append
                 (map (lambda (a) (map (lambda (b) (set-intersection a b)) B))
                      A))
) )


;;; METHOD-ARG-PRECEDENCE

(define (method-arg-type method N)
  (list-ref (method-args-specializers (method-args-info method)) N)
)

(define (method-types gf N)
  (remove-duplicates
     (map (lambda (method) (method-arg-type method N))
          (generic-methods gf))
     id1)
)

(define (precedence type cpl)
  (or (list-pos type cpl)
      0 ;; for types
      (error "precedence: fell off end" type cpl))
)

(define (method-arg-precedence N type methods) ;;-> list of method-sets
  ;; For arg N, return precedence order of methods w.r.t. type
  ;; where type is a subtype of the Nth arg of one or more methods.
  (if (null? methods)
      '()
      (let* ( (cpl (thing-precedence-list type))
              (m-list ;; applicable methods
               (filter
                (lambda (pair)
                  (subtype? type (car pair)))
                (map (lambda (m)
                       (cons (method-arg-type m N) m))
                     methods)))
              (sorted-list ;; (reverse) sorted applicable methods
               (list-sort 
                m-list
                (lambda (p1 p2) ;; NB: reversed sort
                  (<= (precedence (car p1) cpl)
                      (precedence (car p2) cpl)))))
            )
      ;; OK, now sorted by precedence, return a list of sets w same prec
        (let set-loop ( (results '()) (list sorted-list) )
          (if (null? list)
              results
              (let elt-loop ( (target-type   (caar list))  ;; Nth arg type
                              (set (make-set (cdar list))) ;; method
                              (others (cdr list))
                            )
                (cond
                 ((null? others) (reverse (cons set results))) ;; done
                 ((eq? target-type (caar others))
                  ;; add to current set
                  (elt-loop target-type
                            (set-add set (cdar others))
                            (cdr others))
                  )
                 (else ;; set complete, look for more sets
                  (set-loop (cons set results) others)))
  ) ) ) ) ) )
  
  
(define (method-precedence-equal? method-sets-1 method-sets-2)
  (and (= (length method-sets-1) (length method-sets-2))
       (every? (lambda (set1 set2) (eq? (car set1) (car set2)))
                method-sets-1 method-sets-2)))

;==============================================================;
;;; LUA -- lookup-automaton
(define-structure lookup-automaton gf states start-state)

;;; STATE
;; From this state one can use any of the included transitions
(define-structure state
  id (transitions unprintable:) (precedence-order unprintable:))

;; debug
(define (annotated-method-name method methods-list)
  ;; PRECONDITION (memq method methods-list)
  (string-append
   (symbol->string (method-name method))
   ":#"
   (number->string
    (+ 1 (list-pos method methods-list))))
)

(define (display-name thing)
  (cond
   ((class? thing) (class-name thing))
   ((class-info? thing) (class-info-name thing))
   ((type? thing)
    (cond
     ((singleton? thing)
      (format #f "(<singleton> ~a)" (value thing)))
     ((one-of? thing)
      (format #f "(<one-of> ~a)" (value-list thing)))
     ((limited-range? thing)
      (format #f "(<limited-range> ~a [~a..~a])"
              (class-name (superclass thing))
              (min-value thing) (max-value thing)))
     ((subclass-type? thing)
      (format #f "(<subclass-of> ~a)" (thing-name (subclass-of thing))))
     (else (describe thing))))
   (else thing)) ;; punt
)

(define (display-state state methods-list)
  (format #t "~&~a " (state-id state))
  (if (not (eq? 'next-methods
                (car (state-precedence-order state))))
      (format #t "precedence: ~a"
              (map (lambda (l)
                     (map (lambda (m)
                            (annotated-method-name m methods-list))
                          l))
                   (state-precedence-order state)))
      (let ( (method (cadr (state-precedence-order state))) )
        (format #t "most specific method ~a: ~a"
              (annotated-method-name method methods-list)
              (method-args-form (method-args-info method)))))
  (for-each display-transition (state-transitions state))
  (void))

;;; TRANSITION:  state X class X state X ((test type . state).. )
(define-structure transition
  state ;; from this state
  class ;; to dispatch-on/test-for
  default-next-state ;; when class
  test-state-alist)  ;; when type(s)


(define (display-transition transition)
  (let ( (alist              (transition-test-state-alist transition))
         (default-next-state (transition-default-next-state transition))
       )
    (if (null? alist)
        (format #t "~&transition from ~a to ~a when ~a"
            (state-id   (transition-state transition))
            (state-id   default-next-state)
            (thing-name (transition-class transition)))
        (begin
          (format #t "~&transition from ~a when ~a and test"
                  (state-id   (transition-state transition))
                  (thing-name (transition-class transition)))
          (for-each
           (lambda (pair)
             (format #t "~& --> state ~a when ~a"
                     (state-id (cddr pair)) ; next-state
                     (display-name (cadr pair))))  ; type
           alist)
          (when default-next-state
            (format #t "~& --> ~a when test(s) fail"
                    (state-id default-next-state)))
    )))
  (void)
)

(define (display-states lua)
  (let* ( (gf (lookup-automaton-gf lua))
          (methods (generic-methods gf))
        )
    (format #t "~&Generic Method named: ~a~&" (generic-name gf))
    (for-each 
     (lambda (index)
       (format #t "~&===states for arg ~a===" (- index 1))
       (for-each (lambda (s) (display-state s methods))
                 (vector-ref (lookup-automaton-states lua) index))
       (format #t "~&"))
     (iota (method-args-num-required (generic-args-info gf))))
) )

(define (display-gf-sigs gf)
  (pp (map (lambda (m) (method-args-form (method-args-info m)))
           (generic-methods gf)))
)

;;; CONSTRUCT-LOOKUP-AUTOMATON
(define (construct-lookup-automaton gf) ;;-> lookup-automaton
  (let* ( (methods        (generic-methods gf))
          (start-state    (make-state (gensym 'state) '() (list methods)))
          (num-fixed-args (method-args-num-required (generic-args-info gf)))
          (max-arg-num    (- num-fixed-args 1))
          (states         (make-vector (+ 1 num-fixed-args) '()))
        )
    ;; Nota Bene: State N => arg number: (- N 1)
    (define (add-state-for-arg state N)
      (let ( (index (+ N 1)) )
        (vector-set! states
                     index
                     (cons state (vector-ref states index))))
      )
    (define (try-to-set-default transition transitions)
      ;; PRECONDITION: (not (transition-default-next-state t))
      (let ( (the-class (transition-class transition)) )
        (let loop ( (trans-list transitions) (most-specific-tr #f) )
            (cond
             ((null? trans-list) ;; checked 'em all
              (let ( (default-next-state
                       (and most-specific-tr
                            (transition-default-next-state most-specific-tr)))
                   )
              (transition-default-next-state-set!
                 transition
                 default-next-state) ;; OK if #f
              ))
             ((let* ( (t (car trans-list))
                      (tr-class (transition-class t))
                    )
                (and (subclass? the-class tr-class) ;; found super
                     (transition-default-next-state t) ;; super has next-state
                     (not (eq? transition t)) ;; skip self
                     t))
              => (lambda (trans)
                   (if (or (not most-specific-tr)
                           (type-more-specific? (transition-class trans)
                                                (transition-class most-specific-tr)))
                       (loop (cdr trans-list) trans)
                       (loop (cdr trans-list) most-specific-tr)))
              )
             (else (loop (cdr trans-list) most-specific-tr)))
        ))
      )
    (define (process-transitions-in trans-alist) ;; ((class . transition).. )
      ;; Return a list of transitions; try to set all default-next-state's.
      (let ( (transitions (map cdr trans-alist)) )
        (for-each
           (lambda (t)
             (transition-test-state-alist-set!
                t
                (list-sort (transition-test-state-alist t)
                           (lambda (l1 l2) ;; l[i] is (test type . next-state)
                             (type-more-specific? (cadr l1) (cadr l2))))
              )
             (when (not (transition-default-next-state t))
               (try-to-set-default t transitions)))
           transitions)
        transitions
      ))
    (define (build-states-for-arg arg-num state)
      ;; Build transitions for arg position ARG-NUM
      ;; from state STATE, creating target states as needed.
      (let* ( (methods-for-state (domain (state-precedence-order state)))
              (arg-types (map (lambda (method) (method-arg-type method arg-num))
                              methods-for-state))
              (type-closure (type-closure arg-types))
              (arg-n-states (vector-ref states (+ 1 arg-num)))
            )

        (let loop ( (type-set type-closure) (seen '()) )
          ;; seen is an association list of ((class . transition).. )
          (if (set-empty? type-set)
              (state-transitions-set! state (process-transitions-in seen)) ;; done
              (let* ( (type (set-first type-set)
                       )
                      (prec-order
                        (factor-cross-intersect
                          (state-precedence-order state)
                          (method-arg-precedence arg-num type methods-for-state))
                       )
                      (match  ;; find if we already have a state for this case
                       (any?
                          (lambda (state)
                            (and (method-precedence-equal?
                                   (state-precedence-order state)
                                   prec-order)
                                 state)) ;; return the state on a match
                          arg-n-states)
                       )
                      (next-state (if match
                                      match
                                     (make-state (gensym 'state) '() prec-order))
                       )
                      ; Check if transition for this (state X class) already exists.
                      (probe (assq (if (class? type) type (superclass type)) seen)
                      )
                    )
                
                (if (not match) ;; Add new state to states for this arg position
                    (add-state-for-arg next-state arg-num))
                
                (if probe
                    ;; Add to existing transition
                    (let ( (transition (cdr probe)) )
                      (if (class? type)
                          (transition-default-next-state-set! transition next-state)
                          (transition-test-state-alist-set!
                            transition
                            (alist-cons (test-for type)
                                        (cons type next-state)
                                        (transition-test-state-alist transition))
                            ))
                      (loop (set-rest type-set) seen)
                    )
                    ;; Make a new transition
                    (if (class? type)
                        (loop (set-rest type-set)
                              (alist-cons type ;; a class
                                          (make-transition state type next-state '())
                                          seen))
                        (loop (set-rest type-set) ;; type
                              (alist-cons (superclass type) ;; type's super-class
                                          (make-transition state
                                                           (superclass type)
                                                           #f
                                                           (alist-cons (test-for type)
                                                                       (cons type next-state)
                                                                       '()))
                                          seen))
                      )
                    )
                ))) ;; loop
      )) ;; build-states-for-arg

    ;; Make start state for/at arg 0
    (vector-set! states 0 (list start-state)) 
    ;; Build states & transitions for each argument..
    (let arg-loop ( (arg-num 0) )
         (for-each (lambda (state) (build-states-for-arg arg-num state))
                   (vector-ref states arg-num))
         (if (< arg-num max-arg-num)
             (arg-loop (+ 1 arg-num)) ;; next
             (make-lookup-automaton gf states start-state)) ;; result
         )
) )

;;; SIMPLIFY-LUA-STATES
(define (simplify-lua-states lua)
  (define (meth-prec->next-methods mprec)
    (cons 'next-methods (map set-first mprec))
    )
  (let* ( (states     (lookup-automaton-states lua)) 
          (level      (- (vector-length states) 1))
          (state-list (vector-ref states level))
        )
    (for-each
       (lambda (state)
         (state-precedence-order-set!
            state 
            (meth-prec->next-methods
             (state-precedence-order state))))
       state-list)
    lua ;; updated
) )
  
;;@@@FIXME: Measure and see if this code is really
;; carring its weight.
#| ;;======================================================;;
;; Optimize LUA by removing unneeded states:
;;   Start w final states & merge all refs to same
;;   next-method chain.   Then march backward 1 level
;;   at a time & do likewise for previous states.
(define (simplify-lua-states lua)
  (define (meth-prec->next-methods mprec)
    (cons 'next-methods (map set-first mprec))
    )
  (define find (make-assq equal? #f)
    )
  (let* ( (states (lookup-automaton-states lua)) 
          (level (- (vector-length states) 1))
        )
    ;; simplify target methods 1st
    (let methods-loop ( (state-list (vector-ref states level))
                        (simpler-states '())
                        (state-map  '()) ;; alist of state->state
                        (method-map '()) ;; alist of method->state
                      )
      ;; Pick off each method and 1st state which leades to it
      (cond
       ((null? state-list)
        (vector-set! states level simpler-states)
        (simplify-state-transitions states (- level 1) state-map)
        lua ;; done
        )
       (else
        (let* ( (state (car state-list))
                ;; most-specific-method is 1st method in 1st set
                ;; of next-method chain
                (method-list
                   (meth-prec->next-methods
                      (state-precedence-order state)))
              )
          (cond
           ((find method-list method-map)
            ;; already have a state for this method
            => (lambda (pair)
                 ;; add (state . newer-state) to state-map
                 (methods-loop (cdr state-list)
                               simpler-states
                               (alist-cons state
                                           (cdr pair)
                                           state-map)
                               method-map))
            )
           (else ;; 1st time method-list was seen
            ;; replace precedence-order w next-method chain
            (state-precedence-order-set! state method-list)
            ;; add state to simpler-states
            ;; add (method . state) to method-map
            (methods-loop (cdr state-list)
                          (cons state simpler-states)
                          state-map
                          (alist-cons method-list state method-map))))
          )))
) ) )

;;; SIMPLIFY-STATE-TRANSITIONS
;; For each argument level
;; [1] Go through each transition and simplify target states
;; [2] Merge states w same outgoing transitions
(define (simplify-state-transitions states-vec level state-map)
  (define (same-transitions? s1 s2)
    ;; True iff
    ;; [1] s1 and s2 have the same number of transitions
    ;; [2] each type and next-state in a transition in S1 has
    ;; a transition with the same type and next-state in S2.
    (let ( (trz1 (state-transitions s1))
           (trz2 (state-transitions s2))
         )
    (cond
     ((not (= (length trz1) (length trz2)))
      #f
      )
     ((every?
         (lambda (t1)
           (any?
              (lambda (t2)
                (and (eq? (transition-class t1)
                          (transition-class t2))
                     (eq? (transition-next-state t1)
                          (transition-next-state t2))))
              trz2))
         trz1)
      #t
      )
     (else #f))
    ))
  ;; Merge equivalent transitions, checking each pair of states.
  ;; Return a new state-map.
  (define (merge-transitions state-list)
    (let loop ( (states         state-list)
                (simpler-states '())
                (state-map      '())
              )
      (cond
       ((null? states)
        ;; simplify states
        (vector-set! states-vec level simpler-states)
        state-map ;; result
        )
       (else
        (let simplify-loop ( (state          (car states))
                             (to-check       (cdr states))
                             (next-check     '())
                             (simpler-states simpler-states)
                             (states-alist   state-map)
                           )
          (cond
           ((null? to-check)
            (loop next-check (cons state simpler-states) states-alist)
            )
           ((same-transitions? state (car to-check))
            ;; add to new state->state map
            (simplify-loop state
                           (cdr to-check)
                           next-check
                           simpler-states
                           (alist-cons (car to-check)
                                       state
                                       states-alist))
           )
           (else
            ;; look further, but check (car to-check) next time
            (simplify-loop state
                           (cdr to-check)
                           (cons (car to-check) next-check)
                           simpler-states
                           states-alist)))))
    )))

  (let ( (state-list (vector-ref states-vec level)) )
    (for-each ;; [1]
       (lambda (state)
         (for-each
            (lambda (transition)
              (cond
               ((assq (transition-next-state transition)
                      state-map)
                => ;; remap next-state to new target
                (lambda (pair)
                  (transition-next-state-set! transition (cdr pair)))))
             )
            (state-transitions state))
         )
       state-list)
    ;; do for each argument level
    (if (> level 0)
        (simplify-state-transitions
           states-vec
           (- level 1)
           (merge-transitions state-list)) ;; new state-map
        (merge-transitions state-list))
    states-vec
) )
|# ;;======================================================;;

;==============================================================;
;;; Intervals
;;
;; We use the "Type Slicing" technique to determine ranges
;; which are built up from intervals in order to do binary
;; searches to find types (vs subtype tests).
;;
;; We build a LUA (lookup-automaton), each transition of which
;; is translated into an ordered array for the binary search
;; on type (class-id).  This allows us to avoid scanning the entire
;; type hierarchy, which is what is typically done otherwise.

;; For an explination of Type-Slicing see
;; "Fast Algorithm for Creating Space Efficient Dispatching Tables
;; with Application to Multi-Dispatching"
;; by Zibin & Gil
;; OOPSLA '02 [ACM].
;;
;; Note that Jonathan Bachrach's "Simple and Efficient Subclass Tests"
;; [http://people.csail.mit.edu/people/jrb/pve/pve.htm]
;; Already gives us the mechanics to do this.
;;
;; Example from Chen and Turau's paper:
;;            Subclass          Class Hierarchy
;; Class  ID  min max                  A
;;  A     1    1..8                  / | \
;;  B     8    6..8                 B  C  D
;;  C     5    3..7                 \  |\ /\
;;  D     2    2..7                  \ | X  \
;;  E     6    6..7                   \|/ \ /
;;  F     3    3..4                    E   F
;;  G     7    7..7                    |   | 
;;  H     4    4..4                    G   H

;; For 2nd arg, where C is the class of the 1st arg
;;
;;   S2: B->S5, C->S3, D->S4, E->S7, F->S6
;;
;; Coverage in sort order:
;; Class ID   1  2  3  4  5  6  7  8  9  ..
;;                          |-------|B
;;                 |-------------|C 
;;              |----------------|D 
;;
;; Note the overlap in range between classes B, C and D.
;;
;; The CLOSURE function added E and F to the method arg classes
;; to avoid potential inconsistencies introduced by multiple
;; inheritnce from B, C and D.
;;                          |-----|E
;;                 |----|F
;;
;; Sort order for S2 classes via class-more-specific?
;; is   E F B C D ; Intervals are added in this order.
;;
;; Adding intervals by most-specific-class and having
;; more specific "shadow" less specific yields the
;; Final Intervals:
;; Class ID   1  2  3  4  5  6  7  8  9  ..
;;                 |----|F |-----|E
;;              |--|D   |--|C    |--|B 
;;
;; Giving the resultant search-vector for S2:
;;                D    F    C    E    B  <- Class (not encoded)
;;             |  2 |  3 |  5 |  6 |  8 |  9 |  ID Range Start
;;          XX | S4 | S6 | S3 | S7 | S5 | XX |  Next State


;; GENERAL NOTES:

;; Basic strategy for gf's is to delay creation of lookup structure until
;; first use.  The lookup code is then generated and "closure compiled"
;; [@@? Provide a "static" option which gens compiled code ?@@]

;; When the %%subclass-vector%% is regenerated, and the lookup cache
;; does not have to be flushed, the indexes for binary search vectors
;; need to be "refreshed".  So we have FLUSH and REFRESH mechanics.
;; [@@FIXME: REFRESH NYI].

;; The lookup code cache is flushed when ADD-METHOD is invoked on the gf
;; or when a dispatch class (or subclass) is redefined in a way which
;; changes the inheritance hierarchy.  Adding types or leaf classes
;; cannot affect the dispatch code.  Changing the class hierarchy
;; transitively affects only the class-direct-methods of the subclasses
;; involved. [@@FIXME: flush elision NYI]

;; A lazy flush/refresh is done at each gf call by checking
;; that the gf's saved subclass-generation matches the current/global
;; %%subclass-generation%%.


;; Use linear IS-A? search when less than
;; binary-search-cuttoff transitions.
(define binary-search-cuttoff 3) ;;@@ TUNE ME @@

;;; STATE->CODE
(define (state->code state)
  (let* ( (transitions
           (list-sort
              (state-transitions state)
              (lambda (t1 t2)
                (class-more-specific? (transition-class t1)
                                      (transition-class t2)))))
          (num-trans   (length transitions))
        )
    (if (<= num-trans binary-search-cuttoff)
        (build-linear-transition-code        state transitions)
        (build-binary-search-transition-code state transitions))
) )

;; Note: Types may be instances -- which unlike classes do not have names
;;       Type predicates are closures
;; Both cases require a source code reference.
;; Gambit supplies OBJECT->SERIAL-NUMBER and SERIAL-NUMBER->OBJECT.

(define (build-linear-transition-code state transitions)
  ;; return state function for non-final state
  ;; (final state => method call)
  (if (= 1 (length transitions))
      `(,(state-id state)
        (lambda (args)
          (cond ,(transition->cond-clause (car transitions) '(car args) '(cdr args))
                (else (no-applicable-method)))
        ))
      ;; else 2 or more transitions: use COND
      `(,(state-id state)
        (lambda (args)
          (let ( (this-arg   (car args))
                 (other-args (cdr args))
               )
            (cond
             ,@(map (lambda (t) (transition->cond-clause t 'this-arg 'other-args))
                    transitions)
             (else
              (no-applicable-method))))))
) )

(define (transition->cond-clause transition first-arg-exp rest-args-exp)
  ;; return source expression for a COND clause
  (if (null? (transition-test-state-alist transition))
      ;; class only
      `((is-a? ,first-arg-exp ,(class-name (transition-class transition)))
        (,(state-id (transition-default-next-state transition))
          ,rest-args-exp))
      ;; else types & possible default
      `((is-a? ,first-arg-exp ,(class-name (transition-class transition)))
        (cond
         ,@(map 
            (lambda (info) ;; info is (test type . next-state)
              `( ((serial-number->object ,(object->serial-number (car info)))
                  ,first-arg-exp)
                 (,(state-id (cddr info)) ,rest-args-exp)
               )
            )
            (transition-test-state-alist transition))
         ,(if (transition-default-next-state transition)
              `(else (,(state-id (transition-default-next-state transition))  ,rest-args-exp))
              `(else (no-applicable-method)))
       ))
) )

;;@@ TUNE ME @@: Cache last successful search index when binary-search vector
;; is large enough to win; [measure].
(define (build-binary-search-transition-code state transitions)
  `(,(state-id state)
    (lambda (args)
      (let ( (this-arg   (car args))
             (other-args (cdr args))
           )
        ,(transitions->bin-search-code transitions))))
)

(define (transitions->bin-search-code transitions) 
 (let ( (search-data (interval->search-data (make-interval transitions))) )
  `(oops#class-id-binary-search this-arg   ;; dispath arg
                                other-args ;; passed through to next-state invocation
                                ,(cons 'vector (car  search-data))  ;; vector of class-id ranges
                                ,(cons 'vector (cadr search-data))  ;; vector of next-state's
                                no-applicable-method
                                )
) )

;==============================================================;
;;; Build up INTERVALS for Binary Search of Class-IDs
;; An interval is an ordered set of integer ranges
(define empty-interval '())

(define (add-to-interval interval low high type next-state)
  ;; Add new interval, splitting ranges as needed.
  ;; Add most-specific 1st => old superceeds new.
  (if (null? interval)
      (list (list low high type next-state))
      (let* ( (range      (car interval))
              (range-low  (car  range))
              (range-high (cadr range))
            )
        (if (> low range-high)
            ;; look further
            (cons range (add-to-interval (cdr interval) low high type next-state))
            ;; found a place to add
            (cond
             ((< high range-low)
              ;; add before
              (cons (list low high type next-state) interval)
              )
             ((= high range-low)
              (if (= low range-low)
                  ;; Comptetely shadowed; don't add anything.
                  interval
                  ;; add before
                  (cons (list low (- high 1) type next-state) interval)
              ))
             ;; (and (<= low range-high) (> high range-low))
             ((< low range-low)
              (if (<= high range-high)
                  (cons (list low (- range-low 1) type next-state) interval)
                  (cons (list low (- range-low 1) type next-state) ;; add 1st section
                        (cons range
                              (add-to-interval (cdr interval)
                                               (min high (+ 1 range-high))
                                               high
                                               type
                                               next-state))))
              )
             ;; (and (>= low range-low) (<= low range-high) (> high range-low))
             ((> high range-high)
              ;; Exceeds existing; Skip shadowed prefix
              (cons range
                    (add-to-interval (cdr interval)
                                     (min high (+ 1 range-high))
                                     high
                                     type
                                     next-state))
              )
             (else ;; (and (>= low range-low) (<= low range-high)(<= high range-high))
              interval)) ;; Shadowed; ignore
        )   ) ;; end (if (null? interval)..)
) )

(define (make-interval state-transitions)
  (let loop ( (result      empty-interval)
              (transitions
               (list-sort state-transitions
                          (lambda (t1 t2)
                            (class-more-specific?
                             (transition-class t1)
                             (transition-class t2)))))
            )
    (if (null? transitions)
        result
        (let* ( (class (transition-class (car transitions)))
                (low  (class-min-id class))
                (high (class-max-id class))
                (transition (car transitions))
              )
          (loop (add-to-interval result low high class transition)
                (cdr transitions))))
 ) )

(define (interval->search-data interval)
  ;; Interval is ( (low high class transition).. )
  ;; Vectors are '(low1   low2   .. lowN   highN+1)
  ;;             '(state1 state2 .. stateN no-applicable-method)
  ;;             #(class1 class2 .. classN #f) -- for debug [else unused]
  ;; PRECONDITION: (> (length interval) binary-search-cuttoff)
  (list
   ;; list of class-id indices
   (let loop ( (results '()) (elts interval) )
     (if (null? (cdr elts))
         (let* ( (last   (car elts))
                 (low    (car last))
                 (high+1 (+ 1 (cadr last)))
                 )
           (reverse (cons high+1 (cons low results))))
         (loop (cons (caar elts) results) (cdr elts))))
   ;; list of closures for state transitions
   (append
    (map (lambda (i)
           (let ( (transition (car (cdddr i))) )
             (if (null? (transition-test-state-alist transition))
                 ;; No other tests, just go to next state
                 `,(state-id (transition-default-next-state transition))
                 ;; Make other tests as required
                 `(lambda (next-args)
                    (cond
                     ,@(map 
                      (lambda (info) ;; info is (test type . next-state)
                        `( ((serial-number->object
                              ,(object->serial-number (car info))) this-arg)
                           (,(state-id (cddr info)) next-args)
                           )
                        )
                      (transition-test-state-alist transition))
                   ,(if (transition-default-next-state transition)
                        `(else (,(state-id (transition-default-next-state transition))
                                 next-args))
                        `(else (no-applicable-method)))
                   ))
                 )
           ) )
         interval)
    (list 'no-applicable-method))
   ;; vector of classes [for debug only]
;   (list->vector (append (map caddr interval)  
;                         (list #f)))
) )

;==============================================================;
;;; LUA->DISPATCH-CODE
(define (lua->dispatch-code lua) ;; LookUp Automaton
  ;; PRECONDITION: lua has been simplified
  (let* ( (gf         (lookup-automaton-gf lua))
          (gf-name    (generic-name gf))
          (gf-methods (generic-methods gf))
          (num-required-args
              (method-args-num-required
                  (generic-args-info gf)))
        )
    `(let* ( (methods-vec
               (list->vector
                 (generic-methods ,(generic-name (lookup-automaton-gf lua)))))
           )
       (lambda original-args
         (define (no-applicable-method)
           (error "No applicable methods for generic function"
                  (cons ',gf-name original-args)))
         (define (too-few-arguments)
           (##raise-wrong-number-of-arguments-exception
              ,gf-name
              original-args))
         (letrec
             ,(map (lambda (state)
                     (if (eq? 'next-methods
                              (car (state-precedence-order state)))
                         (state->method-code state gf-name gf)
                         (state->code state)))
                   (apply append
                          (vector->list
                           (lookup-automaton-states lua))))
           (if (< (length original-args) ,num-required-args)
               (too-few-arguments)
               (,(state-id (lookup-automaton-start-state lua))
                original-args)))
) ) ) )

(define (method->ref gf-name method gf-methods)
  `(vector-ref methods-vec ,(list-pos method gf-methods)))
         
(define (state->method-code state gf-name gf)
  (let* ( (state-name  (state-id state))
          (gf-methods  (generic-methods gf))
          (method-list (cdr (state-precedence-order state)))
          (num-methods (length method-list))
          (method-refs (map (lambda (m)
                              (method->ref gf-name m gf-methods))
                            method-list))
        )
    (if (= 1 num-methods)
        `(,state-name
          (lambda ignored
            (apply ,(car method-refs)
                   (cons no-next-method original-args))))
        (let (  (next-method-chain
                 (cons 'list
                       (append method-refs '(no-next-method))))
              )
          `(,state-name
            (lambda ignored
              (oops#invoke-method original-args ,next-method-chain))))
 ) ) )

;==============================================================;
;;; INVOKE-METHOD
(define (invoke-method original-args next-method-chain)
  (let next-method-loop
                ( (remaining-methods next-method-chain)
                  (old-args original-args)
                )
              ;; (apply method next-method . args)
              (apply (car remaining-methods)
                     (cons (lambda new-args
                             (next-method-loop
                              (cdr remaining-methods)
                              (if (null? new-args) old-args new-args)))
                           old-args))
) )


;==============================================================;
;;; CLASS-ID-BINARY-SEARCH -- specialized for dispatch
(define (class-id-binary-search search-arg other-args
                                search-vec next-state-vec
                                failed)
  ;; PRECONDITION: search-vec and next-state-vec are padded at the end
  ;; with one 'extra' slot.  For next-state-vec this is no-applicable-method.
  (let ( (wanted (class-id (class-of search-arg))) )
;     (when debug-generics
;       (format #t "~&class-id-binary-search class=~a id=~a"
;               (class-name (class-of search-arg)) wanted))
    (let loop ( (left 0) (right (- (vector-length search-vec) 2)) )
;       (when debug-generics
;             (format #t
;                     "~&class-id-binary-search loop-entry: left=~a right=~a"
;                     left right))
      (if (>= left right)
          ;; stop searching; check
          (if (and (= left right)
                   (<= (vector-ref search-vec left)
                       wanted
                       (vector-ref search-vec (+ left 1))))
              ;; found range -- apply next-state
              ((vector-ref next-state-vec left) other-args)
              ;; else fail
              (failed)
          )
          ;; search step
          (let* ( (mid   (truncate (/ (+ left right) 2)))
                  (probe (vector-ref search-vec mid))
                ) 
;            (when debug-generics
;               (format #t
;                       "~&class-id-binary-search mid=~a probe=~a"
;                       mid probe))
            (if (< wanted probe)
                (loop left (- mid 1))
                ;; (>= wanted probe)
                (if (< wanted (vector-ref search-vec (+ 1 mid)))
                    ;; found range -- apply next-state
                    ((vector-ref next-state-vec mid) other-args)
                    ;; else keep looking
                    (loop (+ mid 1) right)))
) ) ) ) )


;==============================================================;
;;; TEST CRUFT
#|
(include "i")
(define-class A <value> ())
(define-class B A ())
(define-class C A ())
(define-class D A ())
(define-class E (D C B) ())
(define-class F (C D) ())
(define-class G E ())
(define-class H F ())

(define-generic (M (one A)(two A)(three A)(four A))
  (list 'generic-default-M one two three four))

;; m1
(define-method (M (one A) (two B) (three B) (four B))
  (list 'M1 one two three four))
;; m2
(define-method (M (one C) (two C) (three B) (four B))
  (list 'M2 one two three four))
;; m3
(define-method (M (one C) (two D) (three A) (four F))
  (list 'M3 one two three four))

(define lua-for-m (oops#construct-lookup-automaton M))

(oops#display-states lua-for-m)
(newline)
(oops#display-states (oops#simplify-lua-states lua-for-m))
                                        
(pp (oops#lua->dispatch-code lua-for-m)) ;; now simplified

;=======================;
(define lua-for-elts=? (oops#construct-lookup-automaton elts=?))

(oops#display-states lua-for-elts=?)
(newline)

(oops#display-states (oops#simplify-lua-states lua-for-elts=?))
(newline)

(define dispatch-code
  (oops#lua->dispatch-code lua-for-elts=?))

;(pp dispatch-code)
;(newline)

(define dispatch (eval dispatch-code))

(dispatch "abc" "abc")

;;=========================
(define-class <point> ()  ( (x init-value: 0) (y init-value: 0) ) )
(define-class <depth> ()  ( (z init-value: 0) ) )
(define-class <ship> ( <point> ) ( (direction init-value: 40) ) )
(define-class <submarine> ( <ship> <depth> ) ())

(define ship1 (<ship> x: 23 y: 14))
(define sub1  (<submarine> x: 124 z: -23))

(define-method (->string (p <point>))     (format #f "#<point x:~a y:~a>" (x p) (y p)))
(define-method (->string (s <ship>))      (format #f "#<ship ~a>" (next-method)))
(define-method (->string (s <submarine>)) (format #f "#<submarine ~a depth:~a>" (next-method) (z s)))

(define lua-for->string (oops#construct-lookup-automaton ->string))

(oops#display-states lua-for->string)
(newline)

(oops#display-states (oops#simplify-lua-states lua-for->string))
(newline)

(define ->string-code
  (oops#lua->dispatch-code lua-for->string))

(define ->str (eval ->string-code))

(->str sub1) ;;==> "#<submarine #<ship #<point x:124 y:0>> depth:-23>"

;=======================;
(define lua-for-as (oops#construct-lookup-automaton as))

(oops#display-states lua-for-as)
(newline)

(oops#display-states (oops#simplify-lua-states lua-for-as))
(newline)

(define as-dispatch-code
  (oops#lua->dispatch-code lua-for-as))

(pp as-dispatch-code)
(newline)

(define as-dispatch (eval as-dispatch-code))

(as-dispatch <vector> "string")

|#  ;; end Test Cruft
;==============================================================;

#| vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;; Old, working dispatch code [Simple but interpreted].

;(define (compute-applicable-methods gf actual-args)
;  (filter (lambda (m) (method-appliable? m actual-args))
;          (generic-methods gf)))

(define (filter+sort-applicable-methods gf actual-args)
  ;; insert sort of applicable-methods
  (define (insert m methods)
    (cond
     ((null? methods) (cons m methods))
     ((method<=? m (car methods))
      (cons m methods))
     (else (cons (car methods) (insert m (cdr methods)))))
    )
     
  (let loop ( (result '())
              (methods (generic-methods gf))
            )
    (cond
     ((null? methods) result)
     ((method-applicable? (car methods) actual-args)
      (loop (insert (car methods) result) (cdr methods)))
     (else (loop result (cdr methods))))
 ) )


(define (generic-dispatch gf actual-args)

  (define (but-first n list)
    (if (> n 0) (but-first (- n 1) (cdr list)) list))

  (define (actual-keys gf-args-info)
    ;; Only called when keywords are required
    (let* ( (optionals?    (method-args-optional? gf-args-info)) ;; #f or number
            (num-optionals (if optionals? optionals? 0))
            (num-required  (method-args-num-required gf-args-info))
            (key-args
            (but-first (+ num-required num-optionals) actual-args))
          )
      (when (not (even? (length key-args)))
        (call-error "Uneven number of keyword/argument pairs"
                    (generic-name gf) key-args))
      (let loop ( (keys '()) (key-args key-args) )
        (if (null? key-args)
            keys ;; order not required
            (loop (cons (car key-args) keys) (cddr key-args)))      
  ) ) )

  (define (too-few-keys? gf-args-info)
    (let* (  ;; keys? but NOT all-keys:
           (required-keys? (eq? #t (method-args-keys? gf-args-info)))
           (gf-keys        (and required-keys? (method-args-keys gf-args-info)))
           (supplied-keys  (and required-keys? (actual-keys gf-args-info)))
          )
      (if (and required-keys?
               (not (every? (lambda (key) (memq key supplied-keys))
                            gf-keys)))
          (call-error "Missing required keywords"
                      (generic-name gf)
                      (map-append
                       (lambda (key) (and (not (memq key supplied-keys)) key))
                       gf-keys)
                      actual-args)
          #f)))

  ;; Assume things will go well in the typical case
  (let* ( (applicable-methods (filter+sort-applicable-methods gf actual-args))
          (gf-args-info       (generic-args-info    gf))
          (next-method-list   (append applicable-methods
                                     (list no-next-method)))
        )
    (cond
     ((not (or (too-few-keys? gf-args-info) (null? applicable-methods)))
      ;; call 'em with next-method chain
      (let next-method-loop ( (remaining-methods next-method-list)
                              (old-args actual-args)
                            )
        ;; (apply method next-method . args)
        (apply (car remaining-methods)
               (lambda new-args
                 (next-method-loop
                  (cdr remaining-methods)
                  (if (null? new-args) old-args new-args)))
               old-args)
      ))
     ((> (method-args-num-required (generic-args-info gf))
         (length actual-args))
      (call-error "Too few arguments supplied"
                  (generic-name gf)
                  (method-args-num-required (generic-args-info gf))
                  actual-args)
      )
     ((and (> (length actual-args) (method-args-num-required gf-args-info))
           (not (or (method-args-optional? gf-args-info)
                    (method-args-keys?     gf-args-info)
                    (method-args-rest?     gf-args-info))))
      (call-error "Too many arguments supplied" (generic-name gf)
                  (method-args-num-required gf-args-info)
                  actual-args)
      )
     ;; Args look OK at this point
     (default-method
       (apply default-method (cons no-next-method actual-args))
       )
     (else
      (call-error "No applicable methods" (generic-name gf) gf actual-args)))
) )

|# ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(when debug-oops
      (format #t "~%Oops: loaded GENERICS code"))

;===========================E=O=F==============================;

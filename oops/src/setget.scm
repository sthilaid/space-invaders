;;; FILE: "setget.scm"
;;; IMPLEMENTS: Instance setters & getters for Oops
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
  <value>
  class?
  class-of
  format
  instance?
  subclass?
  subtype?
  is-a?
  type?

  debug-oops
))

;; local names
(##namespace
 ("oops#"
  %%num-standard-ref-slots%%
  %%num-standard-set!-slots%%
  %%ref-tag%%
  %%set!-tag%%
  %%the-slots-of-ref%%
  %%the-slots-of-set!%%
  <ref>
  <set!>
  bound?
  class-insert-sort
  class-more-specific?
  ensure-setter
  env-define!
  env-lookup
  make-ref-ref
  make-ref-set!
  make-set!-ref 
  make-set!-set!
  ensure-slot-ref  ; export
  ensure-slot-set! ; export
  proc->callable
  proc->ref
  proc->set!
  ref-getters
  ref-getters-set!
  ref-slot-bind
  ref?
  set!-setters
  set!-setters-set!
  set!-slot-bind
  set!?
  unquote
))
;==============================================================;
(include "common-macros.scm")
;==============================================================;
;; Getters & setters may be
;   - indexed    (instance)
;   - procedures (virtual)
;   - cells      (class)
;
; As slot names must be unique within a class, the class which defines
; a slot determines the slot-offset of indexed slots.  This implies
; that class tests are ordered by CLASS-MORE-SPECIFIC?.
;
; Note that slots are inherited "in order" from parent classes, so
; the 1st class "single inherits" the same offsets.  Rather than
; keeping track of which slots "singly inherit", we check and filter
; those that we don't need to add.  I.e. the slots which have the same
; index of a class they inherit from.
;
; We require IS-A? test for the defining class and optional slot-value
; type for setters.

; ##FIXME: Use binary search (see "generics.scm") when large # of
; accessors with same name.
;
; We need to keep slot (meta) info in classes as can inline accessors
; based on class dispatch knowledge in:
;    - init-fun's (slot initializers)
;    - virtual functions
;    - (some) methods

; Usages:
;   (ensure-slot-ref inst-class slot-name index         kind: indexed:)
;   (ensure-slot-ref inst-class slot-name virt-form     kind: virtual:)
;   (ensure-slot-ref inst-class slot-name cell-ref-form kind: cell:)

;   (ensure-slot-set! inst-class slot-name index          kind: indexed: value-class: class)
;   (ensure-slot-set! inst-class slot-name virt-form      kind: virtual: value-class: class)
;   (ensure-slot-set! inst-class slot-name cell-set!-form kind: cell:    value-class: class)

(define type? class?)       ;; bootstrap: Def'ed in "class-hieratchy.scm"
(define subtype? subclass?) ;; bootstrap: Def'ed in "class-hierarchy.scm"
(define (is-a?) 'bogus)     ;; bootstrap: Def'ed in "class-hierarchy.scm"

(define (un-quote exp)
  (if (and (pair? exp) (eq? 'quote (car exp)))
      (cadr exp)
      (error "Tried to remove 'quote from an un-quoted expression" exp))
 )
;=============================================================================
;;;ensure-slot-ref, <ref>
;; accessors must be callables w slot to add/replace info

(define (ensure-slot-ref class slot-name exp #!key (kind indexed:))
  (cond
   ((not (class? class))
    (call-error ensure-slot-ref "not a class" class)
    )
   ((not (symbol? slot-name))
    (call-error ensure-slot-ref "slot name not a symbol" slot-name)
    )
   ((not (memq kind '(indexed: virtual: cell:)))
    (call-error ensure-slot-ref "slot kind must be one of indexed: virtual: or cell:" kind)
    )
   (else
    (case kind
      ((indexed:) ;; exp is a non-negative integer
       (unless (and (integer? exp) (> exp -1))
         (call-error  ensure-slot-ref "index required, got" exp))
       (let ( (real-index (+ exp %%first-data-slot-index%%)) )
         (ensure-ref slot-name
                     class
                     (lambda (inst)
                       (or (instance? inst)
                           (error "Can't get slot value from a non-instance:" inst))
                       (##vector-ref inst real-index)) real-index)
       ))
      ((virtual:) ;; exp is source for procedure
       (with-exception-catcher
        (lambda (exn)
          (call-error  ensure-slot-ref "procedure required, got" exp))
        (lambda ()
          (let ( (proc (eval exp)) )
            (unless (procedure? proc)
              (call-error  ensure-slot-ref "procedure required, got" exp))
            (ensure-ref slot-name class proc #f)))
       ))
      ((cell:) ;; exp is code to obtain cell
       ;; @@@FIXME: checks
       (let ( (cell exp) )
         (ensure-ref slot-name class (lambda (inst) (cell-ref cell)) #f)
       ))
      (else (error "ensure-slot-ref: unknown kind" kind))
    ))
) )


(define %%the-slots-of-ref%%
  '(data-alist))

(define %%num-standard-ref-slots%%
  (length %%the-slots-of-ref%%) )

(define %%ref-tag%% '("ref")) ;; not eq? to anything else

;; (define-values (proc->ref ref? make-ref-ref make-ref-set! make-ref-bind!)
;;     (proc->callable %%ref-tag%% %%num-standard-ref-slots%%))
(define proc->ref     #f)
(define ref?          #f)
(define make-ref-ref  #f)
(define make-ref-set! #f)
(define ref-slot-bind #f)
(call-with-values
  (lambda ()
    (proc->callable %%ref-tag%% %%num-standard-ref-slots%%))
  (lambda (construct pred? mk-ref mk-set! mk-bind!)
    (set! proc->ref     construct)
    (set! ref?          pred?)
    (set! make-ref-ref  mk-ref)
    (set! make-ref-set! mk-set!)
    (set! ref-slot-bind mk-bind!)))

(define ref-getters
  (make-ref-ref  "getters" 0)) ;; ((name class proc index).. )

(define ref-getters-set!
  (make-ref-set! "getters" 0))

(define-structure getter-info class proc index)

(define <ref> ;; Create & return a new getter
  (lambda (name class proc index)
    (unless (and (symbol?    name)
                 (class?     class)
                 (procedure? proc)
                 (or (eq? index #f)
                     (and (integer? index) (> index -1))))
      (call-error "Can't make <ref> with missing/broken args"
                  <ref> name class proc index))
    (letrec ( (getter  ;;@@FIXME: caching
               (lambda (instance)
                 (let ( (inst-class (class-of instance)) )
                   (let loop ( (left-to-try (ref-getters new-ref)) )
                     (cond
                      ((null? left-to-try)
                       (call-error name "no getter found for" instance)
                       )
                      ((subclass? inst-class (getter-info-class (car left-to-try)))
                       ((getter-info-proc (car left-to-try)) instance)
                       )
                      (else (loop (cdr left-to-try))))))
                 ))
              (new-ref #f)
            )
      (set! new-ref (proc->ref getter))
      (ref-slot-bind new-ref
                     0
                     (list (make-getter-info class proc index)))
      new-ref
) ) )

(define (ensure-ref slot-name instance-class proc index) ;; index is #f or a slot-index
    (cond
     ((not (bound? slot-name))
      (env-define! slot-name (<ref> slot-name instance-class proc index))
      )
     ((not (ref? (env-lookup slot-name)))
      (unless (memq (env-lookup slot-name) '(#!unbound bootstrap))
        (warn "*** Redefining to getter:" slot-name)
        (when debug-oops   ;;@@@DEBUG@@
	  (display "was: ")
          (oops#dump (env-lookup slot-name))
          (when (eq? #!void (env-lookup slot-name)) (step))
          )
        )
      (env-define! slot-name (<ref> slot-name instance-class proc index))
      )
     (else ;; getter exists, augment as required
      (let loop ( (left-to-try (ref-getters (env-lookup slot-name)))
                  (getter (env-lookup slot-name))
                )
        (cond
         ((null? left-to-try) ;; => new entry
          (ref-getters-set!
           getter
           (class-insert-sort (ref-getters getter) (make-getter-info instance-class proc index)))
          )
         ((eq? instance-class (getter-info-class (car left-to-try))) ;; simple replacement (redef)
          (let ( (info (car left-to-try)) )
            (getter-info-proc-set!  info proc)
            (getter-info-index-set! info index))
          )

         ((subclass? instance-class (getter-info-class (car left-to-try))) ;; override if index changed
          (let ( (info (car left-to-try)) )
            (if (and index (getter-info-index info) (= index (getter-info-index info)))
                'unneeded
                (ref-getters-set!
                 getter
                 (class-insert-sort (ref-getters getter)
                                    (make-getter-info instance-class proc index))))
          ))
         (else (loop (cdr left-to-try) getter))))
      ) )
)

;; Keep things sorted by CLASS-MORE-SPECIFIC? (more specific 1st)
(define (class-insert-sort list-of-access-info set-or-ref-thing)
  (define (get-class thing) ;; OO needed here ;^)
    (if (getter-info? thing)
        (getter-info-class thing)
        (setter-info-inst-class thing))
  )
  (let ( (class-of-thing (get-class set-or-ref-thing)) )
    (define (insert-sort list-of-info)
      (cond
       ((null? list-of-info) (list set-or-ref-thing))
       ((class-more-specific? class-of-thing (get-class (car list-of-info)))
        (cons set-or-ref-thing list-of-info))
       (else (cons (car list-of-info) (insert-sort (cdr list-of-info)))))
      )
    (insert-sort list-of-access-info)
 ) )

;=============================================================================
;;;ensure-slot-set!, <set!>
;; accessors must be callables w slot to add/replace info

(define (ensure-slot-set! class slot-name exp #!key (kind indexed:) (value-class <value>))
  (cond
   ((not (or (type? class) (class? class)))
    (call-error ensure-slot-set! "not a class|type" class)
    )
   ((not (symbol? slot-name))
    (call-error ensure-slot-set! "slot name not a symbol" slot-name)
    )
   ((not (memq kind '(indexed: virtual: cell:)))
    (call-error ensure-slot-set! "slot kind must be one of indexed: virtual: or cell:" kind)
    )
   ((not (or (type? value-class) (class? value-class)))
    (call-error ensure-slot-set! "not a class|type:" value-class)
    )
   (else
    (case kind
      ((indexed:) ;; exp is a non-negative integer
       (unless (and (integer? exp) (> exp -1))
         (call-error  ensure-slot-set! "index required, got" exp))
       (let ( (real-index (+ exp %%first-data-slot-index%%)) )
         (ensure-setter slot-name
                        class
                        value-class
                        (lambda (inst new-val)
                          (or (instance? inst)
                              (error "Can't set slots in a non-instance:" inst))
                          (##vector-set! inst real-index new-val))
                        real-index)
       ))
      ((virtual:) ;; exp is source for procedure: ''(lambda (i v) BODY)
       (with-exception-catcher
        (lambda (exn)
          (call-error  ensure-slot-set! "procedure required, got" exp))
        (lambda ()
          (let ( (proc (eval exp)) )
            (unless (procedure? proc)
              (call-error  ensure-slot-set! "procedure required, got" exp))
            (ensure-setter slot-name class value-class proc #f)))
       ))
      ((cell:) ;; exp is code to obtain cell
       ;; @@@FIXME: checks
       (let ( (cell exp) )
         (ensure-setter slot-name class value-class (lambda (inst new-val) (cell-set! cell new-val)) #f)
       )))
) ) )


(define %%the-slots-of-set!%%
  '(data-alist))

(define %%num-standard-set!-slots%%
  (length %%the-slots-of-set!%%) )

(define %%set!-tag%% '("set!")) ;; not eq? to anything else

;; (define-values (proc->set! set!? make-set!-ref make-set!-set! make-set!-bind!)
;;     (proc->callable %%set!-tag%% %%num-standard-set!-slots%%))
(define proc->set!     #f)
(define set!?          #f)
(define make-set!-ref  #f)
(define make-set!-set! #f)
(define set!-slot-bind #f)
(call-with-values
  (lambda ()
    (proc->callable %%set!-tag%% %%num-standard-set!-slots%%))
  (lambda (construct pred? mk-ref mk-set! mk-bind!)
    (set! proc->set!     construct)
    (set! set!?          pred?)
    (set! make-set!-ref  mk-ref)
    (set! make-set!-set! mk-set!)
    (set! set!-slot-bind mk-bind!)))


(define set!-setters
  (make-set!-ref "setters" 0))

(define set!-setters-set!
  (make-set!-set! "setters" 0))

(define-structure setter-info inst-class value-class proc index)

(define <set!> 
  (lambda (name class proc value-proc index)
    (unless (and (or (class? class) (type? class))
                 (procedure? proc)
                 (or (class? value-proc) (type? value-proc) (procedure? value-proc))
                 (symbol?    name))
      (call-error "Can't make <set!> with missing/broken args"
                  <set!> name class proc value-proc))
    (let* ( (new-set! #f)
            (setter  ;;@@FIXME: caching
               (lambda (instance new-value)
                 (let ( (inst-class  (class-of instance))
                        (value-class (class-of new-value))
                      )
                   (let loop ( (left-to-try (set!-setters new-set!)) )
                     (cond
                      ((null? left-to-try)
                       (call-error name "no setter found for" instance)
                       )
                      ((let ( (info (car left-to-try)) )
                         (and (subclass? inst-class  (setter-info-inst-class  info))
                              (is-a?     new-value   (setter-info-value-class info))
                              (setter-info-proc info)))
                       => (lambda (setter) (setter instance new-value))
                       )
                      (else (loop (cdr  left-to-try))))
              ) )))        
            )
      (set! new-set! (proc->set! setter))
      (set!-slot-bind new-set!
                      0
                      (list (make-setter-info class value-proc proc index)))
      new-set!
) ) )

(define (ensure-setter slot-name instance-class value-class proc index)
  (let ( (setter-name
          (string->symbol (string-append (symbol->string slot-name)
                                         "-set!"))) 
       )
    (cond
     ((not (bound? setter-name))
      (env-define! setter-name
                   (<set!> setter-name instance-class proc value-class index))
      )
     ((not (set!? (env-lookup setter-name)))
      (when (and (bound? slot-name)
                 (not (memq (env-lookup slot-name)
                            '(#!unbound bootstrap))))
        (warn "*** Redefining to setter:" setter-name)
	(display "was: ")
        (when debug-oops (oops#dump (env-lookup slot-name))) ;;@@@DEBUG@@
        )
      (env-define! setter-name
                   (<set!> setter-name instance-class proc value-class index))
      )
     (else ;; setter exists, augment as required
      (let ( (setter (env-lookup setter-name)) )
        (let loop ( (left-to-try (set!-setters setter)) )
          (cond
           ((null? left-to-try)
            ;; add new entry
            (set!-setters-set!
             setter
             (class-insert-sort (set!-setters setter)
                                (make-setter-info instance-class value-class proc index)))
            )
           ((eq? instance-class (setter-info-inst-class (car left-to-try)))
            ;; simple replacement (redef)
            (let ( (info (car left-to-try)) )
              (setter-info-value-class-set! info value-class)
              (setter-info-proc-set!        info proc)
              (setter-info-index-set!       info index))
            )
           ((subclass? instance-class (setter-info-inst-class (car left-to-try)))
            ;; override if index changed or value-class differs
            (let ( (info (car left-to-try)) )
              (if (and index ;;@@FIXME: index #f => ???
                       (setter-info-index info)
                       (= index         (setter-info-index       info))
                       (eq? value-class (setter-info-value-class info)))
                'unneeded
                (set!-setters-set!
                 setter
                 (class-insert-sort (set!-setters setter)
                                    (make-setter-info instance-class value-class proc index)))
                )
            ))
           (else (loop (cdr left-to-try)))))
        ) )
) ) )


;==============================================================;

(when debug-oops
      (format #t "~%Oops: loaded Setter/Getter bootstrap code"))

;===========================E=O=F==============================;

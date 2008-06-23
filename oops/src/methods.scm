;;; FILE: "methods.scm"
;;; IMPLEMENTS: Method utility code for Oops
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
  %method	;; @@ move to oops# ??
  <method>
  <type>
  <value>
  any?
  arg-applicable?
  class-direct-subclasses
  class-name
  class?
  every?
  format
  is-a?
  member?
  method-applicable?
  method-args
  method-args-congruent?
  method-args-info
  method-info-specializers<=?
  method-name
  method-procedure
  method-slot-bind
  method<=?
  method?
  subtype?
  type?

  debug-oops
))

;; local names
(##namespace
 ("oops#"
  %%method-tag%%
  %%num-standard-method-slots%%
  %%the-slots-of-method%%
  make-method-ref
  make-method-set!
  method-args->implicit-generic-function-args
  method-args->lambda-list  
  parse-method-args
  proc->method
))

;=============================================================================
(include "common-macros.scm")
;=============================================================================

(define-structure method-args
  num-required specializers rest? optional? keys? keys form)

;;;; args = name | (name+ . rest-name) | (reqs opts rest keys)
;;;; reqs = name*
;;;; opts = #!optional opt* | empty 
;;;;  opt = name | (name init)
;;;; rest = #!rest name | empty 
;;;; keys = #!key key* | empty 
;;;;  key = name | (name init)
;;;
;;; Nota Bene: all-keys: is signified if #!key is used but
;;;  no keywords are specified.

(define (parse-method-args method-args)
  ;; Takes a method parameter-list and
  ;;  returns a method-arg structure.
  (define (bad-specializer name)
    (call-error "Bad specializer" parse-method-args name method-args)
    )
  (define (assure-type name)
    (cond
     ((symbol? name)
      (let* ( (probe (and (bound? name) (env-lookup name)))
              (t (if (or (not probe) (eq? probe #!unbound))
                     (name->class-info name) ;; Check compile-time-db as well
                     probe))
            )
         (if (not (or (type? t) (class-info? t)))
             (bad-specializer name)
             t)
       ))
     (else
      (with-exception-catcher
       (lambda (exn)
         (let ( (probe (name->class-info name)) )
           (if probe
               probe
               (bad-specializer name))))
       (lambda ()
         (let ( (type (eval name)) )
           (or (type?       type)
               (class-info? type)
               (bad-specializer name))
           type))))
    ))

  (cond
   ((symbol? method-args) ;; (method foo ...) => 'foo
    (make-method-args 0 '() #t #f #f '() method-args)
    )
   ((not (pair? method-args))
    (call-error "Arguments must be a symbol or method arg-list"
                parse-method-args method-args)
    )
   (else
    (let loop ( (args         method-args)
                (state        'req) ;; req opts rest keys
                (required     '())
                (specializers '())
                (rest         '())
                (optional     '())
                (keys         '())
              )
#|      (when debug-oops
          (format #t "~&args=~s state=~a req=~s spec=~s rest=~s opt=~s key=~s~&"
                args state required specializers rest optional keys))
 |#
      (cond
       ((null? args) ;; parse is complete
        (if (> (length rest) 1)
            (call-error "Too many rest arguments"
                        parse-method-args rest)
            (make-method-args (length required)
                              (reverse specializers)
                              (not (null? rest))
                              (if (null? optional) #f (length optional))
                              (cond
                               ((not (null? keys)) #t)
                               ((memq #!key method-args) all-keys:)
                               (else #f))
                              (reverse (map (lambda (x)
                                              (->keyword
                                               (if (pair? x) (car x) x)))
                                            keys))
                              method-args)
        ))
       ((eq? (car args) #!optional)
        (if (eq? state 'req)
            (loop (cdr args) 'opts required specializers rest optional keys)
            (call-error "Optional args must follow required arguments"
                         parse-method-args method-args))
        )
       ((eq? (car args) #!rest)
        (if (member? state '(req opts))
            (loop (cdr args) 'rest required specializers rest optional keys)
            (call-error "Rest arg must follow required and optional arguments"
                         parse-method-args method-args))
        )
       ((eq? (car args) #!key)
        (if (member? state '(req opts rest))
            (loop (cdr args) 'keys required specializers rest optional keys)
            (call-error "Key args must follow required, optional, and rest arguments"
                         parse-method-args method-args))
        )
       (else
        (case state
          ((req)
           ;; Only required args can have a type specializers as #!key and
           ;;  #!optional args can have initializers.
           (if (pair? (car args))
               (if (null? (cdar args))
                   (call-error "Bad specializer"
                               parse-method-args
                               (car args))
                   (loop (cdr args) state
                         (cons (car args) required)
                         (cons (assure-type (cadar args)) specializers)
                         rest optional keys)
               )
               (loop (cdr args) state
                     (cons (car args) required)
                     (cons <value> specializers)
                     rest optional keys)
               )
           )
          ((opts) ;; name or (name init-form)
           (loop (cdr args) state required specializers rest (cons (car args) optional) keys)
           )
          ((rest); single symbol
           (if (symbol? (car args))
               (loop (cdr args) state required specializers (cons (car args) rest) optional keys)
               (call-error "Bad rest argument" parse-method-args method-args))
           )
          ((keys) ;; name or (name init-form)
           (loop (cdr args) state required specializers rest optional (cons (car args) keys))
           )
          (else
           (call-error "Ill-formed method arguments"
                       parse-method-args args))
          ))

 ) ) ) ) )

;=============================================================================
;; <method>

(define %%the-slots-of-method%%
  '(name args-info procedure))

(define %%num-standard-method-slots%%
  (length %%the-slots-of-method%%) )

(define %%method-tag%% '("method")) ;; not eq? to anything else

;; (define-values (proc->method method? make-method-ref make-method-set! make-method-bind!)
;;     (proc->callable %%method-tag%% %%num-standard-method-slots%%))
(define proc->method     #f)
;(define method?          #f)
(define make-method-ref  #f)
(define make-method-set! #f)
(define method-slot-bind #f)
(call-with-values
  (lambda ()
    (proc->callable %%method-tag%% %%num-standard-method-slots%%))
  (lambda (construct pred? mk-ref mk-set! mk-bind!)
    (set! proc->method     construct)
    (set! method?          pred?)
    (set! make-method-ref  mk-ref)
    (set! make-method-set! mk-set!)
    (set! method-slot-bind mk-bind!)))


(define method-name
  (make-method-ref "name" 0))

(define method-args-info
  (make-method-ref "args-info" 1))

(define method-procedure
  (make-method-ref "procedure" 2))


(set! <method>
  (lambda (#!key name args-info procedure)
    (unless (and (symbol? name)
                 (method-args? args-info)
                 (procedure? procedure))
      (call-error "Can't make method with missing/broken args"
                  <method> name args-info procedure))
    (let ( (new-method (proc->method procedure)) )
      (method-slot-bind new-method 0 name)
      (method-slot-bind new-method 1 args-info)
      (method-slot-bind new-method 2 procedure)
      new-method
) ) )


;; Invoked by define-method macro
(define (%method name body)
  ;; name is foo when body is (method lambda-list form+)
  ;; name is (name lambda-list) when body is (form+)
  ;; @@FIXME: check for proper form
  (let ( (name (if (symbol? name) name (car name)))
         (args (if (list? name) (cdr name) (cadar body)))
         (body (if (list? name) body (cddar body)))
       )
    (let ( (args-info (parse-method-args args)) )

    `(<method> name: ',name
               args-info: (oops#parse-method-args ,(list 'quote args))
               procedure: (lambda ,(oops#method-args->lambda-list args-info)
                            ,@body)))
) )

;=============================================================================

(define (method-info-specializers<=? specific general)
  (let loop ( (s (method-args-specializers specific))
              (g (method-args-specializers  general))
            )
    (cond
     ((null? s) (null? g))
     ((subtype? (car s) (car g)) 
      (loop (cdr s) (cdr g)))
     (else #f))
) )

(define (method-args-congruent? meth gen)
  ;; Nota Bene: Parameter list congruency.differs from Dylan.
  ;; No type for rest args; No generic rest arg w/o method rest arg.
  (and (method-args? meth)
       (method-args? gen)
       (= (method-args-num-required meth) (method-args-num-required gen))
       (method-info-specializers<=? meth gen)
       (cond
        ((method-args-keys? meth)
         (if (method-args-keys? gen)
             (if (method-args-rest? meth)
                 (method-args-rest? gen)
                 #f)
             #f)
         )
        ((method-args-rest? gen)
         (method-args-rest? meth)) ;; NB: Differs from Dylan
        (else #t)
        ) ; end cond
       (if (method-args-keys? gen)
           (or (eq? all-keys: (method-args-keys? gen))
               (eq? all-keys: (method-args-keys? meth))
               (let ( (gen-keys  (method-args-keys gen))
                      (meth-keys (method-args-keys meth))
                    )
                 ;; Method has all required keywords?
                 (and (not (null? meth-keys))
                      (every? (lambda (k) (member? k gen-keys))
                         (method-args-keys meth)))))
           #t)
) )


(define (method-args->implicit-generic-function-args method-args)
  ;; returns a method-args structure
  (make-method-args
   (method-args-num-required method-args)
   (map (lambda (any) <value>) (method-args-specializers method-args))
   (method-args-rest?      method-args)
   (method-args-optional?  method-args)
   (and (method-args-keys? method-args) #t) ;; NOT all-keys:
   '() ;; no specific keys
   (cons defaulted-args-from: (method-args-form method-args))
   )
)

(define (method-args->lambda-list method-args)
  ;; remove specializers; add next-method at front
  (let ( (args-form   (method-args-form method-args))
         (num-required (method-args-num-required method-args))
       )
    (cond
     ((symbol? args-form) `(next-method . ,args-form))
     ((zero? num-required) (cons 'next-method args-form))
     (else
      (let loop ( (num-to-scan num-required)
                  (partial-result '())
                  (rest args-form)
                )
        (if (zero? num-to-scan)
            (cons 'next-method (append (reverse partial-result) rest))
            (let ( (required-arg (car rest)) )
              (loop (- num-to-scan 1)
                    (cons (if (symbol? required-arg) required-arg (car required-arg))
                          partial-result)
                    (cdr rest)))))))
) )

(define (arg-applicable? specializer arg)
  (is-a? arg specializer))

(define (method-applicable? method actual-args)
  (let ( (desired-args-info (method-args-info method)) )
    (cond
     ((< (length actual-args) (method-args-num-required desired-args-info))
      #f)
     ((null? actual-args)
      #t) ;; no specializers
     (else
      (let ( (specializers (method-args-specializers desired-args-info)) )
        (if (null? specializers)
            #t ;; no specializers
            (let loop ( (specializer       (car specializers))
                        (rest-specializers (cdr specializers))
                        (arg         (car actual-args))
                        (rest-args   (cdr actual-args))
                      )
              (cond
               ((not (arg-applicable? specializer arg)) #f)
               ((null? rest-specializers)
                (or (null? rest-args) (method-args-rest? desired-args-info))
                )
               (else
                (loop (car rest-specializers) (cdr rest-specializers)
                      (car rest-args)         (cdr rest-args) ))))))
 ) ) ) )


(define (method<=? specific general) ;; a.k.a. method-more-specific?
  ;; NB: only checks specializers
  (or (eq? specific general)
      (and (method? specific)
           (method? general)
           (method-info-specializers<=? (method-args-info specific)
                                        (method-args-info general))))
)

(when debug-oops
      (format #t "~%Oops: loaded METHODS code"))

;===========================E=O=F==============================;

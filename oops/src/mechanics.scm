;;; FILE: "mechanics.scm"
;;; IMPLEMENTS: Utilities for Oops
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
;; names exported to top level
(##namespace
 (""
  any?			; export
  every?		; export
  format
  member?		; export

  debug-oops
))

;; local names
(##namespace
 ("oops#"
  %%clone-proc-plus
  ->keyword
  alist-cons
  apply*
  bound?
  cell-ref
  cell-set!
  cell?
  env-define!
  env-ensure!
  env-lookup
  env-set!
  filter
  for-each*
  interaction-environment
  id1
  iota
  list*
  list-find-first
  list-pos
  list-remove
  list-remove!
  list-sort
  make-assq
  make-cell
  make-closure
  make-member-pred
  map*
  map-append
  proc->callable
  remove-duplicates
  warn
  ;; DEBUG
  dump
))
;=============================================================================
(include "common-macros.scm") 

(define (every? pred list . lists) 
  (let loop ( (lists (cons list lists)) )
    (cond ((null? (car lists)) #t)
          ((apply pred (map car lists))
	   (loop (map cdr lists)))
          (else #f))))

(define (any? pred list) 
  (let loop ( (list list) )
    (cond ((null? list) #f)
          ((pred (car list))) ;;; return the value
          (else (loop (cdr list))))))

(define (member? thing list)
  (and (memq thing list) #t))

(define (make-member-pred same?)
  (if (eq? same? eq?)
      member?
      (lambda (thing list)
        (let loop ( (things list) )
          (cond
           ((null? things) #f)
           ((same? thing (car things)) #t)
           (else (loop (cdr things))))))))

(define (->keyword name)
  (cond
   ((string? name) (string->keyword name))
   ((symbol? name) (string->keyword (symbol->string name)))
   ((keyword? name) name)
   (else (error "->keyword: can't make symbol from " name))))

(define (map* fn . list) 	; A map which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car list)) '())
   ((pair? (car list)) 
    (cons (apply fn      (map car list))
	  (apply map* fn (map cdr list))))
   (else  (apply fn list))))

(define list*
  (lambda args
    (letrec ( (chase
	       (lambda (args)
		 (cond ((null? args) '())
		       ((null? (cdr args)) (car args))
		       (else (cons (car args) (chase (cdr args))))))) )
      (chase args))))

(define (apply* proc . args)
  (apply proc (apply list* args)))

(define (for-each* fn . list) 	; A for-each which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car list)) '())
   ((pair? (car list)) 
    (apply fn (map car list)) 
    (apply for-each* fn (map cdr list)))
   (else (apply fn list))))

(define (map-append proc . lists)
      (apply append (apply map (cons proc lists))))

(define (make-assq same? default)
  (lambda (key alist)
    (let loop ( (alist alist) )
      (cond
       ((null? alist) default)
       ((same? key (caar alist)) (car alist))
       (else (loop (cdr alist)))))))

(define (alist-cons a b alist)
  (cons (cons a b) alist))

(define (list-remove! list what #!optional (is-equal? eq?))
  (cond
   ((null? list) '())
   ((not (list? list))
    (call-error "argument is not a list " list-remove! list))
   ((is-equal? (car list) what)
    (warn "can't delete! 1st element of a list" what list)
    (cdr list))
   (else ;; general case
    (let loop ( (prev list) (next (cdr list)) )
      (cond 
       ((null? next) list)
       ((is-equal? what (car next))
	(set-cdr!  prev (cdr next))
	list)
       (else
	(loop next (cdr next))))))
   )
)

(define (list-remove list elt #!optional (is-equal? eq?))
  (cond
   ((null? list) '())
   ((is-equal? elt (car list)) (cdr list))
   (else (cons (car list) (list-remove elt (cdr list)))))
)

(define (id1 x) x) ;; identity function of 1 argument

(define (remove-duplicates list-of-thing accessor)
  (let loop ( (results '()) (remaining list-of-thing) )
    (if (null? remaining)
	(reverse results)
	(let ( (thing (car remaining)) )
	  (if (member? (accessor thing) (map accessor results))
	      (loop results              (cdr remaining))
	      (loop (cons thing results) (cdr remaining))))
) ) )

(define (filter pred? list)
  (cond ((null? list) '())
	((pred? (car list)) 
	 (cons (car list) (filter pred? (cdr list))))
	(else (filter pred? (cdr list)))))

(define (list-find-first pred? list)
  (let loop ( (elts list) )
    (cond
     ((null? elts) #f) ;; not found
     ((pred? (car elts)) (car elts))
     (else (loop (cdr elts))))
) )

(define (list-pos elt list)
  (let loop ( (pos 0) (list list) )
    (cond
     ((null? list) #f)
     ((eq? elt (car list)) pos)
     (else (loop (+ pos 1) (cdr list))))
) )

(define (list-sort unsorted-list <=?)
  (define (insert-sort thing sorted-list)
    (cond
     ((null? sorted-list) (list thing))
     ((<=? thing (car sorted-list))
      (cons thing sorted-list))
     (else (cons (car sorted-list) 
		 (insert-sort thing (cdr sorted-list)))))
    )
  (let loop ( (sorted '()) (unsorted unsorted-list) )
    (if (null? unsorted)
	sorted
	(loop (insert-sort (car unsorted) sorted)
	      (cdr unsorted)))
) )

(define (iota n) ;; return '(0 1 .. n)
  (if (< n 0)
      '() ;; for zero length collections
      (let loop ( (n n) (result '()) )
        (if (zero? n)
            (cons n result)
            (loop (- n 1) (cons n result))))
 ) )

(define (warn . args)
  (format #t "~&***WARNING: ~a~&" args))



;=============================================================================
;; LOW LEVEL ENVIRONMENT ACCESS
;;===============================
;; Gambit internal routines used.
;; Don't try this at home!  ;^}

(define (interaction-environment) '())

(define (bound? name)
  ;; Gambit binds globals as pointer in 3rd slot in symbol objects
  ;; [Gambit Version 4.0 beta 12]
  (cond
   ((symbol? name) (not (zero? (##vector-ref name 3))))
   ((string? name) (not (zero? (##vector-ref (string->symbol name) 3))))
   (else #f))
)

(define (env-lookup name) ;; name is a symbol
 ;; Gambit
 (if (bound? name)
     (##global-var-ref name) ;; Nota Bene: seg fault if unbound!!
     (call-error "Undefined variable" env-lookup name))
)

(define (env-define! var-sym val)
 ;; Gambit
  (unless (bound? var-sym)
	  (##make-global-var var-sym))
  (##global-var-set! var-sym val)
)

(define (env-set! var-sym val)
  ;; Gambit
  (if (bound? var-sym)
      (##global-var-set! var-sym val)
      (error "Variable must be DEFINEd before SET!" var-sym))
)

(define (env-ensure! name value)
  (if (bound? name)
      (env-set! name value)
      (env-define! name value)))


(define make-closure  ;; @@?? Integrate this ??@@
  (lambda (form) (eval form)))


;=============================================================================
;;; CALLABLES are procedures with slots (a.k.a. executable data structures)
;;;; Here used for Classes, Methods & Generic Functions.

;; A Callable is an extended Closure:
;; @@FIXME: !!! make wrapper work for compiled code !!! @@
;;   slot 0 => <type> ==> <procedure>
;;   slot 1 => $code [NB: Circular self reference]
;;   slot 2 => proc
;;   slot 3 => runtime-environment (or #f)
;;   slot 4 => tag
;;   slot 5 => 1st data slot
;;   slot 6 => 2nd data slot
;;   ...


(define %%clone-proc-plus
  (let ( (%%interp-proc-type%% (##vector-ref (eval '(lambda (x) x)) 0)) )
    (lambda (orig-proc num-slots-to-add)
      (if (not (procedure? orig-proc))
          (error "Can't clone a non-procedure" orig-proc)
          (let ( (proc 
                  (if (or (not (##subtyped? orig-proc))
                          (< (##vector-length orig-proc) 4)
                          (not (eq? (##vector-ref orig-proc 0)
                                    %%interp-proc-type%%)))
                      ;;@@FIXME: debug wrapper hack for compiled code @@
                      (##make-interp-procedure orig-proc)  ;; @@@!!@@@ ;;
                      orig-proc))
                 )
            (let* ( (orig-length (##vector-length proc))
                    (new-length (+ num-slots-to-add orig-length))
                    (new-thing (##make-vector new-length #f))
                    (max-index (- orig-length 1))
                    )
              (##subtype-set! new-thing (##subtype proc))
              (let loop ( (index 0) )
                (##vector-set! new-thing
                               index
                               (##vector-ref proc index))
                (if (< index max-index)
                    (loop (+ index 1))
                    new-thing)))
) ) ) ) )

;; Returns (values constructor predicate make-ref make-set! bind-slot!)
(define proc->callable 
   (let* ( (tag-slot (##vector-length (eval '(lambda (x) x))))
           (first-data-slot-index (+ 1 tag-slot))
         )
     (lambda (tag num-slots)
       (letrec ( (proc->thing
                  (lambda (proc)
                    (let ( (new-proc (%%clone-proc-plus proc (+ 1 num-slots))) )
                      (##vector-set! new-proc tag-slot tag)
                      new-proc)))
                 (thing?
                  (lambda (obj)
                    (and (procedure? obj)
                         (> (##vector-length obj) first-data-slot-index)
                         (eq? tag (##vector-ref obj tag-slot)))))
                 (make-slot-ref
                  (lambda (name index)
                    (let ( (real-index (+ index first-data-slot-index)) )
                      (lambda (obj)
                        (if (and (thing? obj)
                                 (< real-index (##vector-length obj)))
                            (##vector-ref obj real-index)
                            (call-error "Cannot access" 'slot-ref name obj))))))
                 (make-slot-set!
                  (lambda (name index)
                    (let ( (real-index (+ index first-data-slot-index)) )
                      (lambda (obj new-value)
                        (if (and (thing? obj)
                                 (< real-index (##vector-length obj)))
                            (##vector-set! obj real-index new-value)
                            (call-error "Cannot slot-set!" name obj))))))
                 (slot-bind!
                  (lambda (obj index value)
                    (let ( (real-index (+ index first-data-slot-index)) )
                      (if (and (thing? obj)
                               (< real-index (##vector-length obj))
                               (not (##vector-ref obj real-index))) ;; #f is unbound default
                          (##vector-set! obj real-index value)
                            (call-error "Cannot slot-set!" index obj)))))
                 )
         (values proc->thing thing? make-slot-ref make-slot-set! slot-bind!)
 ) ) ) )

;=============================================================================
#|
(define-structure cell value)
(define cell-ref  cell-value)
(define cell-set! cell-value-set!)
|#

(define cell?     box?)
(define make-cell box)
(define cell-ref  unbox)
(define cell-set! set-box!)

;=============================================================================


;; DEBUG  for #subtype?'d things
(define (dump thing)
  (if (or (not (##subtyped? thing))
          (zero? (##vector-length thing)))
      (begin (pp thing) (newline))
      (let ( (max-index (- (##vector-length thing) 1)) )
        (let loop ( (index (if (procedure? thing) 2 0)) )
          (format #t "~%[~s]= ~s" index (##vector-ref thing index))
          (if (< index max-index)
              (loop (+ index 1))
              (newline))))))

;===========================E=O=F==============================;

;;; FILE: "Class.scm"
;;; IMPLEMENTS: Class datatype for Oops
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
  <class>			; export
  <value>
  any?
  class-direct-methods		; export
  class-direct-slots		; export
  class-direct-subclasses	; export
  class-direct-supers		; export
  class-direct-subtypes		; export
  class-name			; export
  class-of			; export
  class-precedence-list		; export
  class-slots			; export
  class-slot-bind
  class?			; export
  every?
  find-class			; export
  format
  instance-class
  instance?
  member?
  slot-info?
  slot-info-name
  slot-info-allocation
  slot-info-slot-ref
  slot-info-slot-set!
  slot-info-init-value
  slot-info-init-fun
  slot-info-type

  debug-oops  
))

;; local names
(##namespace
 ("oops#"
  %%class-tag%%
  %%num-standard-class-slots%%
  %%number-of-classes%%
  %%the-slots-of-class%%
  add-class-info
  add-subclass
  class-all-slots-set!
  class-compare
  class-direct-methods-set!
  class-direct-slots-set!
  class-direct-subclasses-set!
  class-direct-supers-set!
  class-direct-subtypes-set!
  class-expansion-db
  class-id
  class-id-set!
  class-info
  class-instance-init
  class-instance-init-set!
  class-depth
  class-depth-set!
  class-mark
  class-mark-set!
  class-max-id
  class-max-id-set!
  class-min-id
  class-min-id-set!
  class-more-specific?
  class-name-set!
  class-offset
  class-offset-set!
  class-precedence-list-set!
  class-ref
  class-slots-set!
  compute-cpl
  dump-class
  make-class-ref
  make-class-set!
  name->class-info
  proc->class
  really-add-subclass
  remove-useless-supers
  reset-subclass-encoding
  thing-direct-slots
  thing-direct-supers
  thing-name
  thing-precedence-list
))

;=============================================================================
(include "common-macros.scm")
;=============================================================================


;=============================================================================
;
; 		    C L A S S    B O O T S T R A P
;
;=============================================================================

;; Nota Bene:  <CLASS> is NOT an <INSTANCE> !!!

(define %%number-of-classes%% 0) ;; @@FIXME: should be slot in <class>

(define %%the-slots-of-class%% 
  (map process-slot
       '((name		            ;; '<class>
          init-function: (lambda () (gensym 'class)) ) ; 0
         (direct-supers             ;; (class ...)        
          init-value: ()) ; 1
         (direct-slots              ;; (slot-info ...)
          init-value: ()) ; 2
         (class-slots       	    ;; ( (name . cell) .. )
          init-value: ()) ; 3
         (class-precedence-list     ;; (class ...) 
          init-value: ()) ; 4
         (instance-init             ;; '(lambda (new-inst) ...) or #f
          init-value: #f) ; 5
         (direct-subclasses         ;; (class ...)
          init-value: ()) ; 6
         (direct-subtypes	    ;; (type ...)
          init-value: ()) ; 7
         (direct-methods	    ;; (method ...)
          init-value: ()) ; 8
         (id			;; for subclass test
          init-value: 0) ;  9
         (min-id		;; for subclass test
          init-value: 0) ; 10
         (max-id		;; for subclass test
          init-value: 0) ; 11
         (offset		;; for subclass test
          init-value: 0) ; 12
         (mark			;; for single-pass class traversal
          init-value: 0) ; 13
         (depth			;; for class-more-specific?
          init-value: 0) ; 14
       )
) )

(define %%num-standard-class-slots%%
  (length %%the-slots-of-class%%) )
 
(define %%class-tag%% '("class")) ;; not eq? to anything else

;; (define-values (proc->class class? make-class-ref make-class-set! class-slot-bind!)
;;     (proc->callable %%class-tag%% %%num-standard-class-slots%%))
(define proc->class     #f)
(define class?          #f)
(define make-class-ref  #f)
(define make-class-set! #f)
(define class-slot-bind #f)
(call-with-values
  (lambda ()
    (proc->callable %%class-tag%% %%num-standard-class-slots%%))
  (lambda (construct pred? mk-ref mk-set! mk-bind!)
    (set! proc->class     construct)
    (set! class?          pred?)
    (set! make-class-ref  mk-ref)
    (set! make-class-set! mk-set!)
    (set! class-slot-bind mk-bind!)))

(define class-name
  (make-class-ref  "name" 0))
  
(define class-name-set!
  (make-class-set! "name" 0))

(define class-direct-supers
  (make-class-ref "direct-supers" 1))

(define class-direct-supers-set!
  (let ( (setter (make-class-set! "direct-supers" 1)) )
    (lambda (class supers)
      (setter class (remove-useless-supers supers)))))
  
(define class-direct-slots	;; (slot-info.. )
  (make-class-ref  "direct-slots" 2))

(define class-direct-slots-set!	
  (make-class-set! "direct-slots" 2))

(define class-slots		;; ((name . cell).. )
  (make-class-ref  "class-slots" 3))

(define class-slots-set!
  (make-class-set! "class-slots" 3))

(define class-precedence-list	;; (class ...)
      (make-class-ref  "class-precedence-list" 4))

(define class-precedence-list-set!
      (make-class-set! "class-precedence-list" 4))

(define class-instance-init	;; (lambda (inst) body) or #f
  (make-class-ref  "instance-init" 5))

(define class-instance-init-set!
  (make-class-set! "instance-init" 5))

(define class-direct-subclasses	;; (class ...)
  (make-class-ref  "direct-subclasses" 6))

(define class-direct-subclasses-set!
  (make-class-set! "direct-subclasses" 6))

(define class-direct-subtypes	;; (type  ...)
  (make-class-ref  "direct-subtypes" 7))

(define class-direct-subtypes-set!
  (make-class-set! "direct-subtypes" 7))

(define class-direct-methods	;; (method ...)
    (make-class-ref  "direct-methods" 8))

(define class-direct-methods-set!
    (make-class-set! "direct-methods" 8))

(define class-id
  (make-class-ref  "id" 9))

(define class-id-set!
  (make-class-set! "id" 9))

(define class-min-id ;; min id of direct-subclasses
  (make-class-ref  "min-id" 10))

(define class-min-id-set!
  (make-class-set! "min-id" 10))

(define class-max-id ;; max id of direct subclasses
  (make-class-ref  "max-id" 11))

(define class-max-id-set!
  (make-class-set! "max-id" 11))

(define class-offset ;; offset into packed vector encoding (see "subclass.scm")
  (make-class-ref  "offset" 12))

(define class-offset-set!
  (make-class-set! "offset" 12))

(define class-mark ;; used internally in class traversal (see class-foreach)
  (make-class-ref  "mark"  13))

(define class-mark-set!
  (make-class-set! "mark"  13))

(define class-depth ;; 1 + max depth of parents
  (make-class-ref  "depth" 14))

(define class-depth-set!
  (make-class-set! "depth" 14))

;=============================================================================

;; As we desire to generate code at macro expansion time (e.g. to compile
;; definitions) we need to keep an expansion-time database of class/slot-info.
;; This is because compiling a definition does NOT cause it to be evaluated.
;; Thus we keep a name->class-info table.  When a name is in the table,
;; then it is used, else if the name as a class exists then we use that
;; information, else an error is raised.  The expansion table only has to
;; exist during compilation.

(define-structure class-info
  id: e5c35ale-6266-478c-9580-9687fc50f6b8
  (name read-only:)
  (direct-supers   unprintable: read-only:)
  (direct-slots    unprintable: read-only:)
  (class-slots     unprintable: read-only:)
  (precedence-list unprintable:))

;; The following are examples of why we want an object system..
(define (thing-name thing)
  (if (class? thing) (class-name thing) (class-info-name thing)))

(define (thing-direct-supers thing)
  (if (class? thing) (class-direct-supers thing) (class-info-direct-supers thing)))

(define (thing-direct-slots thing)
  (if (class? thing) (class-direct-slots thing) (class-info-direct-slots thing)))

(define (thing-class-slots thing)
  (if (class? thing) (class-slots thing) (class-info-class-slots thing)))

(define (thing-precedence-list thing)
  (if (class? thing) (class-precedence-list thing) (class-info-precedence-list thing)))

;; Use Gambit v4 beta13 tables
(define class-expansion-db (make-table weak-keys:   #f
                                       weak-values: #f
                                       test:        eq?
                                    ;; key-test:    symbol?
                                       hash:        eq?-hash))

(define (name->class-info name)
  (table-ref class-expansion-db name #f))

(define (add-class-info class-info)
  (table-set! class-expansion-db
              (class-info-name class-info)
              class-info)
)

  
;==============================================================;
;;; Scheme COMPUTE-CPL code was transliterated with minor changes
;;;  from the Dylan code in:
;;;     The Dylan Reference Manual
;;;     by Andrew Shalit, pub: Apple/Addison-Wesley 1996
;;;     ISBN 0-201-44211-6
;;; Code also published in
;;; 	A Monotonic Superclass Linearization for Dylan
;;;     by K. Barrett, B. Cassels, P. Haahr, D. A. Moon, 
;;;        K. Playford, and P. Tucker Withington, 
;;;     OOPSLA 1996.
;;; available at:
;;;	http://www.webcom.com/~haahr/dylan/linearization-oopsla96.html
;==============================================================;

(define (compute-cpl class)
  ;; a.k.a. compute-class-linearization

  (define (candidate class classes)
    ;; returns class if it can go in the result now,
    ;; otherwise false
    (define (head? list)
      (if (null? list)
          #f
          (eq? class (car list))))
    
    (define (tail? list)
      (if (null? list)
          #f
          (member? class (cdr list))))
    
    (and (any? head? classes)
         (not (any? tail? classes))
         class)
    )
  (define (candidate-direct-superclass class classes)
    (any? (lambda (c) (candidate c classes))
          (thing-direct-supers class)))
  (define (merge-lists reversed-partial-result 
                       remaining-inputs)
    (if (every? null? remaining-inputs)
        (reverse reversed-partial-result)
        (let ( (next (any?
                      (lambda (c)
                        (candidate-direct-superclass c remaining-inputs))
                      reversed-partial-result))
             )

          (if next
              (let ( (remove-next
                      (lambda (list)
                        (cond
                         ((null? list) '())
                         ((eq? (car list) next) (cdr list))
                         (else list))))
                     )
                (merge-lists (cons next reversed-partial-result)
                             (map remove-next remaining-inputs)))
              (error "Inconsistent precedence graph"))) )
    )

     (let ( (c-direct-superclasses (reverse (thing-direct-supers class))) )
       (merge-lists (list class)
                    (append
                     (map thing-precedence-list c-direct-superclasses)
                     (list c-direct-superclasses))))
)

(define (remove-useless-supers supers)
  ;; Remove any super which is contained
  ;; in the CPL of another super.
  (let loop ( (result '()) (remaining supers) )
    (if (null? remaining) 
	(reverse result)
	(let ( (super (car remaining))
	       (others (append result remaining))
	       )
	  (if (member? super 
		       (map-append (lambda (s)
				     (cdr (thing-precedence-list s)))
				   others))
	      (loop result              (cdr remaining))
	      (loop (cons super result) (cdr remaining)))))
 ) )

;==============================================================;
(define reset-subclass-encoding (lambda () 'bootstrap)) 
;==============================================================;
;;; <CLASS>

;; @@FIXME: Class Redefinition
;; @@FIXME: Thread Safety

(define (<class> #!key
                 (name                  (gensym 'class))
                 (direct-supers         '())
                 (direct-slots          '())
                 (class-slots		'())
                 (class-precedence-list '())
                 (instance-init-source  #f)
                 (procedure             required:)
        )
  (let* ( (this-class (proc->class procedure)) )
    ;; @@FIXME: make into a variable in <class> object?
    (class-name-set!              this-class name)          ;; a symbol
    (class-direct-supers-set!  	  this-class direct-supers) ;; (class.. ) 
    (class-direct-slots-set!      this-class direct-slots)  ;; (slot-info.. )
    (class-slots-set!             this-class class-slots)   ;; ((name . cell).. )
    (class-precedence-list-set!   this-class (compute-cpl this-class)) ;; (class.. )
    (class-instance-init-set!     this-class instance-init-source) ;; #f or (lambda (inst) body)
    (class-direct-subclasses-set! this-class '())
    (class-direct-subtypes-set!   this-class '())
    (class-direct-methods-set!    this-class '())
    (class-id-set!                this-class %%number-of-classes%%)
    (class-min-id-set!		  this-class 0)
    (class-max-id-set!		  this-class 0)
    (class-offset-set!		  this-class 0)
    (class-mark-set!              this-class #f)
    (let ( (depth -1) )
      (for-each (lambda (parent)
                  (add-subclass parent this-class)
                  (set! depth (max depth (class-depth parent))))
              direct-supers)
      (class-depth-set! this-class (+ 1 depth))) ;; 1 + max depth of direct supers
    (set! %%number-of-classes%%
          (+ 1  %%number-of-classes%%))                                              
    (reset-subclass-encoding) ;; Actually, this is delayed.  See "subclass.scm"
    this-class
) )



;; Ya wanna to see a bootstrap?  THIS is a bootstrap!!!
(set! <class> (<class> name: '<class> procedure: <class>))


(define <value> '<value>) ;; Bootstrap: set! later

;; Bootstrap: see (set! class-of ---) in "class-hierarchy.scm"
(define class-of
  (lambda (thing)      
    (cond
     ((class? thing) <class>)
     ((instance? thing)
      (instance-class thing))
     (else <value>))))

(define (find-class name #!optional (default #f))
  (cond
   ((and (symbol? name) 
	 (bound? name) ;; Be sure to do this check!!
         (##global-var-ref name)) ;; Seg fault here if unbound!!
    => (lambda (class)
         (if (class? class) class default)))
   ((class? name) name)
   (else default))
)

(define (add-subclass parent new-child) 'bootstrap)

(define (really-add-subclass parent new-child)
  (define (delete-same-name name children)
    (cond
     ((null? children) children)
     ((eq? (class-name (car children)) name)
      (delete-same-name name (cdr children)))
     (else
      (cons (car children)
            (delete-same-name name (cdr children)))))
    )
  (class-direct-subclasses-set!
   parent
   (cons new-child
         (delete-same-name (class-name new-child)
                           (class-direct-subclasses parent))))
 )
        

;; We wish an ordering that tests from leaf to root and
;; more to less specific. class-more-specific? does this by
;; picking [1] greater depth and [2] larger class-ids
;; within the same depth
(define (class-compare c1 c2) ; returns -1 0 1
  (cond
   ((< (class-depth c1) (class-depth c2))
    -1)
   ((> (class-depth c1) (class-depth c2))
    1)
   ((< (class-id c1) (class-id c2))
    -1)
   ((> (class-id c1) (class-id c2))
    1)
   (else 0))) ;; c1 is c2

;; NB: This is a total ordering for the single inheritance case ONLY!
(define (class-more-specific? c1 c2) ;; use to sort classes for testing.
  (= 1 (class-compare c1 c2))) ;; invert <



;=============================================================================
;; DEBUG INFO

(define %%class-getters%%
  `((class-name              . ,class-name)
    (class-direct-supers     . ,class-direct-supers)
    (class-direct-slots	     . ,class-direct-slots)
    (class-direct-subclasses . ,class-direct-subclasses)
    (class-direct-subtypes   . ,class-direct-subtypes)
    (class-direct-methods    . ,class-direct-methods)
    (class-precedence-list   . ,class-precedence-list)
    (class-slots             . ,class-slots)
    (class-instance-init     . ,class-instance-init)
    (class-id                . ,class-id  )
    (class-depth             . ,class-depth)
    ;; others elided
) )

(define (class-ref class name)
  (cond
   ((and (class? class)
         (assq name %%class-getters%%))
    => (lambda (pair) ((cdr pair) class))
    )
   (else
    (error (format #f "class-ref can't get ~a from" name) class))
) )

;;; oops#dump-class

(define (dump-class class)

  (define (show name elt-xform)
    (format #t "~a = ~y~&" name (map elt-xform (class-ref class name))))

  (define (id1 x) x)
  
  (format #t "~%Class: ~a~%" (class-ref class 'class-name))
  (show 'class-direct-supers     class-name)
  (show 'class-direct-subclasses class-name)
  (show 'class-direct-subtypes   id1)
  (show 'class-precedence-list   class-name)
  (map
   (lambda (name) (format #t "~a = ~y" name (class-ref class name)))
   '(class-direct-slots
     class-slots
     class-instance-init
     class-id
     class-depth))
  ;; others elided
  (void)
)
               

;=============================================================================


(when debug-oops
      (format #t "~%Oops: loaded CLASS bootstrap code"))


;===========================E=O=F==============================;

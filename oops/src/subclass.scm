;;; FILE: "subclass.scm"
;;; IMPLEMENTS: Subclass test based on Packed Vector Encoding for Oops
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

;;; Notes:
;;  Uses Packed Vector Encoding as outlined in
;; "Simple and Efficient Subclass Tests"
;;  by Jonathan Bachrach jrb@ai.mit.edu
;; [http://people.csail.mit.edu/people/jrb/pve/pve.htm]

;==============================================================;
(include "gambit-namespace.scm")
(##declare (standard-bindings) (extended-bindings) (fixnum) (separate))
;; top level names
(##namespace
 (""
  <value>
  any?
  class-direct-subclasses
  class-direct-supers
  class-for-each
  class-name
  class?
  every?
  format
  member?
  subclass?  

  debug-oops 
))

;; local names
(##namespace
 ("oops#"
  %%subclass-vector%%
  %recalc-subclass?
  %subclass-debug%
  %subclass-warn%
  %subclass?
  assign-sv-ids
  class-mark
  class-mark-set!
  class-number-limit
  display-classes
  make-scvector
  make-subclass-encoding
  reset-subclass-encoding
  scvector-length
  scvector-ref
  scvector-set!
  set-min-max-ids
))

;==============================================================;
(include "common-macros.scm")
;==============================================================;
(define %subclass-debug% #f)
(define %subclass-warn%  #f) ;; print a warning when (make-subclass-encoding)

(define class-number-limit #xFFFF) ;; Limit of 65535 classes

;; class-id class-min-id class-max-id class-offset

(define %%subclass-vector%%  #f)
;;(define %%number-of-classes%% 0) ;; Maintained by <class>
;;@@FIXME: Make %%number-of-classes%% into <class> variable

(define %%subclass-generation%%  0) ;; Checked by gf dispatch

(define invalid-subclass-generation -1) ;; For gf cache flush

(define (class-for-each proc class)
  ;; Single pass => don't reset mark
  (define the-mark  (gensym 'mark))
  (define (mark class)    (class-mark-set! class the-mark))
  (define (marked? class) (eq? the-mark (class-mark class)))
  (define (class-postorder-for-each class)
    (when (not (marked? class))
      (mark class)
      (for-each (lambda (child) (class-postorder-for-each child))
                (reverse (class-direct-subclasses class)))
      (proc class)))
  (class-postorder-for-each class)
)


;;==================================================

(define make-scvector   make-u16vector)
(define scvector-length u16vector-length)
(define scvector-ref    u16vector-ref)
(define scvector-set!   u16vector-set!)

;;; Delay recalculating the subclass encoding information
;;; until first used.  This lets a number of classes by
;;; defined (e.g. when loading code) without recalc'ing
;;; at each class definition.

;; The REAL subclass? test
(define (%subclass? sub super)
  (or (eq? super <value>)
      (and (class? sub)
           (class? super)
           (= (class-id super)
              (scvector-ref  %%subclass-vector%%
                             (+ (class-offset super)
                                (class-id sub))))))
)

;; Subclass? which recalculates the subclass-vector
(define %recalc-subclass?
  (let ( (calc-mutex
          (make-mutex 'recalculating-subclass?-encodings))
       )
    (lambda (sub super)
      (mutex-lock! calc-mutex) ;; Thread-safe
      (when  %subclass-warn%
        (newline) (display "**Warn: Recalclating subclass encodings") (newline))
      (make-subclass-encoding)
      (set! subclass? %subclass?)
      (set! %%subclass-generation%% (+ 1 %%subclass-generation%%))
      (mutex-unlock! calc-mutex) ;; end mutual exclusion
      (subclass? sub super))
 ) )

;; Called by <class> when a class is created.
(define (reset-subclass-encoding)
  (set! subclass? %recalc-subclass?))

(define subclass? %recalc-subclass?)

;; Return the sum of ranges
(define (set-min-max-ids root-class)
  (let ( (range 0) )
    (class-for-each
     (lambda (class)
       (let* ( (children (class-direct-subclasses class))
               (parent?  (not (null? children)))
               (child-max (and parent? (apply max (map class-max-id children))))
               (child-min (and parent? (apply min (map class-min-id children))))
               (this-id (class-id class))
               )
         (when %subclass-debug%
           (newline) (display "processing ") (display (class-name class)))
         (if parent?
             (begin
               (class-max-id-set! class (max this-id child-max))
               (class-min-id-set! class (min this-id child-min)))
             (begin
               (class-max-id-set! class this-id)
               (class-min-id-set! class this-id)))
         (set! range (+ range 1 (- (class-max-id class) (class-min-id class))))))
     root-class)
    (when %subclass-debug%
      (newline))
    range)
  )
    

;; Return (values num-classes vector-of-classes)
(define (assign-sv-ids root-class base-id)

  (define class-vec (make-vector %%number-of-classes%% #f))

  ;; Single pass => don't reset mark
  (define the-mark  (if (eq? (class-mark root-class) 'ping) 'pong 'ping))
  (define (mark class)    (class-mark-set! class the-mark))
  (define (marked? class) (eq? the-mark (class-mark class)))

  (define (process-ids class this-id)
    (when %subclass-debug%
      (newline) (display "processing ") (display (class-name class)))
    (let parent-loop ( (next-next-id
                        (let child-loop ( (next-id (+ 1 this-id))
                                          (children
                                           (reverse
                                            (class-direct-subclasses class)))
                                          )
                          (cond
                           ((null? children) next-id)
                           (else
                            (child-loop (descend (car children) next-id)
                                        (cdr children))))))
                       (parents (class-direct-supers class))
                       )
      (cond
       ((null? parents) next-next-id)
       (else 
        (parent-loop (descend (car parents) next-next-id) (cdr parents))))))
    
  (define (descend class this-id)
    (if (marked? class)
        this-id
        (begin
          (mark class)
          (class-id-set! class this-id)
          (vector-set! class-vec this-id class)
          (process-ids class this-id)))
    )

  (values (descend root-class base-id) class-vec)
)



(define (make-subclass-encoding)
  (call-with-values
    (lambda () (assign-sv-ids <value> 0))
    (lambda (num-classes class-vec)
      (let* ( (max-index (- num-classes 1))
              (sv-size   (set-min-max-ids <value>))
              (sc-vec    (make-scvector sv-size 0))
            )
        (set! %%number-of-classes%% num-classes)
        ;; assign offsets & stuff sc-vec
        (let loop ( (index 1) (start 0) )
          (let* ( (class   (vector-ref class-vec index))
                  (this-id (class-id class))
                  (min-id  (class-min-id class))
                  (max-id  (class-max-id class))
                  (range   (+ 1 (- max-id min-id)))
                  (offset  (- start min-id))
                )
            (when %subclass-debug%
              (newline)
              (display "Processing ") (display (class-name class))
              (display " (id,min,max,start,range,offset) = (")
              (display this-id) (display " ")
              (display min-id)  (display " ")
              (display max-id)  (display " ")
              (display start)   (display " ")
              (display range)   (display " ")
              (display offset)  (display ")")
              )
            (class-offset-set! class offset)
            (letrec ( (set-sv
                       (lambda (child)
                         (scvector-set! sc-vec (+ offset (class-id child)) this-id)
                         (let ( (grandchildren (class-direct-subclasses child)) )
                           (when (not (null? grandchildren))
                             (for-each set-sv grandchildren)))))
                    )
                                 
              (for-each set-sv (class-direct-subclasses class))
              ;; Each class is also a subclass of itself.
              (scvector-set! sc-vec (+ offset this-id) this-id)) 
            
            (when (< index max-index)
              (loop (+ index 1) (+ start range)))
          ))
        (set! %%subclass-vector%% sc-vec)
        (when %subclass-debug% (newline))
        sc-vec))
) )


  

;==============================================================;
;;;; TEST KRUFT


(define (display-classes)
  (define the-mark  (gensym 'mark))
  (define (mark class)    (class-mark-set! class the-mark))
  (define (marked? class) (eq? the-mark (class-mark class)))
  (define (spaces n)
    (let loop ( (n n) )
      (cond
       ((> n 0)
        (display "    ")
        (loop ( - n 1))))))
  (define (class-inorder-for-each class level)
    (newline)
    (spaces level)
    (display (class-name class))
    (display " id=") (display (class-id class))
    (display "  min: ") (display (class-min-id class))
    (display ", max: ") (display (class-max-id class))
    (when (not (marked? class))
      (mark class)
      (for-each (lambda (child) (class-inorder-for-each child (+ level 1)))
                (reverse (class-direct-subclasses class)))
      ))
  (class-inorder-for-each <value> 0)
  (newline)
)


#|
;; Check subclass? info

(define-structure class name mark id min-id max-id offset direct-supers direct-subclasses)

(define (add-class name . parents)
  (let* ( (supers (if (null? parents) (list <value>) parents))
          (new-class
          (make-class name #f 0 0 0 0 supers '()))
       )
    (for-each (lambda (parent)
                (class-direct-subclasses-set!
                 parent
                 (reverse (cons new-class
                                (reverse
                                 (class-direct-subclasses parent))))))
              supers)
    new-class
) )

(define <value> (make-class '<value> #f
                            0 0 0 0
                            '()
                            '()))

(define A (add-class 'A))
(define B (add-class 'B))
(define C (add-class 'C A B))
(define D (add-class 'D A B))
(define E (add-class 'E B))
(define F (add-class 'F C))
(define G (add-class 'G C D))
(define H (add-class 'H D E))
(define I (add-class 'I E))

(let ( (num-classes 0) )
  (class-for-each
   (lambda (c) (set! num-classes (+ 1 num-classes)))
   <value>)
  (set! %%number-of-classes%% num-classes))

(make-subclass-encoding)


(class-for-each
 (lambda (c)
   (display (class-name c))
   (display ":")
   (display (class-id c))
   (display "  min: ") (display (class-min-id c))
   (display ", max: ") (display (class-max-id c))
   (newline))
 <value>)

|#

#|
;; Check class-more-specific?

(define (sort-list unsorted-list <=?)
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

(define class-list
  (let ( (result '()) )
    (class-for-each (lambda (c) (set! result (cons c result))) <value>)
    result))

(define sorted-classes (sort-list class-list class-more-specific?))

(define (spaces n)
  (let loop ( (n n) )
    (cond
     ((> n 0)
      (display "    ")
      (loop ( - n 1))))))

;; Check class-more-specific?
(let loop ( (l sorted-classes) )
  (newline)
  (spaces (oops#class-depth (car l)))
  (display (class-name (car l)))
  (display " ") (display (oops#class-id (car l)))
  (if (not (null? (cdr l))) (loop (cdr l))))

|#
;===========================E=O=F==============================;

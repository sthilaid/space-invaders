;;; FILE: "ts-table.scm"
;;; IMPLEMENTS: "Thread Safe hash tables for oops.
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
  <dictionary>
  <function>
  <integer>
  <keyed>
  <method>
  <thread-safe>
  <ts-table>
  <type>
  <value>
  <vector>
  add-key-elt!
  any-elt?
  bind-slot!
  copy
  elt-ref
  elt-ref-or
  elt-set!
  element-type
  ensure-generic
  ensure-getters
  ensure-setters
  every-elt?
  fold-left
  for-each-elt
  for-each-key
  for-each-key-elt
  hash-fun
  index-type
  key-ok?
  key=?
  make-ref
  make-set!
  mutex
  num-elts
  remove-key!
  table-buckets
  table-buckets-set!
  table-size
  table-size-set!
  walkers
  walkers-set!

  debug-oops
))

;; local names
(##namespace
 ("oops#"
  %decrement-size
  %increment-size
  %make-add-elide
  %make-add-override
  %make-add-removed
  %make-table-walk-fun
  %per-bucket-threshold
  %table-walker
))

;==============================================================;
(include "common-macros.scm")
(include "oops-macros.scm")
;==============================================================;
;; Elide loader warnings.
(define key-ok?            'bootstrap)
(define table-buckets      'bootstrap)
(define table-buckets-set! 'bootstrap)
(define table-size         'bootstrap)
(define table-size-set!    'bootstrap)
;==============================================================;
;;; <ts-table>
(define-class <ts-table> (<thread-safe> <dictionary>)
  (
;; inherited slots:
;  (index-type    type: <type>
;                 init-value: <value>
;                 allocation: each-subclass:)
;  (key=?         type: <function>
;                 init-value: eq?)
;  (element-type  type: <type>
;                 init-value: <value>
;                 allocation: each-subclass:)
;  (mutex         init-function: (lambda () (make-mutex (gensym 'collection)))
;                 use-init-keyword: #f)
;  (walkers       init-value: '()
;                 use-init-keyword: #f)
   (table-size    init-value: 0 ;; Number of elements
                  type: <integer>
                  use-init-keyword: #f)
   (table-buckets init-function: (lambda () (make-vector 15 '()))
                  type: <vector>
                  use-init-keyword: #f)
   (key-ok?       init-value: (lambda (key) #t)
                  ;; e.g. use SYMBOL? for a symbol-table
                  type: <function>)
   (hash-fun      init-value: eq?-hash
                  type: <function>)
  )
)

(define-structure %table-walker
  proc ;; user proc: (lambda (key value) ...)
  key=?
  overrides-alist
  removes-alist
  elides-list
  walker-proc)

;; returns walker thunk; registers table-walker w table
(define (%make-table-walk-fun table proc)
  ;; capture buckets so table growth will not mutate
  (let* ( (buckets     (table-buckets table))
          (num-buckets (vector-length buckets))
          (last-index  (- num-buckets 1))
          (key=?       (key=? table))
          (hash        (hash-fun table))
          (mutex       (mutex table))
          (elt-chunk-size 10) ;; # elts to traverse within mutex
          (assq?       (make-assq key=? #f)) ;; def'ed in "collection-methods.scm"
        )
    
    (define table-walker
      (make-%table-walker proc key=? '() '() '() walk-table))

    (define (walk-table)
      (dynamic-wind ;; Be call/cc-safe.
          (lambda () (mutex-lock! mutex))
          (lambda () (process-table 0))
          (lambda () (mutex-unlock! mutex)))
      )
    
    (define (make-bucket-test index)
      (lambda (pair) ;; same-bucket? => #t if hash matches
        (= index (remainder (hash (car pair)) num-buckets)))
      )

    (define (process-table bucket-index)
      ;; precondition: invoked w mutex locked
      (if (> bucket-index last-index)
          (begin ;; unregister self w table
            (walkers-set!
             table
             (list-remove (walkers table) table-walker))
            #!void) ;; done
          (let ( (bucket (vector-ref buckets bucket-index))
                 (extra (filter (make-bucket-test bucket-index)
                                (%table-walker-removes-alist table-walker)))
                 )
            (let bucket-loop ( (alist
                                (if (null? extra)
                                    bucket
                                    (append bucket extra)))
                             )
              (cond
               ((null? alist) ;; bucket complete
                (process-table (+ 1 bucket-index))
                )
               ((memq (caar alist) (%table-walker-elides-list table-walker)) ;; skip
                (bucket-loop (cdr alist))
                )
               (else  ;; process key, value
                (let* ( (key (caar alist))
                        (value
                         (cond
                          ((assq? key (%table-walker-overrides-alist table-walker)) 
                           => cdr) ;; substitute
                          (else (cdar alist))))
                        )
                  (mutex-unlock! mutex)
                  (proc key value) ;; let proc (and others!) do side effects to table
                  (mutex-lock! mutex)
                  (bucket-loop (cdr alist)))))))

          )) ;; end process-table

  ;; Register this walker with the table.
  (begin (mutex-lock! mutex)
         (walkers-set!
          table
          (cons table-walker (walkers table)))
         (mutex-unlock! mutex))

  walk-table
) )


  ;; Grow bucket vector when approximately bucket-threshold pairs per bucket
(define %per-bucket-threshold 20)

(define (%make-add-override pair)
  (lambda (table-walker)
    (let ( (overrides-alist (%table-walker-overrides-alist table-walker)) )
      (if (not (assq (car pair) overrides-alist))
          (%table-walker-overrides-alist-set!
           table-walker
           (cons pair overrides-alist)))
) ) )

(define (%make-add-removed pair)
  (lambda (table-walker)
    (let ( (removes-alist (%table-walker-removes-alist table-walker)) )
      (if (not (assq (car pair) removes-alist))
          (%table-walker-removes-alist-set!
           table-walker
           (cons pair removes-alist)))
) ) )

(define (%make-add-elide key)
  (lambda (table-walker)
    (let ( (elides-list (%table-walker-elides-list table-walker)) )
      (if (not (memq key elides-list))
          (%table-walker-elides-list-set!
           table-walker
           (cons key elides-list)))
) ) )


;;=================================================================;;

(define-method (num-elts (table <ts-table>))
  (table-size table))

(define-method (for-each-key-elt (f <function>) (table <ts-table>))
  ((%make-table-walk-fun table f))) ;; create & invoke

(define-method (for-each-key (f <function>) (table <ts-table>))
  (for-each-key-elt (lambda (k v) (f k)) table))

(define-method (for-each-elt (f <function>) (table <ts-table>))
  (for-each-key-elt (lambda (k v) (f v)) table))

(define-method (fold-left  (f <function>) seed (table <ts-table>))
  (let ( (result seed) )
    (for-each-key
     (lambda (elt)
       (set! result (f elt result)))
     table)
    result)
)

(define-method (copy (table <ts-table>))
  (let ( (new-dict (<ts-table>
                    hash-fun: (hash-fun table)
                    key=?:    (key=?    table)
                    key-ok?:  (key-ok?  table)))
       )
    (for-each-key-elt
     (lambda (key elt)
       (add-key-elt! new-dict key elt))
     table)
    ;; return the new table
    new-dict)
 )

(define-method (every-elt? (pred? <function>) (table <ts-table>))
  (call/cc
   (lambda (return)
     (for-each-elt
      (lambda (elt)
        (if (not (pred? elt))
            (return #f)))
      table)
     (return #t))
) )

(define-method (any-elt? (pred? <function>) (table <ts-table>))
  (call/cc
   (lambda (return)
     (for-each-elt
      (lambda (elt)
        (if (pred? elt)
            (return #t)))
      table)
     (return #f))
) )

(define-method (elt-ref-or (table <ts-table>) key default)
  (if (not ((key-ok? table) key))
      (error "elt-ref: bad key: " key)
      (begin
        (mutex-lock! (mutex table))
        (let* ( (buckets-vec (table-buckets table))
                (num-buckets (vector-length buckets-vec))
                (index (remainder ((hash-fun table) key) num-buckets))
                (alist (vector-ref buckets-vec index))
                (assq? (make-assq (key=? table) #f))
                (result (cond
                         ((assq? key alist) => cdr)
                         (else default)))
              )
          (mutex-unlock! (mutex table))
          result)))
)

(define-method (elt-ref (table <ts-table>) key)
  (elt-ref-or table key #f))

(define-method (add-key-elt! (table <ts-table>) key new-value)
  (if (not ((key-ok? table) key))
      (error "elt-set!: bad key for table " key)
      (begin
        (mutex-lock! (mutex table))
        (let* ( (buckets (table-buckets table))
                (num-buckets (vector-length buckets))
                (index (remainder ((hash-fun table) key) num-buckets))
                (alist (vector-ref buckets index))
                (assq? (make-assq (key=? table) #f))
                (probe (assq? key alist))
              )
          (cond
           (probe ;; Key exists; mutate value!
            (for-each (%make-add-override (cons key (cdr probe)))
                      (walkers table))
            (set-cdr! probe new-value)
            )
           (else ;; Key not found; add new key, value pair.
            (for-each (%make-add-elide key) (walkers table))
            (%increment-size table)
            (vector-set! buckets
                         index
                         (cons (cons key new-value) alist))
            ))
          (mutex-unlock! (mutex table))
          table)) ;; return the collection
  )
)

(define-method (elt-set! (table <ts-table>) key new-value)
  (add-key-elt! table key new-value))
  
(define-method (remove-key! (table <ts-table>) key)
  (if (not ((key-ok? table) key))
      #f ;; ok, won't be there
      (begin
        (mutex-lock! (mutex table))
        (let ( (result
                (let* ( (buckets     (table-buckets table))
                        (num-buckets (vector-length buckets))
                        (size        (table-size table))
                        (index       (remainder ((hash-fun table) key)
                                                num-buckets))
                        (alist       (vector-ref buckets index))
                        (key=?       (key=? table))
                        (assq?       (make-assq key=? #f))
                      )
                  (cond
                   ((null? alist)  ;; not found
                    #f)
                   ((key=? (caar alist) key)
                    (for-each (%make-add-removed (car alist))
                              (walkers table))
                    (vector-set! buckets index (cdr alist))
                    (%decrement-size table)
                    #t ;; found
                    )
                   (else
                    (let loop ( (prev alist) (curr (cdr alist)) )
                      (cond
                       ((null? curr)
                        #f) ;; not found
                       ((key=? key (caar curr))
                        (for-each
                         (%make-add-removed (car curr))
                         (walkers table))
                        (set-cdr! prev (cdr curr)) ;; splice it out
                        (%decrement-size table)
                        #t) ;; found
                       (else (loop curr (cdr curr))))))
                   ))) ;; result
             )
          (mutex-unlock! (mutex table))
          result)
) ) )

(define (%increment-size table)
  (let* ( (new-size    (+ 1 (table-size table)))
          (buckets     (table-buckets table))
          (num-buckets (vector-length buckets))
          (hash        (hash-fun table))
        )
    (table-size-set! table new-size)
     ;; Perhaps grow bucket-vector..
    (if (> new-size (* num-buckets %per-bucket-threshold))
        (let* ( (data-vec (make-vector (* 2 num-buckets) '()))
                (data-size (vector-length data-vec))
                (max-bucket-index (- num-buckets 1))
              )
          (let bucket-loop ( (bucket-index 0) )
            (for-each ;; rehash to new vector of alists
             (lambda (name-value-pair)
               (let ( (data-index
                       (remainder (hash (car name-value-pair))
                                  data-size))
                    )
                 (vector-set! data-vec
                              data-index
                              (cons name-value-pair
                                    (vector-ref data-vec data-index)))))
             (vector-ref buckets bucket-index))
            (if (< bucket-index max-bucket-index)
                (bucket-loop (+ bucket-index 1))
                (table-buckets-set! table data-vec)))))
 ) )

(define (%decrement-size table)
  ;; @@FIXMEL shrink as required
  (table-size-set! table (- (table-size table) 1))
)


;;;   ---   E O F   ---   ;;;

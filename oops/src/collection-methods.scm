;;; FILE: "collection-methods.scm"
;;; IMPLEMENTS: Methods for Oops Collections.
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
  ->collection
  1st
  2nd
  3rd
  <add-key-elt>
  <class>
  <collection>
  <condition>
  <dictionary>
  <elt-not-found>
  <error>
  <function>
  <generic>
  <hash-table>
  <indexed>
  <list>
  <method>
  <one-of>
  <pair>
  <restart>
  <set>
  <string>
  <subclass-of-collection>
  <subclass-of-hash-table>
  <subclass-of-list>
  <subclass-of-set>
  <subclass-of-string>
  <subclass-of-vector>
  <subclass-type>
  <type>
  <value>
  <vector>
  CERROR
  ERROR-NO-RETURN
  add-elt
  add-elt!
  add-elts
  add-elts!
  add-key-elt
  add-key-elt!
  any-elt?
  any?
  as
  bind-slot!
  class-name
  class-of
  class-slots
  collect
  concatenate
  concatenate-as
  contains-elt?
  copy
  data
  data-set!
  difference
  elt
  elt-ref
  elt-ref-or
  elt-set!
  elts=?
  empty?
  ensure-generic
  ensure-getters
  ensure-setters
  every-elt?
  every?
  fold-left
  fold-right
  for-each-elt
  for-each-key
  for-each-key-elt
  format
  generic?
  intersection
  instance?
  is-a?
  key
  key=?
  last
  make-ref
  make-set!
  nth
  num-elts
  reject
  remove-all-elts!
  remove-elt
  remove-elt!
  remove-elts
  remove-elts!
  remove-key!
  reverse-elts
  s
  slice
  subclass?
  the-empty-set
  union

  debug-oops
))

;; local names
(##namespace
 ("oops#"
  make-assq
))

;==============================================================;
(include "common-macros.scm")
(include "oops-macros.scm")
;==============================================================;
;; Elide compiler warnings
(define add-key-elt 'bootstrap)
(define elt 'bootstrap)
(define key 'bootstrap)
;==============================================================;
;;@@FIXME: Rationalize w SRFI-44 ??
;;;Partial View:
;;;                              <value>
;;;                                 |
;;;                            <collection>------------------
;;;                           /      |      \         \      \
;;;                   <ordered>  <stretchy> <keyed>  <set>  <null>
;;;      <keyed>      /      \        |         |
;;;            \     /        \   <elastic>     |
;;;             \   /          \ /        \     |
;;;          <indexed>          /  <null>  <dictionary>
;;;          /     |           / \   /     /    |     \
;;;   <vector>  <string> <stream> <list>  /  <treap> <hash-table>
;;;    |              |     |        \   /
;;;    |    <stretchy>|  <queue>    <alist>
;;;    |     |      | |     |
;;;<stretchy-vector>| |  <dequeue>
;;;                 | |
;;;       <stretchy-string>
;;;
;==============================================================;
;; Conditions, Handlers, Restarts.

(define-class <elt-not-found> <error>
  ((return-allowed? allocation: each-subclass:
                    init-value: #t)
   (collection      type: <collection>)
   key
   elt))
                    
  
(define-class <add-key-elt> <restart> ())


;==============================================================;
(define <subclass-of-list>       (<subclass-type> subclass-of: <list>))
(define <subclass-of-set>        (<subclass-type> subclass-of: <set>))
(define <subclass-of-vector>     (<subclass-type> subclass-of: <vector>))
(define <subclass-of-string>     (<subclass-type> subclass-of: <string>))
(define <subclass-of-hash-table> (<subclass-type> subclass-of: <hash-table>))
(define <subclass-of-collection> (<subclass-type> subclass-of: <collection>))

;==============================================================;
;;; <COLLECTON>

(define-generic (num-elts (c <collection>)) )
(define-generic (for-each-elt (f <function>) (c <collection>)) )
(define-generic (fold-left  (f <function>) seed (c <collection>)) )
(define-generic (empty? (c <collection>))  (zero? (num-elts c)))
(define-generic (copy   (c <collection>)) )
(define-generic (concatenate (c <collection>) #!rest other-collections) )
(define-generic (concatenate-as (result-class <class>) #!rest collections) )
(define-generic (every-elt? (pred? <function>) (c <collection>)) )
(define-generic (any-elt?   (pred? <function>) (c <collection>)) )
(define-generic (contains-elt? (c <collection>)
                               (thing <value>)
                               #!optional (same? eq?))
  (any-elt? (lambda (elt) (same? elt thing)) c))

(define-generic (elts=? (c1 <collection>)
                        (c2 <collection>)
                        #!optional (same? eq?))
  (and (every-elt? (lambda (elt) (contains-elt? c2 elt same?)) c1)
       (every-elt? (lambda (elt) (contains-elt? c1 elt same?)) c2)))

(define-generic (->collection (class <subclass-of-collection>) #!rest values)
  (cond
   ((subclass? class <indexed>)
    ;; values is a list of elts
    (let ( (seq (class size: (length values))) )
      (let loop ( (index 0) (elts values) )
        (cond
         ((null? elts) seq)
         (else
          (elt-set! seq index (car elts))
          (loop (+ 1 index) (cdr elts))))))
    )
   ((subclass? class <dictionary>)
    ;; values is a list of (key . elt) cons cells.
    (if (and (not (null? values))
             (not (pair? (car values))))
        (ERROR-NO-RETURN
   "->COLLECTION: (key . pair) cons cells required for a dictionary, got: ~s"
         values))
    (let ( (dict (class)) )
      (for-each
       (lambda (pair) (add-key-elt! dict (car pair) (cdr pair)))
       values)
      dict)
    )
   ((subclass? class <collection>)
    (let loop ( (result (class)) (elts (reverse values)) )
      (if (null? elts)
          result
          (loop (add-elt result (car elts))
                (cdr elts)))
    ))
   (else
    (ERROR-NO-RETURN
     "->COLLECTION: can't make sequence or dictionary from ~s"
     class))
) )
      

;; a.k.a. filter-in
(define-generic (collect (predicate? <function>) (c <collection>)
                        #!optional (result-class <vector>))
  (let ( (reversed-result
          (fold-left (lambda (elt so-far)
                       (if (predicate? elt)
                           (cons elt so-far)
                           so-far))
                     '()
                     c))
        )
    (as result-class (reverse reversed-result))
 ) )

(define-method (collect (predicate? <function>) (c <indexed>)
                        #!optional (result-class <vector>))
  (let ( (result
          (fold-right (lambda (elt so-far)
                       (if (predicate? elt)
                           (cons elt so-far)
                           so-far))
                     '()
                     c))
        )
    (as result-class result)
 ) )

(define-generic (reject (predicate? <function>) (c <collection>)
                        #!optional (result-class <vector>))
  (collect (lambda (elt) (not (predicate? elt))) c result-class))


;;; NOT <elastic> -> always returns a new collection [no "!"]
(define-generic (add-elt     (c <collection>)  elt) )
(define-generic (add-elts    (c <collection>) (elts <collection>)) )
(define-generic (remove-elt  (c <collection>)  elt) )
(define-generic (remove-elts (c <collection>) (elts <collection>)) )


;;; AS
(define-generic (as (t <class>) (col <collection>))
  ;; base case of no transformation
  (if (is-a? col t)
      col
      (next-method) ;; ==> error  @@FIXME:
) )


;;; <STRETCHY>
(define-generic (add-elt!     (s <stretchy>) elt) )
(define-generic (add-elts!    (s <stretchy>) #!rest elts) )


;;; <ELASTIC>
(define-generic (remove-elt!  (s <elastic>) elt) )
(define-generic (remove-elts! (s <elastic>) (elts <collection>)) )
(define-generic (remove-all-elts! (s <elastic>)) )


;;; <KEYED>
(define-generic (for-each-key     (f <function>) (c <keyed>)) ) ;; (lambda (key) ..)
(define-generic (for-each-key-elt (f <function>) (c <keyed>)) ) ;; (lambda (key elt) ..)
(define-generic (elt-ref    (c <keyed>) key) )
(define-generic (elt-ref-or (c <keyed>) key default) )
(define-generic (elt-set!   (c <keyed>) key new-value) )

;;; <ORDERED>
(define-generic (1st  (seq <ordered>)) )
(define-generic (2nd  (seq <ordered>)) )
(define-generic (3rd  (seq <ordered>)) )
(define-generic (last (seq <ordered>)) )
(define-generic (nth  (seq <ordered>) (n <integer>)) )
(define-method (reverse-elts (seq <ordered>))
  (let* ( (len (num-elts seq))
          (new-seq ((class-of seq) size: len)) )
    (let loop ( (left 0) (right (- len 1)) )
      (if (>= left len)
          new-seq
          (begin
            (elt-set! new-seq left (elt-ref seq right))
            (loop (+ left 1) (- right 1))))
) ) )



;;; <INDEXED>

(define-method (1st  (seq <indexed>)) (elt-ref seq 0))
(define-method (2nd  (seq <indexed>)) (elt-ref seq 1))
(define-method (3rd  (seq <indexed>)) (elt-ref seq 2))
(define-method (last (seq <indexed>)) (elt-ref seq (- (num-elts seq) 1)))
(define-method (nth  (seq <indexed>) (n <integer>)) (elt-ref seq n))

(define-generic (slice (seq   <indexed>)
                       (start <integer>) ;; inclusive
                       (end   <integer>) ;; exclusive
                       #!optional (result-class <vector>))
  (let loop ( (elts '()) (index (- end 1)) )
    (if (< index start)
        (as result-class elts)
        (loop (cons (elt-ref seq index) elts) (- index 1)))
) )

(define-method (for-each-elt (f <function>) (seq <indexed>))
  (let ( (size (num-elts seq)) )
    (let loop ( (index 0) )
      (cond
       ((< index size)
        (f (elt-ref seq index))
        (loop (+ 1 index))))
) ) )

(define-method (for-each-key (f <function>) (seq <indexed>))
  (for-each f (iota (- (num-elts seq) 1))))

(define-method (for-each-key-elt (f <function>) (seq <indexed>))
  (let ( (size (num-elts seq)) )
    (let loop ( (index 0) )
      (cond
       ((< index size)
        (f index (elt-ref seq index))
        (loop (+ 1 index))))
) ) )

(define-method (fold-left (f <function>) seed (seq <indexed>))
  (let ( (size (num-elts seq)) )
    (letrec ( (foldl
               (lambda (value index)
                 (if (< index size)
                     (foldl (f (elt-ref seq index) value) (+ index 1))
                     value)))
            )
      (foldl seed 0)
) ) )

(define-generic (fold-right (f <function>) seed (seq <indexed>))
  (let ( (size (num-elts seq)) )
    (letrec ( (foldr
               (lambda (value index)
                 (if (> index 0)
                     (foldr (f (elt-ref seq index) value) (- index 1))
                     value)))
            )
      (foldr seed (- size 1))
) ) )

(define-method (every-elt? (pred? <function>) (seq <indexed>))
  (every? pred? (as <list> seq)))

(define-method (any-elt? (pred? <function>) (seq <indexed>))
  (any? pred? (as <list> seq)))


;;; <DICTIONARY>
(define-generic (add-key-elt! (d <dictionary>) key elt) )
(define-generic (remove-key!  (d <dictionary>) key) )

(define-method (elts=? (d <dictionary>) (c <collection>) #!optional (same? eq?))
  (elts=? (fold-left cons '() d) c same?))

(define-method (elts=? (c <collection>) (d <dictionary>) #!optional (same? eq?))
  (elts=? c (fold-left cons '() d) same?))

(define-method (elts=? (d1 <dictionary>) (d2 <dictionary>) #!optional (same? eq?))
  (elts=? (fold-left cons '() d1) (fold-left cons '() d2) same?))


;==============================================================;
;;; <PAIR>+<NULL> -> <LIST>

;; <collection> protocol
(define-method (num-elts (list <pair>))
  (length list))

(define-method (num-elts (empty <null>))
  0)

(define-method (for-each-elt (f <function>) (list <pair>))
  (for-each f list))

(define-method (for-each-elt (f <function>) (empty <null>))
  #!void)

(define-method (fold-left (f <function>) seed (list <pair>))
  (letrec ( (foldl
               (lambda (value list)
                 (if (null? list)
                     value
                     (foldl (f (car list) value)
                            (cdr list)))))
            )
    (foldl seed list)
) )

(define-method (fold-left (f <function>) seed (empty <null>))
  seed)

(define-method (empty? (list <pair>))
  #f)

(define-method (empty? (empty <null>))
  #t)

(define-method (copy (list <pair>))
  (let loop ( (elts list) )
    (if (null? elts)
        '()
        (cons (car elts) (loop (cdr elts))))))

(define-method (copy (empty <null>))
  empty) ;; singleton

(define-method (concatenate (list <pair>) #!rest other-collections)
  (apply append
         (cons list
               (map (lambda (c) (as <list> c)) other-collections)))
)

(define-method (concatenate (empty <null>) #!rest other-collections)
  (apply append
         (map (lambda (c) (as <list> c)) other-collections))
)

(define-method (concatenate-as (result-class <subclass-of-list>)
                               #!rest collections)
  (apply append
         (map (lambda (c) (as <list> c)) collections))
)

(define-method (every-elt? (pred? <function>) (list <pair>))
  (every? pred? list))

(define-method (every-elt? (pred? <function>) (empty <null>))
  #t)

(define-method (any-elt? (pred? <function>) (list <pair>))
  (any? pred? list))

(define-method (any-elt? (pred? <function>) (empty <null>))
  #f)

(define-method (contains-elt? (list <pair>)
                              elt
                              #!optional (same? eq?))
  ((make-member-pred same?) elt list)
)

(define-method (contains-elt? (empty <null>) elt #!optional same?)
  #f)

(define-method (elts=? (list <pair>) (another-list <pair>)
                       #!optional (same? eq?))
  ;; note: bad for circular lists 8^(
  (and (every? (lambda (elt) (contains-elt? another-list elt same?)) list)
       (every? (lambda (elt) (contains-elt? list elt same?)) another-list)
) )

(define-method (elts=? (empty1 <null>) (empty2 <null>) #!optional same?)
  #t)

(define-method (elts=? (list <pair>) (empty <null>) #!optional same?)
  #f)

(define-method (elts=? (empty <null>) (list <pair>) #!optional same?)
  #f)

(define-method (->collection (result-class <subclass-of-list>) #!rest values)
  values)

(define-method (add-elt (list <pair>) elt)
  (cons elt list))

(define-method (add-elt (empty <null>) elt)
  (cons elt empty))

(define-method (add-elts (list <pair>) (to-add <collection>))
  (append (as <list> to-add) list))

(define-method (add-elts (empty <null>) (to-add <collection>))
  (as <list> to-add))

(define-method (remove-elt (list <pair>) elt)
  (let loop ( (result '()) (elts list) )
    (cond
     ((null? elts) (reverse result))
     ((eq? elt (car elts)) (loop result (cdr elts)))
     (else (loop (cons (car elts) result) (cdr elts))))
) )

(define-method (remove-elt (empty <null>) elt) empty)

(define-method (remove-elts (list <pair>) (to-remove <collection>))
  (let ( (elts-to-remove (as <list> to-remove)) )
    (let loop ( (result '()) (elts list) )
      (cond
       ((null? elts) (reverse result))
       ((memq (car elts) elts-to-remove)
        (loop result (cdr elts)))
       (else (loop (cons (car elts) result) (cdr elts))))
) ) )

(define-method (remove-elts (empty <null>) (to-remove <collection>))
  empty)


(define-method (as (result-class <subclass-of-list>) (list <pair>))
  list)

(define-method (as (result-class <subclass-of-list>) (empty <null>))
  empty)

(define-method (as (result-class <subclass-of-list>) (c <collection>))
  (reverse (fold-left cons '() c)))

(define-method (as (result-class <subclass-of-list>) (c <indexed>))
  (fold-right cons '() c))

(define-method (as (result-class <subclass-of-string>) (empty <null>))
  "")

;; <ordered> protocol
(define-method (1st  (list <pair>)) (car   list))
(define-method (2nd  (list <pair>)) (cadr  list))
(define-method (3rd  (list <pair>)) (caddr list))
(define-method (last (list <pair>))
  (let loop ( (list list) )
    (cond
     ((null? list) '())
     ((null? (cdr list)) (car list))
     ((not (pair? (cdr list)))
      (cdr list)) ;; improper list
     (else (loop (cdr list))))))
     
(define-method (nth  (list <pair>) (n <integer>))
  (list-ref list n))
;   (let loop ( (index n) (list list) )
;     (if (zero? index)
;         (car list)
;         (loop (- index 1) (cdr list)))))

(define-method (reverse-elts (list <pair>))
  (reverse list))

(define-method (reverse-elts (empty <null>))
  empty)


;==============================================================;
;;; <STRING>

;; collection protocol

(define-method (num-elts (seq <string>)) (string-length seq))

(define-method (for-each-elt (f <function>) (seq <string>))
  (let ( (size (string-length seq)) )
    (let loop ( (index 0) )
      (cond
       ((< index size)
        (f (string-ref seq index))
        (loop (+ 1 index))))
) ) )

(define-method (fold-left (f <function>) seed (seq <string>))
  (let ( (size (string-length seq)) )
    (letrec ( (foldl
               (lambda (value index)
                 (if (< index size)
                     (foldl (f (string-ref seq index) value) (+ index 1))
                     value)))
            )
      (foldl seed 0)
) ) )

(define-method (copy (s <string>))
  (let* ( (num-elts (string-length s))
          (new-str  (make-string num-elts))
        )
    (let loop ( (index 0) )
      (if (>= index num-elts)
          new-str
          (begin
            (string-set! new-str index
                         (string-ref s index))
            (loop (+ index 1))))
 ) ) )

(define-method (concatenate (s <string>) #!rest other-collections)
  (apply string-append
         (cons s
               (map (lambda (c) (as <string> c))
                    other-collections)))
)

(define-method (concatenate-as (string-class <subclass-of-string>)
                               #!rest collections)
  (apply string-append
         (map (lambda (c) (as <string> c))
              collections))
)

(define-method (every-elt? (pred? <function>) (s <string>))
  (every? pred? (string->list s)))

(define-method (any-elt? (pred? <function>) (s <string>))
  (any? pred? (string->list s)))

(define-method (contains-elt? (s <string>) (elt <character>)
                              #!optional (same? char=?))
  ((make-member-pred same?) elt (string->list s)))

(define-method (elts=? (s1 <string>) (s2 <string>) #!optional (same? char=?))
  (if (eq? char=? same?)
      (string=? s1 s2)
      (elts=? (string->list s1) (string->list s2) same?))
)

(define-method (add-elt (s <string>) (elt <character>))
  ;; add at end for strings
  (let* ( (num-elts (string-length s))
          (new-str  (make-string (+ 1 num-elts)))
        )
    (let loop ( (index 0) )
      (if (>= index num-elts)
          (begin
            (string-set! new-str index elt)
            new-str)
          (begin
            (string-set! new-str index
                         (string-ref s index))
            (loop (+ index 1))))
 ) ) )

(define-method (add-elts (s <string>) (elts <string>))
  (string-append s elts))

(define-method (add-elts (s <string>) (elts <collection>))
  (list->string
   (append (string->list s) (as <list> elts))))

(define-method (remove-elt (s <string>) (elt <character>))
  (list->string (remove-elt (string->list s) elt)))

(define-method (remove-elts (s <string>) (elts <collection>))
  (list->string (remove-elts (string->list s) (as <list> elts))))

(define-method (as (result-class <subclass-of-string>) (empty <null>))
  "")

(define-method (as (result-class <subclass-of-string>) (list <pair>))
  (list->string list))

(define-method (as (result-class <subclass-of-string>) (vec <vector>))
  (list->string (vector->list vec)))

(define-method (as (result-class <subclass-of-string>) (s <string>))
  s)


;; keyed protocol

(define-method (for-each-key-elt (f <function>) (seq <string>))
  (let ( (size (string-length seq)) )
    (let loop ( (index 0) )
      (cond
       ((< index size)
        (f index (string-ref seq index))
        (loop (+ 1 index))))
) ) )

(define-method (elt-ref  (seq <string>) (index <integer>))
  (string-ref seq index))

(define-method (elt-ref-or (s <string>) (key <integer>) default)
  (if (< -1 key (string-length s))
      (string-ref s key)
      default))

(define-method (elt-set! (seq       <string>)
                         (index     <integer>)
                         (new-value <character>))
  (string-set! seq index new-value)
  seq) ;; return collection as result [allows functional variants]


;; indexed protocol
(define-method (slice (seq   <string>)
                      (start <integer>) ;; inclusive
                      (end   <integer>) ;; exclusive
                      #!optional (result-class <string>))
  (let loop ( (elts '()) (index (- end 1)) )
    (if (< index start)
        (as result-class elts)
        (loop (cons (string-ref seq index) elts) (- index 1)))
) )


(define-method (fold-right (f <function>) seed (seq <string>))
  (let ( (size (string-length seq)) )
    (letrec ( (foldr
               (lambda (value index)
                 (if (>= index 0)
                     (foldr (f (string-ref seq index) value) (- index 1))
                     value)))
            )
      (foldr seed (- size 1))
) ) )

(define-method (reverse-elts (s <string>))
  (list->string (reverse (string->list s))))

  
;==============================================================;
;;; <VECTOR>

;; collection protocol

(define-method (num-elts (seq <vector>)) (vector-length seq))

(define-method (for-each-elt (f <function>) (seq <vector>))
  (let ( (size (vector-length seq)) )
    (let loop ( (index 0) )
      (cond
       ((< index size)
        (f (vector-ref seq index))
        (loop (+ 1 index))))
) ) )

(define-method (for-each-key-elt (f <function>) (seq <vector>))
  (let ( (size (vector-length seq)) )
    (let loop ( (index 0) )
      (cond
       ((< index size)
        (f index (vector-ref seq index))
        (loop (+ 1 index))))
) ) )

(define-method (fold-left (f <function>) seed (seq <vector>))
  (let ( (size (vector-length seq)) )
    (letrec ( (foldl
               (lambda (value index)
                 (if (< index size)
                     (foldl (f (vector-ref seq index) value) (+ index 1))
                     value)))
            )
      (foldl seed 0)
) ) )

(define-method (fold-right (f <function>) seed (seq <vector>))
  (let ( (size (vector-length seq)) )
    (letrec ( (foldr
               (lambda (value index)
                 (if (>= index 0)
                     (foldr (f (vector-ref seq index) value) (- index 1))
                     value)))
            )
      (foldr seed (- size 1))
) ) )

(define-method (copy (v <vector>))
  (let* ( (num-elts (vector-length v))
          (new-vec  (make-vector num-elts))
        )
    (let loop ( (index 0) )
      (if (>= index num-elts)
          new-vec
          (begin
            (vector-set! new-vec index
                         (vector-ref v index))
            (loop (+ index 1))))
 ) ) )

(define-method (concatenate (v <vector>) #!rest other-collections)
  (list->vector
   (apply append
          (cons (vector->list v)
                (map (lambda (c) (as <list> c))
                     other-collections))))
)

(define-method (concatenate-as (vector-class <subclass-of-vector>)
                               #!rest collections)
  (list->vector
   (apply append
          (map (lambda (c) (as <list> c))
               collections)))
)

(define-method (every-elt? (pred? <function>) (v <vector>))
  (every? pred? (vector->list v)))

(define-method (any-elt? (pred? <function>) (v <vector>))
  (any? pred? (vector->list v)))

(define-method (contains-elt? (v <vector>) elt  #!optional (same? eq?))
  ((make-member-pred same?) elt (vector->list v))
)

(define-method (elts=? (v1 <vector>) (v2 <vector>) #!optional (same? eq?))
  (let* ( (vlen1 (vector-length v1))
          (vlen2 (vector-length v2))
          (max-index (- vlen1 1))
       )
    (if (not (= vlen1 vlen2))
        #f
        (let loop ( (index 0) )
          (cond
           ((= index max-index) #t)
           ((same? (vector-ref v1 index)
                    (vector-ref v2 index))
            (loop (+ index 1)))
           (else #f))))
) )

(define-method (add-elt (v <vector>) elt)
  ;; add at end for vectors
  (let* ( (num-elts (vector-length v))
          (new-vec  (make-vector (+ 1 num-elts)))
        )
    (let loop ( (index 0) )
      (if (>= index num-elts)
          (begin
            (vector-set! new-vec index elt)
            new-vec)
          (begin
            (vector-set! new-vec index
                         (vector-ref v index))
            (loop (+ index 1))))
 ) ) )

(define-method (add-elts (v <vector>) (elts <collection>))
  (list->vector
   (append (vector->list v) (as <list> elts))))

(define-method (remove-elt (v <vector>) (elt <value>))
  (list->vector (remove-elt (vector->list v) elt)))

(define-method (remove-elts (v <vector>) (elts <collection>))
  (list->vector (remove-elts (vector->list v) (as <list> elts))))

(define-method (as (result-class <subclass-of-vector>) (string <string>))
  (list->vector (string->list string)))

(define-method (as (result-class <subclass-of-vector>) (list <pair>))
  (list->vector list))

(define-method (as (result-class <subclass-of-vector>) (empty <null>))
  '#())

;; keyed protocol

(define-method (slice (seq   <vector>)
                      (start <integer>) ;; inclusive
                      (end   <integer>) ;; exclusive
                      #!optional (result-class <vector>))
  (let loop ( (elts '()) (index (- end 1)) )
    (if (< index start)
        (as result-class elts)
        (loop (cons (vector-ref seq index) elts) (- index 1)))
) )

(define-method (elt-ref  (seq <vector>) (index <integer>))
  (vector-ref seq index))

(define-method (elt-ref-or (v <vector>) (key <integer>) default)
  (if (< -1 key (vector-length v))
      (vector-ref v key)
      default))

(define-method (elt-set! (seq       <vector>)
                         (index     <integer>)
                         (new-value <value>))
  (vector-set! seq index new-value)
  seq)

(define-method (reverse-elts (v <vector>))
  (list->vector (reverse (vector->list v))))


;==============================================================;
;;; Limited Vectors

#|
procedure: s8vector? obj 
procedure: make-s8vector k [fill] 
procedure: s8vector exact-int8... 
procedure: s8vector-length s8vector 
procedure: s8vector-ref s8vector k 
procedure: s8vector-set! s8vector k exact-int8 
procedure: s8vector->list s8vector 
procedure: list->s8vector list-of-exact-int8 
procedure: s8vector-fill! s8vector fill 
procedure: s8vector-copy s8vector 
procedure: s8vector-append s8vector... 
procedure: subs8vector s8vector start end 
|#




;==============================================================;
;;; <SET>  ;; NB: immutable
(define the-empty-set (<set> data: '()))

(define-method (as (result-class <subclass-of-set>) (list <list>))
  (<set> data: list))

(define-method (every-elt? (pred? <function>) (s <set>))
  (every? pred? (data s)))

(define-method (any-elt? (pred? <function>) (s <set>))
  (any? pred? (data s)))

(define-method (copy (s <set>)) s) ;; s is immutable

(define-generic (union (s <set>) #!rest sets)
  (unless (every? (lambda (s) (is-a? s <set>)) sets)
    (error "UNION: requires <set> arguments" (cons s sets)))
  (let outer ( (set (data s)) (sets (map data sets)) )
    (if (null? sets)
        (<set> data: set)
        (let inner ( (elts set) (others (car sets)) )
          (cond
           ((null? others) (outer elts (cdr sets)))
           ((memq (car others) elts) (inner elts (cdr others)))
           (else (inner (cons (car others) elts) (cdr others)))
        ) )
    )
) )

(define-generic (intersection (s <set>) #!rest sets)
  (define (set-intersection s1 s2)
    (cond ((null? s1) '())
          ((memq (car s1) s2)
           (cons (car s1) (set-intersection (cdr s1) s2)))
          (else
           (set-intersection (cdr s1) s2)))
    )
  (unless (every? (lambda (s) (is-a? s <set>)) sets)
    (error "INTERSECTION: requires <set> arguments" (cons s sets)))
  (let loop ( (set (data s)) (sets (map data sets)) )
    (if (null? sets)
        (<set> data: set)
        (loop (set-intersection set (car sets))
              (cdr sets))
    )
) )

(define-generic (difference (s <set>) #!rest sets)
  (define (set-difference s1 s2)
    (cond
     ((null? s1) '())
     ((memq (car s1) s2) (set-difference (cdr s1) s2))
     (else
      (cons (car s1) (set-difference (cdr s1) s2))))
    )
  (unless (every? (lambda (s) (is-a? s <set>)) sets)
    (error "DIFFERENCE: requires <set> arguments" (cons s sets)))
  (let loop ( (set (data s)) (sets (map data sets)) )
    (if (null? sets) 
        (<set> data: set)
        (loop (set-difference set (car sets))
              (cdr sets)))
) )

(define-method (num-elts (s <set>)) (length (data s)))

(define-method (for-each-elt (f <function>) (s <set>))
  (for-each f (data s)))

(define-method (contains-elt? (s <set>) elt #!optional (same? eq?))
  (any-elt? (lambda (thing) (same? elt thing)) (data s))
)

(define-method (fold-left (f <function>) seed (s <set>))
  (letrec ( (foldl
               (lambda (value list)
                 (if (null? list)
                     value
                     (foldl (f (car list) value)
                            (cdr list)))))
            )
    (foldl seed (data s))
) )


(define-method (add-elt (s <set>) elt) ;; NB: returns a NEW <set>
  (if  (contains-elt? s elt)
       s
       (<set> data: (cons elt (data s)))))

(define-method (add-elts (s <set>) (to-add <collection>))
  (<set> data: (append (as <list> to-add) (data s))))

(define-method (remove-elt (s <set>) elt) ;; NB: returns a NEW <set>
  (let loop ( (result '()) (elts (data s)) )
    (cond
     ((null? elts) (<set> data: result))
     ((eq? elt (car elts)) (loop result (cdr elts)))
     (else (loop (cons (car elts) result) (cdr elts))))
) )

(define-method (remove-elts (s <set>) (to-remove <collection>)) ;; NB: returns a NEW <set>
  (difference s (<set> data: (as <list> to-remove))))


;==============================================================;
;;; <ALIST>
  
(define-method (add-key-elt! (a <alist>) key elt)
  (let ( (alist (data a))
         (assq (make-assq (key=? a) #f))
       )
    (cond
     ((assq key alist) => (lambda (pair) (set-cdr! pair elt)))
     (else (data-set! a (cons (cons key elt) alist))))))

(define-method (elt-ref-or (a <alist>) key default)
  (cond
   (((make-assq (key=? a) #f) key (data a)) => cdr)
   (else default)))
                
(define-method (elt-ref (a <alist>) key)
  ;; Note Bene: NOT AN ERROR
  (elt-ref-or a key #f))

(define-method (elt-set! (a <alist>) key new-elt)
  (let ( (alist (data a))
         (assq (make-assq (key=? a) #f))
       )
    (cond
     ((assq key alist) => (lambda (pair) (set-cdr! pair new-elt) a))
     (else
      (with-restart
       (<add-key-elt>
        format-string:    "Try adding (key, value)= (~s, ~s) to ~a collection"
        format-arguments: (list key new-elt (class-name (class-of a)))
        restart-function: (lambda (self cdn)
                            (add-key-elt a key new-elt)))
      (CERROR (<elt-not-found>
               format-string: "(ELT-SET! ~s ~s ~s): No elt at key!"
               format-arguments: (list a key new-elt)
               collection: a
               key: key
               elt: new-elt)))))
) )

(define-method (remove-key!  (a <alist>) key) 
  (let ( (alist (data a))
         (same? (key=? a))
       )
    (cond
     ((null? alist) a) ;; nothing to remove
     ((same? key (caar alist))
      (data-set! a (cdr alist))
      a)
     (else
      (let loop ( (alist alist) )
        (cond
         ((null? (cdr alist)) a)
         ((same? key (caadr alist))
          (set-cdr! alist (cddr alist))
          a)
         (else
          (loop (cdr list)))))))
) )
  


(define-method (num-elts (a <alist>)) (length (data a)))

(define-method (for-each-elt (f <function>) (a <alist>))
  (let loop ( (list (data a)) )
      (cond
       ((not (null? list))
        (f (cdar list))
        (loop (cdr list))))
) )

(define-method (for-each-key (f <function>) (a <alist>))
  (let loop ( (list (data a)) )
      (cond
       ((not (null? list))
        (f (caar list))
        (loop (cdr list))))
) )

(define-method (for-each-key-elt (f <function>) (a <alist>))
  (let loop ( (list (data a)) )
      (cond
       ((not (null? list))
        (f (caar list) (cdar list))
        (loop (cdr list))))
) )

;; fold operates on elts
(define-method (fold-left (f <function>) seed (a <alist>))
  (letrec ( (foldl
             (lambda (value list)
                 (if (null? list)
                     value
                     (foldl (f (cdar list) value) (cdr list)))))
            )
      (foldl seed (data a))
) )

;==============================================================;
;;; <HASH-TABLE> ;; Gambit's tables

(define-method (add-key-elt! (dict <hash-table>) key elt)
  (table-set! dict key elt))

(define-method (elt-ref-or (dict <hash-table>) key default)
  (table-ref dict key default))
                
(define-method (elt-ref (dict <hash-table>) key)
  ;; Note Bene: NOT AN ERROR
  (table-ref dict key #f))

(define-method (elt-set! (dict <hash-table>) key new-elt)
  (table-set! dict key new-elt))

(define-method (remove-key!  (dict <hash-table>) key) 
  (table-set! dict key))

(define-method (num-elts (dict <hash-table>))
  (table-length dict))

(define-method (for-each-elt (f <function>) (dict <hash-table>))
  (table-for-each (lambda (k v) (f v)) dict))

(define-method (for-each-key (f <function>) (dict <hash-table>))
  (table-for-each (lambda (k v) (f k)) dict))

(define-method (for-each-key-elt (f <function>) (dict <hash-table>))
  (table-for-each f dict))

;; fold operates on elts
(define-method (fold-left (f <function>) seed (dict <hash-table>))
  (let ( (result seed) )
    (table-for-each
     (lambda (key elt)
       (set! result (f elt result)))
     dict)
    result)
)

(define-method (copy (dict <hash-table>))
  ;; @@FIXME: hash, test, weak-keys weak-elts
  (list->table (table->list dict))
 )

(define-method (every-elt? (pred? <function>) (dict <hash-table>))
  (call/cc
   (lambda (return)
     (table-for-each
      (lambda (key elt)
        (if (not (pred? elt))
            (return #f)))
      dict)
     (return #t))
) )

(define-method (any-elt? (pred? <function>) (dict <hash-table>))
  (table-search (lambda (key elt) (pred? elt)) dict))


(define-method (as (result-class <subclass-of-list>) (dict <hash-table>))
  (table->list dict))

(define-method (as (result-class <subclass-of-hash-table>) (list <list>))
  (list->table list))
                
;==============================================================;

(when debug-oops
      (format #t "~%Oops: loaded COLLECTIONS methods"))

;===========================E=O=F==============================;

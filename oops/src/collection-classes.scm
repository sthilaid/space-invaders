;;; FILE: "collection-classes.scm"
;;; IMPLEMENTS: Oops Collection Classes -- methods in another file
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
  <alist>
  <boolean>
  <box>
  <character>
  <class>
  <collection>
  <complex>
  <dictionary>
  <elastic>
  <float32>
  <float64>
  <f32vector>
  <f64vector>
  <function>
  <generic>
  <hash-table>
  <indexed>
  <integer>
  <keyed>
  <keyword>
  <list>
  <meroon>
  <method>
  <null>
  <ordered>
  <pair>
  <rational>
  <real>
  <record>
  <record-type>
  <s16int>
  <s32int>
  <s8int>
  <s16vector>
  <s32vector>
  <s64vector>
  <s8vector>
  <set>
  <singleton>
  <sorted>
  <stretchy-string>
  <stretchy-vector>
  <stretchy>
  <string>
  <record-type>
  <symbol>
  <type>
  <type-union>
  <thread-safe>
  <u16int>
  <u32int>
  <u8int>
  <u16vector>
  <u32vector>
  <u64vector>
  <u8vector>
  <value>
  <vector>
  abstract-class-error
  bind-slot!
  class-of
  class-slots
  class?
  compare
  debug-oops
  element-type
  ensure-getters
  ensure-setters
  fill
  format
  generic?
  hash-fun
  index-type
  instance-class
  instance?
  key=?
  make-ref
  make-set!
  mutex
  size
  weak-keys
  weak-values

))

;; local names
 (##namespace
  ("oops#"
   bound?
   instance-class-set!
   make-instance-plus
   parse-method-args   
 ))

;==============================================================;
(include "common-macros.scm")
(include "oops-macros.scm")
;==============================================================;
;; Elide compiler warnings
(define fill        'bootstrap)
(define hash-fun    'bootstrap)
(define key=?       'bootstrap)
(define mutex       'bootstrap)
(define size        'bootstrap)
(define weak-keys   'bootstrap)
(define weak-values 'bootstrap)
;==============================================================;
;; Bootstrap
(define abstract-class-error
  (lambda ignore
    (error "Can't make instance of an Abstract Class")))

;==============================================================;
;;; See "collection-methods.scm" for class diagram
;==============================================================;
;;; <COLLECTON>

;; There are a number of abstract classes which are used
;; to define collection protocols.

(define-class <collection> <value> ()
  abstract-class-error)

;; one then another..
(define-class <ordered> <collection> ()
  abstract-class-error)

;; the empty collection marker [REDEF]
(define-class <list> (<ordered>) ()
  abstract-class-error)

(define-virtual-class <null> (<list>) ()
  (lambda (inst) '()))

(define-virtual-class <pair> (<list>) ()
  (lambda (noise)
    (error '<pair>
           "Can't create values; use cons")
) )

;; can extend..
(define-class <stretchy> <collection> ()
  abstract-class-error)

;; can shrink as well as extend
(define-class <elastic> <stretchy> ()
  abstract-class-error)

;; maps key -> value
(define-class <keyed> <collection>
  ((index-type   type: <type>
                 init-value: <value>
                 allocation: each-subclass:)
   (key=?       type: <function>
                init-value: eq?)
   (element-type type: <type>
                 init-value: <value>
                 allocation: each-subclass:))
  abstract-class-error)

;; sequence of keys 0 1 2 ...
(define-class <indexed> (<keyed> <ordered>)
  ((index-type  type: <type>
                init-value: <integer> ;; override
                allocation: each-subclass:)
   (size        type: <integer>
                init-value: 0)
   )
  abstract-class-error)

(define-generic (compare this that)
  (cond
   ((< this that) -1)
   ((= this that) 0)
   (else 1)))

(define-class <sorted> <indexed>
  ((compare-function type: <function>
                     init-value: compare))
  abstract-class-error)

(define-class <thread-safe> <collection>
  ( (mutex init-function: (lambda () (make-mutex (gensym 'collection)))
           use-init-keyword: #f)
    (walkers init-value: '()
             use-init-keyword: #f)
  )
)

;==============================================================;
;;  A Virtual Class is used to present Gambit Scheme native values.

;;; <STRING>
(define-virtual-class <string> <indexed>
  ((element-type type: <type>
                 init-value: <character> ;; override
                 allocation: each-subclass:)
   (fill         type: <character>
                 init-value: #\space
                 allocation: override:)
   )
  ;; replace inst w string
  (lambda (inst) (make-string (size inst) (fill inst)))
)


;;; <VECTOR>
(define-virtual-class <vector> <indexed>
  ((fill         type: <value>))
  (lambda (inst) (make-vector (size inst) (fill inst))))


;; A Dictionary maps keys to values
(define-class <dictionary> (<keyed> <elastic>) ()
  abstract-class-error)

(define-virtual-class <hash-table> <dictionary> ;; Gambit's tables
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
   (hash-fun      init-value: eq?-hash
                  type: <function>)
   (weak-keys     init-value: #f
                  type: <boolean>)
   (weak-values   init-value: #f
                  type: <boolean>)
  )
  (lambda (inst)
    (make-table weak-keys:   (weak-keys inst)
                weak-values: (weak-values inst)
                test: (key=? inst)
                hash: (hash-fun inst))) ;; returns the hash-table
)

(define-class <alist> (<dictionary> <ordered>)
  ((data  type: <list>
          init-value: '())
   )
  (lambda (inst) '())
)

(define-class <set> <collection> ;; immutable!
  ((data  type: <list>
          init-value: '()))
)

;;;(define-virtual-class <hash-table> <dictionary> (@@@)) ;; use native tables
;;;(define-class <treap>  <dictionary> (@@@))
;;;(define-class <stream> (<ordered> <elastic>) (@@@)) 
;;;(define-class <queue>   <stream> (@@@))
;;;(define-class <dequeue> <queue> (@@@))


(define-class <stretchy-vector> (<vector> <stretchy>)
  ((count type: <integer>
          init-value: 0)
   (data  type: <vector>
          init-function: (lambda () (make-vector 6)))
  )
)

(define-class <stretchy-string> (<string> <stretchy>)
  ((count type: <integer>
          init-value: 0)
   (data  type: <string>
          init-function: (lambda () (make-string 6)))
  )
)

;==============================================================;
;;; Limited Vectors

(define-virtual-class <u8vector> <vector>
  ((element-type type: <type>
                 init-value: <u8int>
                 allocation: each-subclass:)
;  (fill         type: <u8int>
;                init-value: 0
;                allocation: override:)
  )
  (lambda (inst) (make-u8vector (size inst) (fill inst)))
)

(define-virtual-class <s8vector> <vector>
  ((element-type type: <type>
                 init-value: <s8int>
                 allocation: each-subclass:)
;  (fill         type: <s8int>
;                init-value: 0
;                allocation: override:)
  )
  (lambda (inst) (make-s8vector (size inst) (fill inst)))
)

(define-virtual-class <u16vector> <vector>
  ((element-type type: <type>
                 init-value: <u16int>
                 allocation: each-subclass:)
;  (fill         type: <u16int>
;                init-value: 0
;                allocation: override:)
  )
  (lambda (inst) (make-u16vector (size inst) (fill inst)))
)

(define-virtual-class <s16vector> <vector>
  ((element-type type: <type>
                 init-value: <s16int>
                 allocation: each-subclass:)
;  (fill         type: <s16int>
;                init-value: 0
;                allocation: override:)
  )
  (lambda (inst) (make-s16vector (size inst) (fill inst)))
)

(define-virtual-class <s32vector> <vector>
  ((element-type type: <type>
                 init-value: <s32int>
                 allocation: each-subclass:)
;  (fill         type: <s32int>
;                init-value: 0
;                allocation: override:)
  )
  (lambda (inst) (make-s32vector (size inst) (fill inst)))
)

(define-virtual-class <u32vector> <vector>
  ((element-type type: <type>
                 init-value: <u32int>
                 allocation: each-subclass:)
;  (fill         type: <u32int>
;                init-value: 0
;                allocation: override:)
  )
  (lambda (inst) (make-u32vector (size inst) (fill inst)))
)

(define-virtual-class <f32vector> <vector>
  ((element-type type: <type>
                 init-value: <float32>
                 allocation: each-subclass:)
;  (fill         type: <f32int>
;                init-value: 0
;                allocation: override:)
  )
  (lambda (inst) (make-f32vector (size inst) (fill inst)))
)

(define-virtual-class <s64vector> <vector>
  ((element-type type: <type>
                 init-value: <s64int>
                 allocation: each-subclass:)
;  (fill         type: <s64int>
;                init-value: 0
;                allocation: override:)
  )
  (lambda (inst) (make-s64vector (size inst) (fill inst)))
)

(define-virtual-class <u64vector> <vector>
  ((element-type type: <type>
                 init-value: <u64int>
                 allocation: each-subclass:)
;  (fill         type: <u64int>
;                init-value: 0
;                allocation: override:)
  )
  (lambda (inst) (make-u64vector (size inst) (fill inst)))
)

(define-virtual-class <f64vector> <vector>
  ((element-type type: <type>
                 init-value: <float64>
                 allocation: each-subclass:)
;  (fill         type: <f64int>
;                init-value: 0
;                allocation: override:)
  )
  (lambda (inst) (make-f64vector (size inst) (fill inst)))
)


;==============================================================;
;;; CLASS-OF
(set! class-of 
   (lambda (thing)
     (if (not (##subtyped? thing))
         (case (##type thing)
           ((0) <integer>) ;; fixnum
           ((3) <pair>)
           ((2) (cond  ;; #!eof #t 3...
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
                 (else  <record>))
            )
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
           ((18) <value>) ;;@@
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

(when debug-oops
      (format #t "~%Oops: loaded COLLECTION classes"))

;===========================E=O=F==============================;

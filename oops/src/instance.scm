;;; FILE: "instance.scm"
;;; IMPLEMENTS: Instance datatype for Oops
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
  bind-slot!		; export
  class-of
  format
  instance-class	; export
  instance?		; export
  slot-unbound-value?	; export

  debug-oops
))

;; local names
(##namespace
 ("oops#"
  %%first-data-slot-index%%
  %%instance-subtype%%
  %%instance-type%%
  class-name->instance-name
  make-instance-plus
  really-make-instance
))

;=============================================================================
(include "common-macros.scm")
;=============================================================================

;;(define slot-unbound-value #!unbound )

(define (slot-unbound-value? val)
  (eq? val #!unbound)) ;;slot-unbound-value))


;; An <INSTANCE> is a record/structure with added, anonymous slots.
(define-structure instance class)
; (make-instance <point>)
; (instance? i)
; (instance-class i) => <point>

(define (class-name->instance-name class-name)
  ;; class-name must be a symbol or a string
  (string->symbol
   (string-append "A-"
                  (if (symbol? class-name)
                      (symbol->string class-name)
                      class-name))))

(define really-make-instance make-instance)


;; Instance:
;;   slot 0 => <type> ==> instance
;;   slot 1 => class
;;   slot 2 => 1st data/instance slot
;;   slot 3 => 2nd data slot
;;   ...
(define %%first-data-slot-index%% 2)

(define %%instance-type%%    #f)
(define %%instance-subtype%% #f)

(define make-instance-plus
  (let* ( (proto-obj     (really-make-instance no-class:))
          (proto-type    (##vector-ref proto-obj 0))
          (proto-subtype (##subtype proto-obj))
        )
    
    (set! %%instance-type%%    proto-type)
    (set! %%instance-subtype%% proto-subtype)
    
    (lambda (num-slots-to-add)
      (let ( (new-obj (##make-vector
                       (+ %%first-data-slot-index%% num-slots-to-add)
                       #!unbound)) ;; slot-unbound-value))
           )
        (##subtype-set! new-obj proto-subtype)
        (##vector-set!  new-obj 0 proto-type)
        new-obj))
) )


;; For Class Initializatiom Macro ONLY!!!
(define (bind-slot! instance index value)
  (let ( (real-index (+ index %%first-data-slot-index%%)) )
    (if (eq? #!unbound (##vector-ref instance real-index))
        (##vector-set! instance real-index value)
        (call-error "Can't init an initialized slot!!!"
                    bind-slot! instance index value)
) ) )

(when debug-oops
      (format #t "~%Oops: loaded INSTANCE code"))


;===========================E=O=F==============================;

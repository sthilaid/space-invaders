;;; FILE: "slots.scm"
;;; IMPLEMENTS: slot options for Oops
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
; SLOT_OPTIONS:
;   allocation:
;   { class: | instance: | constant: | virtual: | each-subclass: | override: }
; 	       -- default: instance:
;;  When allocation is override: instance slot def is repaced w new.
;   When allocation is virtual: slot-set!: and slot-ref: are required
;     slot-set!: (lambda (instance new-value) form..)
;     slot-ref:  (lambda (instance) form..)
;   use-init-keyword: { #t | #f } -- default #t
; ;; chose one of { init-value: | init-function: }
;   init-value: <scheme-value>  -- default is slot-unbound-value
;   init-function: (lambda () ...) -- no default
;   type-predicate: (lambda (inst) ..) -- must return { #t | #f }


;==============================================================;
(include "gambit-namespace.scm")
(##declare (standard-bindings) (extended-bindings) (fixnum) (separate))
;; top level names
(##namespace
 (""
  format
  member?
  slot-info?	;; export
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
  class-slot-info?	; export
  indexed-slot-info?	; export
  instance-slot-info?	; export
  make-slot-info	; export
  process-slot		; export
  really-make-slot-info
  virtual-slot-info?	; export
))

;==============================================================;
(include "common-macros.scm")
;==============================================================;
;; Elide loader warnings
(define debug-oops #t)
;==============================================================;

(define-structure slot-info
  name
  allocation
  slot-ref slot-set!
  use-init-keyword
  init-value init-fun
  type)

(define really-make-slot-info make-slot-info)

(set! make-slot-info
  (lambda (name
           #!key
           (allocation instance:)
           (slot-ref  #f)
           (slot-set! #f)
           (use-init-keyword not-set-by-user:) ;; no generic accessor if #f
           (init-value #!unbound)
           (init-fun #f)
           (type     #f))
    (really-make-slot-info
     name allocation slot-ref slot-set! use-init-keyword
     init-value init-fun type)
) )



;=============================================================================
;
; 		    S L O T   D E F I N I T I O N   O P T I O N S
;
;=============================================================================

;; (process-slot <slot-description>) => slot-info struct
;; NOTE: Slot options form is one of:
;;    name
;;    (name key1 option1 key2 option2 ...)
(define (process-slot slot-options)
  
  (define (bad-options)
    (call-error "ill-formed slot options" process-slot slot-options))

  (define (check-slot-info info) ;; Check some basic rules:
    (cond
     ((and (slot-info-init-fun info)
           (not (eq? #!unbound (slot-info-init-value info))))
      (call-error "Can't have both init-value: and init-function:"
                  process-slot
                  slot-options)
      )
     ((not (member? (slot-info-allocation info)
                    '(class: instance: constant: virtual: each-subclass: override: )))
      (call-error "Bad slot allocation" process-slot (slot-info-allocation info))
      )
     ((and (eq? virtual: (slot-info-allocation info))
           (or (not (slot-info-slot-ref info))
               (not (slot-info-slot-set! info))))
      (call-error "Must supply both slot-ref: and slot-set!: functions when allocation is virtual:"
                  process-slot
                  slot-options)
      )
     ((and (memq (slot-info-allocation info) '(class: each-subclass:))
           (not (slot-info-init-fun info))
           (eq? #!unbound (slot-info-init-value info)))
      (call-error "Class slots must specify init-value: or init-function:"
                  process-slot
                  slot-options)
      )
     (else 'OK))
    )

;;  (trace check-slot-info) ;; @@@DEBUG
  (cond
   ((symbol? slot-options) ;; name
    (make-slot-info slot-options)
    )
   ((not (and (pair? slot-options)
              (symbol? (car slot-options))))
    (bad-options)
    )
   (else
    (let ( (name (car slot-options)) )
      (let loop ( (options (cdr slot-options))
                  (allocation instance:)
                  (slot-ref   #f)
                  (slot-set!  #f)
                  (use-init-keyword not-set-by-user:) 
                  (init-value #!unbound)
                  (init-fun   #f)
                  (type       #f))
        (cond
         ((null? options) ;; done
          (let ( (slot-info
                  (really-make-slot-info
                   name allocation slot-ref slot-set! use-init-keyword
                   init-value init-fun type))
               )
            (check-slot-info slot-info)
            slot-info)
          )
         ;; process 1st 2 options (key value ...)
         ((null? (cdr  options)) ;; only key?
          (bad-options)
          )
         (else
          (let ( (key (car options)) (val (cadr options)) )
;            (when debug-oops
;              (format #t "~%key= ~a, val= ~a, options= ~a" key val (cddr options)))
            (case key
              ((allocation:)
               (loop (cddr options) val        slot-ref          slot-set! use-init-keyword init-value init-fun type))
              ((slot-ref:)
               (loop (cddr options) allocation (list 'quote val) slot-set! use-init-keyword init-value init-fun type))
              ((slot-set!:)
               (loop (cddr options) allocation slot-ref  (list 'quote val) use-init-keyword init-value init-fun type))
              ((use-init-keyword:)
               (loop (cddr options) allocation slot-ref          slot-set! val              init-value init-fun type))
              ((init-value:)
               (loop (cddr options) allocation slot-ref          slot-set! use-init-keyword (list 'quote val)  init-fun type))
              ((init-function:)
               (loop (cddr options) allocation slot-ref          slot-set! use-init-keyword init-value (list 'quote val) type))
              ((type:)
               (loop (cddr options) allocation slot-ref          slot-set! use-init-keyword init-value init-fun val       ))
              (else (bad-options)))))))
 ) ) ) )


;; Utility/helpers

(define (class-slot-info? slot-info)
  (member? (slot-info-allocation slot-info)
           '(class: each-subclass:)))

(define (virtual-slot-info? slot-info)
  (eq? (slot-info-allocation slot-info) virtual:))

(define (indexed-slot-info? slot-info)
  (memq (slot-info-allocation slot-info)
        '(instance: override: constant:)))

(define (instance-slot-info? slot-info)
  (or (indexed-slot-info? slot-info)
      (virtual-slot-info? slot-info)))

(when debug-oops
  (format #t "~&Oops: loaded SLOT Support Code"))


;===========================E=O=F==============================;

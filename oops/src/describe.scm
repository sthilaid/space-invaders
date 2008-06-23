;;; FILE: "describe.scm"
;;; IMPLEMENTS: DESCRIBE for Oops
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
  %method
  <generic>
  <method>
  class?
  class-direct-slots
  class-getters
  class-name
  class-of
  class-precedence-list
  class-slots
  describe	;; export
  format
  generic?
  generic-args-info
  generic-name
  generic-methods
  instance-class
  instance?
  method?
  method-name
  method-args-info
  slot-info?
  slot-info-allocation
  slot-info-name

))

;; local names
(##namespace
 ("oops#"
  cell-ref
  class-slot-info? 
  env-lookup
  filter
  indexed-slot-info?
  method-args-form
  virtual-slot-info?
))

;==============================================================;
(include "oops-macros.scm")
(include "common-macros.scm")
;==============================================================;
;; '(require oops)
;; '(require format)

(define-generic (describe thing #!optional (outport #t))
  
  (define (get-slot-info-list class)
    (let loop ( (cpl (class-precedence-list class))
                (slots '())
                (slot-names-so-far '())
              )
      (if (null? cpl)
          slots
          (let ( (added-slots (filter-slots (class-direct-slots (car cpl))
                                            slot-names-so-far))
               )
          (loop (cdr cpl)
                (append added-slots slots)
                (append (map slot-info-name added-slots)
                        slot-names-so-far))))
  ) )
  (define (filter-slots slots-info-list names)
    (filter (lambda (info) (not (memq (slot-info-name info) names)))
            slots-info-list)
  )
  (define (format-args-info args-info port)
    (format port "~a" (method-args-form args-info))
  )

  (let ( (class (class-of thing))
         (port (if (not outport) (open-output-string) outport))
       )
    (cond
     ((class? thing)
      (format port "~s is a ~a~&" (class-name thing) (class-name (class-of thing)))
      (let ( (cells-alist (class-slots thing)) )
	(when (> (length cells-alist) 0)
	  (format port "~&== Class Slots ==~&")
	  (for-each 
	   (lambda (pair)
	     (format port "~&~a: ~s" 
		     (car pair) 
		     (let ( (val (cell-ref (cdr pair))) )
		       (if (class? val) (class-name val) val))))
	   cells-alist)
	  (format port "~&")
        ) )
      )
     ((instance? thing)
      (let* ( (all-slots     (get-slot-info-list class))
	      (indexed-slots (filter indexed-slot-info? all-slots))
	      (virtual-slots (filter virtual-slot-info? all-slots))
	      (class-slots   (filter class-slot-info?   all-slots))
	    )
	(format port "Instance of ~a" (class-name class))
	(when (> (length indexed-slots) 0)
	  (format port "~&== Instance Slots ==~&")
	  (let loop ( (slots indexed-slots) )
	    (when (not (null? slots))
	      (let ( (slot (car slots)) )
		(format port
			"~&~a: ~s"
			(slot-info-name slot)
			((env-lookup (slot-info-name slot)) thing)))
	      (loop (cdr slots)))))
	(when (> (length virtual-slots) 0)
	  (format port "~&== Virtual Slots ==~&")
	  (let loop ( (slots virtual-slots) )
	    (when (not (null? slots))
	      (let ( (slot (car slots)) )
		(format port
			"~&~a: ~s"
			(slot-info-name slot)
			((env-lookup (slot-info-name slot)) thing)))
	      (loop (cdr slots)))))
	(when (> (length class-slots) 0)
	  (format port "~&== Class Slots ==~&")
	  (let loop ( (slots class-slots) )
	    (when (not (null? slots))
	      (let ( (slot (car slots)) )
		(format port
			"~&~a: ~s"
			(slot-info-name slot)
			((env-lookup (slot-info-name slot)) thing)))
	      (loop (cdr slots)))))
	(format port "~&"))
      )
     ((generic? thing)
      (format port "~&Generic function ~a" (generic-name thing))
      (format port " has ~a specific methods" (length (generic-methods thing)))
      (format port " ~& and args: ")
      (format-args-info (generic-args-info thing) port)
      (format port "~&")
      )
     ((method? thing)
      (format port "~&Method ~a with arguments: " (method-name thing))
      (format-args-info (method-args-info thing) port)
      (format port "~&")
      )
     (else ;; Not a Class or an Instance..
      (format port "~s is a ~a~&" thing (class-name (class-of thing)))
      ))
    (if (not outport)
	(get-output-string port)
	(void))
 ) )


;===========================E=O=F==============================;

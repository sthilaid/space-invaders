;;; FILE: "oops-macros.scm"
;;; IMPLEMENTS: Macros requires to use Oops
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
;;; Nota Bene: be sure to (include "oops-macros.scm") !!!
;==============================================================;

(define-macro (define-class name supers slots . instance-init-fun)
  (%moby-define-class  `,name `,supers `,slots #f `,instance-init-fun))

(define-macro (define-method name  . body)
  `(ensure-generic ,(%method  `,name `,body)))

(define-macro (define-generic name . default-body)
  (%define-generic  `,name `,default-body))

;; A Virtual Class is used for native/primitive Scheme values
;; The init-function is required and its values returned in place of an instance.
;; There are 2 interesting places where slot-overrides are used:
;;  each-subclass: (class slot) and override: (instance slot).
;; - each-subclass: is used for common properties (e.g. index-type).
;; - override: is used for instance options used to create native objects
;; (e.g. size, fill).
;; Note that virtual classes allow class slots to be accessed even
;; though, for Scheme native objects, there are no instances.
;; One can access class slots of a non-instance.  Obviously, it is an
;; error to try and access instance slots of non-instances!
;; Slot options are used is to transmit values to init functions.
;; E.g. (define-virtual-class <vector> <indexed> ( (fill  type: <value>) )
;;        (lambda (inst) (make-vector (size inst) (fill inst))))
(define-macro (define-virtual-class name supers slots . instance-init-fun)
  (%moby-define-class  `,name `,supers `,slots #t `,instance-init-fun))


;; Handlers and Restarts are invoked in the dynamic context of the condition/error.
  
(define-macro (with-handler handler . body)
  `(parameterize ( (%%condition-handlers%%
                    (cons ,handler (%%condition-handlers%%)))
                  )
     ,@body))

(define-macro (with-restart restart . body)
  `(parameterize ( (%%restart-handlers%%
                    (cons ,restart (%%restart-handlers%%)))
                  )
     ,@body))

(define-macro (with-handlers handlers . body)
  `(parameterize ( (%%condition-handlers%%
                    (append (list ,@handlers) (%%condition-handlers%%)))
                  )
     ,@body))

(define-macro (with-restarts restarts . body)
  `(parameterize ( (%%restart-handlers%%
                    (append  (list ,@restarts) (%%restart-handlers%%)))
                  )
     ,@body))

;;; Top-level Handler for oops calls
(define-macro (with-oops-handler . forms)
  `(with-exception-catcher
    (lambda (exn) (signal (->oops-exception exn)))
    (lambda () ,@forms)))


;;(define-macro (unwind-condition handler . body) ..)

;===========================E=O=F==============================;

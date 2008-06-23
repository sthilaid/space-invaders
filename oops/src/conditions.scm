;;; FILE: "conditions.scm"
;;; IMPLEMENTS: Signal, conditions, restarts, exception handling for Oops
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

#|

Given requirements of fortune 1000 servers -- processes tend to live
for (2 to 3) years, less than 3 seconds of down time per year,
fail-over, et cetera.  One needs software systems which recover
gracefully from all errors.  [One needs systems which can do live
code updates as well -- which is why you are reading this.  If
you can afford to fail you can write code in any programming language].

Running out of resources (disk space, net connection, core memory,
cpu bandwidth ..) is not typical but is not a programming error
... unless your system does not deal with it!

"Yes", said the doctor, "you do have a hangnail", so he shoots you
and sends a note to your mother to have another child.  Perhaps
a way of saying "Wait a minute! I have some salient state I want
to save here!" ...

Hence this code.

Following CLOS & Dylan, this error system has three parts:
  signalling exceptions
  handling exceptions
  restarts (exception recovery)

The basic idea is that signalling an error happens in the
lowest (most detailed) level where most is known about the
specifics of errors.

Handlers tend to make policy at a high level and don't want
to break encapsulation by knowing low-level specific details.

Restarts are defined in low to mid levels and know enough detail
to correct whatever problem is recognized to keep the system sane
and healthy.  Restarts can be invoked by handler code or
interactively in the debugger.


A CONDITION is an object which encapsulates information on
the "problem"/error/exception.

Conditions are SIGNAL'ed. Each Handler is checked (in the
dynamic environment) to see if it handles the type of
condition which was signalled.

A HANDLER is a function of two arguments: the CONDITION object
and a NEXT-HANDLER function (which is itself a thunk -- a function
of zero arguments).  If the function returns, the values returned
are the value that the SIGNAL returns.  The handler may also take
a non-local exit (as with any code) or (more typically) tail-call
next-handler if it decides to decline to handle the condition.
(lambda (condition next-handler) ...)

Like handlers, RESTARTs have dynamic extent.  Note that, unlike Dylan,
restarts are disjoint from (do not inherit from) conditions.

|#

;==============================================================;
(include "gambit-namespace.scm")
(##declare (standard-bindings) (extended-bindings) (fixnum) (separate))
;; top level names
(##namespace
 (""
  <abandoned-mutex-exn>
  <abort>
  <abstract-class>
  <breakpoint>
  <call-error>
  <class>
  <code+msg>
  <condition>
  <datum-parsing-exn>
  <deadlock-exn>
  <divide-by-zero>
  <error>
  <exception>
  <expression-parsing-exn>
  <foreign-interface-exn>
  <foreign-repeated-return-exn>
  <foreign-to-scheme-conversion-exn>
  <format-info>
  <function>
  <generic>
  <handler>
  <heap-overflow-exn>
  <integer>
  <invalid-hash-number>
  <join-timeout-exn>
  <memory-exn>
  <memory-exn>
  <method>
  <no-such-file-or-directory-exn>
  <os-call-exn>
  <os-exn>
  <proc+args>
  <range-error>
  <reader-exn>
  <restart>
  <retry-apply>
  <return-value-in-place-of>
  <scheduler-exn>
  <scheme-to-foreign-conversion-exn>
  <stack-overflow-exn>
  <started-thread-exn>
  <string>
  <table-exn>
  <terminated-thread-exn>
  <thread-exn>
  <type-error>
  <unbound-global-error>
  <unbound-os-environment-exn>
  <unbound-serial-number>
  <unbound-table-key>
  <uncaught-exception>
  <unknown-error>
  <value>
  <warning>
  BREAK
  CERROR
  ERROR-NO-RETURN
  WARNING
  abstract-class-error
  abstract-class-error
  arguments
  bind-slot!
  class-slots
  condition?
  default-handler
  ensure-generic
  ensure-getters
  ensure-setters
  find-restart
  format
  format-args
  format-string
  function
  generic?
  invoke-debugger
  invoke-restart
  is-a?
  make-ref
  make-set!
  message
  name
  procedure
  report
  restart
  restart-function
  restart-function
  restart-query
  restart-return-false
  return-description
  return-query
  show-restarts
  signal
  type

  debug-oops
))

;; local names
(##namespace
 ("oops#"
  %%condition-handlers%%
  %%current-condition%%
  %%restart-handlers%%
  ->oops-exception  
))

;==============================================================;
;; (require 'oops)
;; (require 'format)
(include "common-macros.scm")
(include "oops-macros.scm")
;==============================================================;
;; Elide compiler warnings
(define ERROR-NO-RETURN  'bootstrap)
(define WARNING           'bootstrap)
(define arguments        'bootstrap)
(define default-handler  'bootstrap)
(define format-args      'bootstrap)
(define format-string    'bootstrap)
(define function         'bootstrap)
(define message          'bootstrap)
(define procedure        'bootstrap)
(define restart-function 'bootstrap)

;==============================================================;
;;;This tree is much simpler than, hence differnt from, Dylan & CLOS.
;;;
;;;                              <value>
;;;                              /      \
;;;                    <condition>      <restart>
;;;                     /       \        
;;;              <exception>  <warning>  
;;;                   |               
;;;             --------------------------------------- ...
;;;            /        |          |          \        \
;;;         <error>  <abort>  <thread-exn>  <os-exn>  <memory-exn> ...
;;;         /    \
;;;<type-error> <call-error> ...
;;;


(define-class <condition> <value>  ;; an abstract class
  ((return-allowed? allocation: each-subclass:
                    init-value: #f))
  abstract-class-error)

(define (condition? thing)
  (is-a? thing <condition>))

(define-class <format-info> () ;; mixin
  ((format-string    type: <string>
                     init-value: "~s"
                     allocation: constant:)
   (format-args      init-value: '()
                     allocation: constant:)))

(define-class <proc+args> <value> ;; mixin
  ((procedure  type: <function>)
   (arguments  init-value: '()))
)

(define-class <code+msg> <value> ;; mixin
  ((error-code type: <integer>)
   (message    type: <string>))
)

;;; <EXCEPTION>
(define-class <exception> (<condition> <format-info>) ())
(define-class <abort> <exception> ())
(define-class <unknown-error> <exception> ())

;;; <ERROR>
(define-class <error> <exception> ())
(define-class <range-error> (<error> <proc+args>) ())
(define-class <call-error> (<error> <proc+args>)
  ((return-allowed? allocation: each-subclass: init-value: #t))) ;; override
(define-class <unbound-global-error> <error>
  ((name type: <string>)))
(define-class <divide-by-zero> <call-error> ())
(define-class <type-error> <call-error> 
  ((value type: <value>
          allocation: constant:)
   (type  type: <type>
          allocation: constant:))
)


;;; <WARNING>
(define-class <warning> (<condition> <format-info>) ())

;;; <MEMORY-EXN>
(define-class <memory-exn> <exception> ())
(define-class <heap-overflow-exn> <memory-exn> ())
(define-class <stack-overflow-exn> <memory-exn> ())

;;; <OS-EXN>
(define-class <os-exn>  (<exception> <proc+args>) ())
(define-class <os-call-exn> (<os-exn> <code+msg>) ())
(define-class <no-such-file-or-directory-exn> <os-exn> ())
(define-class <unbound-os-environment-exn>    <os-exn> ())

;;; <THREAD-EXN>
(define-class <thread-exn> <exception> ())
(define-class <scheduler-exn>         <thread-exn> ((cause type: <exception>)))
(define-class <deadlock-exn>          <thread-exn> ())
(define-class <abandoned-mutex-exn>   <thread-exn> ())
(define-class <join-timeout-exn>      <thread-exn> ())
(define-class <started-thread-exn>    <thread-exn> ())
(define-class <terminated-thread-exn> <thread-exn> ())
(define-class <uncaught-exception>    <thread-exn> ((cause type: <exception>)))

;;; <FOREIGN-INTERFACE-EXN>
(define-class <foreign-interface-exn> <exception>
  ((foreign-language type: <string> init-value: "C")))
(define-class <scheme-to-foreign-conversion-exn>
  (<foreign-interface-exn> <proc+args> <code+msg>)
  ())
(define-class <foreign-to-scheme-conversion-exn>
  (<foreign-interface-exn> <proc+args> <code+msg>)
  ())
(define-class <foreign-repeated-return-exn>  <foreign-interface-exn> ())

;;; <READER-EXN>
(define-class <reader-exn> <exception> () abstract-class-error)
(define-class <datum-parsing-exn>      <reader-exn> ())
(define-class <expression-parsing-exn> <reader-exn> ())

;;; <MEMORY-EXN>
(define-class <memory-exn> <exception> ())

;;; <BREAKPOINT>
(define-class <breakpoint> <exception> ())

;;; <TABLE-EXN>
(define-class <table-exn> (<exception> <proc+args>) ())
(define-class <invalid-hash-number>   <table-exn> ())
(define-class <unbound-table-key>     <table-exn> ())
(define-class <unbound-serial-number> <table-exn> ())


;;; <ABSTRACT-CLASS>
(define-class <abstract-class> <error> ())
(define abstract-class-error
  (lambda ignored
    (ERROR-NO-RETURN
     (<abstract-class>
      format-string: "Can't make an instance of an abstract class"))))


;;;  <RESTART>
(define-class <restart> <format-info>
  ((restart-function  type: <function>)) ;; (lambda (self condition) ...)
 )

(define-class <retry-apply> <restart> ()) ;; for <call-error>
(define-class <return-value-in-place-of> <restart> ())



;;; <HANDLER>
(define-class <handler> ()
  ((type      type: <condition>)
   (function  type: <function>))
)



;==============================================================;
;; @@FIXME: Integrate Restarts w REPL
(define invoke-debugger ##repl-debug)


(define-method (default-handler (c <condition>)) #f)

(define-method (default-handler (e <exception>))
  (let ( (port (current-error-port)) )
    (format port "~&***~?"
            (format-string e)
            (format-args e))
    (show-restarts port)
    (invoke-debugger "<exception>")))

(define-method (default-handler (w <warning>))
  (format (current-error-port)
          "~&***WARNING: ~?~&"
          (format-string w)
          (format-args w))
  #f)

(define-method (default-handler (a <abort>))
  (abort a))

(define-method (default-handler (s <format-info>))
  (error (format #f "~?" (format-string s) (format-args s))))

(define-method (default-handler (breakpoint <breakpoint>))
  (let ( (port (current-error-port)) )
    (newline port)
    (report breakpoint port)
    (show-restarts port)
    (invoke-debugger "breakpoint")))

(define-method (return-description (c <condition>)) #f)

(define-method (restart-query (r <restart>)) #f) ;;@@ r ?

(define-method (return-query (c <condition>)) #f) ;;@@ c ?

(define-generic (report (fi <format-info>) #!optional (outport (current-error-port)))
  ;; Note lack of (newline)
  (format outport "~?" (format-string fi) (format-args fi))
)

#|
(define (check-type obj type)
  (if (is-a? obj type)
      #t
      (signal (<type-error> format-string: "Expected type ~s but have ~s"
                            format-args: (list type obj)
                            value: obj
                            type: type)))
)
|#

;==============================================================;
(define %%condition-handlers%% (make-parameter '()))
(define %%restart-handlers%%   (make-parameter '()))
(define %%current-condition%%  (make-parameter  #f))

; Answer 1st restart of indicated class or #f
(define (find-restart restart-class)
  (let loop ( (restarts (%%restart-handlers%%)) )
    (cond
     ((null? restarts) #f)
     ((is-a? (car restarts) restart-class) (car restarts))
     (else
      (loop (cdr restarts))))
) )

(define (invoke-restart restart condition)
  ((restart-function restart) restart condition))

(define (show-restarts #!optional (port (current-error-port)))
  (let ( (condition (%%current-condition%%))
         (restarts  (%%restart-handlers%%))
       )
    (cond
     ((not condition)
      (format port "~%No condition to recover from")
      )
     ((null? restarts)
      (format port "~%No restarts available for ~a" condition)
      )
     (else
      (let loop ( (index 0) (restarts restarts) )
        (when (not (null? restarts))
          (format port "~% (restart ~a) => ~?"
                  index
                  (format-string    (car restarts))
                  (format-args (car restarts)))
          (loop (+ index 1) (cdr restarts))))
      ))
    (newline port)
) )

;; @@FIXME: In Debugger, must use ,(c (restart N))
;;          Should be just (restart N)
(define (restart restart-index)
  (if (not (and (integer? restart-index)
                (>= restart-index 0)))
      (begin
        (WARNING "Bad restart index ~a" restart-index)
        (show-restarts)
        )
      (let* ( (restarts  (%%restart-handlers%%)) )
        (let loop ( (count 0) (restarts restarts) )
          (cond
           ((null? restarts)
            (WARNING "Bad restart index ~a" restart-index)
            (show-restarts)
            )
           ((= count restart-index)
            (invoke-restart (car restarts) (%%current-condition%%))
            )
           (else (loop (+ count 1) (cdr restarts)))))))
)


(define (signal condition #!rest arguments)
  (cond
   ((string? condition)
    (signal (<warning> format-string: condition
                       format-args: arguments))
    )
   ((not (null? arguments))
    (error "SIGNAL: Can only specify args when condition is a string"
           condition arguments)
    )
   ((is-a? condition <condition>)
    ;; typical case..
    (%%current-condition%% condition)
    (let search ( (handlers (%%condition-handlers%%)) )
      (cond
       ((null? handlers) (default-handler condition))
       ((is-a? condition (type (car handlers)))
        ((function (car handlers))
         condition
         (lambda () (search (cdr handlers))))
        )
       (else
        (search (cdr handlers)))
       )
      ))
   (else
    (error "SIGNAL: not a condition" condition))
) )

;==============================================================;
;; NOTE BENE: CaSe MaTtErS

(define restart-return-false
  (<restart> format-string:    "Return #f"
              restart-function: (lambda ignored #f)))
  
(define-method (ERROR-NO-RETURN (s <string>) #!rest args)
  (signal (<error> format-string: s format-args: args))
  (abort "Attempt to return from Non-Recoverable ERROR!!"))

(define-method (ERROR-NO-RETURN (c <condition>) #!rest noise)
  (signal c)
  (abort "Attempt to return from Non-Recoverable ERROR!!"))

(define-method (CERROR (c <condition>) #!rest noise)
  (with-restart restart-return-false
                (signal c)))

(define-method (CERROR (s <string>) #!rest arguments)
  (with-restart restart-return-false
   (signal (<error> format-string: s
                    format-args: arguments)))
)

(define-method (WARNING (s <string>) #!rest args)
  (signal (<warning> format-string: s format-args: args)))

(define (BREAK #!rest whatever)
  (with-restart restart-return-false
     (signal
        (<breakpoint>
         format-string: (if (null? whatever) "~&***BREAK~&" "~&***BREAK ~s~&")
         format-args: whatever)))
)

;==============================================================;


(define (->oops-exception exn)
  ;; map Gambit's native exceptions to <exception>s

  (cond
   ((condition? exn) exn ;; already there
    )
   
   ((abandoned-mutex-exception? exn)
    (<abandoned-mutex-exn> format-string: "MUTEX was abandoned")
    )

   ((sfun-conversion-exception? exn)
    (let ( (proc (sfun-conversion-exception-procedure exn))
           (args (sfun-conversion-exception-arguments exn))
           (err  (sfun-conversion-exception-code exn))
           (msg  (or (sfun-conversion-exception-message exn)
                     (err-code->string
                      (sfun-conversion-exception-code exn))))
         )
      (<foreign-to-scheme-conversion-exn>
       procedure: proc
       arguments: args
       error-code: err
       message:    msg
       format-string: "~a: ~a ~a"
       format-args: (list msg proc args))
      ))

   ((cfun-conversion-exception? exn)
    (let ( (proc (cfun-conversion-exception-procedure exn))
           (args (cfun-conversion-exception-arguments exn))
           (err  (cfun-conversion-exception-code exn))
           (msg  (or (cfun-conversion-exception-message exn)
                     (err-code->string
                      (cfun-conversion-exception-code exn))))
         )
      (<scheme-to-foreign-conversion-exn> 
       procedure: proc
       arguments: args
       error-code: err
       message:    msg
       format-string: "~a: ~a ~a"
       format-args: (list msg proc args))
      ))

   ((datum-parsing-exception? exn)
    (<datum-parsing-exn>
     format-string: "Datum parsing exception: ~a: ~s"
     format-args: (list (datum-parsing-exception-kind exn)
                             (datum-parsing-exception-parameters exn)))
    )
   
   ((deadlock-exception? exn)
    (<deadlock-exn> format-string: "Deadlock detected")
    )
   
   ((divide-by-zero-exception? exn)
    (<divide-by-zero>
     format-string:  "Divide by zero ~a"
     format-args: (divide-by-zero-exception-arguments exn)
     operation:   (divide-by-zero-exception-procedure exn)
     arguments:   (divide-by-zero-exception-arguments exn))
    )

   ((error-exception? exn)
    ;; @@FIXME: Differentiate/handle-special the primitive calls to ERROR???
    (let* ( (args (error-exception-parameters exn))
            (args-len (length args))
            )
      (<error>
       format-string: "~a: ~a"
       format-args:   (cons  (error-exception-message exn)
                             (error-exception-parameters exn)))
      ))

   ((unbound-os-environment-variable-exception? exn)
    (let ( (proc (unbound-os-environment-variable-exception-procedure exn))
           (args (unbound-os-environment-variable-exception-arguments exn))
           )
      (<unbound-os-environment-exn>
       procedure: proc
       arguments: args
       format-string: "Unbound OS environment variable ~a"
       format-args: args)
      ))
          
   ((expression-parsing-exception? exn)
    (<expression-parsing-exn>
     format-string: "Expression parsing exception: ~a: ~s at ~y"
     format-args:
     (list (expression-parsing-exception-kind exn)
           (or (##assq (expression-parsing-exception-kind exn)
                       ##expression-parsing-exception-names)
               (expression-parsing-exception-parameters exn))
           (let* ((source (expression-parsing-exception-source exn))
                  (locat (##source-locat source)))
             (if (##not locat)
                 (##desourcify source)
                 locat))
           )
    ))
   
   ((heap-overflow-exception? exn)
    (<memory-exn> format-string:  "Heap overflow")
    )
   
   ((improper-length-list-exception? exn)
    (<call-error>
     format-string: "List is not of proper length"
     procedure: (improper-length-list-exception-procedure exn)
     arguments: (improper-length-list-exception-arguments exn))
    )
   
   ((join-timeout-exception? exn)
    (<join-timeout-exn>
     procedure: (join-timeout-exception-procedure exn)
     arguments: (join-timeout-exception-arguments exn)
     format-string: "'thread-join!' timed out")
    )
   
   ((keyword-expected-exception? exn)
    (<call-error> format-string: "Keyword argument expected"
                  procedure: (keyword-expected-exception-procedure exn)
                  arguments: (keyword-expected-exception-arguments exn))
    )
   
   ((multiple-c-return-exception? exn)
    (<foreign-repeated-return-exn>
     foreign-language: "C"
     format-string: "Attempt to return to a ~a language function that has already returned"
     format-args: "C")
    )
   
   ((noncontinuable-exception? exn)
    (<abort> format-string: "An ABORTed computation cannot be continued")
    )
   
   ((nonprocedure-operator-exception? exn)
    (let ( (proc (##inverse-eval (nonprocedure-operator-exception-operator exn)))
           (args (nonprocedure-operator-exception-arguments exn))
           )
      (<call-error>
       format-string: "Operator is not a PROCEDURE ~a"
       format-args: (list proc)
       procedure: proc
       arguments: args)
      ))

   ((number-of-arguments-limit-exception? exn)
    (<call-error>
     format-string: "Number of arguments exceeds implementation limit"
     procedure: (number-of-arguments-limit-exception-procedure exn)
     arguments: (number-of-arguments-limit-exception-arguments exn))
    )
   
   ((os-exception? exn)
    (let ( (proc (os-exception-procedure exn))
           (args (os-exception-arguments exn))
           (err  (os-exception-code exn))
           (msg  (or (os-exception-message exn)
                     (err-code->string (os-exception-code exn))))
         )
      (<os-call-exn> procedure: proc
                     arguments: args
                     error-code: err
                     message:    msg
                     format-string: "~a: ~a ~a"
                     format-args: (list msg proc args))
      ))

   ((no-such-file-or-directory-exception? exn)
    (<no-such-file-or-directory-exn>
     procedure: (no-such-file-or-directory-exception-procedure exn)
     arguments: (no-such-file-or-directory-exception-arguments exn)
     format-string: "No such file or directory ~a"
     format-args: (no-such-file-or-directory-exception-arguments exn))
    )

   ((range-exception? exn)
    (<call-error> format-string: "Argument out of range ~s"
                  format-args: (list (cons (##procedure-name
                                            (range-exception-procedure
                                             exn))
                                            (range-exception-arguments
                                             exn)))
                  procedure: (range-exception-procedure exn)
                  arguments: (range-exception-arguments exn))
    )
   
   ((scheduler-exception? exn)
    (let ( (cause (->oops-exception
                   (scheduler-exception-reason exn)))
           )
      (<scheduler-exn>
       format-string: (format #f
                              "Scheduler reported the exception: ~? "
                              (format-string cause)
                              (format-args cause))
       cause: cause)
      ))
           
   ((stack-overflow-exception? exn)
    (<stack-overflow-exn> format-string: "Stack overflow")
    )
   
   ((started-thread-exception? exn)
    (<started-thread-exn>
     procedure: (started-thread-exception-procedure exn)
     arguments: (started-thread-exception-arguments exn)
     format-string: "Thread already started")
    )
   
   ((terminated-thread-exception? exn)
    (<terminated-thread-exn>
     procedure: (terminated-thread-exception-procedure exn)
     arguments: (terminated-thread-exception-arguments exn)
     format-string: "Thread is terminated")
    )
   
   ((type-exception? exn)
    (<type-error>
     format-string: "Instance of ~a expected as argument ~a of ~a"
     format-args:
     (let ( (type-id (type-exception-type-id exn)) )
       (list
        (if (##type? type-id)
            type-id
            (let ( (exn-name
                    (assq type-id ##type-exception-names))
                 )
              (if exn-name (cdr exn-name) "Unknown type")))
        (type-exception-arg-num exn)
        (##make-call-form (type-exception-procedure exn)
                          (type-exception-arguments exn)
                          15) ;; max #args
       ) )
       procedure: (type-exception-procedure exn)
       arguments: (type-exception-arguments exn)
       value: (list-ref (type-exception-arguments exn)
                        (- (type-exception-arg-num exn) 1))
       type: (type-exception-type-id exn))
     )
   
   ((unbound-global-exception? exn)
    (<unbound-global-error>
     format-string: "Unbound variable ~a"
     format-args: (list (unbound-global-exception-variable exn))
     name: (unbound-global-exception-variable exn))
    )

   ((uncaught-exception? exn)
    (let ( (cause (->oops-exception
                   (uncaught-exception-reason exn)))
           )
      (<uncaught-exception>
       procedure: (uncaught-exception-procedure exn)
       arguments: (uncaught-exception-arguments exn)
       format-string: (format #f
                              "Uncaught exception: ~? "
                              (format-string cause)
                              (format-args cause))
       cause: cause)
      ))

   ((unknown-keyword-argument-exception? exn)
    (<call-error>
     format-string: "Unknown keyword argument passed to procedure ~a"
     format-args: (list (##procedure-name
                         (unknown-keyword-argument-exception-procedure
                          exn)))
     procedure: (unknown-keyword-argument-exception-procedure exn)
     arguments: (unknown-keyword-argument-exception-arguments exn))
    )
   
   ((wrong-number-of-arguments-exception? exn)
    (<call-error>
     format-string: "Wrong number of arguments passed to procedure ~a"
     format-args: (list (##procedure-name
                         (wrong-number-of-arguments-exception-procedure
                          exn)))
     procedure: (wrong-number-of-arguments-exception-procedure exn)
     arguments: (wrong-number-of-arguments-exception-arguments exn))
    )

   ((invalid-hash-number-exception? exn)
    (<invalid-hash-number>
     format-string: "Invalid hash number"
     procedure: (invalid-hash-number-exception-procedure exn)
     arguments: (invalid-hash-number-exception-arguments exn))
    )
    
   ((unbound-table-key-exception? exn)
    (<unbound-table-key>
     format-string: "Unbound table key"
     procedure: (unbound-table-key-exception-procedure exn)
     arguments: (unbound-table-key-exception-arguments exn))
    )
    
   ((unbound-serial-number-exception? exn)
    (<unbound-serial-number>
     format-string: "Unbound serial number"
     procedure: (unbound-serial-number-exception-procedure exn)
     arguments: (unbound-serial-number-exception-arguments exn))
    )
    
   
   (else
    (<unknown-error> format-string: "Unknown exception ~a "
                     format-args: exn))
 ) )



;==============================================================;

(when debug-oops
      (format #t "~%Oops: loaded CONDITION/HANDLER/RESTART code"))

;===========================E=O=F==============================;

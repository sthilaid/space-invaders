;;;
(include "oops-macros.scm")

(define-class <file-not-found> <exception>
  ((file-name type: <string>)
   (return-allowed? allocation: each-subclass: init-value: #t)) ;; override
)

(define-class <try-a-different-file> <restart>
  ((file-name type: <string>))
)

(define (test-me)
  (with-handler
   (<handler>
    type: <file-not-found>
    function: (lambda (condition next-handler)
                (let ( (restart (find-restart <try-a-different-file>)) )
                  (if restart
                      (invoke-restart restart condition)
                      (next-handler))))
    )
   (open "file-that-doesnt-exist")
) )

(define-method (open the-file)
  (with-restart
   
   (<try-a-different-file>
    format-string: "Open a different file instead of \"~A\""
    format-args: (list the-file)
    file-name: "my-emergency-backup-file"
    restart-function: (lambda (self cdn) (open (file-name self)))
    )

   (guts-of-open the-file)
) )


(define-method (guts-of-open (the-file <string>))
  (if (string=? the-file "file-that-doesnt-exist")
    (begin
     (format #t "~%failed to open ~s~%" the-file)
     (signal (<file-not-found> file-name: the-file)))
    (format #t "~%opened ~s~%" the-file)))


(define-method (print (self <file-not-found>) #!key (port #t))
  (format port "The file ~A was not found" (file-name self)))


(define-method (restart-query (restart <try-a-different-file>))
  (format #t "I'm pretending to ask you for a file name...~%")
  (file-name-set! restart "new-file-to-use"))

(define (test-me-harder)
  (with-handler
   (<handler>
     type: <type-error>
     function: (lambda (condition next-handler)
                (with-restarts
                 ( (<retry-apply>
                    format-string: "Retry applying ~a to ~s"
                    format-args: (list (##procedure-name (procedure condition))
                                       (arguments condition))
                    restart-function: (lambda (self condition)
                                        (apply (procedure condition)
                                               (arguments condition)))
                    )
                   (<return-value-in-place-of>
                    format-string: "Return a value in place of ~s"
                    format-args: (list (##make-call-form (procedure condition)
                                                         (arguments condition)
                                                         15))
                    restart-function: (lambda (self condition)
                                        (format (current-output-port)
                                                "~&Value to return: ")
                                        (read (current-input-port)))
                    )
                 )
                (let ( (port (current-error-port)) )
                  (format port "~&~?~&"
                          (format-string condition)
                          (format-args condition))
                  (show-restarts port)
                  (invoke-debugger "call error")
                  )))
    )
   (let ( (bogus (lambda () (cons 3 4))) )
     (with-oops-handler
      (* 3 (+ 5 (bogus)))))
) )


;===========================E=O=F==============================;

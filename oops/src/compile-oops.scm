#|
OpenBSD:
  limit stacksize 32m
  limit datasize 512m
|#

(include "oops-files.scm")

(for-each
 (lambda (file)
   (let ( (file-name (string-append file ".scm")) )
     (newline)
     (display "COMPILING: ") (display file-name)
     (newline)
     (compile-file file-name '(report debug)) ;; (report)
     (newline)
     (display "LOADING: ") (display file)
     (newline)
     (load file)))
 %oops-files)

'done

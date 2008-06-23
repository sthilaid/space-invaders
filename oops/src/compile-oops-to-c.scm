(include "oops-files.scm")

(for-each
 (lambda (file)
   (let ( (file-name (string-append file ".scm")) )
     (newline)
     (display "COMPILING: ") (display file-name)
     (newline)
     (compile-file-to-c file-name '(report debug)) 
     (newline)
     (display "LOADING: ") (display file)
     (newline)
     (load file)))
 %oops-files)

'done

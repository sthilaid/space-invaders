;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: scm-lib-macro.scm
;;
;; description: Small utils macro library
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-macro (for var init-val condition true . false)
  (let ((loop (gensym 'loop)))
    `(let ,loop ((,var ,init-val))
          (if ,condition
              (begin ,true (,loop (+ ,var 1)))
              ,(if (not (null? false))
                   false)))))

(define-macro (while cond action . actions)
  (define loop (gensym 'loop))
  (define condvar (gensym 'condvar))
  `(let ,loop ((,condvar ,cond))
        (if ,condvar
            (begin ,action ,@actions (,loop ,cond)))))

(define-macro (enum x . xs)
  `(begin
     ,@(let loop ((i 0) (xs (cons x xs)) (acc '()))
         (if (not (pair? xs))
             (reverse acc)
             (loop (+ i 1) (cdr xs) (cons `(define ,(car xs) ,i) acc))))))

(define-macro (to-string e1 . es)
  `(with-output-to-string "" (lambda () ,e1 ,@es)))

(define-macro (when test . body)
  `(if ,test (begin ,@body)))

(define-macro (unless test . body)
  `(if ,test #!void (begin ,@body)))

(define-macro (xor arg1 arg2)
  (let ((v1 (gensym 'v1))
        (v2 (gensym 'v2)))
   `(let ((,v1 ,arg1)
          (,v2 ,arg2))
      (or (and ,v1 ,v2)
          (and (not ,v1) (not ,v2))))))

(define-macro (currify f arrity)
  (define (fold-l f acc list)
    (if (not (pair? list))
        acc
        (fold-l f (f acc (car list)) (cdr list))))
  (define (create-list number thunk)
    (let loop ((n number) (acc '()))
      (if (> n 0)
          (loop (- n 1) (cons (thunk) acc))
          acc)))
  (let ((args (create-list arrity gensym)))
    (fold-l (lambda (acc arg) `(lambda (,arg) ,acc))
            `(,f ,@args)
            args)))


(define-macro (cast-pointer in-type out-type value)
  `((c-lambda (,in-type) ,out-type
              "___result_voidstar = ___arg1;\n")
    ,value))


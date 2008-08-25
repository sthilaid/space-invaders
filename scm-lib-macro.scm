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

(define-macro (to-string e1 . es)
  `(with-output-to-string "" (lambda () ,e1 ,@es)))

(define-macro (when test . body)
  `(if ,test (begin ,@body)))

(define-macro (unless test . body)
  `(if ,test #!void (begin ,@body)))


(define-macro (cast-pointer in-type out-type value)
  `((c-lambda (,in-type) ,out-type
              "___result_voidstar = ___arg1;\n")
    ,value))

;; (define-macro (make-vector2d height width . init-val)
;;   (let ((vector-sym (gensym 'vector-sym))
;;         (row-sym (gensym 'row-sym)))
;;     `(let ((,vector-sym (make-vector ,height #f)))
;;        (for ,row-sym 0 (< ,row-sym ,height)
;;             (vector-set! ,vector-sym ,row-sym
;;                          (make-vector ,width ,(if (pair? init-val)
;;                                                   (car init-val)
;;                                                   #f))))
;;        ,vector-sym)))

;; (define-macro (vector2d-ref vector row col)
;;   `(vector-ref (vector-ref ,vector ,row) ,col))

;; (define-macro (vector2d-set! vector row col val)
;;   `(vector-set! (vector-ref ,vector ,row) ,col ,val))

;; (define-macro (make-vector3d i-length j-length k-length . init-val)
;;   (let ((vector-sym (gensym 'vector-sym))
;;         (i-sym (gensym 'i-sym))
;;         (j-sym (gensym 'j-sym)))
;;     `(let ((,vector-sym (make-vector2d ,i-length ,j-length #f)))
;;        (for ,i-sym 0 (< ,i-sym ,i-length)
;;             (for ,j-sym 0 (< ,j-sym ,j-length)
;;                  (vector2d-set! ,vector-sym ,i-sym ,j-sym
;;                                 (make-vector ,k-length ,(if (pair? init-val)
;;                                                             (car init-val)
;;                                                             #f)))))
;;        ,vector-sym)))

;; (define-macro (vector3d-ref vector i j k)
;;   `(vector-ref (vector2d-ref ,vector ,i ,j) ,k))

;; (define-macro (vector3d-set! vector i j k val)
;;   `(vector-set! (vector2d-ref ,vector ,i ,j) ,k ,val))

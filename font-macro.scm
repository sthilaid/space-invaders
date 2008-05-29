(c-declare "#include <GL/gl.h>\n")

(define-macro (runtime-font-generation-code font-name char-width char-height
                                            good-char-width good-char-height)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))

  (let ((char-table  (gensym 'char-table))
        (char (gensym 'char))
        (update-fun
         `(c-lambda
           (int
            int int
            unsigned-int unsigned-int unsigned-int unsigned-int) void
            ,(with-output-to-string
               ""
               (lambda ()
                 (show font-name
                       "[___arg1][___arg2][___arg3][0] = ___arg4;\n")
                 (show font-name
                       "[___arg1][___arg2][___arg3][1] = ___arg5;\n")
                 (show font-name
                       "[___arg1][___arg2][___arg3][2] = ___arg6;\n")
                 (show font-name
                       "[___arg1][___arg2][___arg3][3] = ___arg7;\n"))))))
    `(let ((,char-table (get-font-table ,font-name ,char-width ,char-height)))
       (for-each
        (lambda (,char) (load-font-char ,font-name ,char ,update-fun
                                        ,char-width ,char-height
                                        ,good-char-width ,good-char-height))
        (table->list ,char-table)))))

(define-macro (define-symmetric-font font-name
                char-width char-height . options)
  (define (sort-chars char-list)
    (quick-sort
     (lambda (a b)
       (< (get-char-index font-name (caar a) (cadar a))
          (get-char-index font-name (caar b) (cadar b))))
     (lambda (a b)
       (= (get-char-index font-name (caar a) (cadar a))
          (get-char-index font-name (caar b) (cadar b))))
     (lambda (a b)
       (> (get-char-index font-name (caar a) (cadar a))
          (get-char-index font-name (caar b) (cadar b))))
     char-list))
  
  (include "font.scm")
  (include "scm-lib-macro.scm")
  (include "ppm-reader.scm")
  (include "scm-lib.scm")

  (init-char-indexes! font-name)
  (let* ((good-char-width (next-power-of-2 char-width))
         (good-char-height (next-power-of-2 char-height))
         (font-char-table (get-font-table font-name char-width char-height))
         (static-font-elements-declarations
          `(c-declare
            ,(to-string
              (show "GLubyte "font-name
                    "["(table-length font-char-table)"]"
                    "["good-char-height"]"
                    "["good-char-width"]"
                    "[4] = {")
              (let ((n 0)
                    (last-n (- (table-length font-char-table) 1)))
                (for-each
                 (lambda (el)
                   (let ((color (caar el))
                         (char (cadar el))
                         (pixels (cdr el)))
                     (show "{")
                     (for y 0 (< y good-char-height)
                       (begin
                         (for x 0 (< x good-char-width)
                           (let* ((out-of-bounds?
                                   (or (>= x char-width) (>= y char-height)))
                                  (current-pix
                                   (if (not out-of-bounds?)
                                       (list-ref pixels (+ (* y char-width) x))
                                       '()))
                                  (r (if out-of-bounds? 0 (car current-pix)))
                                  (g (if out-of-bounds? 0 (cadr current-pix)))
                                  (b (if out-of-bounds? 0 (caddr current-pix)))
                                  (a (if out-of-bounds?
                                         0
                                         (if (< (+ r g b) 10) 0 255))))
                             (show r "," g "," b "," a)
                             (if (< x (- good-char-width 1)) (show ","))))
                         (if (= y (- good-char-height 1))
                             (show "}")
                             (show ",")))))
                   (if (< n last-n)
                       (begin (set! n (+ n 1)) (show ","))))
                 (sort-chars (table->list font-char-table))))
              (show "};\n"))))
         (static-font-elements-generation
          `((lambda () (init-char-indexes! ,font-name))))
        
        (dynamic-font-elements-declarations
         `(c-declare ,(to-string (show "GLubyte "font-name
                                       "["(table-length font-char-table)"]"
                                       "["good-char-height"]"
                                       "["good-char-width"]"
                                       "[4]\n;"))))
        (dynamic-font-elements-generation
         `((lambda ()
             (runtime-font-generation-code
              ,font-name ,char-width ,char-height
              ,good-char-width ,good-char-height))))
        (texture-declaration-code ";\n")
        (texture-generation-code "1+1;\n")
        (texture-init-script
         (let ((tex-id-sym (gensym 'tex-id)))
          `(lambda (,tex-id-sym)
             (lambda ()
               (glBindTexture GL_TEXTURE_2D ,tex-id-sym)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
               ))))
        (get-pointer-code
         `(c-lambda (int) (pointer void)
           ,(with-output-to-string ""
              (lambda ()
                (show "___result_voidstar = "font-name"[___arg1];\n"))))))
          
    `(begin
       ,@(if (memq 'static options)
             (list
              static-font-elements-declarations
              static-font-elements-generation)
             (list
              dynamic-font-elements-declarations
              dynamic-font-elements-generation))
       (define-texture ,texture-declaration-code ,texture-generation-code
         ,texture-init-script ,font-name ,good-char-width ,good-char-height) 
       (add-new-font!
        ,font-name
        (make-font ,font-name
                   (texture-id (retrieve-texture ,font-name))
                   ,good-char-width ,good-char-height
                   ,get-pointer-code)))))

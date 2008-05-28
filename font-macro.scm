(c-declare "#include <GL/gl.h>\n")

(define-macro (define-symmetric-font font-name char-width char-height)
  (include "font.scm")
  (include "scm-lib-macro.scm")
  (include "ppm-reader.scm")
  (include "scm-lib.scm")
          
  (let* ((good-char-width (next-power-of-2 char-width))
         (good-char-height (next-power-of-2 char-height))
         (font-char-table (get-font-table font-name char-width char-height))
         (font-table->list (table->list font-char-table))
         (font-elements-declarations
          (with-output-to-string
            ""
            (lambda ()
              (for-each
               (lambda (el)
                 (let ((color (caar el))
                       (char (cadar el))
                       (pixels (cdr el)))
                   (show "GLubyte "(get-char-ptr-name font-name color char)
                         "["good-char-height"]["good-char-width"][4] = {")
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
                           (show "};\n")
                           (show ","))))))
               font-table->list))))
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
        (pointer-assq-list
         (map (lambda (el)
                (let* ((key (car el))
                       (color (car key))
                       (char (cadr key)))
                  `(cons ,(list 'quote key)
                         ((c-lambda
                           () (pointer void)
                           ,(string-append "___result_voidstar = (void*)"
                                           (get-char-ptr-name font-name
                                                              color char)
                                          ";\n"))))))
              font-table->list)))
    `(begin
       (c-declare ,font-elements-declarations)
       (define-texture ,texture-declaration-code ,texture-generation-code
         ,texture-init-script ,font-name ,good-char-width ,good-char-height) 
       (add-new-font!
        ,font-name
        (make-font ,font-name
                   (texture-id (retrieve-texture ,font-name))
                   ,good-char-width ,good-char-height
                   (list->table ,(cons 'list pointer-assq-list)))))))

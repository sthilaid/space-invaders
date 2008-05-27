(c-declare "#include <GL/gl.h>\n")

;; (define-macro (generate-update-function font-name char-width char-height)
;;   (define (show . args)
;;     (for-each (lambda (x) (if (string? x) (display x) (write x))) args))

;;   (let ((update-pix-sym (gensym 'update-pix-sym))
;;         (char-font-sym (gensym 'char-font-sym))
;;         (texture-id-sym (gensym 'texture-id-sym))
;;         (current-pixel-sym (gensym 'current-pixel-sym)))
;;     `(let ((,update-pix-sym
;;             (c-lambda
;;              (int int unsigned-int unsigned-int unsigned-int unsigned-int)
;;              void
;;              ,(with-output-to-string
;;                 ""
;;                 (lambda ()
;;                   (show font-name "[___arg2][___arg1][0] = ___arg3;\n")
;;                   (show font-name "[___arg2][___arg1][1] = ___arg4;\n")
;;                   (show font-name "[___arg2][___arg1][2] = ___arg5;\n")
;;                   (show font-name "[___arg2][___arg1][3] = ___arg6;\n"))))))
;;        (lambda (,char-font-sym ,texture-id-sym)
;;          (for y 0 (< y ,char-height)
;;            (for x 0 (< x ,char-width)
;;              (let* ((,current-pixel-sym
;;                      (list-ref ,char-font-sym (+ (* y ,char-width) x)))
;;                     (r (car ,current-pixel-sym))
;;                     (g (cadr ,current-pixel-sym))
;;                     (b (caddr ,current-pixel-sym))
;;                     (a (if (< (+ r g b) 10) 0 255)))
;;                (,update-pix-sym x y r g b a))))
  
;;          (glBindTexture GL_TEXTURE_2D ,texture-id-sym)
;;          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
;;          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
;;          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
;;          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
;;          (create-texture ,font-name ,char-width ,char-height)))))


(define-macro (define-symmetric-font font-name char-width char-height)
  (include "font.scm")
  (include "scm-lib-macro.scm")
  (include "ppm-reader.scm")
  (include "scm-lib.scm")
          
  (let* ((genid (let ((id 0))
                  (lambda () (set! id (+ id 1)) id)))
         (get-char-ptr-name
          (let ((id-table (make-table)))
            (lambda (font-name color char)
              (let ((id (table-ref id-table (list color char) #f)))
                (if (not id)
                    (let ((new-id (genid)))
                      (table-set! id-table (list color char) new-id)
                      (set! id new-id)))
                (with-output-to-string
                  ""
                  (lambda ()
                    (show font-name "_" id)))))))
         (font-char-table (get-font-table font-name char-width char-height))
         (font-table->list (table->list font-char-table))
         (font-elements-declarations
          (with-output-to-string
            ""
            (lambda ()
              (for-each
               (lambda (el)
                 (let ((color (caar el))
                       (char (cadar el)))
                   (show "GLubyte "(get-char-ptr-name font-name color char)
                         "["char-height"]["char-width"][4];\n")))
               font-table->list))))
         (font-elements-generations
          (with-output-to-string
            ""
            (lambda ()
              (for-each
               (lambda (el)
                 (let ((color (caar el))
                       (char (cadar el))
                       (pixels (cdr el)))
                   (for y 0 (< y char-height)
                     (for x 0 (< x char-width)
                       (let* ((current-pix
                               (list-ref pixels (+ (* y char-width) x)))
                              (r (car current-pix))
                              (g (cadr current-pix))
                              (b (caddr current-pix))
                              (a (if (< (+ r g b) 10) 0 255)))
                         (show (get-char-ptr-name font-name color char)
                               "["y"]["x"][0] = "r";\n")
                         (show (get-char-ptr-name font-name color char)
                               "["y"]["x"][1] = "g";\n")
                         (show (get-char-ptr-name font-name color char)
                               "["y"]["x"][2] = "b";\n")
                         (show (get-char-ptr-name font-name color char)
                               "["y"]["x"][3] = "a";\n"))))))
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
        (font-update-table-list
         (map
          (lambda (el)
            (let ((color (caar el))
                  (char (cadar el))
                  (tex-id-sym (gensym 'tex-id)))
              `(cons ,(list 'quote (list color char))
                     (lambda ()
                       (let ((,tex-id-sym
                              (texture-id (retrieve-texture ,font-name))))
                         (glBindTexture GL_TEXTURE_2D ,tex-id-sym)
                         (attach-texture
                          ,(get-char-ptr-name font-name color char)
                          ,char-width
                          ,char-height))))))
          font-table->list)))
    `(begin
       (c-declare ,font-elements-declarations)
       ((c-lambda () void ,font-elements-generations))
       (define-texture ,texture-declaration-code ,texture-generation-code
         ,texture-init-script ,font-name ,char-width ,char-height) 
       (add-new-font!
        ,font-name
        (make-font ,font-name
                   (texture-id (retrieve-texture ,font-name))
                   ,char-width ,char-height
                   (list->table ,(cons 'list font-update-table-list)))))))

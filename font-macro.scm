(c-declare "#include <GL/gl.h>\n")

(define-macro (generate-update-function font-name char-width char-height)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))

  (let ((update-pix-sym (gensym 'update-pix-sym))
        (char-font-sym (gensym 'char-font-sym))
        (texture-id-sym (gensym 'texture-id-sym))
        (current-pixel-sym (gensym 'current-pixel-sym)))
    `(let ((,update-pix-sym
            (c-lambda
             (int int unsigned-int unsigned-int unsigned-int unsigned-int)
             void
             ,(with-output-to-string
                ""
                (lambda ()
                  (show font-name "[___arg2][___arg1][0] = ___arg3;\n")
                  (show font-name "[___arg2][___arg1][1] = ___arg4;\n")
                  (show font-name "[___arg2][___arg1][2] = ___arg5;\n")
                  (show font-name "[___arg2][___arg1][3] = ___arg6;\n"))))))
       (lambda (,char-font-sym ,texture-id-sym)
         (for y 0 (< y ,char-height)
           (for x 0 (< x ,char-width)
             (let* ((,current-pixel-sym
                     (list-ref ,char-font-sym (+ (* y ,char-width) x)))
                    (r (car ,current-pixel-sym))
                    (g (cadr ,current-pixel-sym))
                    (b (caddr ,current-pixel-sym))
                    (a (if (< (+ r g b) 10) 0 255)))
               (,update-pix-sym x y r g b a))))
  
         (glBindTexture GL_TEXTURE_2D ,texture-id-sym)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
         (create-texture ,font-name ,char-width ,char-height)))))


(define-macro (define-symmetric-font font-name char-width char-height)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))

  (let ((declaration-code 
         (with-output-to-string
           ""
           (lambda ()
             (show "GLubyte internal_font_image["char-height"]["
                   char-width"][4];\n"))))
        ;; dummy texture generation code used. Will be set later,
        ;; before being drawn.
        (generation-code "1+1;")
        (update-fun 'todo))
    `(begin
       (define-texture ,declaration-code ,generation-code
         "internal_font_image" ,char-width ,char-height)
       (define current-font
         (make-font ,font-name
                    (texture-id
                     (table-ref texture-table "internal_font_image"))
                    ,char-width ,char-height
                    (get-font-table ,font-name ,char-width ,char-height)
                    (generate-update-function "internal_font_image"
                                              ,char-width
                                              ,char-height))))))

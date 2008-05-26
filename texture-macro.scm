(define-macro (create-texture image-pointer-name width height)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  
  `((c-lambda () void
     ,(with-output-to-string ""
        (lambda ()
          (show "glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, " width ", "
                height ", 0, GL_RGBA, GL_UNSIGNED_BYTE, "
                image-pointer-name ");\n"))))))

(define-macro (define-texture declaration-code generation-code
                tex-pointer width height)
;;   (include "ppm-reader.scm")
;;   (include "scm-lib.scm")

  (let* ((tex-id (gensym 'tex-id))
         ;; this init script code must be embeded in an environment
         ;; that will bind the ,tex-id variable.
         (init-script
          `(begin
             (glBindTexture GL_TEXTURE_2D ,tex-id)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
             (create-texture ,tex-pointer ,width ,height))))
    `(begin
       (c-declare ,declaration-code)
       ((c-lambda () void ,generation-code))
       (let ((,tex-id (genTexture)))
         (table-set! texture-table
                     ,tex-pointer
                     (make-texture ,tex-id ,width ,height
                                   (lambda () ,init-script)))))))

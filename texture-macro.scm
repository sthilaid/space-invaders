(define-macro (attach-texture image-pointer-name width height)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  
  `((c-lambda () void
     ,(with-output-to-string ""
        (lambda ()
          (show "glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, " width ", "
                height ", 0, GL_RGBA, GL_UNSIGNED_BYTE, "
                image-pointer-name ");\n"))))))

(define-macro (define-texture
                declaration-code generation-code
                init-script
                tex-pointer width height)
  (let* ((tex-id (gensym 'tex-id)))
    `(begin
       (c-declare ,declaration-code)
       ((c-lambda () void ,generation-code))
       (let ((,tex-id (genTexture)))
         (table-set! texture-table
                     ,tex-pointer
                     (make-texture ,tex-id ,width ,height
                                   (,init-script ,tex-id)))))))

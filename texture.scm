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
  (include "ppm-reader.scm")
  (include "scm-lib.scm")

  (let* ((tex-id (gensym 'tex-id))
         ;; this init script code must be embeded in an environment
         ;; that will bind the ,tex-id variable.
         (init-script
          `(begin
             (pp (list 'generating 'texture ,tex-id))
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

(define-type texture id width height init-script)
(define texture-table (make-table))

(define genTexture (let ((i 5))
                     (lambda () (set! i (+ i 1)) i)))

(define (initialize-textures!)
  (for-each (lambda (t)
              (pp `(generating texture ,(car t)))
              ((texture-init-script (cdr t))))
            (table->list texture-table)))

(define (draw-texture name x y)
  (let* ((texture-obj (table-ref texture-table name))
         (tex-id (texture-id texture-obj))
         (width (texture-width texture-obj))
         (height (texture-height texture-obj)))
    (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
    (glColor4f 0. 0. 0. 1.)
    (glEnable GL_TEXTURE_2D)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_ADD)
    (glBindTexture GL_TEXTURE_2D tex-id)
    (glBegin GL_QUADS)
    (begin
      (glTexCoord2f 0.0 0.0) (glVertex2i x y)
      (glTexCoord2f 0.0 1.0) (glVertex2i x (+ y height))
      (glTexCoord2f 1.0 1.0) (glVertex2i (+ x width) (+ y height))
      (glTexCoord2f 1.0 0.0) (glVertex2i (+ x width) y))
    (glEnd)
    (glDisable GL_TEXTURE_2D)
    ;;(glBlendFunc GL_DST_COLOR GL_DST_ALPHA)
;;     (glBlendFunc GL_ZERO GL_SRC_COLOR)
;;     (glColor4f 0. 0. 1. 1.)
;;     (glBegin GL_QUADS)
;;     (begin
;;       (glVertex2i x y)
;;       (glVertex2i x (+ y height))
;;       (glVertex2i (+ x width) (+ y height))
;;       (glVertex2i (+ x width) y))
;;     (glEnd)
))
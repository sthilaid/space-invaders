(define-type texture id width height init-script)

(define texture-table (make-table))

(define genTexture (let ((i 5))
                     (lambda () (set! i (+ i 1)) i)))

(define (initialize-textures!)
  (for-each (lambda (t)
              (pp `(initializing texture ,(car t)))
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
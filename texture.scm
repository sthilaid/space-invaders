;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: texture.scm
;;
;; description: This file contains runtime functions related to the
;; texture abastraction defined in texture-macro.scmtexture-macro.scm.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type texture id width height init-script)

;; Global texture table that contains every registred texture data.
(define texture-table (make-table))


;; Returns the texture data type corresponding to the given string
;; name. If no such texture is found, a unknown-texture-name exception
;; will be raised.
(define (retrieve-texture texture-name)
  (let ((tex (table-ref texture-table texture-name #f)))
    (if (not tex)
        (raise 'unknown-texture-name)
        tex)))

;; A gensym equivalent that generates fresh integer texture id or in
;; opengl terms, a fresh texture name.
(define genTexture (let ((i 5))
                     (lambda () (set! i (+ i 1)) i)))

;; This function will call the init-script of all the registred
;; textures (registred with (define-texture ...).
(define (initialize-textures!)
  (for-each (lambda (t)
              ((texture-init-script (cdr t))))
            (table->list texture-table)))

;; Function which will dray the texture of given string name to the 2d
;; (x,y) plane coordinate.
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
    (glDisable GL_TEXTURE_2D)))
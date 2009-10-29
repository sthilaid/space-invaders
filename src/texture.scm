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

(define-type texture id width height)

;; A gensym equivalent that generates fresh integer texture id or in
;; opengl terms, a fresh texture name.
(define genTexture (let ((i 5))
                     (lambda () (set! i (+ i 1)) i)))

(define debug-textures '())

;; Function which will dray the texture of given string name to the 2d
;; (x,y) plane coordinate.
(define (draw-texture texture-obj x y #!key (width #f) (height #f))
  (let* ((tex-id      (texture-id texture-obj))
         (tex-width   (texture-width texture-obj))  
         (tex-height  (texture-height texture-obj))
         (width       (if width  width  tex-width))
         (height      (if height height tex-height))
         (tex-x-max   (exact->inexact (/ width  tex-width)))
         (tex-y-max   (exact->inexact (/ height tex-height))))
    (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
    (glColor4f 0. 0. 0. 1.)
    (glEnable GL_TEXTURE_2D)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_ADD)
    (glBindTexture GL_TEXTURE_2D tex-id)
    (glBegin GL_QUADS)
    (begin
      (glTexCoord2f 0.0       0.0)       (glVertex2i x           y)
      (glTexCoord2f 0.0       tex-y-max) (glVertex2i x           (+ y height))
      (glTexCoord2f tex-x-max tex-y-max) (glVertex2i (+ x width) (+ y height))
      (glTexCoord2f tex-x-max 0.0)       (glVertex2i (+ x width) y))
    (glEnd)
    (glDisable GL_TEXTURE_2D)))
(c-declare "#include <GL/gl.h>\n")

(define-type texture id width height init-script)
(define texture-table (make-table))

(define genTexture (let ((i 5))
                     (lambda () (set! i (+ i 1)) i)))

(define-macro (create-texture image-name width height)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  
  `((c-lambda () void
     ,(with-output-to-string ""
        (lambda ()
          (show "glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, " width ", "
                height ", 0, GL_RGBA, GL_UNSIGNED_BYTE, "
                image-name "_image);\n"))))))

(define-macro (define-sprite filename)
  (include "ppm-reader.scm")
  (include "scm-lib.scm")

  (let* ((image-name (path-strip-directory
                      (path-strip-extension filename)))
         (ppm-data (parse-ppm-image-file filename))
         (width (ppm-image-width ppm-data))
         (height (ppm-image-height ppm-data))
         (width-power-of-2 (next-power-of-2 width))
         (height-power-of-2 (next-power-of-2 height))
         (c-declare-code
          (with-output-to-string ""
            (lambda ()
              (show "GLubyte "
                    image-name "_image[" height-power-of-2 "]["
                    width-power-of-2 "][4];\n"))))
         (image-generation-code
          (with-output-to-string ""
            (lambda ()
              (define pixels (ppm-image-pixels ppm-data))
              ;; Fill up the buffer such that filling areas outside
              ;; the original sprite contains 0 alpha color
              (for y 0 (< y height-power-of-2)
                (for x 0 (< x width-power-of-2)
                     (let* ((out-of-bound? (or (>= x width) (>= y height)))
                            (pixel (if (not out-of-bound?)
                                       (list-ref pixels (+ (* y width) x))
                                       '()))
                            (r (if out-of-bound? 0 (car pixel)))
                            (g (if out-of-bound? 0 (cadr pixel)))
                            (b (if out-of-bound? 0 (caddr pixel)))
                            ;; if sum of colors is lower than 20, the
                            ;; color is assumed here to be transparent
                            (alpha (if out-of-bound?
                                       0
                                       (if (< (+ r g b) 20) 0 255))))
                       (show image-name "_image["y"]["x"][0] = "r";\n"
                             image-name "_image["y"]["x"][1] = "g";\n"
                             image-name "_image["y"]["x"][2] = "b";\n"
                             image-name "_image["y"]["x"][3] = "
                             alpha";\n")))))))
         (get-pointer
          (lambda (name)
            `((c-lambda () GLubyte* ,(string-append "___result_voidstar = "
                                                    name
                                                    ";\n")))))
         (tex-id (gensym 'tex-id))
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
             (create-texture ,image-name
                             ,width-power-of-2 ,height-power-of-2))))
    `(begin
      (c-declare ,c-declare-code)
      ((c-lambda () void ,image-generation-code))
      (let ((,tex-id (genTexture)))
        (table-set! texture-table
                    ,image-name
                    (make-texture ,tex-id ,width-power-of-2 ,height-power-of-2
                                  (lambda () ,init-script)))))))

(define-macro (test-image-code filename)
  (define-macro (for var init-val condition true . false)
    (let ((loop (gensym 'loop)))
      `(let ,loop ((,var ,init-val))
            (if ,condition
                (begin ,true (,loop (+ ,var 1)))
                ,(if (not (null? false))
                     false)))))
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))

  (define image-name (path-strip-directory
                      (path-strip-extension filename)))
  `(let* ((tex (table-ref texture-table ,image-name))
          (width (texture-width tex))
          (height (texture-height tex)))
     (for y 0 (< y height)
       (for x 0 (< x width)
         ((c-lambda (int int) void
            ,(with-output-to-string ""
               (lambda ()
                 (show
                  "printf(\""
                  image-name "_image[%d][%d] = {%d,%d,%d}\\n\","
                  "___arg2, ___arg1,"
                  image-name "_image[___arg2][___arg1][0],"
                  image-name "_image[___arg2][___arg1][1],"
                  image-name "_image[___arg2][___arg1][2]);"))))
          x y)))))

(define (initialize-textures!)
  (for-each (lambda (t)
              (pp `(generating texture ,(car t)))
              ((texture-init-script (cdr t))))
            (table->list texture-table)))

(define (draw-sprite name x y)
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

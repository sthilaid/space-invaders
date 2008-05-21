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
          (show "glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, " width ", "
                height ", 0, GL_RGB, GL_UNSIGNED_BYTE, "
                image-name "_image);\n"))))))

;; (define-macro (scaleImage image-name wIn hIn wOut hOut)
;;   (define (show . args)
;;     (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  
;;   `((c-lambda () void
;;     ,(with-output-to-string ""
;;       (lambda ()
;;         (show "gluScaleImage(GL_RGB, "
;;               wIn", "hIn", GL_UNSIGNED_BYTE, "
;;               image-name "_source_image,"
;;               wOut", "hOut", GL_UNSIGNED_BYTE, "
;;               image-name "_image);\n"))))))
                            


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
;;               (show "GLubyte "
;;                     image-name "_source_image[" height "][" width "][3];\n")
              (show "GLubyte "
                    image-name "_image[" height-power-of-2 "]["
                    width-power-of-2 "][3];\n"))))
         (image-generation-code
          (with-output-to-string ""
            (lambda ()
              (for y 0 (< y height)
                (for x 0 (< x width)
                     (let ((pixel (list-ref (ppm-image-pixels ppm-data)
                                            (+ (* y width) x))))
                       (show image-name "_image["y"]["x"][0] = "
                             (car pixel)";\n"
                             image-name "_image["y"]["x"][1] = "
                             (cadr pixel)";\n"
                             image-name "_image["y"]["x"][2] = "
                             (caddr pixel)";\n")))))))
         (get-pointer
          (lambda (name)
            `((c-lambda () GLubyte* ,(string-append "___result_voidstar = "
                                                    name
                                                    ";\n")))))
;;          (image-scaling-code
;;           (with-output-to-string ""
;;             (lambda ()
;;               (show "gluScaleImage(GL_RGB, "
;;                     width", "height", GL_UNSIGNED_BYTE, "
;;                     image-name "_source_image,"
;;                     width-power-of-2", "height-power-of-2", GL_UNSIGNED_BYTE, "
;;                     image-name "_image);\n"))))
;;          (image-scaling-code
;;           `(gluScaleImage GL_RGB ,width ,height GL_UNSIGNED_BYTE
;;                           ,(get-pointer
;;                             (string-append image-name "_source_image"))
;;                           ,width-power-of-2 ,height-power-of-2 GL_UNSIGNED_BYTE
;;                           ,(get-pointer
;;                             (string-append image-name "_image"))))
                          
         (tex-id (gensym 'tex-id))
         ;; this init script code must be embeded in an environment
         ;; that will bind the ,tex-id variable.
         (init-script
          `(begin
             (pp (list 'generating 'texture ,tex-id))
;;              (glShadeModel GL_FLAT)
;;              (glPixelStorei GL_UNPACK_ALIGNMENT 1)
             (glBindTexture GL_TEXTURE_2D ,tex-id)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
             (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
             (create-texture ,image-name ,width-power-of-2 ,height-power-of-2)
             (ensure-texture-bound ,tex-id))))
    `(begin
      (c-declare ,c-declare-code)
      ((c-lambda () void ,image-generation-code))
;;       ((c-lambda () void ,image-scaling-code))
;;       ((lambda () ,image-scaling-code))
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
  `(begin
;;      (let* ((tex (table-ref texture-table ,image-name))
;;             (width (texture-width tex))
;;             (height (texture-height tex)))
;;        (for y 0 (< y height)
;;          (for x 0 (< x width)
;;            ((c-lambda (int int) void
;;               ,(with-output-to-string ""
;;                  (lambda ()
;;                    (show
;;                     "printf(\""
;;                     image-name "_source_image[%d][%d] = {%d,%d,%d}\\n\","
;;                     "___arg2, ___arg1,"
;;                     image-name "_source_image[___arg2][___arg1][0],"
;;                     image-name "_source_image[___arg2][___arg1][1],"
;;                     image-name "_source_image[___arg2][___arg1][2]);"))))
;;             x y))))
     (let* ((tex (table-ref texture-table ,image-name))
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
            x y))))))

(define (initialize-textures!)
  (for-each (lambda (t)
              (pp `(generating texture ,(car t)))
              ((texture-init-script (cdr t))))
            (table->list texture-table)))

;; GLboolean glAreTexturesResident(GLsizei n, const GLuint*textureNames, GLboolean *residences);
(define (ensure-texture-bound id)
  ((c-lambda (int) void
#<<end
GLboolean bool[1] = {GL_FALSE};
GLboolean return_val = glAreTexturesResident(1, &___arg1, bool);
printf("is texture number %d resident: %d(%d)\n", ___arg1, return_val, bool[1]);
end
) id))
           

(define (draw-sprite name x y)
  (let* ((texture-obj (table-ref texture-table name))
         ;;(texture-obj (table-ref texture-table "easy0"))
         (tex-id (texture-id texture-obj))
         (width (texture-width texture-obj))
         (height (texture-height texture-obj)))
;;    (ensure-texture-bound tex-id)
    (glColor3f 0. 0. 1.)
    (glEnable GL_TEXTURE_2D)
    (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
    (glBindTexture GL_TEXTURE_2D tex-id)
;;     (pp `(texture-name: ,name id: ,tex-id))
    (glBegin GL_QUADS)
    (begin
      (glTexCoord2f 0.0 0.0) (glVertex2i x y)
      (glTexCoord2f 0.0 1.0) (glVertex2i x (+ y height))
      (glTexCoord2f 1.0 1.0) (glVertex2i (+ x width) (+ y height))
      (glTexCoord2f 1.0 0.0) (glVertex2i (+ x width) y))
    (glEnd)
    (glDisable GL_TEXTURE_2D)))
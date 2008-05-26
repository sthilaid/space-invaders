(c-declare "#include <GL/gl.h>\n")

(define-macro (define-sprite filename)
  (include "ppm-reader.scm")
  (include "texture-macro.scm")
  (include "scm-lib-macro.scm")
  (include "scm-lib.scm")

  (let* ((image-name (path-strip-directory
                      (path-strip-extension filename)))
         (ppm-data (parse-ppm-image-file filename))
         (width (ppm-image-width ppm-data))
         (height (ppm-image-height ppm-data))
         (width-power-of-2 (next-power-of-2 width))
         (height-power-of-2 (next-power-of-2 height))
         (declaration-code
          (with-output-to-string ""
            (lambda ()
              (show "GLubyte "
                    image-name "[" height-power-of-2 "]["
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
                       (show image-name "["y"]["x"][0] = "r";\n"
                             image-name "["y"]["x"][1] = "g";\n"
                             image-name "["y"]["x"][2] = "b";\n"
                             image-name "["y"]["x"][3] = "
                             alpha";\n"))))))))
    `(define-texture ,declaration-code ,image-generation-code
       ,image-name ,width-power-of-2 ,height-power-of-2)))

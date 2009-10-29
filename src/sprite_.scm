;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: sprite-macro.scm
;;
;; description: This file contains the define-sprite macro which
;; enables one to load an image in an ascii ppm image format as a
;; sprite. This will parse the image and generate c-code that will
;; fill save this image inside the video card using the texture
;; abstraction defined in texture-macro.scm.
;;
;; Note: Since sprites are not used anymore in space-invaders, this
;; macro was not updated and the generated c code should be very big
;; and not very efficient. Please refert to the font-macro.scm file
;; for a better implementation.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(c-declare "#include <gl.h>\n")

(define-macro (define-sprite filename)
  (include "ppm-reader.scm")
  (include "texture_.scm")
  (include "../include/scm-lib_.scm")
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
                             alpha";\n")))))))
         (init-script
          (let ((tex-id-sym (gensym 'tex-id)))
          `(lambda (,tex-id-sym)
             (lambda ()
               (glBindTexture GL_TEXTURE_2D ,tex-id-sym)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
               (attach-texture ,image-name ,width-power-of-2
                               ,height-power-of-2))))))

    `(define-texture ,declaration-code ,image-generation-code ,init-script
       ,image-name ,width-power-of-2 ,height-power-of-2)))

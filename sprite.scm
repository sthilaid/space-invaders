;; This wrapper function will generate the C array corresponding to
;; the RGB components of the read PPM image, such that this array can
;; be used by opengl's glDrawPixels with format equal to GL_RGB and
;; type equal to GL_INT. The buffer containing the data is accessible
;; with the filename (without the extension nor the directory).
(define-macro (include-ppm-pixel-sprite filename)
  (include "ppm-reader.scm")

  ;; some definitions from scm-lib.scm
  (define-macro (for var init-val condition true . false)
  (let ((loop (gensym 'loop)))
    `(let ,loop ((,var ,init-val))
          (if ,condition
              (begin ,true (,loop (+ ,var 1)))
              ,(if (not (null? false))
                   false)))))
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  (define (pipe p c)
    (receive (out in) (open-string-pipe '(direction: output))
             (let ((t
                    (thread-start!
                     (make-thread
                      (lambda ()
                        (with-output-to-port out p))))))
               (with-input-from-port in c))))

  (let* ((image-name (path-strip-directory
                      (path-strip-extension filename)))
         (ppm-data (parse-ppm-image-file filename))
         (width (ppm-image-width ppm-data))
         (height (ppm-image-height ppm-data))
         (pixel-number (length (ppm-image-pixels ppm-data)))
         (c-code
          (with-output-to-string ""
            (lambda ()
              (show "GLubyte "
                    image-name "_pixelmap[" (* 3 width height) "] = {")
              (for index 0 (< index pixel-number)
                   (let ((pixel (list-ref (ppm-image-pixels ppm-data) index)))
                     (show (car pixel) "," (cadr pixel) "," (caddr pixel))
                     (if (not (= index (- pixel-number 1))) (show ","))))
              (show "};\n")

              (show "pixelmap " image-name " = {"
                    width "," height "," image-name "_pixelmap};\n")))))
    `(c-declare ,c-code)))

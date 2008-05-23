;;(include "scm-lib.scm")
;;(include "opengl-header.scm")
(c-declare "#include <GL/gl.h>\n")

(define-type font id texture-id width height table)
(define (font-get-char-font font-obj color char)
  (table-ref (font-table font-obj) `(,color ,char)))

(define (get-char pixels-vector x y width height char-width char-height)
  (let* ((x0 (- width (* (+ x 1) char-width)))
         (y0 (- height (* (+ y 1) char-height)))
         (x-max (+ x0 char-width))
         (y-max (+ y0 char-height)))
    (let loopy ((y y0)(acc-y '()))
      (if (< y y-max)
          (loopy
           (+ y 1)
           (append 
            (let loopx ((x x0) (acc-x '()))
              (if (< x x-max)
                  (loopx (+ x 1)
                         (cons
                          (vector-ref pixels-vector (+ (* y width) x))
                          acc-x))
                  acc-x))
            acc-y))
          acc-y))))

(define (test char w h)
  (include "scm-lib.scm")
  (for y 0 (< y h)
    (for x 0 (< x w)
      (let ((p (list-ref char (+ (* y w) x))))
        (if (> (apply + p) 0)
            (display "x")
            (display "_"))
        (if (= x (- w 1)) (newline))))))

(define (get-font-table font-name char-width char-height)
  (include "ppm-reader.scm")
  (include "scm-lib.scm")
  
  (let* ((image-file (string-append "fonts/" (symbol->string font-name )
                                    ".ppm"))
         (font-data-file
          (string-append "fonts/" (symbol->string font-name )
                         ".scm"))
         (ppm-data (parse-ppm-image-file image-file))
         (font-data (with-input-from-file font-data-file
                      (lambda () (read))))
         (width (ppm-image-width ppm-data))
         (height (ppm-image-height ppm-data))
         (pixels (list->vector (ppm-image-pixels ppm-data)))
         (x-max (quotient width char-width))
         (y-max (quotient height char-height))
         (char-table (make-table)))
    (if (not (and (= (modulo width char-width) 0)
                  (= (modulo height char-height) 0)))
        (error "invalid font image char size."))
    (let ((chars-pixels
           (let loopy ((y 0)(acc-y '()))
             (if (< y y-max)
                 (loopy
                  (+ y 1)
                  (append
                   (let loopx ((x 0) (acc-x '()))
                     (if (< x x-max)
                         (loopx (+ x 1)
                                (cons (get-char pixels x y width height
                                                char-width char-height)
                                      acc-x))
                         acc-x))
                   acc-y))
                 (reverse acc-y)))))
      (let loop-colors ((colors (cadr (assq colors: font-data)))
                 (chars-pixels chars-pixels))
        (if (not (pair? colors))
            char-table
            (let loop-chars ((chars (cadr (assq chars: font-data)))
                             (chars-pixels chars-pixels))
              (if (not (pair? chars))
                  (loop-colors (cdr colors) chars-pixels)
                  (begin
                    (table-set! char-table (list (car colors) (car chars))
                                (car chars-pixels))
;;                     (pp `(set (,(car colors) ,(car chars)) to:))
;;                     (test (car chars-pixels) 8 8)
                    (loop-chars (cdr chars) (cdr chars-pixels))))))))))

(define-macro (define-symmetric-font font-name char-width char-height)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))

  (let ((declaration-code ";")
;;          (with-output-to-string
;;            ""
;;            (lambda ()
;;              (show "GLubyte internal_font_image["char-height"]["
;;                    char-width"][4];\n"))))
        ;; dummy texture generation code used. Will be set later,
        ;; before being drawn.
        (generation-code "1+1;"))
    `(begin
       (define-texture ,declaration-code ,generation-code
         "internal_font_image" ,char-width ,char-height)
       (define current-font
         (make-font ,font-name
                    (texture-id
                     (table-ref texture-table "internal_font_image"))
                    ,char-width ,char-height
                    (get-font-table ,font-name ,char-width ,char-height)))
       
       (define update-current-char-font!
         (c-lambda (int int unsigned-int
                        unsigned-int unsigned-int unsigned-int) void
#<<end
  internal_font_image[___arg2][___arg1][0] = ___arg3;
  internal_font_image[___arg2][___arg1][0] = ___arg4;
  internal_font_image[___arg2][___arg1][0] = ___arg5;
  internal_font_image[___arg2][___arg1][0] = ___arg6;
end
)))))

k
(c-declare "GLubyte internal_font_image[8][8][4];\n")
(define current-font #f)

(define (draw-char color char x y)
  (include "texture.scm")
  (define-macro (for var init-val condition true . false)
  (let ((loop (gensym 'loop)))
    `(let ,loop ((,var ,init-val))
          (if ,condition
              (begin ,true (,loop (+ ,var 1)))
              ,(if (not (null? false))
                   false)))))

  (let ((char-font (font-get-char-font current-font color char))
        (char-width (font-width current-font))
        (char-height (font-height current-font))
        (texture-id (font-texture-id current-font)))
    (for y 0 (< y char-height)
      (for x 0 (< x char-width)
        (let* ((current-pix (list-ref char-font (+ (* y char-width) x)))
               (r (car current-pix))
               (g (cadr current-pix))
               (b (caddr current-pix))
               (a (if (< (+ r g b) 10) 0 255)))
            (update-current-char-font! x y r g b a))))

    (glBindTexture GL_TEXTURE_2D texture-id)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
    (create-texture "internal_font_image" 8 8))

  (draw-texture "internal_font_image" x y))

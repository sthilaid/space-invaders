(include "scm-lib-macro.scm")

(define-type font id texture-id width height table)

(define (change-current-char! font color char)
  ((table-ref (font-table font) (list color char))))


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
                  (reverse acc-x)))
            acc-y))
          (reverse acc-y)))))

(define (test char w h)
;;  (include "scm-lib.scm")
  (for y 0 (< y h)
    (for x 0 (< x w)
      (let ((p (list-ref char (+ (* y w) x))))
        (if (> (apply + p) 0)
            (display "x")
            (display "_"))
        (if (= x (- w 1)) (newline))))))

(define (get-font-table font-name char-width char-height)
;;  (include "ppm-reader.scm")
;;  (include "scm-lib.scm")
  
  (let* ((image-file     (string-append "fonts/" font-name ".ppm"))
         (font-data-file (string-append "fonts/" font-name ".scm"))
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




(define current-font #f)

(define global-fonts-table (make-table))

(define (add-new-font! font-name font-obj)
  (table-set! global-fonts-table font-name font-obj))

(define (get-font font-name)
  (table-ref global-fonts-table font-name))


(define (draw-char font-name color char x y x-offset)
  (let* ((current-font (get-font font-name))
         (char-width (font-width current-font)))
    (change-current-char! current-font color char)
    (draw-texture font-name (+ x (* x-offset char-width)) y)))

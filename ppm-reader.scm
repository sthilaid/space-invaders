;;(include "scm-lib.scm")

(define-type ppm-image width height color-depth pixels)

;; Strips comments from a ppm image file from stdin and outputs the
;; result on stdout
(define (strip-comments)
  (let loop ((line (read-line)))
    (if (not (eof-object? line))
        (begin
          (if (or (string=? line "")
                  (not (char=? (string-ref line 0) #\#)))
              (begin (display (string-append line "\n"))
                     (force-output)))
          (loop (read-line)))))
  (if (not (tty? (current-output-port)))
      (close-output-port (current-output-port))))

;; Parses a ppm image from the stdin into a scheme ppm-image type.
(define (parse-ppm-image)
  (if (not (eq? (read) 'P3))
      (error "unsupported ppm format"))
  (let ((width (read))
        (height (read))
        (color-depth (read)))
    (let loop ((index 0) (data (read)) (current-pixel '()) (pixels '()))
      (if (eof-object? data)
          (if (not (= index 0))
              (error "bad image format detected...")
              (make-ppm-image width height color-depth pixels))
          (if (= index 2)
              (loop 0 (read) '() (cons (reverse (cons data current-pixel))
                                       pixels))
              (loop (+ index 1) (read) (cons data current-pixel) pixels))))))

(define (parse-ppm-image-file filename)
  (if (not (file-exists? filename))
      (error (string-append "Image " filename " does not exists.")))
  (pipe
   (lambda () (with-input-from-file filename strip-comments))
   parse-ppm-image))

;; (define (rgb-pixels-to-boolean-point-list ppm-data)
;;   (define width (ppm-image-width ppm-data))
;;   (define height (ppm-image-height ppm-data))
;;   (map (lambda (pixel) (> (+ (car pixel) (cadr pixel) (caddr pixel)) 200))
;;        (ppm-image-pixels ppm-data)))
    

;; This wrapper function will generate the C array corresponding to
;; the RGB components of the read PPM image, such that this array can
;; be used by opengl's glDrawPixels with format equal to GL_RGB and
;; type equal to GL_INT. The buffer containing the data is accessible
;; with the filename (without the extension nor the directory).
(define-macro (include-ppm-pixel-sprite filename)
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

  (define-type ppm-image width height color-depth pixels)

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

  (if (not (file-exists? filename))
      (error (string-append "Image " filename " does not exists.")))
  
  (let* ((image-name (path-strip-directory
                      (path-strip-extension filename)))
         (ppm-data
          (pipe
           (lambda () (with-input-from-file filename strip-comments))
           parse-ppm-image))
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

(define (fold-l f acc list)
  (if (not (pair? list))
      acc
      (fold-l f (f acc (car list)) (cdr list))))

(define (compose f . gs)
  (lambda (x) (fold-l (lambda (acc f) (f acc))
                      x
                      (reverse (cons f gs)))))

(define (dyn-load file1 file2)
  (let* ((ts1 (if (file-exists? file1)
                  (time->seconds(file-info-last-modification-time
                                 (file-info file1)))
                  0))
         (ts2 (if (file-exists? file2)
                  (time->seconds (file-info-last-modification-time
                                  (file-info file2)))
                  0))
         (loaded-file (if (> ts1 ts2) file1 file2)))
    (println "loading: " loaded-file)
    (load loaded-file)))

(define (load-game src-dir lib-dir lib-files)
  (define strip (compose path-strip-extension path-strip-directory))
  (define (lib-file->src f) (string-append src-dir "/" (strip f) ".scm"))
  (define (lib-file->lib f) (string-append lib-dir "/" (strip f) ".o1"))
  (for-each (lambda (f) (dyn-load (lib-file->src f) (lib-file->lib f)))
            lib-files)
  (main))

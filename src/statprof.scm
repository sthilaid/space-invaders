;; statprof.scm -- A statistical profiler for Gambit-C 4.0

;; See the README file for license and usage information.

;; $Id$


;; ----------------------------------------------------------------------------
;; Profiling & interruption handling

(define *buckets* #f)
(define *total* 0)

(define (profile-start!)
  (set! *buckets* '())
  (set! *total* 0)
  (##interrupt-vector-set! 1 profile-heartbeat!))

(define (profile-stop!)
  (##interrupt-vector-set! 1 ##thread-heartbeat!))

;; (define (identify-continuation cont) ; first version
;;   (or (##continuation-creator cont)
;;       'unknown))

(define (identify-continuation cont) ; second version
  (let ((locat (##continuation-locat cont)))
    (if locat
        (let* ((container (##locat-container locat))
               (file (##container->file container)))
          (if file
              (let* ((filepos (##position->filepos (##locat-position locat)))
                     (line (##fixnum.+ (##filepos-line filepos) 1))
                     (col (##fixnum.+ (##filepos-col filepos) 1)))
                (list file line col))
              'unknown))
        'unknown)))

(define (profile-heartbeat!)
  (##continuation-capture
   (lambda (cont)
     (##thread-heartbeat!)
     (let ((id (identify-continuation cont)))
       (if (not (eq? id 'unknown))
           (let ((bucket (assoc (car id) *buckets*)))
             (set! *total* (+ *total* 1))
             (if (not bucket)
                 (begin
                   (set! *buckets* (cons 
                                    (cons (car id) 
                                          ;; fixme: arbitrary hard limit
                                          ;; on the length of source
                                          ;; files
                                          (make-vector 5000 0)) 
                                    *buckets*))
                   (set! bucket (car *buckets*))))

             (vector-set! (cdr bucket)
                          (cadr id) 
                          (+ (vector-ref (cdr bucket) 
                                         (cadr id))
                             1))))))))


;; ----------------------------------------------------------------------------
;; Text formatting

(define (pad-left s l c)
  (let loop ((s (string->list s)))
    (if (< (length s) l)
        (loop (cons c s))
        (list->string s))))


;; ----------------------------------------------------------------------------
;; Palette generation & color formatting

(define (gradient from to step)
  (let ((inc (map (lambda (x) (/ x step))
                  (map - to from))))
    
    (let loop ((i 0)
               (acc '()))
      (if (= i step) 
          (reverse acc)
          (loop (+ i 1)
                (cons (map 
                       (lambda (x o) 
                         (round (+ x (* i o))))
                       from
                       inc)
                      acc))))))

(define (as-rgb col)
  (apply string-append
         (map
          (lambda (x)
            (pad-left (number->string x 16) 2 #\0))
          col)))

(define palette
  (list->vector 
   (cons '(255 255 255) 
         (gradient '(127 127 255) 
                   '(255 127 127)
                   16))))


;; ----------------------------------------------------------------------------
;; Functions to generate the report

(define (write-profile-report profile-name)

  (define (iota1 n)
    (let loop ((n n)
               (l '()))
      (if (>= n 1) 
          (loop (- n 1) (cons n l))
          l)))
  
  (define directory-name (string-append (current-directory)
                                        profile-name
                                        "/"))
  (with-exception-catcher
   (lambda (e)
     ;; ignore the exception, it probably means that the directory
     ;; already existed.  If there's another problem it will be
     ;; signaled later.
     #f) 
   (lambda ()
     (create-directory (list path: directory-name
                             permissions: #o755))))
  
  (let ((max-intensity 
         (apply max
                (map
                 (lambda (data)
                   (apply max 
                          (vector->list data)))
                 (map cdr *buckets*)))))

    (map 
     (lambda (bucket)
       (let ((file (car bucket))
             (data (cdr bucket)))
       
         (define (get-color n)
           (let ((i (vector-ref data n)))
             (if (= i 0)
                 (as-rgb (vector-ref palette 0))
                 (let ((x (* (/ (log (+ 1. i))
                                (ceiling (log max-intensity)))
                             (- (vector-length palette) 1))))
                   (as-rgb (vector-ref palette 
                                       (inexact->exact (ceiling x))))))))

         (with-output-to-file (string-append 
                               directory-name
                               (path-strip-directory file)
                               ".html")
           (let ((lines (call-with-input-file file 
                          (lambda (p) (read-all p read-line)))))
             (lambda ()
               (display
                (sexp->html
                 `(html 
                   (body
                    (table 
                     cellspacing: 0 
                     cellpadding: 0
                     border: 0
                     style: "font-size: 12px;"
                     ,@(map
                        (lambda (line line#)
                          `(tr 
                            (td ,(string-append 
                                  (number->string line#)
                                  ": "))
                            ;; (td 
                            ;;  align: center
                            ;;  ,(let ((n (vector-ref data line#)))
                            ;;     (if (= n 0)
                            ;;         ""
                            ;;         (string-append "[" 
                            ;;                        (number->string n)
                            ;;                        "/"
                            ;;                        (number->string *total*)
                            ;;                        "]"))))
                            
                            (td 
                             align: center
                             ,(let ((n (vector-ref data line#)))
                                (if (= n 0)
                                    ""
                                    (string-append
                                     (number->string
                                      (round% (/ n *total*)))
                                     "% "))))
                               
                            (td (pre style: ,(string-append     
                                              "background-color:#"
                                              (get-color line#))
                                     ,line))))
                        lines
                        (iota1 (length lines)))))))))))))
     
     *buckets*))

  (with-output-to-file (string-append directory-name "index.html")
    (lambda ()
      (display
       (sexp->html
        `(html
          (body
           ,@(map (lambda (bucket)
                    (let ((file-path (string-append 
                                      directory-name
                                      (path-strip-directory (car bucket)) 
                                      ".html")))
                      `(p (a href: ,file-path ,file-path)
                          " ["
                          ,(round%
                            (/ (apply + (vector->list (cdr bucket)))
                               *total*)) 
                          " %]")))
                  *buckets*))))))))

(define (round% n)
  (/ (round
      (* 10000 n))
     100.))


;; ----------------------------------------------------------------------------
;; Included file "html.scm"
;; ----------------------------------------------------------------------------

;; html.scm -- A simple html generator for Gambit-C 4.0

;; Written by Guillaume Germain (germaing@iro.umontreal.ca)
;; This code is released in the public domain.


(define (stringify x)
  (with-output-to-string ""
    (lambda ()
      (display x))))

(define (to-escaped-string x)
  (stringify 
   (map (lambda (c)
          (case c
            ((#\<) "&lt;")
            ((#\>) "&gt;")
            ((#\&) "&amp;")
            (else c)))
        (string->list 
         (stringify x)))))

;; Quick and dirty conversion of s-expressions to html
(define (sexp->html exp)
  
  ;; write the opening tag
  (define (open-tag exp)
    (cond
     ;; null tag isn't valid
     ((null? exp) 
      (error "null tag"))
     
     ;; a tag must be a list beginning with a symbol
     ((and (pair? exp)
           (symbol? (car exp)))
      (list "<" 
            (car exp)
            " " 
            (maybe-args (car exp) (cdr exp))))
     
     (else
      (error "invalid tag" exp))))

  ;; take care of the keywords / arguments
  (define (maybe-args tag exp)

    (cond
     ;; does the rest of the list begins with a keyword
     ((and (pair? exp)
           (keyword? (car exp)))

      ;; does the keyword has an associated value?
      (if (or (null? (cdr exp))
              (keyword? (cadr exp)))
          ;; no, we don't put an associated value
          (list (keyword->string (car exp))
                " "
                (maybe-args tag (cdr exp)))
          ;; yes, we take the next element in the list as the value
          (list (keyword->string (car exp))
                "=\""
                (cadr exp)
                "\" "
                (maybe-args tag (cddr exp)))))

     ;; must just be some content
     (else
      (content tag exp))))

  ;; handle the content of the tag and closing it
  (define (content tag exp)
    (cond
     ;; no content...
     ((null? exp)
      ;;(list "></" tag ">"))           ; close as "<br></br>"
      (list "/>"))                      ; close as "<br/>"

     ;; write the content, handle tags inside
     ((pair? exp)
      (list ">"
            (map (lambda (e)
                   (if (pair? e)
                       (open-tag e)
                       (to-escaped-string e)))
                 exp)
            "</"
            tag
            ">"))

     ;; non-null terminated list?
     (else
      (error "strange content..."))))

  ;; we rely on Gambit's flattening of list when printed with DISPLAY
  (with-output-to-string ""
                         (lambda ()
                           (display (open-tag exp)))))

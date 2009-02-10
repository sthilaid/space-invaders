


;; Critical section used with the semaphores. action and actions will
;; be executed inside the given semaphore (that should be a mutex for
;; a critical-section) and the semaphore will be released after the
;; execution of the actions is finished. The last returned value of
;; action/actions will be returned by this macro.
(define-macro (critical-section! sem action . actions)
  (let ((result (gensym 'crit-section-result)))
    `(begin
       (sem-lock! ,sem)
       (let ((,result (begin ,action ,@actions)))
         (sem-unlock! ,sem)
         ,result))))


;; Will result in termination the currently running coroutine and
;; executing directly the specified continuation coroutine, bypassing
;; the other coroutines waiting in the coroutine queue.
(define-macro (prioritized-continuation continuation-corout)
  (define arg (gensym 'arg))
  (define c (gensym 'continuation-corout))
  `(let ((,c ,continuation-corout))
     (prioritize! ,c)
     (corout-kont-set! ,c (let ((k (corout-kont ,c)))
                           (lambda (,arg)
                             (unprioritize! ,c)
                             (if (procedure? k)
                                 (k ,arg)
                                 (continuation-return k ,arg)))))
     (terminate-corout ,c)))

(define-macro (prioritized-thunk-continuation continuation-thunk)
  `(begin
     (prioritize! (current-corout))
     (yield)
     (unprioritize! (current-corout))
     (,continuation-thunk)))



(define-macro (continue-with continuation-corout)
  `(terminate-corout ,continuation-corout))

(define-macro (continue-with-thunk! continuation-thunk)
  `(begin
     (yield)
     (,continuation-thunk)))



(define-macro (compose-thunks . thunks)
  (define (id id1 id2) `(gensym (symbol-append 'composition-of- ,id1 ,id2)))
  (define (composition thunks)
    (cond ((not (pair? thunks))
           (error (string-append "thunk composition must"
                                 "contain at least 1 thunk")))
          ((and (pair? thunks)
                (null? (cdr thunks)))
           (car thunks))
          (else
           `(lambda ()
              (,(car thunks))
              (continue-with-thunk! ,(composition (cdr thunks)))))))
  (composition thunks))

(define-macro (timeout? toval . code)
  `(with-exception-catcher
    (lambda (e) (if (eq? e mailbox-timeout-exception)
                    ,toval
                    (raise e)))
    (lambda () ,@code)))


(define-macro (recv pattern-list)
  (define-type ast node-type test timeout body)
  (define (pattern->ast pat)
    (if (list? pat)
        (cond
         ;; matching ('symbol return-value)
         ((and (list? (car pat))
               (eq? (caar pat) 'quote))
          (make-ast 'pattern-match
                    `(lambda (x) (eq? x ',(cadar pat))) -1 (cdr pat)))
         ((and (eq? (car pat) 'after)
               (number? (cadr pat)))
          (make-ast 'after `(lambda (x) #f) (cadr pat) (cddr pat)))
         (else (error "ill-formed recv pattern expression")))))
  (define (generate-code pat)
    (let* ((ast (pattern->ast pat))
           (?-req `(?? ,(ast-test ast) timeout: ,(ast-timeout ast))))
      `(and ,(if (eq? (ast-node-type ast) 'after)
                 `(timeout? #t ,?-req)
                 `(timeout? #f ,?-req))
            (begin ,@(ast-body ast)))))

  (list 'quote `(or ,@(map generate-code pattern-list))))

(define (??? #!key (timeout 'infinity) #!rest preds)
  (define mailbox (corout-mailbox (current-corout)))
  (let loop ()
    (cond ((queue-find-and-remove! pred mailbox)
           => (lambda (val) val))
          (else
           (if (number? timeout)
               (begin
                (continuation-capture
                 (lambda (k)
                   (let ((corout (current-corout)))
                     (corout-kont-set! corout k)
                     (corout-sleeping?-set! corout #t)
                     (sleep-for timeout))))
                (raise mailbox-timeout-exception))
               (begin (continuation-capture
                       (lambda (k)
                         (let ((corout (current-corout)))
                           (corout-kont-set! corout k)
                           (corout-sleeping?-set! corout #t)
                           (resume-scheduling))))
                      (loop)))))))

(define-macro (recv pattern-list)
  (define-type ast node-type test timeout body)
  (define (pattern->ast pat)
    (if (list? pat)
        (cond
         ;; matching ('symbol return-value)
         ((and (list? (car pat))
               (eq? (caar pat) 'quote))
          (make-ast 'pattern-match
                    `(lambda (x) (eq? x ',(cadar pat))) -1 (cdr pat)))
         ((and (eq? (car pat) 'after)
               (number? (cadr pat)))
          (make-ast 'after `(lambda (x) #f) (cadr pat) (cddr pat)))
         (else (error "ill-formed recv pattern expression")))))
  (define (generate-code pat)
    (let* ((ast (pattern->ast pat))
           (?-req `(?? ,(ast-test ast) timeout: ,(ast-timeout ast))))
      `(and ,(if (eq? (ast-node-type ast) 'after)
                 `(timeout? #t ,?-req)
                 `(timeout? #f ,?-req))
            (begin ,@(ast-body ast)))))

  (list 'quote `(or ,@(map generate-code pattern-list))))


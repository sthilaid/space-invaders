;; (requires match.scm)

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
  (define (make-ast test-pattern eval-pattern)
    (cons test-pattern eval-pattern))
  (define (ast-test-pattern x) (car x))
  (define (ast-eval-pattern x) (cdr x))
  (define (pattern->ast pat)
    (if (list? pat)
        (make-ast `(,(car pat) #t)
                  `(,(car pat) ,@(cdr pat)))))
  (define (generate-predicate asts)
    `(lambda (msg) (match msg ,@(map ast-test-pattern asts))))
  (define (generate-on-msg-found asts)
    `(lambda (msg) (match msg ,@(map ast-eval-pattern asts))))
  (define (filter pred list)
    (cond
     ((not (pair? list)) '())
     ((pred (car list)) (cons (car list) (filter pred (cdr list))))
     (else (filter pred (cdr list)))))
  (include "match.scm")
  (load "scm-lib")

  (let* ((last-pat (take-right pattern-list 1))
         (timeout-val (if (eq? (caar last-pat) 'after)
                          (cadar last-pat)
                          'infinity))
         (timeout-ret-val (if (eq? (caar last-pat) 'after)
                              (caddar last-pat)
                              '(raise mailbox-timeout-exception)))
         (cleaned-patterns (filter
                            (lambda (x) (match x
                                               ((after ,_ ,_) #f)
                                               (,_ #t)))
                                   pattern-list))
         (asts (map pattern->ast cleaned-patterns)))
    (list 'quote
          `(let ((mailbox (corout-mailbox (current-corout))))
             (let loop ()
               (cond ((queue-find-and-remove!
                       ,(generate-predicate asts)
                       mailbox)
                      => ,(generate-on-msg-found asts))
                     (else
                      ,(if (eq? timeout-val 'infinity)
                           `(begin (continuation-capture
                                    (lambda (k)
                                      (let ((corout (current-corout)))
                                        (corout-kont-set! corout k)
                                        (corout-sleeping?-set! corout #t)
                                        (resume-scheduling))))
                                   (loop))
                           `(let ((msg-q-size (queue-size mailbox)))
                              (sleep-for timeout)
                              ;; might be awoken either from a (!) or timeout
                              (if (= (queue-size mailbox) msg-q-size)
                                  (raise mailbox-timeout-exception)
                                  (loop)))))))))))

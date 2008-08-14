


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
                             (k ,arg))))
     (terminate-corout ,c)))

(define-macro (prioritized-thunk-continuation thunk-cont)
  `(begin
     (corout-kont-set! (current-corout)
                       (lambda (,(gensym 'dummy-arg)) (,thunk-cont)))
     (prioritized-continuation (current-corout))))


(define-macro (continue-with continuation-corout)
  `(terminate-corout ,continuation-corout))

(define-macro (continue-with-thunk! continuation-thunk)
  `(begin (yield)
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


(define-macro (dynamic-corout-extent before-thunk body-thunk after-thunk)
  (define result (gensym 'result))
  `(lambda ()
     (push! ,before-thunk (corout-on-entry (current-corout)))
     (push! ,after-thunk (corout-on-exit (current-corout)))
     (for-each (lambda (f) (f))
               (reverse (stack->list (corout-on-entry (current-corout)))))
     ;; Note: There is no need to add calls to the on-exit thunks
     ;; because the body is supposed to be wrapped and exit with a
     ;; terminate-corout call which calls the on-exit thunks.
     (,body-thunk)))

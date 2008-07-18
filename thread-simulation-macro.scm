


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

(define-macro (corout-continuation continuation-corout)
  `(terminate-corout ,continuation-corout))

(define-macro (corout-thunk-continuation! continuation-thunk)
  `(begin
     (corout-kont-set! (current-corout)
                       (lambda (,(gensym 'dummy-arg)) (,continuation-thunk)))
     (corout-continuation (current-corout))))


(define-macro (compose-thunks . thunks)
  (define (id id1 id2) `(gensym (symbol-append 'composition-of- ,id1 ,id2)))
  (define (composition thunks)
    (cond ((not (pair? thunks))
           (error (string-append "thunk composition must"
                                 "contain at least 1 thunk")))
          ((and (pair? thunks)
                (null? (cdr thunks)))
           `(lambda (,(gensym 'dummy-arg))
              (terminate-corout (,(car thunks)))))
        
          (else `(lambda (,(gensym 'dummy-arg))
                   (,(car thunks))
                   (corout-kont-set! (current-corout)
                                     ,(composition (cdr thunks)))
                   (corout-continuation (current-corout))))))

    `(lambda () (,(composition thunks) 'dummy)))
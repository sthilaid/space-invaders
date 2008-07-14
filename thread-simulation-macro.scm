


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

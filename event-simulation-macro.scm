
(define-macro (in delta thunk)
  ;; or (+ !!-current-time-!! ,delta). Not sure if its better to not
  ;; be accurate in times (as with using !!-current-time-!! or be
  ;; slower by rechecking the current-time and be more accurate...
  `(schedule-event! (!!-event-queue-!!)
                    (+ (- (time->seconds (current-time))
                          (!!-simulation-start-time-!!))
                       ,delta)
                    ,thunk))

(define-macro (critical-section! sem action . actions)
  (let ((result (gensym 'crit-section-result)))
    `(begin
       (sem-lock! ,sem)
       (let ((,result (begin ,action ,@actions)))
         (sem-unlock! ,sem)
         ,result))))

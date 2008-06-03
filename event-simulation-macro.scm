;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: event-simulation-macro.scm
;;
;; description: Contains small macros to simplify coding with event
;; simulation.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Will register the givent thunk to be ran in delta secons into the
;; *currently running* simulation. Thus, the 'in' macro can only be
;; used from within a simulation event. Usage exemples can be seen in
;; the file event-simulation.scm.
(define-macro (in delta thunk)
  ;; or (+ !!-current-time-!! ,delta). Not sure if its better to not
  ;; be accurate in times (as with using !!-current-time-!! or be
  ;; slower by rechecking the current-time and be more accurate...
  `(schedule-event! (!!-event-queue-!!)
                    (+ (- (time->seconds (current-time))
                          (!!-simulation-start-time-!!))
                       ,delta)
                    ,thunk))

;; Critical section used with the event-simulation semaphores. action
;; and actions will be executed inside the given semaphore (that
;; should be a mutex for a critical-section) and the semaphore will be
;; released after the execution of the actions is finished. The last
;; returned value of action/actions will be returned by this macro.
(define-macro (critical-section! sem action . actions)
  (let ((result (gensym 'crit-section-result)))
    `(begin
       (sem-lock! ,sem)
       (let ((,result (begin ,action ,@actions)))
         (sem-unlock! ,sem)
         ,result))))

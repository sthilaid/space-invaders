(include "scm-lib.scm")
(include "event-simulation.scm")

(define invader-row-number 5)
(define invader-col-number 11)
(define ship-movement-speed 4)
(define invader-spacing 16)
(define player-laser-speed 4)
(define player-movement-speed 4)


(define-type pos2d x y)

(define (inverse-dir dir . options)
  (let ((x-fact (if (memq 'x options) -1 1))
        (y-fact (if (memq 'y options) -1 1)))
    (make-pos2d (* x-fact (pos2d-x dir))
               (* y-fact (pos2d-y dir)))))

(define-type game-object id type pos state speed
  extender: define-type-of-game-object)

(define-type-of-game-object invader-ship row col)
(define-type-of-game-object player-ship)
(define-type-of-game-object laser-obj)

(define-type object-type id height width)
(define type-id object-type-id)
(define type-height object-type-height)
(define type-width object-type-width)

(define types
  ;; Bounding boxes for all ship types must be equal such that they
  ;; behave the same way in the level.
  `( (easy ,(make-object-type 'easy 8 12))
     (medium ,(make-object-type 'medium 8 12))
     (hard ,(make-object-type 'hard 8 12))
     (mothership ,(make-object-type 'mothership 7 16))
     (player ,(make-object-type 'player 8 13))
;;      (side-wall ,(make-object-type 'side-wall 125 1))
;;      (horiz-wall ,(make-object-type 'horiz-wall 1 200))
     (laserA ,(make-object-type 'laserA 3 6))
     (laserB ,(make-object-type 'laserB 3 6))
     (laserP ,(make-object-type 'laserP 1 5))
     (shield ,(make-object-type 'shield 22 16))
   ))

(define (get-type type-name)
  (let ((type (assq type-name types)))
  (if type
      (cadr type)
      (error (string-append "no such type: " type-name)))))

(define-type wall-struct rect)
(define (new-wall x y width height)
  (make-wall-struct (make-rect x y width height)))
(define wall-rect wall-struct-rect)

(define-type level height width object-table walls) ;invaders player lasers
(define (level-add-object lvl obj)
  (table-set! (level-object-table lvl) (game-object-id obj) obj))
(define (level-remove-object lvl obj)
  (table-set! (level-object-table lvl) (game-object-id obj)))

(define (level-all-objects lvl)
  (map cdr (table->list (level-object-table lvl))))

(define (level-invaders lvl)
  (filter invader-ship? (level-all-objects lvl)))
          
(define (level-player lvl)
  (table-ref (level-object-table lvl) 'player))

(define (new-level)
  (define invaders '())
  (define x-offset 30)
  (define y-offset (- 265 152))
  (define max-x 228)
  (define max-y 265)
  
  (define (determine-type-id max-y)
    (cond ((< max-y 2) 'easy)
          ((< max-y 4) 'medium)
          (else 'hard)))

  (for h 0 (< h invader-row-number)
    (let ((current-type (get-type (determine-type-id h))))
      (for w 0 (< w invader-col-number)
        (let* ((x (+ x-offset (* w invader-spacing)))
               (y (+ y-offset (* h invader-spacing)))
               (pos (make-pos2d x y))
               (state 1)
               (speed (make-pos2d ship-movement-speed 0))
               (row h)
               (col w))
          (set! invaders
                (cons (make-invader-ship (gensym 'inv)
                                         current-type pos state speed row col)
                      invaders))))))

  (let* ((walls (list (new-wall 0 0 +inf.0 -inf.0)
                      (new-wall 0 0 -inf.0 +inf.0)
                      (new-wall max-x max-y -inf.0 +inf.0)
                      (new-wall max-x max-y +inf.0 -inf.0)))
         (player-type (get-type 'player))
         (pos (make-pos2d 22 (- max-y 240)))
         (state 1)
         (speed (make-pos2d 0 0))
         (player-ship (make-player-ship 'player
                                        player-type pos state speed))
         (lasers '())
         (lvl (make-level max-y max-x (make-table) walls)))
    (for-each (lambda (x) (level-add-object lvl x)) invaders)
    (level-add-object lvl player-ship)
    lvl))
              

(define (cycle-state state) (modulo (+ state 1) 2))

(define (move-object! ship delta-x delta-y)
  (let* ((pos (game-object-pos ship) )
         (x (pos2d-x pos))
         (y (pos2d-y pos))
         (speed (game-object-speed ship))
         (state (game-object-state ship)))
    (game-object-state-set! ship (cycle-state state))
    (pos2d-x-set! speed delta-x)
    (pos2d-y-set! speed delta-y)
    (pos2d-x-set! pos (+ x delta-x))
    (pos2d-y-set! pos (+ y delta-y))))

(define (move-ship-row! level row-index)
  (let* ((row-invaders
          (filter (lambda (inv) (= (invader-ship-row inv) row-index))
                  (level-invaders level)))
         (collision? (exists (lambda (inv) (detect-collision? inv level))
                             row-invaders))
         (old-dx (pos2d-x (game-object-speed (car row-invaders))))
         (dx (if collision? (* old-dx -1) old-dx))
         (dy (if collision? (- invader-spacing) 0)))

    (for-each (lambda (inv) (move-object! inv dx dy))
              row-invaders)))

(define (shoot-laser! level laser-type shooter-obj dy)
  (let* ((shooter-x (pos2d-x (game-object-pos shooter-obj)))
         (shooter-y (pos2d-y (game-object-pos shooter-obj)))
         (x (+ shooter-x (floor (/ (type-width (game-object-type shooter-obj))
                                   2))))
         (y (if (< dy 0)
                shooter-y
                (+ shooter-y
                   (type-height (game-object-type shooter-obj))))))
    (level-add-object level
                      (make-laser-obj (gensym 'laser-obj)
                                      (get-type laser-type)
                                      (make-pos2d x y)
                                      0
                                      (make-pos2d 0 dy)))))

(define (create-invader-event level)
  (define event-time-interval 10)
  (define (next-event row-index)
    (lambda ()
      (move-ship-row! level row-index)
      (if (= row-index (- invader-row-number 1))
          ;; the sleep delay is a function such that when the level is full of
          ;; invaders (55 invaders) then the delay is 0.15 and when there is
          ;; no invader left, it is 0.01. Thus the equation system:
          ;; 55x + xy = 15/100 and 0x + xy = 1/100 was solved.
          (let ((invader-nb (length (level-invaders current-level))))
             (thread-sleep! (+ (* 7/2750 invader-nb) 1/100))))
      (in event-time-interval
          (next-event (modulo (+ row-index 1) invader-row-number)))))
  (next-event 0))

;; (define (create-laser-event level)
;;   (define (laser-event)
;;     (move-left

(define (create-manager-event current-level)
  (define manager-time-interfal 1)
  (define player (level-player current-level))

  (define (manager-event)
    (define msg (thread-receive 0 #f))
    (if msg
        (case msg
          ((shoot-laser) (shoot-laser! current-level 'laserP
                                       (level-player current-level)
                                       player-laser-speed))
          ((move-right)  (move-object! player player-movement-speed 0))
          ((move-left)   (move-object! player (- player-movement-speed) 0))
          (else (error "Unknown message received in manager event."))))
    
    (in manager-time-interfal manager-event))

  manager-event)

(define (game-loop level)
  (define sim (create-simulation))

  (lambda ()
    (schedule-event! sim 0 (create-invader-event level))
    (schedule-event! sim 0 (create-manager-event level))
    (start-simulation! sim +inf.0)))


(define-type rect x y width height)

(define (detect-collision? ship level)
  (define (detect-obj-col? obj1 obj2)
    (let* ((obj1-pos (game-object-pos obj1))
           (obj2-pos (game-object-pos obj2)))
      (and (not (eq? obj1 obj2))
           (rectangle-collision?
            (make-rect (pos2d-x obj1-pos) (pos2d-y obj1-pos)
                       (type-width (game-object-type obj1))
                       (type-height (game-object-type obj1)))
            (make-rect (pos2d-x obj2-pos) (pos2d-y obj2-pos)
                       (type-width (game-object-type obj2))
                       (type-height (game-object-type obj2)))))))
  
  (or (exists (lambda (inv) (detect-obj-col? ship inv))
              (level-invaders level))
      (exists (lambda (wall)
                (rectangle-collision?
                 (make-rect (pos2d-x (game-object-pos ship))
                            (pos2d-y (game-object-pos ship))
                            (type-width (game-object-type ship))
                            (type-height (game-object-type ship)))
                 (wall-rect wall)))
              (level-walls level))))


(define (rectangle-collision? r1 r2)
  (let* ((r1-x1 (rect-x r1))
         (r1-x2 (+ r1-x1 (rect-width r1)))
         (r1-x-min (min r1-x1 r1-x2))
         (r1-x-max (max r1-x1 r1-x2))
         (r1-y1 (rect-y r1))
         (r1-y2 (+ r1-y1 (rect-height r1)))
         (r1-y-min (min r1-y1 r1-y2))
         (r1-y-max (max r1-y1 r1-y2))

         (r2-x1 (rect-x r2))
         (r2-x2 (+ r2-x1 (rect-width r2)))
         (r2-x-min (min r2-x1 r2-x2))
         (r2-x-max (max r2-x1 r2-x2))
         (r2-y1 (rect-y r2))
         (r2-y2 (+ r2-y1 (rect-height r2)))
         (r2-y-min (min r2-y1 r2-y2))
         (r2-y-max (max r2-y1 r2-y2)))
    (if (or (< r1-x-max r2-x-min)
            (> r1-x-min r2-x-max)
            (< r1-y-max r2-y-min)
            (> r1-y-min r2-y-max))
        #f
        #t)))

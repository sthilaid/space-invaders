(include "scm-lib.scm")
(include "event-simulation.scm")

(define invader-row-number 5)
(define invader-col-number 11)
(define ship-movement-speed 2)
(define invader-spacing 16)


(define-type pos2d x y)

(define (inverse-dir dir . options)
  (let ((x-fact (if (memq 'x options) -1 1))
        (y-fact (if (memq 'y options) -1 1)))
    (make-pos2d (* x-fact (pos2d-x dir))
               (* y-fact (pos2d-y dir)))))

(define-type ship type pos state speed
  extender: define-type-of-ship)

(define-type-of-ship invader-ship row col)
(define-type-of-ship player-ship)

(define-type ship-type id height width)
(define types
  `( (easy ,(make-ship-type 'easy 8 12))
     (medium ,(make-ship-type 'medium 8 11))
     (hard ,(make-ship-type 'hard 8 8))
     (mothership ,(make-ship-type 'mothership 7 16))
     (player ,(make-ship-type 'player 8 13))
     (side-wall ,(make-ship-type 'side-wall 125 1))
     (horiz-wall ,(make-ship-type 'horiz-wall 1 200))
   ))
(define (get-type type-name)
  (let ((type (assq type-name types)))
  (if type
      (cadr type)
      (error (string-append "no such type: " type-name)))))

(define type-id ship-type-id)
(define type-height ship-type-height)
(define type-width ship-type-width)


(define-type wall-struct rect)
(define (new-wall x y width height)
  (make-wall-struct (make-rect x y width height)))
(define wall-rect wall-struct-rect)

(define-type level-struct height width invaders player walls)
(define level-height level-struct-height)
(define level-width level-struct-width)
(define level-invaders level-struct-invaders)
(define level-player level-struct-player)
(define level-walls level-struct-walls)

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
                (cons (make-invader-ship current-type pos state speed row col)
                      invaders))))))

  (let* ((walls (list (new-wall 0 0 +inf.0 -inf.0)
                      (new-wall 0 0 -inf.0 +inf.0)
                      (new-wall max-x max-y -inf.0 +inf.0)
                      (new-wall max-x max-y +inf.0 -inf.0)))
         (player-type (get-type 'player))
         (pos (make-pos2d 22 (- max-y 240)))
         (state 1)
         (speed (make-pos2d 0 0))
         (player-ship (make-player-ship player-type pos state speed)))
    (make-level-struct max-y max-x invaders player-ship walls)))

(define (cycle-state state) (modulo (+ state 1) 2))

(define (move-ship! ship delta-x delta-y)
  (let* ((pos (ship-pos ship) )
         (x (pos2d-x pos))
         (y (pos2d-y pos))
         (speed (ship-speed ship))
         (state (ship-state ship)))
    (ship-state-set! ship (cycle-state state))
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
         (old-dx (pos2d-x (ship-speed (car row-invaders))))
         (dx (if collision? (* old-dx -1) old-dx))
         (dy (if collision? (- invader-spacing) 0)))

    (for-each (lambda (inv) (move-ship! inv dx dy))
              row-invaders)))

(define (create-invader-event level)
  (define event-time-interval 1)
  (define (next-event row-index)
    (lambda ()
      (move-ship-row! level row-index)
      (in event-time-interval
          (next-event (modulo (+ row-index 1) invader-row-number)))))
  (next-event 0))
        

(define (create-game-loop-thunk level)
  (define sim (create-simulation))
  
  (schedule-event! sim 0 (create-invader-event level))
  (lambda ()
    (simulation-step! sim)))


(define-type rect x y width height)

(define (detect-collision? ship level)
  (define (detect-ship-col? ship1 ship2)
    (let* ((ship1-pos (ship-pos ship1))
           (ship2-pos (ship-pos ship2)))
      (and (not (eq? ship1 ship2))
           (rectangle-collision?
            (make-rect (pos2d-x ship1-pos) (pos2d-y ship1-pos)
                       (type-width (ship-type ship1))
                       (type-height (ship-type ship1)))
            (make-rect (pos2d-x ship2-pos) (pos2d-y ship2-pos)
                       (type-width (ship-type ship2))
                       (type-height (ship-type ship2)))))))
  
  (or (exists (lambda (inv) (detect-ship-col? ship inv))
              (level-invaders level))
      (exists (lambda (wall)
                (rectangle-collision?
                 (make-rect (pos2d-x (ship-pos ship))
                            (pos2d-y (ship-pos ship))
                            (type-width (ship-type ship))
                            (type-height (ship-type ship)))
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

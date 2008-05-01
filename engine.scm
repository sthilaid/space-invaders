(include "scm-lib.scm")
(include "event-simulation.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define screen-max-x 228)
(define screen-max-y 265)
(define wall-offset 14)
(define gamefield-max-x (- screen-max-x wall-offset))
(define gamefield-max-y (- screen-max-y wall-offset))


(define invader-row-number 5)
(define invader-col-number 11)
(define invader-spacing 16)

(define ship-movement-speed 1)
(define player-movement-speed 5)
(define player-laser-speed 1)
(define invader-laser-speed 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures definitions and operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 2d position coordinate ;;;;
(define-type pos2d x y)

(define (pos2d-add p1 p2)
  (make-pos2d (+ (pos2d-x p1) (pos2d-x p2))
              (+ (pos2d-y p1) (pos2d-y p2))))

(define (inverse-dir dir . options)
  (let ((x-fact (if (memq 'x options) -1 1))
        (y-fact (if (memq 'y options) -1 1)))
    (make-pos2d (* x-fact (pos2d-x dir))
               (* y-fact (pos2d-y dir)))))


;;;; Rectangle structure used in collision detection ;;;;
(define-type rect x y width height)


;;;; General game object description ;;;;
(define-type game-object id type pos state speed
  extender: define-type-of-game-object)

(define (cycle-state! obj)
  (game-object-state-set!
   obj (modulo (+ (game-object-state obj) 1)
               (object-type-state-num (game-object-type obj)))))

(define (get-bounding-box obj)
  (make-rect (pos2d-x (game-object-pos obj))
             (pos2d-y (game-object-pos obj))
             (type-width (game-object-type obj))
             (type-height (game-object-type obj))))

;;;; Specific game object descriptions ;;;;
(define-type-of-game-object invader-ship row col)
(define-type-of-game-object player-ship)
(define-type-of-game-object mothership)
(define-type-of-game-object laser-obj)
(define-type-of-game-object shield particles)

(define (generate-shields)
  (define shield-type (get-type 'shield))
  (define speed (make-pos2d 0 0))
  (define (generate-particles)
    (define p make-pos2d)
    (list (p 0 0) (p 1 0) (p 2 0) (p 3 0) (p 4 0)
          (p 16 0) (p 17 0) (p 18 0) (p 19 0) (p 20 0) (p 21 0)
          
          (p 0 1) (p 1 1) (p 2 1) (p 3 1) (p 4 1)
          (p 16 1) (p 17 1) (p 18 1) (p 19 1) (p 20 1) (p 21 1)
          
          (p 0 2) (p 1 2) (p 2 2) (p 3 2) (p 4 2) (p 5 2) (p 15 2)
          (p 16 2) (p 17 2) (p 18 2) (p 19 2) (p 20 2) (p 21 2)

          (p 0 3) (p 1 3) (p 2 3) (p 3 3) (p 4 3) (p 5 3) (p 6 3) (p 14 3)
          (p 15 3)(p 16 3) (p 17 3) (p 18 3) (p 19 3) (p 20 3) (p 21 3)

          (p 0 4) (p 1 4) (p 2 4) (p 3 4) (p 4 4) (p 5 4) (p 6 4) (p 7 4)
          (p 8 4) (p 9 4) (p 10 4) (p 11 4) (p 12 4) (p 13 4) (p 14 4)
          (p 15 4)(p 16 4) (p 17 4) (p 18 4) (p 19 4) (p 20 4) (p 21 4)

          (p 0 5) (p 1 5) (p 2 5) (p 3 5) (p 4 5) (p 5 5) (p 6 5) (p 7 5)
          (p 8 5) (p 9 5) (p 10 5) (p 11 5) (p 12 5) (p 13 5) (p 14 5)
          (p 15 5)(p 16 5) (p 17 5) (p 18 5) (p 19 5) (p 20 5) (p 21 5)

          (p 0 6) (p 1 6) (p 2 6) (p 3 6) (p 4 6) (p 5 6) (p 6 6) (p 7 6)
          (p 8 6) (p 9 6) (p 10 6) (p 11 6) (p 12 6) (p 13 6) (p 14 6)
          (p 15 6)(p 16 6) (p 17 6) (p 18 6) (p 19 6) (p 20 6) (p 21 6)

          (p 0 7) (p 1 7) (p 2 7) (p 3 7) (p 4 7) (p 5 7) (p 6 7) (p 7 7)
          (p 8 7) (p 9 7) (p 10 7) (p 11 7) (p 12 7) (p 13 7) (p 14 7)
          (p 15 7)(p 16 7) (p 17 7) (p 18 7) (p 19 7) (p 20 7) (p 21 7)

          (p 0 8) (p 1 8) (p 2 8) (p 3 8) (p 4 8) (p 5 8) (p 6 8) (p 7 8)
          (p 8 8) (p 9 8) (p 10 8) (p 11 8) (p 12 8) (p 13 8) (p 14 8)
          (p 15 8)(p 16 8) (p 17 8) (p 18 8) (p 19 8) (p 20 8) (p 21 8)

          (p 0 9) (p 1 9) (p 2 9) (p 3 9) (p 4 9) (p 5 9) (p 6 9) (p 7 9)
          (p 8 9) (p 9 9) (p 10 9) (p 11 9) (p 12 9) (p 13 9) (p 14 9)
          (p 15 9)(p 16 9) (p 17 9) (p 18 9) (p 19 9) (p 20 9) (p 21 9)

          (p 0 10) (p 1 10) (p 2 10) (p 3 10) (p 4 10) (p 5 10) (p 6 10)
          (p 7 10) (p 8 10) (p 9 10) (p 10 10) (p 11 10) (p 12 10) (p 13 10)
          (p 14 10)(p 15 10)(p 16 10) (p 17 10) (p 18 10) (p 19 10) (p 20 10)
          (p 21 10)

          (p 0 11) (p 1 11) (p 2 11) (p 3 11) (p 4 11) (p 5 11) (p 6 11)
          (p 7 11) (p 8 11) (p 9 11) (p 10 11) (p 11 11) (p 12 11) (p 13 11)
          (p 14 11)(p 15 11)(p 16 11) (p 17 11) (p 18 11) (p 19 11) (p 20 11)
          (p 21 11)

          (p 1 12) (p 2 12) (p 3 12) (p 4 12) (p 5 12) (p 6 12)
          (p 7 12) (p 8 12) (p 9 12) (p 10 12) (p 11 12) (p 12 12) (p 13 12)
          (p 14 12)(p 15 12)(p 16 12) (p 17 12) (p 18 12) (p 19 12) (p 20 12)

          (p 2 13) (p 3 13) (p 4 13) (p 5 13) (p 6 13)
          (p 7 13) (p 8 13) (p 9 13) (p 10 13) (p 11 13) (p 12 13) (p 13 13)
          (p 14 13)(p 15 13)(p 16 13) (p 17 13) (p 18 13) (p 19 13)
          
          (p 3 14) (p 4 14) (p 5 14) (p 6 14) (p 7 14) (p 8 14) (p 9 14)
          (p 10 14) (p 11 14) (p 12 14) (p 13 14) (p 14 14)(p 15 14)
          (p 16 14) (p 17 14) (p 18 14)

          (p 4 15) (p 5 15) (p 6 15) (p 7 15) (p 8 15) (p 9 15)
          (p 10 15) (p 11 15) (p 12 15) (p 13 15) (p 14 15)(p 15 15)
          (p 16 15) (p 17 15)))
  
  (list (make-shield 'shield1 shield-type (make-pos2d  36 40) 0
                     speed (generate-particles))
        (make-shield 'shield2 shield-type (make-pos2d  81 40) 0
                     speed (generate-particles))
        (make-shield 'shield3 shield-type (make-pos2d 126 40) 0
                     speed (generate-particles))
        (make-shield 'shield4 shield-type (make-pos2d 171 40) 0
                     speed (generate-particles))))


;;;; Game object type definition ;;;;
(define-type object-type id width height state-num value)
(define type-id object-type-id)
(define type-height object-type-height)
(define type-width object-type-width)

;; Global associative list of all object types
(define types
  ;; Bounding boxes for all ship types must be equal such that they
  ;; behave the same way in the level.
  `( (easy ,(make-object-type 'easy 12 8 2 10))
     (medium ,(make-object-type 'medium 12 8 2 20))
     (hard ,(make-object-type 'hard 12 8 2 30))
     (mothership ,(make-object-type 'mothership 16 7 1 100))
     (player ,(make-object-type 'player 13 8 1 0))
     (laserA ,(make-object-type 'laserA 3 6 2 0))
     (laserB ,(make-object-type 'laserB 3 6 3 0))
     (laserP ,(make-object-type 'laserP 1 5 1 0))
     (shield ,(make-object-type 'shield 22 16 1 0))
     (explodeI ,(make-object-type 'explodeI 13 8 1 0))
   ))

(define (get-type type-name)
  (let ((type (assq type-name types)))
  (if type
      (cadr type)
      (error (string-append "no such type: " type-name)))))


;;;; Wall or game boundary structure ;;;;
(define-type wall-struct rect)
(define (new-wall x y width height)
  (make-wall-struct (make-rect x y width height)))
(define wall? wall-struct?)
(define wall-rect wall-struct-rect)

(define (generate-walls)
  (list (new-wall wall-offset 0 +inf.0 -inf.0)
        (new-wall wall-offset 0 -inf.0 +inf.0)
        (new-wall gamefield-max-x screen-max-y -inf.0 +inf.0)
        (new-wall gamefield-max-x screen-max-y +inf.0 -inf.0)))


;;;; Game level description ;;;;
(define-type level height width object-table walls shields score)
(define (level-add-object! lvl obj)
   (table-set! (level-object-table lvl) (game-object-id obj) obj))

(define (level-remove-object! lvl obj)
   (table-set! (level-object-table lvl) (game-object-id obj)))

(define (level-all-objects lvl)
   (map cdr (table->list (level-object-table lvl))))

(define (level-invaders lvl)
   (filter invader-ship? (level-all-objects lvl)))

;; Returns (not efficiently) the list of all invaders located on the
;; specified row index or '() if none exists.
(define (get-invaders-from-row level row-index)
  (filter (lambda (inv) (= (invader-ship-row inv) row-index))
            (level-invaders level)))

(define (get-all-invader-rows level)
  (let loop ((i 0) (acc '()))
    (if (< i invader-row-number)
        (loop (+ i 1) (cons (get-invaders-from-row level i) acc))
        (reverse (cleanse acc)))))
          
(define (level-player lvl)
   (table-ref (level-object-table lvl) 'player))

(define (level-player-laser lvl)
   (table-ref (level-object-table lvl) 'player-laser #f))

(define (level-mothership lvl)
   (table-ref (level-object-table lvl) 'mothership #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Level Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-level)
  (define invaders '())
  (define x-offset 30)
  (define y-offset (- 265 152))
  (define (determine-type-id y)
    (cond ((< y 2) 'easy)
          ((< y 4) 'medium)
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

  (let* ((walls (generate-walls))
         (shields (generate-shields))
         (player-type (get-type 'player))
         (pos (make-pos2d 22 (- screen-max-y 240)))
         (state 1)
         (speed (make-pos2d 0 0))
         (player-ship (make-player-ship 'player
                                        player-type pos state speed))
         (lvl (make-level screen-max-y screen-max-x (make-table)
                          walls shields 0)))
    (for-each (lambda (x) (level-add-object! lvl x)) invaders)
    (level-add-object! lvl player-ship)
    lvl))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gameplay procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move-object! obj)
  (let* ((pos (game-object-pos obj) )
         (x (pos2d-x pos))
         (y (pos2d-y pos))
         (speed (game-object-speed obj))
         (dx (pos2d-x speed))
         (dy (pos2d-y speed)))
    (cycle-state! obj)
    (pos2d-x-set! pos (+ x dx))
    (pos2d-y-set! pos (+ y dy))))

(define (move-ship-row! level row-index)
  (for-each (lambda (inv) (move-object! inv))
            (get-invaders-from-row level row-index)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simulation Events and Game Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-invader-move-refresh-rate
  ;; the sleep delay is a function such that when the level is full of
  ;; invaders (55 invaders) then the delay is 0.1 and when there is
  ;; no invader left, it is 0.01. Thus the equation system:
  ;; 55x + xy = 1/10 and 0x + xy = 1/100 was solved.
  (let* ((min-delta 1/100)
         (max-delta 1/10)
         (max-inv-nb 55)
         (slope (/ (- max-delta min-delta) max-inv-nb)))
    (lambda (level)
      (let ((x (length (level-invaders level))))
        (+  (* slope x) min-delta)))))

;; Event that will move a single row of invaders
(define (create-init-invader-move-event level)
  (lambda ()
    (let* ((rows (get-all-invader-rows level))
           (walls (level-walls level))
           (wall-collision?
            (exists
             (lambda (row)
               (exists (lambda (inv) (obj-wall-collision? inv walls)) row))
             rows)))
      (if (null? rows)
          (show "Game over with " (level-score level) " points.\n")
          (let ((old-dx (pos2d-x (game-object-speed (caar rows))))
                (delta-t (* (length rows)
                            (get-invader-move-refresh-rate level))))
            (if wall-collision?
                (begin
                  (schedule-invader-move! 0 0 (- invader-spacing) level)
                  (schedule-invader-move! delta-t (- old-dx) 0 level)
                  (in (* 2 delta-t) (create-init-invader-move-event level)))
                (begin
                  (schedule-invader-move! 0 old-dx 0 level)
                  (in delta-t (create-init-invader-move-event level)))))))))

(define (schedule-invader-move! init-dt dx dy level)
  (define dt (get-invader-move-refresh-rate level))

  (for i 0 (< i invader-row-number)
     (in (+ init-dt (* i dt))
         (lambda ()
           (for-each (lambda (inv) (let ((speed (make-pos2d dx dy)))
                                     (game-object-speed-set! inv speed)))
                     (get-invaders-from-row level i))
           (move-ship-row! level i)))))

;; Creates a new mothership and schedules its first move event.
(define (create-new-mothership-event level)
  (lambda ()
    (let ((mothership
           (make-mothership 'mothership (get-type 'mothership)
                            (make-pos2d wall-offset 201) 0 (make-pos2d 1 0))))
    (level-add-object! level mothership)
    (in 0 (create-mothership-event level)))))
    
;; Event that moves a mothership and handles its collisions.
(define (create-mothership-event level)
  (define mothership-update-interval 0.02)
  (define (mothership-event)
    (define mothership (level-mothership level))
    (if mothership
        (begin
          (move-object! mothership)
          (let ((collision-obj (detect-collision? mothership level)))
            (if collision-obj
                (begin (cond ((wall? collision-obj)
                              (level-remove-object! level mothership))
                             ((laser-obj? collision-obj)
                              (level-score-set!
                               level
                               (+ (level-score level)
                                  (object-type-value
                                   (game-object-type mothership))))
                              (explode-invader! level mothership)))
                       ;; Schedule next mothership
                       (let ((delta-t (+ (random-integer 3) 1)))
                         (in delta-t (create-new-mothership-event level))))
                (in mothership-update-interval mothership-event))))))
  mothership-event)
    

;; An invader laser event wraps up a shoot-laser! such that it will
;; create a new laser that will come from a candidate invader (one
;; that is in front of the player).
;;
;; The current implementation is very innefficient (O(n^2)).
(define (create-invader-laser-event level)
  (define (rect-inv-collision? rect)
    (lambda (inv)
      (let* ((pos (game-object-pos inv))
             (type (game-object-type inv))
             (width (type-width type))
             (heigth (type-height type))
             (inv-rect (make-rect (pos2d-x pos) (pos2d-y pos) width heigth)))
        (rectangle-collision? rect inv-rect))))
                                  
  (define (bottom-invader? inv)
    (let* ((pos (game-object-pos inv))
           (rect (make-rect (pos2d-x pos) (- (pos2d-y pos) 1)
                            invader-spacing (- (pos2d-y pos)))))
      (not (exists (rect-inv-collision? rect) (level-invaders level)))))
                       
  (define (get-candidates)
    (filter bottom-invader? (level-invaders level)))

  (lambda ()
    (let* ((candidates (get-candidates))
           (canditate-nb (length candidates))
           (shooting-invader
            (if (> canditate-nb 0)
                (list-ref candidates (random-integer (length candidates)))
                #f)))
      (if shooting-invader
          (shoot-laser! level
                        (list-ref (list 'laserA 'laserB) (random-integer 2))
                        shooting-invader
                        (- invader-laser-speed))))))

;; Wrapper function over create-laser-event which will create a new
;; laser object instance of specifiex type and place it correctly next
;; to the shooting object.
(define (shoot-laser! level laser-type shooter-obj dy)
  (if (not (and (eq? laser-type 'laserP)
                (level-player-laser level)))
      (let* ((shooter-x (pos2d-x (game-object-pos shooter-obj)))
             (shooter-y (pos2d-y (game-object-pos shooter-obj)))
             (x (+ shooter-x
                   (floor (/ (type-width (game-object-type shooter-obj)) 2))))
             (y (if (< dy 0)
                    (- shooter-y (type-height (get-type laser-type)))
                    (+ shooter-y
                       (type-height (game-object-type shooter-obj)))))
             (laser-id (if (eq? laser-type 'laserP)
                           'player-laser
                           (gensym 'inv-laser)))
             (laser-obj (make-laser-obj
                         laser-id
                         (get-type laser-type)
                         (make-pos2d x y)
                         0
                         (make-pos2d 0 dy))))
        (in 0 (create-laser-event laser-obj level))
        (level-add-object! level laser-obj))))

;; Will generate the events associated with a laser object such that
;; it will be moved regularly dy pixels on the y axis. The game logic
;; of a laser is thus defined by the returned event.
(define (create-laser-event laser-obj level)
  (define player-laser-update-interval 0.005)
  (define invader-laser-update-interval 0.01)
  (define next-invader-laser-interval 0.5)
  (define (laser-event)
    (move-object! laser-obj)
    (let ((collision-obj (detect-collision? laser-obj level)))
      (if collision-obj
          (begin
            ;;(show "collision occured with " collision-obj "\n")
            (cond ((invader-ship? collision-obj) 
                   (level-score-set!
                    level (+ (level-score level)
                             (object-type-value
                              (game-object-type collision-obj))))
                   (explode-invader! level collision-obj))
              
                  ((and (laser-obj? collision-obj)
                        (not (eq? collision-obj (level-player-laser level))))
                   (level-remove-object! level collision-obj)
                   (in next-invader-laser-interval
                       (create-invader-laser-event level)))
             
                  ((player-ship? collision-obj)
                   (pp 'todo-lose-one-life-and-restart))

                  ((shield? collision-obj)
                   (pp 'damage-shield))

                  ((mothership? collision-obj)
                   (level-score-set!
                    level (+ (level-score level)
                             (object-type-value
                              (game-object-type collision-obj))))
                   (explode-invader! level collision-obj)
                   (let ((delta-t (+ (random-integer 3) 1)))
                     (in delta-t (create-new-mothership-event level)))))
            
            (if (not (eq? laser-obj (level-player-laser level)))
                (in next-invader-laser-interval
                    (create-invader-laser-event level)))
            (level-remove-object! level laser-obj))
          
          ;; if no collisions, continue on with the laser motion
          (let ((delta-t (if (eq? (level-player-laser level) laser-obj)
                             player-laser-update-interval
                             invader-laser-update-interval)))
            (in delta-t laser-event)))))

  laser-event)

;; dispalys an explosion where the invader was and removes it from the
;; level. This will freeze the game events, as it seem to do in the
;; original game.
(define (explode-invader! level inv)
  (define animation-duration 0.3)
  (game-object-type-set! inv (get-type 'explodeI))
  (game-object-state-set! inv 0)
  (in animation-duration (create-explode-invader-end-event! level inv)))

;; Event that will stop an invader explosion animation
(define (create-explode-invader-end-event! level inv)
  (lambda ()
    (level-remove-object! level inv)))

;; the manager event is a regular event that polls and handle user
;; input by looking into the thread's mailbox. It is assumed that the
;; discrete event simulation is perfomed in it's own thread and that
;; user input is passed to the simulation via this mechanism.
(define (create-manager-event current-level)
  (define manager-time-interfal 0.005)
  (define player (level-player current-level))

  (define (manager-event)
    (define msg (thread-receive 0 #f))
    (if msg
        (case msg
          ((shoot-laser) (shoot-laser! current-level 'laserP
                                       (level-player current-level)
                                       player-laser-speed))
          ((move-right)
           (let ((new-speed (make-pos2d player-movement-speed 0)))
             (game-object-speed-set! player new-speed))
           (move-object! player))
          
          ((move-left)
           (let ((new-speed (make-pos2d (- player-movement-speed) 0)))
             (game-object-speed-set! player new-speed))
           (move-object! player))
          
          ((show-score) (pp `(score is ,(level-score current-level))))
          (else (error "Unknown message received in manager event."))))
    (in manager-time-interfal manager-event))

  manager-event)

;; Event that will send a message to the ui asking for a redraw.
(define (create-redraw-event ui-thread level)
  (define refresh-rate 0.05)
  (define (duplicate obj) obj) ;;TODO: Dummy duplication!!
  (define (redraw-event)
    (thread-send ui-thread (duplicate level))
    (in refresh-rate redraw-event))
  redraw-event)

;; Setup of initial game events and start the simulation.
(define (game-loop ui-thread level)
  (define sim (create-simulation))

  (lambda ()
    (schedule-event! sim 0 (create-init-invader-move-event level))
    (schedule-event! sim 0 (create-manager-event level))
    (schedule-event! sim 1 (create-invader-laser-event level))
    (schedule-event! sim 0 (create-redraw-event ui-thread level))
    (schedule-event! sim (+ (random-integer 3) 1)
                     (create-new-mothership-event level))
    (start-simulation! sim +inf.0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns #f is the object is not colliding, else returns an object
;; which is in collision with obj. Only one object is return, even if
;; multiple collision are occurring.
(define (detect-collision? obj level)
  ;; exists is exptected to return the object that satisfy the condition
  (or (exists (lambda (collision-obj) (obj-obj-collision? obj collision-obj))
              (level-all-objects level))
      (obj-shield-collision? obj (level-shields level))
      (obj-wall-collision? obj (level-walls level))))

;; collision detection between 2 game objects
(define (obj-obj-collision? obj1 obj2)
  (let* ((obj1-pos (game-object-pos obj1))
         (obj2-pos (game-object-pos obj2)))
    (and (not (eq? obj1 obj2))
         (rectangle-collision?
          (get-bounding-box obj1)
          (get-bounding-box obj2)))))

(define (obj-wall-collision? obj walls)
  (exists (lambda (wall)
            (rectangle-collision?
             (get-bounding-box obj)
             (wall-rect wall)))
          walls))

(define (obj-shield-collision? obj shields)
  (exists (lambda (shield)
            (if (obj-obj-collision? obj shield)
                (let* ((pos (game-object-pos shield)))
                  (exists (lambda (particle)
                            (point-rect-collision? (pos2d-add pos particle)
                                                   (get-bounding-box obj)))
                          (shield-particles shield)))
                #f))
          shields))

;; Simple rectangular collision detection. Not optimized.
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
    (not (or (< r1-x-max r2-x-min)
             (> r1-x-min r2-x-max)
             (< r1-y-max r2-y-min)
             (> r1-y-min r2-y-max)))))

(define (point-rect-collision? point rect)
  (let* ((rect-x1 (rect-x rect))
         (rect-x2 (+ rect-x1 (rect-width rect)))
         (rect-x-min (min rect-x1 rect-x2))
         (rect-x-max (max rect-x1 rect-x2))
         (rect-y1 (rect-y rect))
         (rect-y2 (+ rect-y1 (rect-height rect)))
         (rect-y-min (min rect-y1 rect-y2))
         (rect-y-max (max rect-y1 rect-y2))
         (point-x (pos2d-x point))
         (point-y (pos2d-y point)))
    (and (>= point-x rect-x-min)
         (<= point-x rect-x-max)
         (>= point-y rect-y-min)
         (<= point-y rect-y-max))))
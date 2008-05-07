(include "scm-lib.scm")
(include "ppm-reader.scm")
(include "event-simulation.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define screen-max-x 228)
(define screen-max-y 265)
(define wall-offset 14)
(define screen-bottom-y-offset 9)
(define gamefield-max-x (- screen-max-x wall-offset))
(define gamefield-max-y (- screen-max-y wall-offset))


(define invader-row-number 5)
(define invader-col-number 11)
;; (define invader-row-number 2)
;; (define invader-col-number 5)

(define invader-spacing 16)

(define invader-x-movement-speed 2)
(define invader-y-movement-speed 8)
(define player-movement-speed 2)
(define player-laser-speed 1)
(define invader-laser-speed 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures definitions and operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 2d position coordinate additionnal operations ;;;;
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
  (make-rect (+ (pos2d-x (game-object-pos obj))
                (rect-x (object-type-bbox (game-object-type obj))))
             (+ (pos2d-y (game-object-pos obj))
                (rect-y (object-type-bbox (game-object-type obj))))
             (type-width (game-object-type obj))
             (type-height (game-object-type obj))))

;;;; Specific game object descriptions ;;;;
(define-type-of-game-object invader-ship row col)
(define-type-of-game-object player-ship)
(define-type-of-game-object mothership)
(define-type-of-game-object laser-obj)
(define-type-of-game-object shield particles)

;;;; Game object type definition ;;;;
(define-type object-type id bbox state-num score-value)
(define type-id object-type-id)
(define (type-height t) (rect-height (object-type-bbox t)))
(define (type-width t) (rect-width (object-type-bbox t)))

;; Global associative list of all object types
(define types
  ;; Bounding boxes for all ship types must be equal such that they
  ;; behave the same way in the level.
  `( (easy ,(make-object-type 'easy (make-rect 0 0 12 8) 2 10))
     (medium ,(make-object-type 'medium (make-rect 0 0 12 8) 2 20))
     (hard ,(make-object-type 'hard (make-rect 0 0 12 8) 2 30))
     (mothership ,(make-object-type 'mothership (make-rect 0 0 16 7) 1 100))
     (player ,(make-object-type 'player (make-rect 0 0 13 8) 1 0))
     (laserA ,(make-object-type 'laserA (make-rect 1 0 1 6) 2 0))
     (laserB ,(make-object-type 'laserB (make-rect 1 0 1 6) 3 0))
     (laserP ,(make-object-type 'laserP (make-rect 0 0 1 5) 1 0))
     (shield ,(make-object-type 'shield (make-rect 0 0 22 16) 1 0))
     (explodeI ,(make-object-type 'explodeI (make-rect 0 0 13 8) 1 0))
     (explodeInvL ,(make-object-type 'explodeInvL (make-rect 0 0 6 8) 1 0))
     (explodeS ,(make-object-type 'explodeS (make-rect 0 0 8 8) 1 0))
     (explodeP ,(make-object-type 'explodeP (make-rect 0 0 16 8) 2 0))
   ))

(define (get-type type-name)
  (let ((type (assq type-name types)))
  (if type
      (cadr type)
      (error (string-append "no such type: " type-name)))))

;;;; centered explosion particle positions ;;;;
(define invader-laser-explosion-particles
  (rgb-pixels-to-boolean-point-list
   (parse-ppm-image-file "sprites/explodeInvL0.ppm") 'dont-center))

(define player-laser-explosion-particles
  (rgb-pixels-to-boolean-point-list
   (parse-ppm-image-file "sprites/explodeL0.ppm") 'center))


;;;; Shields ;;;;
(define (generate-shields)
  (define shield-type (get-type 'shield))
  (define speed (make-pos2d 0 0))
  (define (generate-particles)
      (rgb-pixels-to-boolean-point-list
       (parse-ppm-image-file "sprites/shield0.ppm")))

  (list (make-shield 'shield1 shield-type (make-pos2d  36 40) 0
                     speed (generate-particles))
        (make-shield 'shield2 shield-type (make-pos2d  81 40) 0
                     speed (generate-particles))
        (make-shield 'shield3 shield-type (make-pos2d 126 40) 0
                     speed (generate-particles))
        (make-shield 'shield4 shield-type (make-pos2d 171 40) 0
                     speed (generate-particles))))

(define (get-laser-penetration-pos laser-obj)
  (let* ((delta 3)
         (pos (game-object-pos laser-obj))
         (dy (pos2d-y (game-object-speed laser-obj)))
         (delta-vect 
          (cond ((< dy 0) (make-pos2d 0 (- delta)))
                ((> dy 0) (make-pos2d 0 delta))
                (else (make-pos2d 0 0)))))
    (pos2d-add pos delta-vect)))


(define (shield-explosion! shield explosion-pos explosion-speed
                           explosion-particles)
  (define pos (game-object-pos shield))
  (define particles (shield-particles shield))
  
  (define relative-expl-particles
    (let ((relative-expl-pos (pos2d-sub explosion-pos
                                        pos)))
      (map (lambda (ex-part) (pos2d-add ex-part relative-expl-pos))
         explosion-particles)))
  
  (define (particle-member p p-list)
    (if (not (pair? p-list))
        #f
        (if (pos2d= p (car p-list))
            p-list
            (particle-member p (cdr p-list)))))
  
  (define new-particles
    (filter (lambda (p) (not (particle-member p relative-expl-particles)))
            particles))
  (shield-particles-set! shield new-particles))



;;;; Wall or game boundary structure ;;;;
(define-type wall-struct rect)
(define (new-wall x y width height)
  (make-wall-struct (make-rect x y width height)))
(define wall? wall-struct?)
(define wall-rect wall-struct-rect)

(define (generate-walls)
  (list (new-wall wall-offset screen-bottom-y-offset +inf.0 -inf.0) ;;
        (new-wall wall-offset screen-bottom-y-offset -inf.0 +inf.0)
        (new-wall gamefield-max-x screen-max-y -inf.0 +inf.0)
        (new-wall gamefield-max-x screen-max-y +inf.0 -inf.0)))


;;;; Game level description ;;;;
(define-type level height width object-table walls shields score lives)
(define (level-add-object! lvl obj)
   (table-set! (level-object-table lvl) (game-object-id obj) obj))

(define (level-remove-object! lvl obj)
   (table-set! (level-object-table lvl) (game-object-id obj)))

(define (level-all-objects lvl)
   (map cdr (table->list (level-object-table lvl))))

(define (level-invaders lvl)
   (filter invader-ship? (level-all-objects lvl)))

(define (level-loose-1-life! lvl)
  (level-lives-set! lvl (- (level-lives lvl) 1))
  (if (<= (level-lives lvl) 0) (pp 'todo-game-over?)))

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
   (table-ref (level-object-table lvl) 'player #f))

(define (level-player-laser lvl)
   (table-ref (level-object-table lvl) 'player-laser #f))

(define (level-mothership lvl)
   (table-ref (level-object-table lvl) 'mothership #f))

(define (new-player! level)
  (let* ((player-type (get-type 'player))
         (pos (make-pos2d 22 (- screen-max-y 240)))
         (state 0)
         (speed (make-pos2d 0 0))
         (player-ship (make-player-ship 'player
                                        player-type pos state speed)))
    (level-add-object! level player-ship)))

  

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
               (speed (make-pos2d invader-x-movement-speed 0))
               (row h)
               (col w))
          (set! invaders
                (cons (make-invader-ship (gensym 'inv)
                                         current-type pos state speed row col)
                      invaders))))))

  (let* ((walls (generate-walls))
         (shields (generate-shields))
         (lvl (make-level screen-max-y screen-max-x (make-table)
                          walls shields 0 3)))
    (for-each (lambda (x) (level-add-object! lvl x)) invaders)
    (new-player! lvl)
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
          (let* ((old-dx (pos2d-x (game-object-speed (caar rows))))
                 (dt (get-invader-move-refresh-rate level))
                 (duration (* (length rows) dt)))
                             
            (if wall-collision?
                (begin
                  (in 0 (create-invader-row-move-event!
                         dt 0 (- invader-y-movement-speed) level))
                  (in duration
                      (create-invader-row-move-event! dt (- old-dx) 0 level))
                  (in (* 2 duration) (create-init-invader-move-event level)))
                (begin
                  (in 0 (create-invader-row-move-event! dt old-dx 0 level))
                  (in duration (create-init-invader-move-event level)))))))))

(define (create-invader-row-move-event! dt dx dy level)
  (define rows (get-all-invader-rows level))
  (define (inv-row-move-event row-index)
    (lambda ()
      (if (< row-index invader-row-number)
          (let ((current-row (get-invaders-from-row level row-index)))
            (if (not (null? current-row))
                (begin 
                  (for-each
                   (lambda (inv) (let ((speed (make-pos2d dx dy)))
                                   (game-object-speed-set! inv speed)))
                   current-row)
                  (move-ship-row! level row-index)
                  (in dt (inv-row-move-event (+ row-index 1))))
                ((inv-row-move-event (+ row-index 1))))))))
  (inv-row-move-event 0))

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
                                  (object-type-score-value
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
  (define next-invader-laser-interval 0.2)
  (define type (game-object-type laser-obj))
  (define (laser-event)
    ;; centered laser position (depending on the laser type...
    (define pos (let ((pos (game-object-pos laser-obj)))
                  (pos2d-add pos
                             (make-pos2d (floor (/ (type-width type) 2)) 0))))
    
    (move-object! laser-obj)
    (let ((collision-obj (detect-collision? laser-obj level)))
      (if collision-obj
          (begin
            ;;(show "collision occured with " collision-obj "\n")
            (cond ((invader-ship? collision-obj) 
                   (level-score-set!
                    level (+ (level-score level)
                             (object-type-score-value
                              (game-object-type collision-obj))))
                   (explode-invader! level collision-obj))
              
                  ((and (laser-obj? collision-obj)
                        (not (eq? collision-obj (level-player-laser level))))
                   (level-remove-object! level collision-obj)
                   (in next-invader-laser-interval
                       (create-invader-laser-event level)))
             
                  ((player-ship? collision-obj)
                   (level-loose-1-life! level)
                   (explode-player! level collision-obj))

                  ((shield? collision-obj)
                   (let ((penetrated-pos
                          (get-laser-penetration-pos laser-obj)))
                     (explode-laser! level laser-obj)
                     (shield-explosion!
                      collision-obj
                      penetrated-pos
                      (game-object-speed laser-obj)
                      (if (eq? (type-id type) 'laserP)
                          player-laser-explosion-particles
                          invader-laser-explosion-particles))))

                  ((mothership? collision-obj)
                   (level-score-set!
                    level (+ (level-score level)
                             (object-type-score-value
                              (game-object-type collision-obj))))
                   (explode-invader! level collision-obj)
                   (let ((delta-t (+ (random-integer 3) 1)))
                     (in delta-t (create-new-mothership-event level))))

                  ((wall? collision-obj)
                   (explode-laser! level laser-obj)))

            (level-remove-object! level laser-obj)
            (if (not (eq? (type-id type) 'laserP))
                (in next-invader-laser-interval
                    (create-invader-laser-event level))))
            
          
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
  (in animation-duration (create-explosion-end-event! level inv)))

(define (explode-player! level player)
  (define animation-duration 1.5)
  (define expl-obj
    (make-game-object (gensym 'explosion)
                      (get-type 'explodeP)
                      (game-object-pos player)
                      0 (make-pos2d 0 0)))
  (level-add-object! level expl-obj)
  (level-remove-object! level player)
  (in 0
      (player-explosion-animation-event level expl-obj animation-duration))
  (in (+ animation-duration 0.000001) (lambda () (new-player! level))))

(define (player-explosion-animation-event level expl-obj duration)
  (define frame-rate 0.3)
  (define init-time (time->seconds (current-time)))
  (define (animation-ev dt)
    (lambda ()
      (cycle-state! expl-obj)
      (if (< dt duration)
          (in frame-rate (animation-ev (+ dt frame-rate)))
          (level-remove-object! level expl-obj))))
  (animation-ev 0))

(define (explode-laser! level laser-obj)
  (define animation-duration 0.3)
  (define (laser-type-id) (object-type-id (game-object-type laser-obj)))
  (define expl-type (if (eq? (laser-type-id) 'laserP)
                        (get-type 'explodeS)
                        (get-type 'explodeInvL)))
  (define (center-pos pos)
    ;; FIXME: ugly hack where only player laser's explotion are
    ;; centered in x. I'm unsure why other lasers don't require this
    ;; shift so far...
    (if (eq? (laser-type-id) 'laserP)
        (pos2d-sub pos (make-pos2d (floor (/ (type-width expl-type) 2))
                                   0))
        pos))
  (define obj
    (make-game-object (gensym 'explosion)
                      expl-type
                      (center-pos (get-laser-penetration-pos laser-obj))
                      0 (make-pos2d 0 0)))
  (level-add-object! level obj)
  (in animation-duration (create-explosion-end-event! level obj)))


;; Event that will stop an invader explosion animation
(define (create-explosion-end-event! level inv)
  (lambda ()
    (level-remove-object! level inv)))

;; the manager event is a regular event that polls and handle user
;; input by looking into the thread's mailbox. It is assumed that the
;; discrete event simulation is perfomed in it's own thread and that
;; user input is passed to the simulation via this mechanism.
(define (create-manager-event current-level)
  (define manager-time-interfal 0.005)
  
  (define (manager-event)
    (define player (level-player current-level))
    (define msg (thread-receive 0 #f))
    (if msg
        (case msg
          ((shoot-laser)
           (if player
               (shoot-laser! current-level 'laserP
                             (level-player current-level)
                             player-laser-speed)))
          ((move-right)
           (if player
               (let ((new-speed (make-pos2d player-movement-speed 0)))
                 (game-object-speed-set! player new-speed)
                 (move-object! player))))
          
          ((move-left)
           (if player
               (let ((new-speed (make-pos2d (- player-movement-speed) 0)))
                 (game-object-speed-set! player new-speed)
                 (move-object! player))))
          
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
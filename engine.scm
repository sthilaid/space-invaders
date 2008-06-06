;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: engine.scm
;;
;; description: Space invaders game engine. This source file contains
;; all the definition required to model closely the orignal arcade
;; version of space invaders, as can be seen on mame. The heart of the
;; game is located into the game-loop function which has the task to
;; start intro and game levels, handle hi-scores, etc...
;;
;; Here is the global architecture of the game model when an intro
;; animation is being played:
;;
;; Gambit-thread:     game-loop
;;                        |
;;                        |
;;                        |
;; Event sim:     (play-level intro-lvl)
;;
;; Here is the global architecture of the game model when a game is
;; being played:
;;
;; Gambit-thread:     game-loop
;;                    |        \
;;                    |         \ (optionnal for 2p games)
;;                    |          \
;; Coroutines:    p1-corout <-> p2-corout
;;                    |              \
;;                    |               \
;; Event sim:  (play-level lvl-p1) (play-level lvl-p2)
;;
;; The model was designed to be as loosely coupled with the user
;; interface as possible, but some coupling is necessary to be able to
;; respond to user input inside the model. Maybe this is not the best
;; way, but currently the user input is managed in the model. Also,
;; the model controls the redraw commands that are to be sent to the
;; user interface via gambit thread's mailbox system.
;;
;; All the game is designed to run in a discrete event
;; simulation. This approach is a mix of a threading system and a
;; coroutine system. Once an event gets the control of the simulation,
;; it has it as long as it likes (which might be evil). But the usage
;; of the event simulation enables to schedule nicely events in time
;; such that the flow occurs naturally. There is basically two main
;; types of events used in this game: Thread like types which
;; automacially reschedule themselves in the futur (similar to a
;; yield), and some animation threads which might reschedule
;; themselves for a certain duration, but will eventually pass the
;; event's place in the simulation to a *continuation* event. Thus the
;; animations and some game events are using CPS to enable an easy
;; abstraction and modularity of theses events.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(include "event-simulation-macro.scm")

;;*****************************************************************************
;;
;;                            Global constants
;;
;;*****************************************************************************

(define screen-max-x 228)
(define screen-max-y 265)
(define wall-x-offset 14)
(define screen-bottom-offset 9)
(define screen-top-offset 51)

(define gamefield-min-x wall-x-offset)
(define gamefield-min-y screen-bottom-offset)
(define gamefield-max-x (- screen-max-x wall-x-offset))
(define gamefield-max-y (- screen-max-y screen-top-offset))

(define user-interface-thread #f)

(define invader-row-number 5)
(define invader-col-number 11)
;; (define invader-row-number 2)
;; (define invader-col-number 2)

(define invader-spacing 16)

(define invader-x-movement-speed 2)
(define invader-y-movement-speed 8)
(define player-movement-speed 2)
(define player-laser-speed 2)
(define invader-laser-speed 2)
(define mothership-movement-speed 1)

(define player-laser-last-destruction-time 0)


;; Simulation delays
(define mothership-update-interval 0.02)
(define player-laser-update-interval 0.001)
(define invader-laser-update-interval 0.02)
(define next-invader-laser-interval 0.2)
(define manager-time-interfal 0.001)
(define redraw-interval 0.01)
(define animation-end-wait-delay 2)


(define player-laser-refresh-constraint 0.6)

(define (mothership-random-delay) (+ (random-integer 10) 5))

(define get-invader-move-refresh-rate
  ;; the sleep delay is a function such that when the level is full of
  ;; invaders (55 invaders) then the delay is 0.1 and when there is
  ;; no invader left, it is 0.01. Thus the equation system:
  ;; 55x + xy = 1/10 and 0x + xy = 1/100 was solved.
  (let* ((min-delta 0.0001)
         (max-delta 0.15)
         (max-inv-nb 55)
         (slope (/ (- max-delta min-delta) max-inv-nb)))
    (lambda (level)
      (let ((x (length (level-invaders level))))
        (+  (* slope x) min-delta)))))




;;*****************************************************************************
;;
;;             Multiplayer coroutines stuff
;;
;;*****************************************************************************

;; dynamically scoped coroutines pointers that should be available
;; inside 2 players level simulations
(define p1-corout (make-parameter #f))
(define p2-corout (make-parameter #f))

(define (send-update-msg-to-other level finished?)
  (let ((msg (cons (game-level-score level) finished?)))
    (if (eq? (game-level-player-id level) 'p2)
        (! (p1-corout) msg)
        (! (p2-corout) msg))))

(define (receive-update-msg-from-other! level)
  (if (and (2p-game-level? level)
           (not (corout-empty-mailbox?)))
      (let ((msg (?)))
        (2p-game-level-other-score-set! level (car msg))
        (2p-game-level-other-finished?-set! level (cdr msg)))))



;;*****************************************************************************
;;
;;             Data Structures definitions and operations
;;
;;*****************************************************************************

;;;; 2d position coordinate additionnal operations ;;;;
(define (inverse-dir dir . options)
  (let ((x-fact (if (memq 'x options) -1 1))
        (y-fact (if (memq 'y options) -1 1)))
    (make-pos2d (* x-fact (pos2d-x dir))
               (* y-fact (pos2d-y dir)))))


;;;; Rectangle structure used in collision detection ;;;;
(define-type rect x y width height)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main game object definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; General game object description ;;;;
(define-type game-object id type pos state color speed
  extender: define-type-of-game-object)

(define (cycle-state! obj)
  (define current-state (game-object-state obj))
  (if (number? current-state)
      (game-object-state-set!
       obj (modulo (+ current-state 1)
                   (object-type-state-num (game-object-type obj))))))

(define (get-bounding-box obj)
  (make-rect (+ (pos2d-x (game-object-pos obj))
                (rect-x (object-type-bbox (game-object-type obj))))
             (+ (pos2d-y (game-object-pos obj))
                (rect-y (object-type-bbox (game-object-type obj))))
             (type-width (game-object-type obj))
             (type-height (game-object-type obj))))

;; In function of a certain coordinate, will choose the appropriate
;; color for the object (red if in the upper-screen, white in the
;; middle and green in the lower part).
(define choose-color
  (let ((red-bottom (- screen-max-y 81))
        (normal-bottom (- screen-max-y 201))
        (green-bottom (- screen-max-y 259)))
    (lambda (pos)
      (let ((y (pos2d-y pos)))
        (cond 
         ((> y red-bottom) 'red)
         ((> y normal-bottom) 'white)
         ((> y green-bottom) 'green)
         (else 'white))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other object derived types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Specific game object descriptions ;;;;
(define-type-of-game-object invader-ship row col)
(define-type-of-game-object player-ship)
(define-type-of-game-object mothership)
(define-type-of-game-object laser-obj)
(define-type-of-game-object shield particles)
(define-type-of-game-object message-obj text)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; object type definition ;;;;
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
     ;; Little bounding box hack here to facilitate the laser
     ;; penetration into shields.
     (laserA ,(make-object-type 'laserA (make-rect 1 0 1 7) 6 0))
     (laserB ,(make-object-type 'laserB (make-rect 1 0 1 7) 8 0))
     (laserC ,(make-object-type 'laserC (make-rect 1 0 1 7) 4 0))
     (player_laser
      ,(make-object-type 'player_laser (make-rect 0 0 1 7) 1 0))
     (shield ,(make-object-type 'shield (make-rect 0 0 22 16) 1 0))
     (invader_explosion
      ,(make-object-type 'invader_explosion (make-rect 0 0 13 8) 1 0))
     (invader_laser_explosion
      ,(make-object-type 'invader_laser_explosion (make-rect 0 0 6 8) 1 0))
     (player_laser_explosion
      ,(make-object-type 'player_laser_explosion (make-rect 0 0 8 8) 1 0))
     (player_explosion
      ,(make-object-type 'player_explosion (make-rect 0 0 16 8) 2 0))
     (mothership_explosion
      ,(make-object-type 'mothership_explosion (make-rect 0 0 21 8) 1 0))
     (message ,(make-object-type 'message (make-rect 0 0 0 0) 0 0))
   ))

(define (get-type type-name)
  (let ((type (assq type-name types)))
  (if type
      (cadr type)
      (error (string-append "no such type: " (symbol->string type-name))))))

(define (get-type-id obj)
  (cond ((game-object? obj)
         (object-type-id (game-object-type obj)))
        ((wall-struct? obj) 'wall)
        (else (error "unknown object type"))))

;; Returns #t only if the type-id corresponds to an explosion
(define (is-explosion? type-id)
  (case type-id
    ((invader_laser_explosion
      player_laser_explosion invader_explosion player_explosion) #t)
    (else #f)))

(define (get-object-score-value obj)
  (object-type-score-value (game-object-type obj)))
      
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Particles objects and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function used in the rgb-pixels-to-boolean-point-list calls to
;; filter only pixels having some color in it. This will result in a
;; point cloud corresponding to the read image.
(define (rgb-threshold? r g b)
  (> (+ r g b) 150))

;;;; centered explosion particle positions ;;;;
(define invader-laser-explosion-particles
  (rgb-pixels-to-boolean-point-list
   (parse-ppm-image-file "sprites/explodeInvL0.ppm")
   rgb-threshold? 'dont-center))

;; Generate a rectangle of points that should correspond to the
;; specified invader's bounding box. This is used with the
;; invader-shield collision resolution.
(define (invader-ship-particles inv)
  (define height (type-height (game-object-type inv)))
  (define width (type-width (game-object-type inv)))
  (let loop-y ((y 0) (acc '()))
    (if (< y height)
        (loop-y (+ y 1)
                (append (let loop-x ((x 0) (acc '()))
                          (if (< x width)
                              (loop-x (+ x 1) (cons (make-pos2d x y) acc))
                              acc))
                        acc))
        acc)))

;; Particles of a player laser explosion
(define player-laser-explosion-particles
  (rgb-pixels-to-boolean-point-list
   (parse-ppm-image-file "sprites/explodeL0.ppm")
   rgb-threshold? 'center))

;; Calculate the interpenetration distance and add it to the laser's
;; position. 
(define (get-laser-penetration-pos laser-obj)
  (let* ((delta 2)
         (pos (game-object-pos laser-obj))
         (dy (pos2d-y (game-object-speed laser-obj)))
         (delta-vect 
          (cond ((< dy 0) (make-pos2d 0 (- delta)))
                ((> dy 0) (make-pos2d 0 delta))
                (else (make-pos2d 0 0)))))
    (pos2d-add pos delta-vect)))


;;;; Shields ;;;;
(define (generate-shields)
  (define shield-type (get-type 'shield))
  (define speed (make-pos2d 0 0))
  (define (generate-particles)
      (rgb-pixels-to-boolean-point-list
       (parse-ppm-image-file "sprites/shield0.ppm")
       rgb-threshold?))

  (list (make-shield 'shield1 shield-type (make-pos2d  36 40) 0 'green
                     speed (generate-particles))
        (make-shield 'shield2 shield-type (make-pos2d  81 40) 0 'green
                     speed (generate-particles))
        (make-shield 'shield3 shield-type (make-pos2d 126 40) 0 'green
                     speed (generate-particles))
        (make-shield 'shield4 shield-type (make-pos2d 171 40) 0 'green
                     speed (generate-particles))))

;; Get particle cloud associated with a certain colliding object.
(define (get-explosion-particles colliding-obj)
  (let ((type-id (type-id (game-object-type colliding-obj))))
    (cond ((eq? type-id 'player_laser) player-laser-explosion-particles)
          ((or (eq? type-id 'laserA)
               (eq? type-id 'laserB)
               (eq? type-id 'laserC))
           invader-laser-explosion-particles)
          (else
           (invader-ship-particles colliding-obj)))))

;; Damage the givent shield such that all the particles inside the
;; colliding-obj are removed from the shield's particles.
(define (shield-explosion! shield colliding-obj)
  (define explosion-particles (get-explosion-particles colliding-obj))
  (define explosion-pos (game-object-pos colliding-obj))
  (define explosion-speed (game-object-speed colliding-obj))
  (define shield-pos (game-object-pos shield))
  (define particles (shield-particles shield))
  
  (define relative-expl-particles
    (let ((relative-expl-pos (pos2d-sub explosion-pos
                                        shield-pos)))
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


;;;; Wall ;;;;
(define (damage-wall! level laser-obj)
  (define explosion-particles (get-explosion-particles laser-obj))
  (define pos (game-object-pos laser-obj))
  (define wall-damage
    (map 
     (lambda (p) (pos2d-add p pos))
     (filter
      (lambda (p) (= (pos2d-y p) 0))
      explosion-particles)))
  (level-damage-wall! level wall-damage))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wall data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Wall or game boundary structure ;;;;
(define-type wall-struct rect id)
(define (new-wall x y width height id)
  (make-wall-struct (make-rect x y width height ) id))
(define wall? wall-struct?)
(define wall-rect wall-struct-rect)
(define wall-id wall-struct-id)

(define (generate-walls)
  (list (new-wall wall-x-offset screen-bottom-offset +inf.0 -inf.0 'bottom)
        (new-wall wall-x-offset screen-bottom-offset -inf.0 +inf.0 'left)
        (new-wall gamefield-max-x gamefield-max-y -inf.0 +inf.0 'top)
        (new-wall gamefield-max-x gamefield-max-y +inf.0 -inf.0 'right)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level related data structure and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Game level description ;;;;
(define-type level
  height width object-table hi-score sim mutex
  extender: define-type-of-level)

;; A word should be mentionned on the draw-game-field? parameter since
;; it is mostly a hack to let know the user interface not to draw all
;; the invaders, shields, etc... This will occur in a 2p mid-game when
;; switching context from 1 player to another and that a PLAY
;; PLAYER<X> will be redisplayed. I am conscient that this is not
;; clean, but it greatly simplifies the problem.
(define-type-of-level game-level
  player-id score lives walls wall-damage draw-game-field?
  extender: define-type-of-game-level)

(define-type-of-game-level 2p-game-level
  other-finished? other-score)

;; Level utilitary functions
(define (level-add-object! lvl obj)
  (table-set! (level-object-table lvl) (game-object-id obj) obj))

(define (level-remove-object! lvl obj)
  (table-set! (level-object-table lvl) (game-object-id obj)))

(define (level-exists lvl obj-id)
  (table-ref (level-object-table lvl) obj-id #f))

(define level-get level-exists)

(define (level-all-objects lvl)
  (map cdr (table->list (level-object-table lvl))))

(define (level-invaders lvl)
  (filter invader-ship? (level-all-objects lvl)))

(define (level-messages lvl)
  (filter message-obj? (level-all-objects lvl)))

(define (game-level-shields level)
  (filter shield? (level-all-objects level)))

(define (level-loose-1-life! lvl)
  (game-level-lives-set! lvl (- (game-level-lives lvl) 1)))

(define (level-increase-score! level obj)
  (game-level-score-set! level
                    (+ (game-level-score level)
                       (get-object-score-value obj))))

(define (level-damage-wall! level damage)
  (define current-damage (game-level-wall-damage level))
  (game-level-wall-damage-set! level (union current-damage damage)))

(define (game-over! level)
  (show "Game over with " (game-level-score level) " points.\n")
  ;;(exit-simulation (game-level-score level)))
  (terminate-corout (game-level-score level)))


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
                                        player-type pos state 'green speed)))
    (level-add-object! level player-ship)))

(define (play-level level)
  (start-simulation! (level-sim level) +inf.0))

(define (get-score-string score)
  (cond ((= score 0) "0000")
        ((< score 10) (string-append "000" (number->string score)))
        ((< score 100) (string-append "00" (number->string score)))
        ((< score 1000) (string-append "0" (number->string score)))
        (else (number->string score))))


;;*****************************************************************************
;;
;;                           Game Level Creation
;;
;;*****************************************************************************

;; Abstraction that will add to the level all the usual top banner
;; messages.
(define (add-global-score-messages! level)
  (define y 254)
  (define type (get-type 'message))
  (define state 'white)
  (define speed (make-pos2d 0 0))
  (define hi-score (level-hi-score level))
  (for-each
   (lambda (m) (level-add-object! level m))
   (append 
    (list
     (make-message-obj 'top-banner type (make-pos2d 2 y) state 'white speed
                       "SCORE<1>  HI-SCORE  SCORE<2>")
     (make-message-obj 'hi-score-msg type
                       (make-pos2d 95 (- y 17)) state 'white speed
                       (get-score-string hi-score)))
    (if (game-level? level)
        (list
         (make-message-obj 'player2-score-msg type
                           (make-pos2d 175 (- y 17)) state 'white speed "")
         (make-message-obj 'player1-score-msg type
                           (make-pos2d 15 (- y 17)) state 'white speed ""))
        '()))))

;; New game level generation. Works for both single and 2 player
;; games, but the behaviour will differ a bit in the 2 cases.
(define (new-level init-score hi-score number-of-players player-id)
  (let* ((walls (generate-walls))
         (wall-damage '())
         (shields (generate-shields))
         (sim (create-simulation))
         (lives 3)
         (draw-game-field? #t)
         (other-finished? #f)  ;; only used for 2p games
         (other-score 0)       ;; only used for 2p games
         (level (if (= number-of-players 2)

                    (make-2p-game-level
                     screen-max-y screen-max-x (make-table)
                     hi-score sim (new-mutex)
                     player-id init-score lives 
                     walls wall-damage draw-game-field?
                     other-finished? other-score)
                    
                    (make-game-level
                     screen-max-y screen-max-x (make-table)
                     hi-score sim (new-mutex)
                     player-id init-score lives 
                     walls wall-damage draw-game-field?))))
    
    (add-global-score-messages! level)
    (for-each (lambda (s) (level-add-object! level s)) shields)

    ;; Schedule the initial game animation and start events
    (schedule-event!
     sim 0
     (start-of-game-animation-event
      level 
      (generate-invaders-event
       level
       (lambda ()
         (new-player! level)
         (in 0 (create-init-invader-move-event level))
         (in 1 (create-invader-laser-event level))
         (in (mothership-random-delay)
             (create-new-mothership-event level))))))

    ;; Also schedule manager events
    (schedule-event! sim 0 (create-main-manager-event level))
    (schedule-event! sim 0 (create-redraw-event user-interface-thread level))
    level))

;; Creation of the default initial intro movie
(define (new-animation-level-A hi-score)
  (let* ((sim (create-simulation))
         (level (make-level screen-max-y screen-max-x (make-table)
                            hi-score sim (new-mutex))))
    (add-global-score-messages! level)
    
    (schedule-event! sim 0 (create-animation-A-event level))
    (schedule-event! sim 0 (create-intro-manager-event level))
    (schedule-event! sim 0 (create-redraw-event user-interface-thread level))
    level))

;; Creation of the instructions intro movie
(define (new-animation-level-B hi-score)
  (let* ((sim (create-simulation))
         (level (make-level screen-max-y screen-max-x (make-table)
                            hi-score sim (new-mutex))))
    (add-global-score-messages! level)
    
    (schedule-event! sim 0 (create-animation-B-event level))
    (schedule-event! sim 0 (create-intro-manager-event level))
    (schedule-event! sim 0 (create-redraw-event user-interface-thread level))
    level))

;; Creation of a new demo movie. The demo movie is a tweaked normal
;; game level where the user input manager is the same as for the
;; other intro movies, and a special AI event is scheduled to control
;; the player. The demo should stop as soon as the player dies.
(define (new-animation-level-demo hi-score)
  (let* ((walls (generate-walls))
         (wall-damage '())
         (shields (generate-shields))
         (sim (create-simulation))
         (player-id 'demo)
         (lives 3)
         (score 0)
         (draw-game-field? #t)
         (other-finished? #f)  ;; only used for 2p games
         (other-score 0)       ;; only used for 2p games
         (level (make-game-level
                 screen-max-y screen-max-x (make-table)
                 hi-score sim (new-mutex)
                 player-id score lives 
                 walls wall-damage draw-game-field?)))
    
    (add-global-score-messages! level)
    (level-add-object!
     level
     (make-message-obj 'demo-msg (get-type 'message)
                       (make-pos2d 100 (- screen-max-y 60))
                       'dummy 'white (make-pos2d 0 0) "DEMO"))
    (for-each (lambda (s) (level-add-object! level s)) shields)

    ;; Schedule initial demo game events
    (schedule-event!
     sim 0
     (generate-invaders-event
      level
      (lambda ()
        (new-player! level)
        (schedule-event! sim 0 (create-ai-player-event level))
        ;; Extra: The flashing demo was not present in the arcade
        ;; version, but I believe it is better to avoid confusion...
        (in 0 (create-text-flash-animation-event
               level
               (level-get level 'demo-msg)
               +inf.0 end-of-continuation-event))
        (in 0 (create-init-invader-move-event level))
        (in 1 (create-invader-laser-event level))
        (in (mothership-random-delay)
            (create-new-mothership-event level)))))

    (schedule-event! sim 0 (create-intro-manager-event level))
    (schedule-event! sim 0 (create-redraw-event user-interface-thread level))
    level))



;;*****************************************************************************
;;
;;                         Movement Gameplay
;;
;;*****************************************************************************

;; Will perform the object move. If a collision is detected, the
;; collision resolution is automatically called and the result of the
;; collision resolution (usually the obj with whoom the collision
;; occured will be returned. Thus, a simple object movement is not
;; limited to moving the obj, but may imply much more calculation
;; depending on the collision type. If no collision is detected, #f is
;; returned.
(define (move-object! level obj)
  (define (move-object-raw! obj)
    (let* ((pos (game-object-pos obj) )
           (x (pos2d-x pos))
           (y (pos2d-y pos))
           (speed (game-object-speed obj))
           (dx (pos2d-x speed))
           (dy (pos2d-y speed)))
      (cycle-state! obj)
      (game-object-color-set! obj (choose-color pos))
      (pos2d-x-set! pos (+ x dx))
      (pos2d-y-set! pos (+ y dy))))
  
  ;; 1st move the object, then detect/respond to a collision if
  ;; required.
  (move-object-raw! obj)
  (let ((collision-obj (detect-collision? obj level)))
    (if collision-obj
        (resolve-collision! level obj collision-obj)
        #f)))
  
;; Moves the entire ship row of specified index
(define (move-ship-row! level row-index)
  (for-each (lambda (inv) (move-object! level inv))
            (get-invaders-from-row level row-index)))




;;*****************************************************************************
;;
;;                         Collision Management
;;
;;*****************************************************************************


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns #f is the object is not colliding, else returns an object
;; which is in collision with obj. Only one object is return, even if
;; multiple collision are occurring.
(define (detect-collision? obj level)
  ;; exists is exptected to return the object that satisfy the condition
  (or (exists (lambda (collision-obj)
                (if (shield? collision-obj)
                    (obj-shield-collision? obj collision-obj)
                    (obj-obj-collision? obj collision-obj)))
              (level-all-objects level))
;;      (obj-shield-collision? obj (game-level-shields level))
      (obj-wall-collision? obj (game-level-walls level))))

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

(define (obj-shield-collision? obj shield)
  (if (obj-obj-collision? obj shield)
      (let* ((pos (game-object-pos shield)))
        (exists (lambda (particle)
                  (point-rect-collision? (pos2d-add pos particle)
                                         (get-bounding-box obj)))
                (shield-particles shield)))
      #f))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision resolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Will resolve the collision between obj and collision-obj, depending
;; on their types.
(define (resolve-collision! level obj collision-obj)
  (cond
   ((player-ship? obj) (resolve-player-collision! level obj collision-obj))
   ((laser-obj? obj) (resolve-laser-collision! level obj collision-obj))
   ((invader-ship? obj) (resolve-invader-collision! level obj collision-obj))
   ((mothership? obj) (resolve-mothership-collision! level obj collision-obj))
   (else
    (error "cannot resolve object collision.")))
  (get-type-id collision-obj))

;; Abstraction used to simplify the resolve-laser-collision!
;; function. This will remove the laser object from the level and
;; depending on the laser type, might re-scheduled a new laser shot.
(define (destroy-laser! level laser-obj)
  (level-remove-object! level laser-obj)
  (if (not (eq? (object-type-id (game-object-type laser-obj)) 'player_laser))
      (in next-invader-laser-interval (create-invader-laser-event level))
      (set! player-laser-last-destruction-time
            (time->seconds (current-time)))))

(define (resolve-laser-collision! level laser-obj collision-obj)
  (define type-id (object-type-id (game-object-type laser-obj)))

  ;; Collision resolution is straightforward
  (cond ((invader-ship? collision-obj) 
         (level-increase-score! level collision-obj)
         (explode-invader! level collision-obj)
         (destroy-laser! level laser-obj))
        
        ((laser-obj? collision-obj)
         (let ((inv-laser
                (if (not (eq? type-id 'player_laser)) laser-obj collision-obj)))
           (explode-laser! level inv-laser)
           (destroy-laser! level inv-laser)))
        
        ((player-ship? collision-obj)
         (explode-player! level collision-obj)
         (destroy-laser! level laser-obj))

        ((shield? collision-obj)
         (let ((penetrated-pos (get-laser-penetration-pos laser-obj)))
           (game-object-pos-set! laser-obj penetrated-pos)
           (explode-laser! level laser-obj)
           (shield-explosion! collision-obj laser-obj))
         (destroy-laser! level laser-obj))

        ((mothership? collision-obj)
         (destroy-laser! level laser-obj)
         (explode-mothership! level collision-obj)
         (level-increase-score! level collision-obj)
         (let ((delta-t (mothership-random-delay)))
           (in delta-t (create-new-mothership-event level))))

        ((wall? collision-obj)
         (damage-wall! level laser-obj)
         (explode-laser! level laser-obj)
         (destroy-laser! level laser-obj))))

  
(define (resolve-invader-collision! level invader collision-obj)
  (cond ((laser-obj? collision-obj)
         (resolve-laser-collision! level collision-obj invader))
        ((shield? collision-obj)
         (shield-explosion! collision-obj invader))
        ((wall? collision-obj)
         (if (eq? (wall-id collision-obj) 'bottom)
             (game-over! level)))))

(define (resolve-player-collision! level player collision-obj)
  (cond ((wall? collision-obj)
         (let ((current-speed (game-object-speed player)))
           (game-object-speed-set! player
                                   (make-pos2d (- (pos2d-x current-speed))
                                               (pos2d-y current-speed)))
           (move-object! level player)))
        
        ((laser-obj? collision-obj)
         (resolve-laser-collision! level collision-obj player))
        
        ((invader-ship? collision-obj)
         (explode-player! level collision-obj)
         (explode-invader! level collision-obj))))

(define (resolve-mothership-collision! level mothership collision-obj)
  (cond ((wall? collision-obj)
         (level-remove-object! level mothership)
         (let ((delta-t (mothership-random-delay)))
           (in delta-t (create-new-mothership-event level))))
        
        ((laser-obj? collision-obj)
         (resolve-laser-collision! level collision-obj mothership))))
  


;;*****************************************************************************
;;
;;                 Gameplay related simulation events 
;;
;;*****************************************************************************

;; Macro used to synchronize certain game events, such that the game
;; may be paused.
(define-macro (synchronized-event-thunk level action . actions)
  `(lambda ()
     (critical-section! (level-mutex ,level)
        ,action
        ,@actions)))

;; A dummy event that just stop the continuation of a CPS event
(define (end-of-continuation-event) 'eoc-event)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of game level animations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Will animate a flashing PLAY PLAYER<X> in a black screen.
(define (start-of-game-animation-event level continuation)
  (define animation-duration 3)
  (define player-id (game-level-player-id level))
  (lambda ()
    (let* ((pos (make-pos2d 61 (- screen-max-y 136)))
           (type (get-type 'message))
           (state 'white)
           (speed (make-pos2d 0 0))
           (text (if (eq? player-id 'p2)
                     "PLAY  PLAYER<2>"
                     "PLAY  PLAYER<1>"))
           (msg (make-message-obj 'start-msg type pos state 'white speed text))
           (new-cont
            (lambda ()
              (level-remove-object! level msg)
              (game-level-draw-game-field?-set! level #t)
              (in 0 continuation))))
      (game-level-draw-game-field?-set! level #f)
      (level-add-object! level msg)
      (let ((score-msg-obj (if (eq? player-id 'p2)
                               'player2-score-msg
                               'player1-score-msg)))
        (in 0 (create-text-flash-animation-event
               level
               (level-get level score-msg-obj)
               animation-duration new-cont))))))

;; Generate all the invaders in a level, with a little animation which
;; displays them one after the other with a small dt interval.
(define (generate-invaders-event level continuation)
  (define animation-delay 0.01)
  (define x-offset 30)
  (define y-offset (- 265 152))

  (define (determine-type-id col)
    (cond ((< col 2) 'easy)
          ((< col 4) 'medium)
          (else 'hard)))

  (define (generate-inv! row col)
    (let* ((x (+ x-offset (* col invader-spacing)))
           (y (+ y-offset (* row invader-spacing)))
           (pos (make-pos2d x y))
           (current-type (get-type (determine-type-id row)))
           (state 1)
           (speed (make-pos2d invader-x-movement-speed 0))
           (invader
            (make-invader-ship
             (gensym 'inv) current-type pos state 'white speed row col)))
      (level-add-object! level invader)))
                         
  (define (generate-inv-event row col)
    (synchronized-event-thunk level
     (if (< row invader-row-number)
         (if (< col invader-col-number)
             (begin (generate-inv! row col)
                    (in animation-delay (generate-inv-event row (+ col 1))))
             (in 0 (generate-inv-event (+ row 1) 0)))
         (in animation-delay continuation))))
  (generate-inv-event 0 0))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gameplay related simulation events 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Event that will move a single row of invaders
(define (create-init-invader-move-event level)
  (synchronized-event-thunk level
    (let* ((rows (get-all-invader-rows level))
           (walls (game-level-walls level))
           (wall-collision?
            (exists
             (lambda (row)
               (exists (lambda (inv) (obj-wall-collision? inv walls)) row))
             rows)))
      (if (null? rows)
          ;; Regenerate invaders when they all died
          (in 0 (generate-invaders-event
                 level (create-init-invader-move-event level)))
          (let* ((old-dx (pos2d-x (game-object-speed (caar rows))))
                 (dt (get-invader-move-refresh-rate level))
                 (duration (* (length rows) dt)))
            (if wall-collision?
                (begin
                  (in 0 (create-invader-row-move-event!
                         dt 0 (- invader-y-movement-speed) level
                         (create-invader-wall-movement-continuation-event
                          old-dx level))))
                (begin
                  (in 0 (create-invader-row-move-event!
                         dt old-dx 0 level
                         (create-init-invader-move-event level))))))))))

;; Animation continuation that will be used after a wall collision to
;; make the invader move in the opposite x direction.
(define (create-invader-wall-movement-continuation-event old-dx level)
  (synchronized-event-thunk level
    (let ((rows (get-all-invader-rows level)))
      (if (null? rows)
          ;; Regenerate invaders when they all died
          (in 0 (generate-invaders-event
                 level (create-init-invader-move-event level)))
          (let* ((dt (get-invader-move-refresh-rate level)))
            (in 0
                (create-invader-row-move-event!
                 dt (- old-dx) 0 level
                 (create-init-invader-move-event level))))))))

;; Will move the invaders, one row at a time of dx and dy, then call
;; continuation event will be scheduled
(define (create-invader-row-move-event! dt dx dy level continuation)
  (define rows (get-all-invader-rows level))
  (define (inv-row-move-event row-index)
    (synchronized-event-thunk level
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
                (in 0 (inv-row-move-event (+ row-index 1)))))
          (in dt continuation))))
  (inv-row-move-event 0))

;; Creates a new mothership and schedules its first move event.
(define (create-new-mothership-event level)
  (synchronized-event-thunk level
    (let* ((dx-mult (list-ref '(1 -1) (random-integer 2)))
           (mothership
            (make-mothership 'mothership
                             (get-type 'mothership)
                             (make-pos2d (if (> dx-mult 0)
                                             gamefield-min-x
                                             (- gamefield-max-x 16))
                                         201)
                            0
                            'red
                            (make-pos2d (* dx-mult mothership-movement-speed)
                                        0))))
    (level-add-object! level mothership)
    (in 0 (create-mothership-event level)))))

;; Event that moves a mothership and handles its collisions.
 (define (create-mothership-event level)
  (define mothership-event
    (synchronized-event-thunk level
      (let ((mothership (level-mothership level)))
        (if mothership
            (let ((collision-occured? (move-object! level mothership)))
              (if (or (not collision-occured?)
                      (is-explosion? collision-occured?)
                      (eq? collision-occured? 'message))
                  (in mothership-update-interval mothership-event)))))))
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
           (rect (make-rect (pos2d-x pos)        ;; x
                            (- (pos2d-y pos) 1)  ;; y
                            (type-width (game-object-type inv)) ;; width
                            (- (pos2d-y pos))))) ;; height
      (not (exists (rect-inv-collision? rect) (level-invaders level)))))
                       
  (define (get-candidates)
    (filter bottom-invader? (level-invaders level)))

  (synchronized-event-thunk level
    (if (not (exists (lambda (obj)
                       (and (laser-obj? obj)
                            (not (eq? (object-type-id (game-object-type obj))
                                      'player_laser))))
                     (level-all-objects level)))
        (let* ((candidates (get-candidates))
               (canditate-nb (length candidates))
               (shooting-invader
                (if (> canditate-nb 0)
                    (list-ref candidates (random-integer (length candidates)))
                    #f)))
          (if shooting-invader
              (shoot-laser! level
                            (list-ref (list 'laserA 'laserB 'laserC)
                                      (random-integer 3))
                            shooting-invader
                            (- invader-laser-speed)))))))

;; Wrapper function over create-laser-event which will create a new
;; laser object instance of specifiex type and place it correctly next
;; to the shooting object.
(define (shoot-laser! level laser-type shooter-obj dy)
  ;; if the shot laser is a player laser, there must not be another
  ;; player laser in the game or the player-laser-refresh-constraint
  ;; must be elabsed before shooting a new one.
  (if (not (and (eq? laser-type 'player_laser)
                (or (level-player-laser level)
                    (< (- (time->seconds (current-time))
                          player-laser-last-destruction-time)
                       player-laser-refresh-constraint))))
                    
      (let* ((shooter-x (pos2d-x (game-object-pos shooter-obj)))
             (shooter-y (pos2d-y (game-object-pos shooter-obj)))
             (x (+ shooter-x
                   (floor (/ (type-width (game-object-type shooter-obj)) 2))))
             (y (if (< dy 0)
                    (- shooter-y
                       (type-height (get-type laser-type))
                       invader-y-movement-speed)
                    (+ shooter-y
                       (type-height (game-object-type shooter-obj)))))
             (laser-id (if (eq? laser-type 'player_laser)
                           'player-laser
                           (gensym 'inv-laser)))
             (laser-obj (make-laser-obj
                         laser-id
                         (get-type laser-type)
                         (make-pos2d x y)
                         0
                         'white
                         (make-pos2d 0 dy))))
        (level-add-object! level laser-obj)
        (in 0 (create-laser-event laser-obj level)))))
        

;; Will generate the events associated with a laser object such that
;; it will be moved regularly dy pixels on the y axis. The game logic
;; of a laser is thus defined by the returned event.
(define (create-laser-event laser-obj level)
  (define type (game-object-type laser-obj))
  (define laser-event
    (synchronized-event-thunk level
      ;; centered laser position (depending on the laser type...
      (let ((pos (let ((pos (game-object-pos laser-obj)))
                   (pos2d-add pos
                              (make-pos2d (floor (/ (type-width type) 2)) 0))))
            (collision-occured? (move-object! level laser-obj)))
        (if (or (not collision-occured?)
                (level-exists level (game-object-id laser-obj)))
            ;; if no collisions, continue on with the laser motion
            (let ((delta-t (if (eq? (level-player-laser level) laser-obj)
                               player-laser-update-interval
                               invader-laser-update-interval)))
              (in delta-t laser-event))))))
  laser-event)

;; dispalys an explosion where the invader was and removes it from the
;; level. This will freeze the game events, as it seem to do in the
;; original game.
(define (explode-invader! level inv)
  (define animation-duration 0.3)
  (game-object-type-set! inv (get-type 'invader_explosion))
  (game-object-state-set! inv 0)
  (in animation-duration
      (create-explosion-end-event! level inv end-of-continuation-event)))

;; dispalys the explosion of the mothership was and removes it from
;; the level. The points for killing that mothership will then be
;; randomly calculated and displayed after the explosion.
(define (explode-mothership! level mothership)
  (define animation-duration 0.3)
  (define pos (game-object-pos mothership))
  (define score-val (list-ref '(50 100 150) (random-integer 3)))
  (define expl-obj
    (make-game-object (gensym 'explosion)
                      (get-type 'mothership_explosion)
                      pos
                      0 (choose-color pos)
                      (make-pos2d 0 0)))
  ;; Update the global mothership's score value to the current value...
  (object-type-score-value-set! (get-type 'mothership) score-val)
  (level-remove-object! level mothership)
  (level-add-object! level expl-obj)
  (in animation-duration
      (create-explosion-end-event!
       level expl-obj
       (show-points-event level pos score-val end-of-continuation-event))))

;; Will display the points value of a mothership kill at pos
(define (show-points-event level pos points continuation)
  (define duration 1)
  (define msg
    (let ((id (gensym 'mothership-points))
          (type (get-type 'message))
          (state 'dummy-state)
          (color (choose-color pos))
          (speed (make-pos2d 0 0))
          (text (number->string points)))
    (make-message-obj id type pos state color speed text)))
  (synchronized-event-thunk
   level
   (level-add-object! level msg)
   (in duration
       (synchronized-event-thunk level
        (level-remove-object! level msg)
        (in 0 continuation)))))

;; Event related to the explosion of the player ship. The continuation
;; of this event is not trivial because of the many possibilities that
;; the game can give in. The result for a single player is straight
;; forward, but in a 2 player game, the game over animation will vary
;; depending if the other player is also game-over or not, and if the
;; player is not game over, the animation "PLAY PLAYER<X>" must be
;; pre-scheduled before yielding the coroutine such that when it gets
;; back, that animation must be loaded first.
(define (explode-player! level player)
  (define animation-duration 1.5)
  (define expl-obj
    (make-game-object (gensym 'explosion)
                      (get-type 'player_explosion)
                      (game-object-pos player)
                      0 (choose-color (game-object-pos player))
                      (make-pos2d 0 0)))
  (level-loose-1-life! level)
  (level-add-object! level expl-obj)
  (level-remove-object! level player)
  (let ((continuation
         (if (<= (game-level-lives level) 0)
             (if (2p-game-level? level)
                 (if (2p-game-level-other-finished? level)
                     (begin
                       (game-over-2p-animation-event
                        level
                        (game-over-animation-event
                         level (lambda () (game-over! level)))))
                     (begin
                       (send-update-msg-to-other level #t)
                       (game-over-2p-animation-event
                        level (lambda () (game-over! level)))))
                 (game-over-animation-event
                  level (lambda () (game-over! level))))
             (lambda ()
               (if (2p-game-level? level)
                   (begin
                     (send-update-msg-to-other level #f)
                     (in NOW! (start-of-game-animation-event
                               level (return-to-player-event level)))
                     (yield-corout))
                   (begin
                     (yield-corout)
                     (sem-unlock! (level-mutex level))
                     (new-player! level)))))))
    (in 0 (lambda ()
            (sem-lock! (level-mutex level))
            (in 0 (player-explosion-animation-event
                   level expl-obj animation-duration continuation))))))

;; Event used in 2 player games, where the games returns to the
;; current game.
(define (return-to-player-event level)
  (lambda ()
    (sem-unlock! (level-mutex level))
    (new-player! level)))
        
;; Animation of the player ship explosion
(define (player-explosion-animation-event level expl-obj duration continuation)
  (define frame-rate 0.1)
  (define (animation-ev dt)
    (lambda ()
      (cycle-state! expl-obj)
      (if (< dt duration)
          (in frame-rate (animation-ev (+ dt frame-rate)))
          (begin (level-remove-object! level expl-obj)
                 (in 0 continuation)))))
  (animation-ev 0))


;; Destruction and animation of a laser explosion.
(define (explode-laser! level laser-obj)
  (define animation-duration 0.3)
  (define (laser-type-id) (object-type-id (game-object-type laser-obj)))
  (define expl-type (if (eq? (laser-type-id) 'player_laser)
                        (get-type 'player_laser_explosion)
                        (get-type 'invader_laser_explosion)))
  (define (center-pos pos)
    ;; FIXME: ugly hack where only player laser's explotion are
    ;; centered in x. I'm unsure why other lasers don't require this
    ;; shift so far...
    (if (eq? (laser-type-id) 'player_laser)
        (pos2d-sub pos (make-pos2d (floor (/ (type-width expl-type) 2))
                                   0))
        pos))
  (define obj
    (make-game-object (gensym 'explosion)
                      expl-type
                      (center-pos (game-object-pos laser-obj))
                      0
                      (choose-color (game-object-pos laser-obj))
                      (make-pos2d 0 0)))
  (level-add-object! level obj)
  (in animation-duration
      (create-explosion-end-event! level obj end-of-continuation-event)))


;; Event that will stop an invader explosion animation
(define (create-explosion-end-event! level inv continuation)
  (synchronized-event-thunk level
    (level-remove-object! level inv)
    (in 0 continuation)))



;;*****************************************************************************
;;
;;                            Animation events 
;;
;;*****************************************************************************

;; event that will use msg-obj and will gradually add the letters
;; contained in the msg string to it, creating a type-writer effect.
(define (animate-message msg-obj msg cont)
  (define animation-delay 0.1)
  (letrec
      ((anim-event
        (lambda (str)
          (lambda ()
            (if (string=? str "")
                (in animation-delay cont)
                (let ((current-text (message-obj-text msg-obj)))
                  (message-obj-text-set!
                   msg-obj
                   (string-append current-text (substring str 0 1)))
                  (in animation-delay
                      (anim-event (substring str 1
                                              (string-length str))))))))))
    (anim-event msg)))

;; Flashing animation where the msg-obj will flash for a certain duration.
(define (create-text-flash-animation-event level msg-obj duration continuation)
  (define animation-delay 0.2)
  (define original-color (game-object-color msg-obj))
  (define (cycle-msg-state! msg-obj)
    (let ((current-color (game-object-color msg-obj)))
      (game-object-color-set!
       msg-obj
       (if (eq? current-color 'black) original-color 'black))))
  (define (flash-ev dt)
    (lambda ()
      (if (< dt duration)
          (begin (cycle-msg-state! msg-obj)
                 (in animation-delay (flash-ev (+ dt animation-delay))))
          (begin
            (game-object-color-set! msg-obj original-color)
            (in 0 continuation)))))
  (flash-ev 0))

;; Special animation that occur in the intro movie A (main intro)
(define (create-animation-A-event level)
  (define msg-type (get-type 'message))
  (define speed (make-pos2d 0 0))
  (define state 'white)

  (lambda ()
    ;; messages declaration
    (let* ((play (let ((pos (make-pos2d 101 (- screen-max-y 88))))
                   (make-message-obj
                    'play msg-type pos state (choose-color pos) speed "")))
           (space (let ((pos (make-pos2d 61 (- screen-max-y 112))))
                    (make-message-obj
                     'space msg-type pos state (choose-color pos) speed "")))
           (score (let ((pos (make-pos2d 37 (- screen-max-y 144))))
                    (make-message-obj
                     'score msg-type pos state (choose-color pos) speed "")))
           (mother (let ((pos (make-pos2d 85 (- screen-max-y 160))))
                     (make-message-obj
                      'mother msg-type pos state (choose-color pos) speed "")))
           (hard (let ((pos (make-pos2d 85 (- screen-max-y 176))))
                   (make-message-obj
                    'hard msg-type pos state (choose-color pos) speed "")))
           (medium (let ((pos (make-pos2d 85 (- screen-max-y 192))))
                     (make-message-obj
                      'medium msg-type pos state (choose-color pos) speed "")))
           (easy (let ((pos (make-pos2d 85 (- screen-max-y 208))))
                   (make-message-obj
                    'easy msg-type pos state (choose-color pos) speed "")))
           (anim-messages
            (list play space score mother hard medium easy)))
      ;; add all the messages
      (for-each (lambda (m) (level-add-object! level m)) anim-messages )
      ;; schedule the animation
      (in 0 (animate-message
             play "PLAY"
             (animate-message
              space "SPACE   INVADERS"
              (create-animate-score-adv-table-event level)))))))

;; Animation A follow-up, which will create a table showing the score
;; for killing each kind of invaders.
(define (create-animate-score-adv-table-event level)
  (define speed (make-pos2d 0 0))
  (define (pos x y) (make-pos2d x (- screen-max-y y)))
  (lambda ()
    ;; create the ship prototypes
    (let ((mothership (make-mothership 'mothership
                                       (get-type 'mothership)
                                       (pos 68 160)
                                       0
                                       (choose-color (pos 68 160))
                                       speed))
          (hard-ship (make-invader-ship 'hard-ship
                                        (get-type 'hard)
                                        (pos 72 176)
                                        0
                                        (choose-color (pos 72 176))
                                        speed
                                        0 0))
          (medium-ship (make-invader-ship 'medium-ship
                                          (get-type 'medium)
                                          (pos 71 192)
                                          0
                                          (choose-color (pos 71 192))
                                          speed
                                          0 0))
          (easy-ship (make-invader-ship 'easy-ship
                                        (get-type 'easy)
                                        (pos 70 208)
                                        0
                                        (choose-color (pos 70 208))
                                        speed
                                        0 0))
          (score-msg-obj (level-get level 'score)))
      ;; add all the new ships into the intro level
      (for-each (lambda (ship) (level-add-object! level ship))
                (list mothership hard-ship medium-ship easy-ship))
      ;; Display in one shot the score advance msg
      (message-obj-text-set! score-msg-obj "*SCORE ADVANCE TABLE*"))

    ;; and animate the other messages (where the msg-obj have been
    ;; pre-allocated in the create-animation-A-event.
    (in 0 (animate-message
           (level-get level 'mother) "=? MYSTERY"
           (animate-message
            (level-get level 'hard) "=30 POINTS"
            (animate-message
             (level-get level 'medium) "=20 POINTS"
             (animate-message
              (level-get level 'easy) "=10 POINTS"
              (lambda ()
                (in animation-end-wait-delay
                    (lambda ()
                      (set! other-animations-index
                            (modulo (+ other-animations-index 1)
                                    (length other-animations)))
                      (exit-simulation
                       (list-ref other-animations
                                 other-animations-index))))))))))))

;; Similare to intro animation A, but more simple, where some very
;; basic game instruction get displayed on screen.
(define (create-animation-B-event level)
  (define msg-type (get-type 'message))
  (define speed (make-pos2d 0 0))
  (define state 'white)

  (lambda ()
    (let* ((instruction
            (let ((pos (make-pos2d 70 (- screen-max-y 100))))
              (make-message-obj
               'instruction msg-type pos state (choose-color pos) speed "")))
           (press1 (let ((pos (make-pos2d 20 (- screen-max-y 130))))
                    (make-message-obj
                     'press1 msg-type pos state (choose-color pos) speed "")))
           (press2 (let ((pos (make-pos2d 20 (- screen-max-y 154))))
                     (make-message-obj
                      'press2 msg-type pos state (choose-color pos) speed "")))
           (score (let ((pos (make-pos2d 37 (- screen-max-y 144))))
                    (make-message-obj
                     'score msg-type pos state (choose-color pos) speed "")))
           (anim-messages
            (list instruction press1 press2 score)))
      (for-each (lambda (m) (level-add-object! level m)) anim-messages )
      (in 0 (animate-message
             instruction "INSTRUCTIONS"
             (animate-message
              press1 "PRESS '1' FOR 1 PLAYER"
              (animate-message
               press2 "PRESS '2' FOR 2 PLAYERS"
               (lambda ()
                 (in animation-end-wait-delay
                     (lambda () (exit-simulation 'intro-A)))))))))))

;; Ai reaction event. This thread-like event will simulate a player
;; behaviour, based on random "thoughts", where in fact a thought is
;; an animation either moving the player or make him shoot.
(define (create-ai-player-event level)
  (define ai-reaction-interval 0.01)
  (define ai-movement-duration-max 1)
  (define ai-movement-delay 0.02)
  
  ;; simple abastraction used in left/right movements
  (define (move-player! dx)
    (let ((player (level-player level))
          (new-speed (make-pos2d dx 0)))
      (game-object-speed-set! player new-speed)
      (move-object! level player)))

  ;; will move left the ai player for a random amount of time
  (define (create-move-left-animation-event)
    (define duration (* (random-real) ai-movement-duration-max))
    (define (move-left-event dt)
      (lambda ()
        (if (level-player level)
            (let ((collision?
                   (move-player! (- player-movement-speed))))
              (if (and (< dt duration) (not collision?))
                  (in ai-movement-delay
                      (move-left-event (+ dt ai-movement-delay)))
                  (in ai-reaction-interval (create-ai-player-event level))))
            (in NOW! end-of-demo-event))))
    (move-left-event 0))

  ;; will move right the ai player for a random amount of time
  (define (create-move-right-animation-event)
    (define duration (* (random-real) ai-movement-duration-max))
    (define (move-right-event dt)
      (lambda ()
        (if (level-player level)
            (let ((collision?
                   (move-player! player-movement-speed)))
              (if (and (< dt duration) (not collision?))
                  (in ai-movement-delay
                      (move-right-event (+ dt ai-movement-delay)))
                  (in ai-reaction-interval (create-ai-player-event level))))
            (in NOW! end-of-demo-event))))
    (move-right-event 0))

  ;; Will make the ai player shoot a laser
  (define (create-ai-laser-event)
    (lambda ()
      (if (level-player level)
          (begin
            (shoot-laser! level 'player_laser
                          (level-player level)
                          player-laser-speed)
            (in ai-reaction-interval (create-ai-player-event level)))
          (in NOW! end-of-demo-event))))

  ;; This will end the demo. NOTE: here the synchronized-event-thunk
  ;; is very important, even if the demo cannot be paused. This is so
  ;; because the demo should end only when the player explosion is
  ;; finished.
  (define end-of-demo-event
    (synchronized-event-thunk level
     (corout-kill-all!)))

  ;; If the player is dead, end the demo, elso randomly choose an
  ;; action. The action are determined using a cheap weighted
  ;; distribution.
  (if (not (level-player level))
       end-of-demo-event
       (let ((actions (list create-move-left-animation-event
                            create-move-right-animation-event
                            create-ai-laser-event
                            create-ai-laser-event
                            create-ai-laser-event
                            create-ai-laser-event
                            create-ai-laser-event
                            create-ai-laser-event)))
         ((list-ref actions (random-integer (length actions)))))))

;; Will display in the top screen the final game over message.
(define (game-over-animation-event level continuation)
  (define continuation-delay 2)
  (lambda ()
    (let* ((type (get-type 'message))
           (pos (make-pos2d 77 (- screen-max-y 60)))
           (speed (make-pos2d 0 0))
           (msg-obj (make-message-obj 'game-over-msg type
                                      pos 'red 'red speed "")))
      (level-add-object! level msg-obj)
      (in 0 (animate-message
             msg-obj "GAME OVER"
             (lambda () (in continuation-delay continuation)))))))

;; Will display in the lower scren the game over message for 1 player
;; in a 2 player game. A final game over message should be displayed
;; afterward if both player are game over.
(define (game-over-2p-animation-event level continuation)
  (define continuation-delay 1.2)
  (lambda ()
    (let* ((player-id (game-level-player-id level))
           (text (if (eq? player-id 'p2)
                     "GAME OVER PLAYER<2>"
                     "GAME OVER PLAYER<1>"))
           (type (get-type 'message))
           (pos (make-pos2d 37 (- screen-max-y 248)))
           (speed (make-pos2d 0 0))
           (msg-obj (make-message-obj 'game-over-msg type
                                      pos 'green 'green speed "")))
      (level-add-object! level msg-obj)
      (in 0 (animate-message
             msg-obj text
             (lambda () (in continuation-delay continuation)))))))

  



;;*****************************************************************************
;;
;;                              Manager events 
;;
;;*****************************************************************************

;; the manager event is a regular event that polls and handle user
;; input by looking into the thread's mailbox. It is assumed that the
;; discrete event simulation is perfomed in it's own thread and that
;; user input is passed to the simulation via this mechanism.
(define (create-main-manager-event level)
  (define game-paused? #f)
  (define (player-can-move?) (and (level-player level) (not game-paused?)))
  (define manager-event
    (lambda ()
      (let ((player (level-player level))
            (msg (thread-receive 0 #f)))
        (if msg
            (case msg
              ((#\space)
               (if (player-can-move?)
                   (shoot-laser! level 'player_laser
                                 (level-player level)
                                 player-laser-speed)))
              ((right-arrow)
               (if (player-can-move?)
                   (let ((new-speed (make-pos2d player-movement-speed 0)))
                     (game-object-speed-set! player new-speed)
                     (move-object! level player))))
              
              ((left-arrow)
               (if (player-can-move?)
                   (let ((new-speed (make-pos2d (- player-movement-speed) 0)))
                     (game-object-speed-set! player new-speed)
                     (move-object! level player))))
              
              ((#\p #\P)
               (if game-paused?
                   (sem-unlock! (level-mutex level))
                   (sem-lock! (level-mutex level)))
               (set! game-paused? (not game-paused?)))

              ((#\r #\R) (corout-kill-all!))

              ((#\d #\D) (error "DEBUG"))))
        (in manager-time-interfal manager-event))))
  manager-event)

;; Simple key manager that is used durint the intro/demo screens
(define (create-intro-manager-event level)
  (define game-paused? #f)
  (define manager-event
    (lambda ()
      (let ((msg (thread-receive 0 #f)))
        (if msg
            (case msg
              ((#\1) (exit-simulation 'start-1p-game))
              ((#\2) (exit-simulation 'start-2p-game))
              ((#\r #\R) (exit-simulation 'intro-A))
              ((#\d #\D) (error "DEBUG"))))
        (in manager-time-interfal manager-event))))
  manager-event)

;; Event that will send a message to the ui asking for a redraw.
(define (create-redraw-event ui-thread level)
  ;; will update the 2 top score messages if required.
  (define (update-score-msg! level)
    (if (game-level? level) 
        (let* ((is-player2? (eq? (game-level-player-id level) 'p2))
               (score-obj
                (level-get level (if is-player2?
                                     'player2-score-msg
                                     'player1-score-msg)))
               (other-score-obj
                (level-get level (if is-player2?
                                     'player1-score-msg
                                     'player2-score-msg))))
          (receive-update-msg-from-other! level)
          (message-obj-text-set!
           score-obj (get-score-string (game-level-score level)))
          (if (2p-game-level? level)
              (message-obj-text-set!
               other-score-obj
               (get-score-string (2p-game-level-other-score level)))))))
  (define (redraw-event)
    (update-score-msg! level)
    (thread-send ui-thread level)
    (in redraw-interval redraw-event))
  
  redraw-event)



;;*****************************************************************************
;;
;;                              Game loop
;;
;;*****************************************************************************

;; high score will be stored in this file
(define hi-score-filename ".spaceinvaders")

;; Go read from the high-score file the current high-score, if none is
;; found or if the file is mal formateed, 0 is taken as hi-score.
(define (retrieve-hi-score)
 (with-exception-catcher
   (lambda (e)
     (case e ((bad-hi-score-file) (delete-file hi-score-filename)))
     0)
   (lambda ()
     (let ((hi-score
            (with-input-from-file `(path: ,hi-score-filename)
              (lambda () (read)))))
       (if (not (number? hi-score))
           (raise 'bad-hi-score-file)
           hi-score)))))

;; Save givent hi-score to the high-score file
(define (save-hi-score hi-score)
  (with-output-to-file `(path: ,hi-score-filename create: maybe truncate: #t)
    (lambda ()
      (write hi-score))))

;; Global variables used by the intro-A event to alternate between the
;; other intro movies.
(define other-animations (list 'demo 'intro-B))
(define other-animations-index 0)

;; Main game loop that dispatch and run the different intro/game levels
(define (game-loop ui-thread)
  (define init-high-score (retrieve-hi-score))
  (set! user-interface-thread ui-thread)
    (lambda ()
      ;; infinite loop, where the result of playing a level should
      ;; either tell explicitely which level should be played next, or
      ;; return the hi-score of the last played game.
      (let inf-loop ((hi-score init-high-score)
                     (result (play-level
                              (new-animation-level-A init-high-score))))
        (cond
         ((and (number? result) (> result hi-score))
          (save-hi-score result)
          (inf-loop result
                    (play-level (new-animation-level-A result))))
         
         ((eq? result 'intro-A)
          (inf-loop hi-score
                    (play-level (new-animation-level-A hi-score))))

         ((eq? result 'intro-B)
          (inf-loop hi-score
                    (play-level (new-animation-level-B hi-score))))

         ((eq? result 'demo)
          (let ((ai (new-corout 
                     'ai (lambda ()
                           (play-level
                            (new-animation-level-demo hi-score))))))
            (inf-loop hi-score (corout-simple-boot ai))))
         
         ((eq? result 'start-1p-game)
          (let ((p1 (new-corout 
                     'p1
                     (lambda () (play-level (new-level 0 hi-score 1 'p1))))))
            (inf-loop hi-score (corout-simple-boot p1))))

         ((eq? result 'start-2p-game)
          (let ((p1
                 (new-corout
                  'p1 (lambda () (play-level (new-level 0 hi-score 2 'p1)))))
                (p2
                 (new-corout 
                  'p2 (lambda () (play-level (new-level 0 hi-score 2 'p2))))))

            (parameterize ((p1-corout p1)
                           (p2-corout p2))
              (inf-loop hi-score (corout-boot (lambda (acc v) (max acc v))
                                              p1 p2)))))
         (else
          (inf-loop hi-score
                    (play-level (new-animation-level-A hi-score))))))))

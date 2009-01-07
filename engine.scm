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
;; corout:     (play-level intro-lvl)
;;
;; Here is the global architecture of the game model when a game is
;; being played:
;;
;; Gambit-thread:     game-loop
;;                    |        \
;;                    |         \ (optionnal for 2p games)
;;                    |          \
;; Coroutines:    p1-corout     p2-corout
;;                    |              \
;;                    |               \
;; Coroutines: (play-level lvl-p1) (play-level lvl-p2)
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

(include "class.scm")
(set-manual-method-developpement!)

(include "thread-simulation-macro.scm")

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
(define player-laser-update-interval 0.0001)
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
;;                        Sound playback
;;
;;*****************************************************************************

(define (play-sound sound-sym)
  (thread-send user-interface-thread `(play-sfx ,sound-sym)))

(define (stop-sound sound-sym)
  (thread-send user-interface-thread `(stop-sfx ,sound-sym)))




;;*****************************************************************************
;;
;;             Multiplayer coroutines stuff
;;
;;*****************************************************************************

;; dynamically scoped coroutines level pointers that should be
;; available inside 2 players level simulations
(define p1-level (make-parameter #f))
(define p2-level (make-parameter #f))

(define player1 'p1)
(define player2 'p2)
(define (is-player1? level)
  (eq? (game-level-player-id level) player1))
(define (is-player2? level)
  (eq? (game-level-player-id level) player2))

(define (other-player-level level)
  (if (is-player2? level)
      (p1-level)
      (p2-level)))

(define (update-other-player-state! level)
  (if (2p-game-level? level)
      (let ((other-level (other-player-level level)))
        (2p-game-level-other-score-set! level (game-level-score other-level))
        (2p-game-level-other-finished?-set!
         level (2p-game-level-finished? other-level)))))



;;*****************************************************************************
;;
;;             Data Structures definitions and operations
;;
;;*****************************************************************************

;;;; 2d position coordinate additionnal operations ;;;;
(define (inverse-dir dir . options)
  (let ((x-fact (if (memq 'x options) -1 1))
        (y-fact (if (memq 'y options) -1 1)))
    (make-point (* x-fact (point-x dir))
               (* y-fact (point-y dir)))))


;;;; Rectangle structure used in collision detection ;;;;
(define-class point () (slot: x) (slot: y))
(define-class rect (point) (slot: width) (slot: height))

;; these are translation of the pos2d lib from scm-lib into oo
(define (point-add p1 p2) (make-point (+ (point-x p1) (point-x p2))
                                      (+ (point-y p1) (point-y p2))))
(define (point-sub p1 p2) (make-point (- (point-x p1) (point-x p2))
                                      (- (point-y p1) (point-y p2))))
(define (point-scalar-prod p1 p2)
  (+ (* (point-x p1) (point-x p2)) (* (point-y p1) (point-y p2))))
(define (point-cartesian-distance p1 p2)
  (sqrt (+ (expt (- (point-x p1) (point-x p2)) 2)
           (expt (- (point-y p1) (point-y p2)) 2))))

(define (point-complexify p) (make-rectangular (point-x p) (point-y p)))

(define (point= p1 p2)
  (and (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main game object definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; General game object description ;;;;
(define-class game-object ()
  (slot: id)
  (slot: pos)
  (slot: state)
  (slot: color)
  (slot: speed)
  (class-slot: sprite-id)
  (class-slot: bbox)
  (class-slot: state-num)
  (class-slot: score-value))

(define (cycle-state! obj)
  (define current-state (game-object-state obj))
  (if (number? current-state)
      (game-object-state-set!
       obj (modulo (+ current-state 1)
                   (game-object-state-num obj)))))

(define (get-bounding-box obj)
  (make-rect (+ (point-x (game-object-pos obj)) (rect-x (game-object-bbox obj)))
             (+ (point-y (game-object-pos obj)) (rect-y (game-object-bbox obj)))
             (type-width obj)
             (type-height obj)))

;; In function of a certain coordinate, will choose the appropriate
;; color for the object (red if in the upper-screen, white in the
;; middle and green in the lower part).
(define choose-color
  (let ((red-bottom (- screen-max-y 81))
        (normal-bottom (- screen-max-y 201))
        (green-bottom (- screen-max-y 259)))
    (lambda (pos)
      (let ((y (point-y pos)))
        (cond 
         ((> y red-bottom) 'red)
         ((> y normal-bottom) 'white)
         ((> y green-bottom) 'green)
         (else 'white))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other object derived types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (setup-static-fields! class-name
                                    sprite-id bbox state-num score-value)
  (begin
    `(,(symbol-append class-name '-sprite-id-set!)   ,sprite-id)
    `(,(symbol-append class-name '-bbox-set!)        ,bbox)
    `(,(symbol-append class-name '-state-num-set!)   ,state-num)
    `(,(symbol-append class-name '-score-value-set!) ,score-value)))

;;;; Abstract classes ;;;;

(define-class sprite-obj ())


;;;; Specific game object descriptions ;;;;

;; Evil Invaders!
(define-class invader-ship (game-object sprite-obj) (slot: row) (slot: col))
(define-class easy-invader   (invader-ship))
(define-class medium-invader (invader-ship))
(define-class hard-invader   (invader-ship))
(setup-static-fields! easy-invader 'easy (make-rect 0 0 12 8) 2 10)
(setup-static-fields! medium-invader 'medium (make-rect 0 0 12 8) 2 20)
(setup-static-fields! hard-invader 'hard (make-rect 0 0 12 8) 2 30)


(define-class player-ship  (game-object sprite-obj))
(setup-static-fields! player-ship 'player (make-rect 0 0 13 8) 1 0)

(define-class mothership   (game-object sprite-obj))
(setup-static-fields! mothership 'mothership (make-rect 0 0 16 7) 1 100)

(define-class laser-obj    (game-object sprite-obj))
(define-class laserA         (laser-obj))
(define-class laserB         (laser-obj))
(define-class laserC         (laser-obj))
(define-class player_laser   (laser-obj))
(setup-static-fields! laserA 'laserA (make-rect 1 0 1 7) 6 0)
(setup-static-fields! laserB 'laserB (make-rect 1 0 1 7) 8 0)
(setup-static-fields! laserC 'laserC (make-rect 1 0 1 7) 4 0)
(setup-static-fields! player_laser 'player_laser (make-rect 0 0 1 7) 1 0)

(define-class shield       (game-object) (slot: particles))
(setup-static-fields! shield 'shield (make-rect 0 0 22 16) 1 0)

(define-class explosion    (game-object sprite-obj))
(define-class invader_explosion       (explosion))
(define-class invader_laser_explosion (explosion))
(define-class player_explosion        (explosion))
(define-class player_laser_explosion  (explosion))
(define-class mothership_explosion    (explosion))
(setup-static-fields!
 invader_explosion 'invader_explosion (make-rect 0 0 13 8) 1 0)
(setup-static-fields!
 invader_laser_explosion 'invader_laser_explosion (make-rect 0 0 6 8) 1 0)
(setup-static-fields!
 player_explosion 'player_explosion (make-rect 0 0 16 8) 2 0)
(setup-static-fields! 
 player_laser_explosion 'player_laser_explosion (make-rect 0 0 8 8) 1 0)
(setup-static-fields!
 mothership_explosion 'mothership_explosion (make-rect 0 0 21 8) 1 0)

(define-class message-obj  (game-object) (slot: text))
(setup-static-fields! message-obj 'message (make-rect 0 0 0 0) 0 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: to remove!

;;;; object type definition ;;;;
(define-type object-type id bbox state-num score-value)
(define type-id game-object-sprite-id)
(define (type-height t) (rect-height (game-object-bbox t)))
(define (type-width t) (rect-width (game-object-bbox t)))



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
  (define height (type-height inv))
  (define width (type-width inv))
  (let loop-y ((y 0) (acc '()))
    (if (< y height)
        (loop-y (+ y 1)
                (append (let loop-x ((x 0) (acc '()))
                          (if (< x width)
                              (loop-x (+ x 1) (cons (make-point x y) acc))
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
         (dy (point-y (game-object-speed laser-obj)))
         (delta-vect 
          (cond ((< dy 0) (make-point 0 (- delta)))
                ((> dy 0) (make-point 0 delta))
                (else (make-point 0 0)))))
    (point-add pos delta-vect)))


;;;; Shields ;;;;
(define (generate-shields)
  (define speed (make-point 0 0))
  (define (generate-particles)
      (rgb-pixels-to-boolean-point-list
       (parse-ppm-image-file "sprites/shield0.ppm")
       rgb-threshold?))

  (list (make-shield 'shield1 (make-point  36 40) 0 'green
                     speed (generate-particles))
        (make-shield 'shield2 (make-point  81 40) 0 'green
                     speed (generate-particles))
        (make-shield 'shield3 (make-point 126 40) 0 'green
                     speed (generate-particles))
        (make-shield 'shield4 (make-point 171 40) 0 'green
                     speed (generate-particles))))

;; Get particle cloud associated with a certain colliding object.
(define (get-explosion-particles colliding-obj)
  (cond ((player_laser? colliding-obj) player-laser-explosion-particles)
        ((laser-obj? colliding-obj)    invader-laser-explosion-particles)
        (else (invader-ship-particles colliding-obj))))

;; Damage the givent shield such that all the particles inside the
;; colliding-obj are removed from the shield's particles.
(define (shield-explosion! shield colliding-obj)
  (define explosion-particles (get-explosion-particles colliding-obj))
  (define explosion-pos (game-object-pos colliding-obj))
  (define explosion-speed (game-object-speed colliding-obj))
  (define shield-pos (game-object-pos shield))
  (define particles (shield-particles shield))
  
  (define relative-expl-particles
    (let ((relative-expl-pos (point-sub explosion-pos
                                        shield-pos)))
      (map (lambda (ex-part) (point-add ex-part relative-expl-pos))
         explosion-particles)))
  
  (define (particle-member p p-list)
    (if (not (pair? p-list))
        #f
        (if (point= p (car p-list))
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
     (lambda (p) (point-add p pos))
     (filter
      (lambda (p) (= (point-y p) 0))
      explosion-particles)))
  (level-damage-wall! level wall-damage))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wall data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Wall or game boundary structure ;;;;
(define-class wall () (slot: rect) (slot: id))
(define (new-wall x y width height id)
  (make-wall (make-rect x y width height ) id))

(define (generate-walls)
  (list (new-wall wall-x-offset screen-bottom-offset +inf.0 -inf.0 'bottom)
        (new-wall wall-x-offset screen-bottom-offset -inf.0 +inf.0 'left)
        (new-wall gamefield-max-x gamefield-max-y -inf.0 +inf.0 'top)
        (new-wall gamefield-max-x gamefield-max-y +inf.0 -inf.0 'right)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level related data structure and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Game level description ;;;;
(define-class level ()
  (slot: height)
  (slot: width)
  (slot: object-table)
  (slot: hi-score)
  (slot: init-corouts) 
  (slot: mutex))

;; A word should be mentionned on the draw-game-field? parameter since
;; it is mostly a hack to let know the user interface not to draw all
;; the invaders, shields, etc... This will occur in a 2p mid-game when
;; switching context from 1 player to another and that a PLAY
;; PLAYER<X> will be redisplayed. I am conscient that this is not
;; clean, but it greatly simplifies the problem.
(define-class game-level (level)
  (slot: player-id)
  (slot: score)
  (slot: lives)
  (slot: walls)
  (slot: wall-damage)
  (slot: draw-game-field?))


(define-class 2p-game-level (game-level)
  (slot: finished?)
  (slot: other-finished?)
  (slot: other-score))

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
                       (game-object-score-value obj))))

(define (level-damage-wall! level damage)
  (define current-damage (game-level-wall-damage level))
  (game-level-wall-damage-set! level
                               (generic-union equal? current-damage damage)))

(define (game-over! level)
  (kill-all! (game-level-score level)))


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
  (let* ((pos (make-point 22 (- screen-max-y 240)))
         (state 0)
         (speed (make-point 0 0))
         (player-ship (make-player-ship 'player pos state 'green speed)))
    (level-add-object! level player-ship)))

(define (play-level level)
  (apply simple-boot (level-init-corouts level)))

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
  (define state 'white)
  (define speed (make-point 0 0))
  (define hi-score (level-hi-score level))
  (for-each
   (lambda (m) (level-add-object! level m))
   (append 
    (list
     (make-message-obj 'top-banner (make-point 2 y) state 'white speed
                       "SCORE<1>  HI-SCORE  SCORE<2>")
     (make-message-obj 'hi-score-msg
                       (make-point 95 (- y 17)) state 'white speed
                       (get-score-string hi-score)))
    (if (game-level? level)
        (list
         (make-message-obj 'player2-score-msg 
                           (make-point 175 (- y 17)) state 'white speed "")
         (make-message-obj 'player1-score-msg 
                           (make-point 15 (- y 17)) state 'white speed ""))
        '()))))

;; New game level generation. Works for both single and 2 player
;; games, but the behaviour will differ a bit in the 2 cases.
(define (new-level init-score hi-score number-of-players player-id)
  (let* ((walls (generate-walls))
         (wall-damage '())
         (shields (generate-shields))
         (init-corouts 'unbound)
         (lives 3)
         (draw-game-field? #t)
         (finished? #f)        ; only used for 2p games
         (other-finished? #f)  ; only used for 2p games
         (other-score 0)       ; only used for 2p games
         (level (if (= number-of-players 2)

                    (make-2p-game-level
                     screen-max-y screen-max-x (make-table)
                     hi-score init-corouts (new-mutex)
                     player-id init-score lives 
                     walls wall-damage draw-game-field?
                     finished? other-finished? other-score)
                    
                    (make-game-level
                     screen-max-y screen-max-x (make-table)
                     hi-score init-corouts (new-mutex)
                     player-id init-score lives 
                     walls wall-damage draw-game-field?)))
         (init-corouts
          (list (new-corout
                 'invader-move
                 (compose-thunks
                  ;; Intro anim
                  (start-of-game-animation level)
                  (generate-invaders level)

                  ;; Setup level and start primordial game corouts
                  (lambda ()
                    (new-player! level)
                    (spawn-brother-thunk
                     'invader-laser-corout
                     (compose-thunks
                      (lambda () (sleep-for 1))
                      (create-invader-laser level)))
                    (spawn-brother-thunk
                     'mother-ship-corout
                     (compose-thunks
                      (lambda () (sleep-for (mothership-random-delay)))
                      (create-new-mothership level))))
                  
                  (create-init-invader-move level)))
                
                (new-corout 'main-manag (create-main-manager level))
                (new-corout 'redraw (create-redraw level)))))
    
    (add-global-score-messages! level)
    (for-each (lambda (s) (level-add-object! level s)) shields)
    (level-init-corouts-set! level init-corouts)

    level))

;; Creation of the default initial intro movie
(define (new-animation-level-A hi-score)
  (let* ((level (make-level screen-max-y screen-max-x (make-table)
                            hi-score 'unbound (new-mutex)))
         (init-corouts
          (list (new-corout 'animA (create-animation-A level))
                (new-corout 'intro-man (create-intro-manager level))
                (new-corout 'redraw    (create-redraw level)))))
    (level-init-corouts-set! level init-corouts)
    (add-global-score-messages! level)
    level))

;; Creation of the instructions intro movie
(define (new-animation-level-B hi-score)
  (let* ((level (make-level screen-max-y screen-max-x (make-table)
                            hi-score 'unbound (new-mutex)))
         (init-corouts
          (list (new-corout 'animB (create-animation-B level))
                (new-corout 'intro-man (create-intro-manager level))
                (new-corout 'redraw (create-redraw level)))))
    (level-init-corouts-set! level init-corouts)
    (add-global-score-messages! level)
    level))

;; Creation of a new demo movie. The demo movie is a tweaked normal
;; game level where the user input manager is the same as for the
;; other intro movies, and a special AI event is scheduled to control
;; the player. The demo should stop as soon as the player dies.
(define (new-animation-level-demo hi-score)
  (let* ((walls (generate-walls))
         (wall-damage '())
         (shields (generate-shields))
         (init-corouts 'unbound)
         (player-id 'demo)
         (lives 3)
         (score 0)
         (draw-game-field? #t)
         (other-finished? #f)  ; only used for 2p games
         (other-score 0)       ; only used for 2p games
         (level (make-game-level
                 screen-max-y screen-max-x (make-table)
                 hi-score init-corouts (new-mutex)
                 player-id score lives 
                 walls wall-damage draw-game-field?))
         (init-corouts
          (list (new-corout
                 'invader-move
                 (compose-thunks
                  (generate-invaders level)
                  ;; Setup level and start primordial game corouts
                  (lambda ()
                    (new-player! level)
                    (spawn-brother-thunk 'ai (create-ai-player level))
                    (spawn-brother-thunk
                     'flashing-demo-msg
                     (create-text-flash-animation
                      level (level-get level 'demo-msg) +inf.0))
                    (spawn-brother-thunk
                     'invader-laser-corout
                     (compose-thunks
                      (lambda () (sleep-for 1))
                      (create-invader-laser level)))
                    (spawn-brother-thunk
                     'mother-ship-corout
                     (compose-thunks
                      (lambda () (sleep-for (mothership-random-delay)))
                      (create-new-mothership level))))
                  
                  (create-init-invader-move level)))
                
                (new-corout 'inro-manag (create-intro-manager level))
                (new-corout 'redraw (create-redraw level)))))
    
    (add-global-score-messages! level)
    (level-add-object!
     level
     (make-message-obj 'demo-msg 
                       (make-point 100 (- screen-max-y 60))
                       'dummy 'white (make-point 0 0) "DEMO"))
    
    (for-each (lambda (s) (level-add-object! level s)) shields)
    (level-init-corouts-set! level init-corouts)

    level))

;; Creation of the instructions intro movie
(define (new-credits-level hi-score)
  (let* ((level (make-level screen-max-y screen-max-x (make-table)
                            hi-score 'unbound (new-mutex)))
         (init-corouts
          (list
           (new-corout 'credit-anim (create-animation-credit level))
           (new-corout 'intro-man   (create-intro-manager level))
           (new-corout 'redraw      (create-redraw level)))))
    (add-global-score-messages! level)
    (level-init-corouts-set! level init-corouts)

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
           (x (point-x pos))
           (y (point-y pos))
           (speed (game-object-speed obj))
           (dx (point-x speed))
           (dy (point-y speed)))
      (cycle-state! obj)
      (game-object-color-set! obj (choose-color pos))
      (point-x-set! pos (+ x dx))
      (point-y-set! pos (+ y dy))))
  
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

(define-generic (detect-collision? obj1 obj2))

;; Returns #f is the object is not colliding, else returns an object
;; which is in collision with obj. Only one object is return, even if
;; multiple collision are occurring.
(define-method (detect-collision? (obj game-object) (lvl level))
  ;; exists is exptected to return the object that satisfy the condition
  (exists (lambda (collision-obj)
            (detect-collision? obj collision-obj))
          (append (game-level-walls lvl)
                  (level-all-objects lvl))))

;; collision detection between 2 game objects
(define-method (detect-collision? (obj1 game-object) (obj2 game-object))
  (let* ((obj1-pos (game-object-pos obj1))
         (obj2-pos (game-object-pos obj2)))
    (and (not (eq? obj1 obj2))
         (detect-collision? (get-bounding-box obj1)
                            (get-bounding-box obj2)))))

(define-method (detect-collision? (obj game-object) (wall wall))
  (detect-collision? (get-bounding-box obj)
                     (wall-rect wall)))

(define-method (detect-collision? (obj game-object) (shield shield))
  (if (obj-obj-collision? obj (cast shield 'game-object))
      (let* ((pos (game-object-pos shield)))
        (exists (lambda (particle)
                  (point-rect-collision? (point-add pos particle)
                                         (get-bounding-box obj)))
                (shield-particles shield)))
      #f))

;; Simple rectangular collision detection. Not optimized.
(define-method (detect-collision? (r1 rect) (r2 rect))
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

(define-method (detect-collision? (point point) (rect rect))
  (let* ((rect-x1 (rect-x rect))
         (rect-x2 (+ rect-x1 (rect-width rect)))
         (rect-x-min (min rect-x1 rect-x2))
         (rect-x-max (max rect-x1 rect-x2))
         (rect-y1 (rect-y rect))
         (rect-y2 (+ rect-y1 (rect-height rect)))
         (rect-y-min (min rect-y1 rect-y2))
         (rect-y-max (max rect-y1 rect-y2))
         (point-x (point-x point))
         (point-y (point-y point)))
    (and (>= point-x rect-x-min)
         (<= point-x rect-x-max)
         (>= point-y rect-y-min)
         (<= point-y rect-y-max))))
(define-method (detect-collision? (rect rect) (point point))
  (detect-collision? point rect))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision resolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic (resolve-collision! level obj1 obj2))

;; Abstraction used to simplify the resolve-laser-collision!
;; function. This will remove the laser object from the level and
;; depending on the laser type, might re-scheduled a new laser shot.
(define (destroy-laser! level laser-obj)
  (level-remove-object! level laser-obj)
  (if (not (eq? (game-object-sprite-id laser-obj) 'player_laser))
      (spawn-brother-thunk
       'inv-laser (compose-thunks
                   (lambda () (sleep-for next-invader-laser-interval))
                   (create-invader-laser level)))
      (set! player-laser-last-destruction-time
            (time->seconds (current-time)))))

(define-method (resolve-collision! level (laser laser-obj) (inv invader-ship))
  (invader-ship? inv) 
  (level-increase-score! level inv)
  (destroy-laser! level laser)
  (explode-invader! level inv))
(define-method (resolve-collision! level (inv invader-ship) (laser laser-obj))
  (resolve-collision! laser invader))

(define-method (resolve-collision! level (laser1 laser-obj) (laser2 laser-obj))
  (let ((inv-laser (if (player_laser? laser1) laser2 laser1)))
    (explode-laser! level inv-laser)
    (destroy-laser! level inv-laser)))

(define-method (resolve-collision! level (laser laser-obj) (player player-ship))
  (spawn-brother-thunk 'player-explosion-anim
                       (lambda () (explode-player! level player)))
  (destroy-laser! level laser))
(define-method (resolve-collision! level (player player-ship) (laser laser-obj))
  (resolve-collision! level laser player))


(define-method (resolve-collision! level (laser laser-obj) (shield shield))
  (let ((penetrated-pos (get-laser-penetration-pos laser)))
    (game-object-pos-set! laser penetrated-pos)
    (explode-laser! level laser)
    (shield-explosion! shield laser))
  (destroy-laser! level laser))
(define-method (resolve-collision! level (shield shield) (laser laser-obj))
  (resolve-collision! laser shield))

(define-method (resolve-collision! level (laser laser-obj) (mother mothership))
  (destroy-laser! level laser)
  (explode-mothership! level mother)
  (level-increase-score! level mother)
  (let ((delta-t (mothership-random-delay)))
    (spawn-brother-thunk
     'mothership (compose-thunks
                  (lambda () (sleep-for delta-t))
                  (create-new-mothership level)))))
(define-method (resolve-collision! level (mother mothership) (laser laser-obj))
  (resolve-collision! laser mother))

(define-method (resolve-collision! level (laser laser-obj) (wall wall))
  (damage-wall! level laser)
  (explode-laser! level laser)
  (destroy-laser! level laser))
(define-method (resolve-collision! level (wall wall) (laser laser-obj))
  (resolve-collision! laser wall))

(define-method (resolve-collision! level (invader invader-ship) (shield shield))
  (shield-explosion! shield invader))

(define-method (resolve-collision! level (invader invader-ship) (wall wall))
  (if (eq? (wall-id collision-obj) 'bottom)
      (game-over! level)))

;; Player collisions
(define-method (resolve-collision! level (player player-ship) (wall wall))
  (let ((current-speed (game-object-speed player)))
    (game-object-speed-set! player
                            (make-point (- (point-x current-speed))
                                        (point-y current-speed)))
    (move-object! level player)))
(define-method (resolve-collision! level
                                   (player player-ship)
                                   (inv    invader-ship))
  (spawn-brother-thunk (lambda () (explode-player! level collision-obj)))
  (explode-invader! level collision-obj))


(define-method (resolve-collision! level
                                              (mother mothership) (wall wall))
  (level-remove-object! level mother)
  (let ((delta-t (mothership-random-delay)))
    (spawn-brother-thunk
     'mothership (compose-thunks
                  (lambda () (sleep-for delta-t))
                  (create-new-mothership level)))))


;;*****************************************************************************
;;
;;                 Gameplay related simulation events 
;;
;;*****************************************************************************

(define-macro (synchronized-thunk level action . actions)
  `(dynamic-corout-extent
    (lambda () (sem-lock! (level-mutex ,level)))
    (lambda () ,action ,@actions)
    (lambda () (sem-unlock! (level-mutex ,level)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of game level animations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Will animate a flashing PLAY PLAYER<X> in a black screen.
(define (start-of-game-animation level)
  (define animation-duration 3)
  (define player-id (game-level-player-id level))
  (lambda ()
    (let* ((pos (make-point 61 (- screen-max-y 136)))
           (state 'white)
           (speed (make-point 0 0))
           (text (if (is-player2? level)
                     "PLAY  PLAYER<2>"
                     "PLAY  PLAYER<1>"))
           (msg (make-message-obj 'start-msg pos
                                  state 'white speed text)))
      (game-level-draw-game-field?-set! level #f)
      (level-add-object! level msg)
      (let ((score-msg-obj (if (is-player2? level)
                               'player2-score-msg
                               'player1-score-msg)))
        (continue-with-thunk!
         (compose-thunks
          (create-text-flash-animation
           level (level-get level score-msg-obj) animation-duration)
          (lambda () (level-remove-object! level msg)
                  (game-level-draw-game-field?-set! level #t))
          #;
          (lambda () (sleep-for animation-duration))))))))

;; ;; Generate all the invaders in a level, with a little animation which
;; ;; displays them one after the other with a small dt interval.
(define (generate-invaders level)
  (define animation-delay 0.01)
  (define x-offset 30)
  (define y-offset (- 265 152))

  (define (determine-type-id col)
    (cond ((< col 2) make-easy-invader)
          ((< col 4) make-medium-invader)
          (else make-hard-invader)))

  (define (generate-inv! row col)
    (let* ((x (+ x-offset (* col invader-spacing)))
           (y (+ y-offset (* row invader-spacing)))
           (pos (make-point x y))
           (current-type-instantiator (determine-type-id row))
           (state 1)
           (speed (make-point invader-x-movement-speed 0))
           (invader
            (current-type-instantiator
             (gensym 'inv) pos state 'white speed row col)))
      (level-add-object! level invader)))
                         
  (synchronized-thunk level
    (let loop ((row 0) (col 0))
      (if (< row invader-row-number)
          (if (< col invader-col-number)
              (begin (generate-inv! row col)
                     (sleep-for animation-delay)
                     (loop row (+ col 1)))
              (begin
                (yield)
                (loop (+ row 1) 0)))
          (continue-with-thunk!
           (lambda () (sleep-for animation-delay)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gameplay related simulation events 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;; Event that will move a single row of invaders
(define (create-init-invader-move level)
  (synchronized-thunk level
    (let* ((rows (get-all-invader-rows level))
           (walls (game-level-walls level))
           (wall-collision?
            (exists
             (lambda (row)
               (exists (lambda (inv) (detect-collision? inv walls)) row))
             rows)))
      (if (null? rows)
          ;; Regenerate invaders when they all died
          (continue-with-thunk!
           (lambda () (generate-invaders
                       level (create-init-invader-move level))))
          (let* ((old-dx (point-x (game-object-speed (caar rows))))
                 (dt (get-invader-move-refresh-rate level))
                 (duration (* (length rows) dt)))
            (if wall-collision?
                (continue-with-thunk!
                  (compose-thunks
                   (create-invader-row-move!
                    0 (- invader-y-movement-speed) level)
                   (lambda () (sleep-for dt))
                   (create-invader-wall-movement-continuation
                     old-dx level)
                   (lambda () (sleep-for dt))
                   ;; should this event creation be put after the sleep?
                   (create-init-invader-move level)))
                
                (continue-with-thunk!
                 (compose-thunks
                  (create-invader-row-move! old-dx 0 level)
                  (lambda () (sleep-for dt))
                  (create-init-invader-move level)))))))))

;; Animation continuation that will be used after a wall collision to
;; make the invader move in the opposite x direction.
(define (create-invader-wall-movement-continuation old-dx level)
  (synchronized-thunk level
    (let ((rows (get-all-invader-rows level)))
      (if (null? rows)
          ;; Regenerate invaders when they all died
          (continue-with-thunk!
           (compose-thunks (generate-invaders level)
                           (create-init-invader-move level)))
          (continue-with-thunk!
           (create-invader-row-move! (- old-dx) 0 level))))))

;; Will move the invaders, one row at a time of dx and dy, then call
;; continuation event will be scheduled
(define (create-invader-row-move! dx dy level)
  (define rows (get-all-invader-rows level))
  (synchronized-thunk level
    (let ((dt (get-invader-move-refresh-rate level)))
      (let loop ((row-index 0))
        (if (< row-index invader-row-number)
            (let ((current-row (get-invaders-from-row level row-index)))
              (if (not (null? current-row))
                  (begin
                    (for-each
                     (lambda (inv) (let ((speed (make-point dx dy)))
                                     (game-object-speed-set! inv speed)))
                     current-row)
                    (move-ship-row! level row-index)
                    (sleep-for dt))
                  (yield))
              (loop (+ row-index 1))))))))

;; Creates a new mothership and schedules its first move event.
(define (create-new-mothership level)
  (synchronized-thunk level
    (let* ((dx-mult (list-ref '(1 -1) (random-integer 2)))
           (mothership
            (make-mothership 'mothership
                             (make-point (if (> dx-mult 0)
                                             gamefield-min-x
                                             (- gamefield-max-x 16))
                                         201)
                             0
                            'red
                            (make-point (* dx-mult mothership-movement-speed)
                                        0))))
      (play-sound 'mothership-sfx)
      (level-add-object! level mothership)
      (continue-with-thunk! (create-mothership level)))))

;; Event that moves a mothership and handles its collisions.
 (define (create-mothership level)
   (synchronized-thunk level
     (let loop ((mothership (level-mothership level)))
       (if mothership
           (let ((collision-occured? (move-object! level mothership)))
             (if (or (not collision-occured?)
                     (explosion? collision-occured?)
                     (eq? collision-occured? 'message))
                 (begin (sleep-for mothership-update-interval)
                        (loop (level-mothership level)))))))))



;; An invader laser event wraps up a shoot-laser! such that it will
;; create a new laser that will come from a candidate invader (one
;; that is in front of the player).
;;
;; The current implementation is very innefficient (O(n^2)).
(define (create-invader-laser level)
  (define (rect-inv-collision? rect)
    (lambda (inv)
      (let* ((pos (game-object-pos inv))
             (width (type-width inv))
             (heigth (type-height inv))
             (inv-rect (make-rect (point-x pos) (point-y pos) width heigth)))
        (detect-collision? rect inv-rect))))
                                  
  (define (bottom-invader? inv)
    (let* ((pos (game-object-pos inv))
           (rect (make-rect (point-x pos)        ;; x
                            (- (point-y pos) 1)  ;; y
                            (type-width inv)     ;; width
                            (- (point-y pos))))) ;; height
      (not (exists (rect-inv-collision? rect) (level-invaders level)))))
                       
  (define (get-candidates)
    (filter bottom-invader? (level-invaders level)))

  (synchronized-thunk level
    (if (not (exists (lambda (obj)
                       (and (laser-obj? obj)
                            (not (eq? (game-object-sprite-id obj)
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
                            (list-ref
                             (list make-laserA make-laserB make-laserC)
                             (random-integer 3))
                            shooting-invader
                            (- invader-laser-speed)))))))

;; Wrapper function over create-laser which will create a new
;; laser object instance of specifiex type and place it correctly next
;; to the shooting object.
(define (shoot-laser! level laser-creator shooter-obj dy)
  (define is-player-laser? (eq? laser-creator make-player_laser))
  ;; if the shot laser is a player laser, there must not be another
  ;; player laser in the game or the player-laser-refresh-constraint
  ;; must be elabsed before shooting a new one.
  (if (not (and is-player-laser?
                (or (level-player-laser level)
                    (< (- (time->seconds (current-time))
                          player-laser-last-destruction-time)
                       player-laser-refresh-constraint))))
                    
      (let* ((laser-obj (laser-creator #f #f #f #f #f))
             (shooter-x (point-x (game-object-pos shooter-obj)))
             (shooter-y (point-y (game-object-pos shooter-obj)))
             (x (+ shooter-x
                   (floor (/ (type-width shooter-obj) 2))))
             (y (if (< dy 0)
                    (- shooter-y
                       (type-height laser-obj )
                       invader-y-movement-speed)
                    (+ shooter-y
                       (type-height shooter-obj))))
             (laser-id (if is-player-laser?
                           'player-laser
                           (gensym 'inv-laser))))
        (laser-obj-id-set! laser-obj laser-id)
        (laser-obj-pos-set! laser-obj (make-point x y))
        (laser-obj-state-set! laser-obj 0)
        (laser-obj-color-set! laser-obj 'white)
        (laser-obj-speed-set! laser-obj (make-point 0 dy))
        (level-add-object! level laser-obj)
        ;; Fixme: unsure here...
        (spawn-brother-thunk 'laser-mov (create-laser laser-obj level)))))
        

;; Will generate the events associated with a laser object such that
;; it will be moved regularly dy pixels on the y axis. The game logic
;; of a laser is thus defined by the returned event.
(define (create-laser laser-obj level)
  (synchronized-thunk level
    (let loop ()
     ;; centered laser position (depending on the laser type...
      (let ((pos (let ((pos (game-object-pos laser-obj)))
                   (point-add pos (make-point
                                   (floor (/ (type-width laser-obj) 2))
                                   0))))
            (collision-occured? (move-object! level laser-obj)))
        (if (or (not collision-occured?)
                (level-exists level (game-object-id laser-obj)))
            ;; if no collisions, continue on with the laser motion
            (let ((delta-t (if (eq? (level-player-laser level) laser-obj)
                               player-laser-update-interval
                               invader-laser-update-interval)))
              (sleep-for delta-t)
              (loop)))))))

;; (define-class game-object ()
;;   (slot: id)
;;   (slot: pos)
;;   (slot: state)
;;   (slot: color)
;;   (slot: speed)
;;   (class-slot: sprite-id)
;;   (class-slot: bbox)
;;   (class-slot: state-num)
;;   (class-slot: score-value))

;; dispalys an explosion where the invader was and removes it from the
;; level. This will freeze the game events, as it seem to do in the
;; original game.
(define (explode-invader! level inv)
  (define animation-duration 0.3)
  (level-remove-object! level inv)
  (let ((expl (make-invader_explosion
               (game-object-id inv)
               (game-object-pos inv)
               0 ;state reinitialised
               (game-object-color inv)
               (game-object-speed inv))))
    (sleep-for animation-duration)
    (continue-with-thunk!
     (create-explosion-end! level expl))))

;; dispalys the explosion of the mothership was and removes it from
;; the level. The points for killing that mothership will then be
;; randomly calculated and displayed after the explosion.
(define (explode-mothership! level mothership)
  (define animation-duration 0.3)
  (define pos (game-object-pos mothership))
  (define score-val (list-ref '(50 100 150) (random-integer 3)))
  (define expl-obj
    (make-mothership_explosion (gensym 'explosion)
                               pos
                               0
                               (choose-color pos)
                               (make-point 0 0)))
  ;; Update the global mothership's score value to the current value...
  (mothership-score-value-set! score-val)
  (level-remove-object! level mothership)
  (level-add-object! level expl-obj)
  (sleep-for animation-duration)
  (continue-with-thunk!
   (compose-thunks
    (create-explosion-end! level expl-obj)
    (show-points level pos score-val))))

;; Will display the points value of a mothership kill at pos
(define (show-points level pos points)
  (define duration 1)
  (define msg
    (let ((id (gensym 'mothership-points))
          (state 'dummy-state)
          (color (choose-color pos))
          (speed (make-point 0 0))
          (text (number->string points)))
    (make-message-obj id pos state color speed text)))
  (synchronized-thunk
   level
   (level-add-object! level msg)
   (sleep-for duration)
   (level-remove-object! level msg)))

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
    (make-player_explosion (gensym 'explosion)
                           (game-object-pos player)
                           0
                           (choose-color (game-object-pos player))
                           (make-point 0 0)))
  (level-loose-1-life! level)
  (level-add-object! level expl-obj)
  (level-remove-object! level player)
  (let ((continuation
         (if (<= (game-level-lives level) 0)
             (if (2p-game-level? level)
                 (begin
                   (2p-game-level-finished?-set! level #t)
                   (if (2p-game-level-other-finished? level)
                       (compose-thunks
                        (game-over-2p-animation level)
                        (game-over-animation level)
                        (lambda () (game-over! level)))
                       (begin
                         (compose-thunks
                          (game-over-2p-animation level)
                          (lambda () (game-over! level))))))
                 (compose-thunks
                  (game-over-animation level)
                  (lambda () (game-over! level))))
             (if (2p-game-level? level)
                 (lambda ()
                   (super-yield) ; return to super scheduler
                   (prioritized-thunk-continuation
                    (compose-thunks
                     (start-of-game-animation level)
                     (return-to-player level))))
                 (return-to-player level)))))
    (continue-with-thunk!
     (compose-thunks
      ;; mutex lock must be performed in a separate corout because it
      ;; must be out of the synchronized-thunk's extent!
      (lambda () (sem-lock! (level-mutex level)))
      (player-explosion-animation level expl-obj animation-duration)
      continuation))))

;; Event used in 2 player games, where the games returns to the
;; current game.
(define (return-to-player level)
  (lambda ()
   (sem-unlock! (level-mutex level))
   (new-player! level)))

        
;; Animation of the player ship explosion
(define (player-explosion-animation level expl-obj duration)
  (define frame-rate 0.1)
  (lambda ()
    (let loop ((dt 0))
      (cycle-state! expl-obj)
      (if (< dt duration)
          (begin (sleep-for frame-rate)
                 (loop (+ dt frame-rate)))
          (level-remove-object! level expl-obj)))))


;; Destruction and animation of a laser explosion.
(define (explode-laser! level laser-obj)
  (define animation-duration 0.3)
  (define (center-pos pos expl-type-width)
    ;; FIXME: ugly hack where only player laser's explotion are
    ;; centered in x. I'm unsure why other lasers don't require this
    ;; shift so far...
    (if (player_laser? laser-obj)
        (point-sub pos (make-point (floor (/ expl-type-width 2))
                                   0))
        pos))
  (let* ((expl-type-constructor (if (player_laser? laser-obj)
                                    make-player_laser_explosion
                                    make-invader_laser_explosion))
         (obj
          (expl-type-constructor (gensym 'explosion)
                                 (game-object-pos laser-obj) ; not centered
                                 0
                                 (choose-color (game-object-pos laser-obj))
                                 (make-point 0 0))))
    (update obj laser pos
            (lambda (p) (center-pos p (type-width obj))))
    (level-remove-object! level laser-obj)
    (level-add-object! level obj)
    (sleep-for animation-duration)
    (continue-with-thunk! (create-explosion-end! level obj))))


;; Event that will stop an invader explosion animation
(define (create-explosion-end! level inv)
  (synchronized-thunk level
    (level-remove-object! level inv)))



;;*****************************************************************************
;;
;;                            Animation events 
;;
;;*****************************************************************************

;; event that will use msg-obj and will gradually add the letters
;; contained in the msg string to it, creating a type-writer effect.
(define (animate-message msg-obj msg)
  (define animation-delay 0.1)
  (lambda ()
    (let loop ((str msg))
      (if (not (string=? str ""))
          (let ((current-text (message-obj-text msg-obj)))
            (message-obj-text-set! msg-obj
                                   (string-append current-text
                                                  (substring str 0 1)))
            (sleep-for animation-delay)
            (loop (substring str 1 (string-length str))))))))

;; Flashing animation where the msg-obj will flash for a certain duration.
(define (create-text-flash-animation level msg-obj duration)
  (define animation-delay 0.2)
  (define original-color (game-object-color msg-obj))
  (define (cycle-msg-state! msg-obj)
    (let ((current-color (game-object-color msg-obj)))
      (game-object-color-set!
       msg-obj
       (if (eq? current-color 'black) original-color 'black))))
  (lambda ()
    (let loop ((dt 0))
      (if (< dt duration)
          (begin (cycle-msg-state! msg-obj)
                 (sleep-for animation-delay)
                 (loop (+ dt animation-delay)))
          (begin
            (game-object-color-set! msg-obj original-color)
            'finished)))))

;; Special animation that occur in the intro movie A (main intro)
(define (create-animation-A level)
  (define speed (make-point 0 0))
  (define state 'white)

  (lambda ()
    ;; messages declaration
    (let* ((play   (let ((pos (make-point 101 (- screen-max-y 88))))
                     (make-message-obj
                      'play pos state (choose-color pos) speed "")))
           (space  (let ((pos (make-point 61 (- screen-max-y 112))))
                     (make-message-obj
                      'space pos state (choose-color pos) speed "")))
           (score  (let ((pos (make-point 37 (- screen-max-y 144))))
                     (make-message-obj
                      'score pos state (choose-color pos) speed "")))
           (mother (let ((pos (make-point 85 (- screen-max-y 160))))
                     (make-message-obj
                      'mother pos state (choose-color pos) speed "")))
           (hard   (let ((pos (make-point 85 (- screen-max-y 176))))
                     (make-message-obj
                      'hard pos state (choose-color pos) speed "")))
           (medium (let ((pos (make-point 85 (- screen-max-y 192))))
                     (make-message-obj
                      'medium pos state (choose-color pos) speed "")))
           (easy   (let ((pos (make-point 85 (- screen-max-y 208))))
                     (make-message-obj
                      'easy pos state (choose-color pos) speed "")))
           (anim-messages
            (list play space score mother hard medium easy)))
      ;; add all the messages
      (for-each (lambda (m) (level-add-object! level m)) anim-messages )
      ;; schedule the animation
      (continue-with-thunk!
       (compose-thunks
        (animate-message play "PLAY")
        (animate-message space "SPACE   INVADERS")
        (create-animate-score-adv-table level))))))



;; Animation A follow-up, which will create a table showing the score
;; for killing each kind of invaders.
(define (create-animate-score-adv-table level)
  (define speed (make-point 0 0))
  (define (pos x y) (make-point x (- screen-max-y y)))
  (lambda ()
    ;; create the ship prototypes
    (let ((mothership (make-mothership 'mothership
                                       (pos 68 160)
                                       0
                                       (choose-color (pos 68 160))
                                       speed))
          (hard-ship (make-invader-ship 'hard-ship
                                        (pos 72 176)
                                        0
                                        (choose-color (pos 72 176))
                                        speed
                                        0 0))
          (medium-ship (make-invader-ship 'medium-ship
                                          (pos 71 192)
                                          0
                                          (choose-color (pos 71 192))
                                          speed
                                          0 0))
          (easy-ship (make-invader-ship 'easy-ship
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
    ;; pre-allocated in the create-animation-A.
    (continue-with-thunk!
     (compose-thunks
      (animate-message (level-get level 'mother) "=? MYSTERY")
      (animate-message (level-get level 'hard) "=30 POINTS")
      (animate-message (level-get level 'medium) "=20 POINTS")
      (animate-message (level-get level 'easy) "=10 POINTS")
      (lambda ()
        (sleep-for animation-end-wait-delay)
        (continue-with-thunk!
         (lambda ()
           (set! other-animations-index
                 (modulo (+ other-animations-index 1)
                         (length other-animations)))
           (kill-all!
            (list-ref other-animations
                      other-animations-index)))))))))

;; Similare to intro animation A, but more simple, where some very
;; basic game instruction get displayed on screen.
(define (create-animation-B level)
  (define speed (make-point 0 0))
  (define state 'white)

  (lambda ()
    (let* ((instruction
            (let ((pos (make-point 70 (- screen-max-y 100))))
              (make-message-obj
               'instruction pos state (choose-color pos) speed "")))
           (press1 (let ((pos (make-point 20 (- screen-max-y 130))))
                    (make-message-obj
                     'press1 pos state (choose-color pos) speed "")))
           (press2 (let ((pos (make-point 20 (- screen-max-y 154))))
                     (make-message-obj
                      'press2 pos state (choose-color pos) speed "")))
           (score (let ((pos (make-point 37 (- screen-max-y 144))))
                    (make-message-obj
                     'score pos state (choose-color pos) speed "")))
           (anim-messages
            (list instruction press1 press2 score)))
      (for-each (lambda (m) (level-add-object! level m)) anim-messages )
      (continue-with-thunk!
       (compose-thunks
        (animate-message instruction "INSTRUCTIONS")
        (animate-message press1 "PRESS '1' FOR 1 PLAYER")
        (animate-message press2 "PRESS '2' FOR 2 PLAYERS")
        (lambda ()
          (sleep-for animation-end-wait-delay)
          (kill-all! 'intro-A)))))))

(define (create-animation-credit level)
  (define speed (make-point 0 0))
  (define state 'white)

  (lambda ()
    (let* ((producer
            (let ((pos (make-point 65 (- screen-max-y 80))))
              (make-message-obj
               'producer pos state 'white speed "")))
           (dsth (let ((pos (make-point 59 (- screen-max-y 107))))
                    (make-message-obj
                     'dsth pos state 'red speed "")))
           (support (let ((pos (make-point 65 (- screen-max-y 150))))
                      (make-message-obj
                       'support pos state 'white speed "")))
           (support-chars (let ((pos (make-point 20 (- screen-max-y 180))))
                    (make-message-obj
                     'support-chars pos state 'blue speed "")))
           (you (let ((pos (make-point 100 (- screen-max-y 220))))
                  (make-message-obj
                   'you pos state 'yellow speed "")))
           (anim-messages
            (list producer dsth support support-chars you)))
      (for-each (lambda (m) (level-add-object! level m)) anim-messages )
      (play-sfx 'star-wars-op)
      (continue-with-thunk!
       (compose-thunks
        (animate-message producer "Game Producer:")
        (animate-message dsth "David St-Hilaire")
        (animate-message support "Support Team:")
        (animate-message support-chars "Julie, the LTP and ... ")
        (animate-message you "You!")
        (lambda ()
          (sleep-for 40) ;;about the end of the song?
          (stop-sfx 'star-wars-op)
          (kill-all! 'intro-A)))))))


;; Ai reaction event. This thread-like event will simulate a player
;; behaviour, based on random "thoughts", where in fact a thought is
;; an animation either moving the player or make him shoot.
(define (create-ai-player level)
  (define ai-reaction-interval 0.01)
  (define ai-movement-duration-max 1)
  (define ai-movement-delay 0.02)
  
  ;; simple abastraction used in left/right movements
  (define (move-player! dx)
    (let ((player (level-player level))
          (new-speed (make-point dx 0)))
      (game-object-speed-set! player new-speed)
      (move-object! level player)))

  ;; will move left the ai player for a random amount of time
  (define (create-move-left-animation)
    (define duration (* (random-real) ai-movement-duration-max))
    (let loop ((dt 0))
      (if (level-player level)
          (let ((collision?
                 (move-player! (- player-movement-speed))))
            (if (and (< dt duration) (not collision?))
                (begin (sleep-for ai-movement-delay)
                       (loop (+ dt ai-movement-delay)))))
          (prioritized-thunk-continuation end-of-demo))))

  ;; will move right the ai player for a random amount of time
  (define (create-move-right-animation)
    (define duration (* (random-real) ai-movement-duration-max))
    (let loop ((dt 0))
      (if (level-player level)
          (let ((collision?
                 (move-player! player-movement-speed)))
            (if (and (< dt duration) (not collision?))
                (begin (sleep-for ai-movement-delay)
                       (loop (+ dt ai-movement-delay)))))
          (prioritized-thunk-continuation end-of-demo))))

  ;; Will make the ai player shoot a laser
  (define (create-ai-laser)
    (if (level-player level)
        (begin
          (shoot-laser! level make-player_laser
                        (level-player level)
                        player_laser-speed))
        (prioritized-thunk-continuation end-of-demo)))

  ;; This will end the demo. NOTE: here the synchronized-thunk
  ;; is very important, even if the demo cannot be paused. This is so
  ;; because the demo should end only when the player explosion is
  ;; finished.
  (define end-of-demo
    (synchronized-thunk level (kill-all! 'demo-over)))

  ;; If the player is dead, end the demo, elso randomly choose an
  ;; action. The action are determined using a cheap weighted
  ;; distribution.
  (let ((actions (list create-move-left-animation
                            create-move-right-animation
                            create-ai-laser
                            create-ai-laser
                            create-ai-laser
                            create-ai-laser
                            create-ai-laser
                            create-ai-laser)))
    (lambda ()
      (let loop ()
        (if (not (level-player level))
            (end-of-demo)
            (begin
              ((list-ref actions (random-integer (length actions))))
              (sleep-for ai-reaction-interval)
              (loop)))))))


;; Will display in the top screen the final game over message.
(define (game-over-animation level)
  (define continuation-delay 2)
  (lambda ()
    (let* ((pos (make-point 77 (- screen-max-y 60)))
           (speed (make-point 0 0))
           (msg-obj (make-message-obj 'game-over-msg 
                                      pos 'red 'red speed "")))
      (level-add-object! level msg-obj)
      (continue-with-thunk!
       (compose-thunks
        (animate-message msg-obj "GAME OVER")
        (lambda () (sleep-for continuation-delay)))))))

;; Will display in the lower scren the game over message for 1 player
;; in a 2 player game. A final game over message should be displayed
;; afterward if both player are game over.
(define (game-over-2p-animation level)
  (define continuation-delay 1.2)
  (lambda ()
    (let* ((player-id (game-level-player-id level))
           (text (if (is-player2? level)
                     "GAME OVER PLAYER<2>"
                     "GAME OVER PLAYER<1>"))
           (pos (make-point 37 (- screen-max-y 248)))
           (speed (make-point 0 0))
           (msg-obj (make-message-obj 'game-over-msg
                                      pos 'green 'green speed "")))
      (level-add-object! level msg-obj)
      (continue-with-thunk!
       (compose-thunks (animate-message msg-obj text)
                       (lambda () (sleep-for continuation-delay)))))))

  



;;*****************************************************************************
;;
;;                              Manager events 
;;
;;*****************************************************************************

;; the manager event is a regular event that polls and handle user
;; input by looking into the thread's mailbox. It is assumed that the
;; discrete event simulation is perfomed in it's own thread and that
;; user input is passed to the simulation via this mechanism.
(define (create-main-manager level)
  (define game-paused? #f)
  (define (player-can-move?) (and (level-player level) (not game-paused?)))
  (lambda ()
    (let loop ()
     (let ((player (level-player level))
           (msg (thread-receive 0 #f)))
       (if msg
           (case msg
             ((space)
              (if (player-can-move?)
                  (shoot-laser! level make-player_laser
                                (level-player level)
                                player_laser-speed)))
             ((right)
              (if (player-can-move?)
                  (let ((new-speed (make-point player-movement-speed 0)))
                    (game-object-speed-set! player new-speed)
                    (move-object! level player))))
              
             ((left)
              (if (player-can-move?)
                  (let ((new-speed (make-point (- player-movement-speed) 0)))
                    (game-object-speed-set! player new-speed)
                    (move-object! level player))))
              
             ((p)
              (if game-paused?
                  (sem-unlock! (level-mutex level))
                  (sem-lock! (level-mutex level)))
              (set! game-paused? (not game-paused?)))

             ((r)
              (stop-sfx 'all)
              (super-kill-all! 'reset))

             ((d) (error "DEBUG"))))
       (sleep-for manager-time-interfal)
       (loop)))))

;; Simple key manager that is used durint the intro/demo screens
(define (create-intro-manager level)
  (define game-paused? #f)
  (lambda ()
    (let loop ((msg (thread-receive 0 #f)))
      (if msg
          (case msg
            ((1) (stop-sfx 'all) (kill-all! 'start-1p-game))
            ((2) (stop-sfx 'all) (kill-all! 'start-2p-game))
            ((c) (stop-sfx 'all) (kill-all! 'show-credits))
            ((r) (stop-sfx 'all) (kill-all! 'intro-A))
            ((d) (error "DEBUG"))))
      (sleep-for manager-time-interfal)
      (loop (thread-receive 0 #f)))))

;; Event that will send a message to the ui asking for a redraw.
(define (create-redraw level)
  ;; will update the 2 top score messages if required.
  (define (update-score-msg! level)
    (if (game-level? level) 
        (let* ((is-p2? (is-player2? level))
               (score-obj
                (level-get level (if is-p2?
                                     'player2-score-msg
                                     'player1-score-msg)))
               (other-score-obj
                (level-get level (if is-p2?
                                     'player1-score-msg
                                     'player2-score-msg))))
          (update-other-player-state! level)
          (message-obj-text-set!
           score-obj (get-score-string (game-level-score level)))
          (if (2p-game-level? level)
              (message-obj-text-set!
               other-score-obj
               (get-score-string (2p-game-level-other-score level)))))))
  (lambda ()
    (let loop ()
      (update-score-msg! level)
      (thread-send user-interface-thread `(redraw ,level))
      (sleep-for redraw-interval)
      (loop))))



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

         ((eq? result 'show-credits)
          (inf-loop hi-score
                    (play-level (new-credits-level hi-score))))

         ((eq? result 'demo)
          (let ((ai (new-corout 
                     'ai (lambda ()
                           (play-level
                            (new-animation-level-demo hi-score))))))
            (inf-loop hi-score (simple-boot ai))))
         
         ((eq? result 'start-1p-game)
          (let ((p1 (new-corout 
                     player1
                     (lambda ()
                       (play-level (new-level 0 hi-score 1 player1))))))
            (inf-loop hi-score (simple-boot p1))))

         ((eq? result 'start-2p-game)
          (parameterize ((p1-level (new-level 0 hi-score 2 player1))
                         (p2-level (new-level 0 hi-score 2 player2)))
           (let ((p1
                  (new-corout
                   player1
                   (lambda () (play-level (p1-level)))))
                 (p2
                  (new-corout 
                   player2
                   (lambda () (play-level (p2-level))))))
             (inf-loop hi-score (boot (lambda (acc v) (max acc v)) p1 p2)))))

         (else
          (inf-loop hi-score
                    (play-level (new-animation-level-A hi-score))))))))


;;*****************************************************************************
;;
;;                              Rendering
;;
;;*****************************************************************************

(define-generic (render obj))

(define-method (render (msg-obj message-obj))
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (let* ((pos (game-object-pos msg-obj))
         (x (point-x pos))
         (y (point-y pos))
         (color (game-object-color msg-obj))
         (str (message-obj-text msg-obj)))
    (render-string x y str color)))
    ;;(display-message x y (message-obj-text msg-obj) state)))

(define-method (render (shield shield))
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (set-openGL-color 'green)
  (for-each (lambda (particle)
              (let* ((shield-x (point-x (game-object-pos shield)))
                     (shield-y (point-y (game-object-pos shield)))
                     (x (+ shield-x (point-x particle)))
                     (y (+ shield-y (point-y particle))))
                (glBegin GL_QUADS)
                (glVertex2i x y)
                (glVertex2i x (- y 1))
                (glVertex2i (+ x 1) (- y 1))
                (glVertex2i (+ x 1) y)
                (glEnd)))
            (shield-particles shield)))

(define-method (render (obj sprite-obj))
  (let ((x (point-x (game-object-pos obj)))
        (y (point-y (game-object-pos obj)))
        (state (game-object-state obj))
        (color (game-object-color obj)))
    (render-fontified-sprite (object-sprite-id obj) x y state color)))

(define-method (render (level level))
  ;; Draw all objects
  (for-each render (level-all-objects level)))

(define-method (render (level game-level))
  (render (cast level 'level))
  
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

  ;; Must verify if the game field should be drawn or not...
  (if (game-level-draw-game-field? level)
      (begin
        ;; Draw horizontal bottom green wall and remove damaged parts by
        ;; redrawing over it in black.
        (set-openGL-color 'green)
        (glBegin GL_QUADS)
        (glVertex2i 0 9)
        (glVertex2i 0 10)
        (glVertex2i screen-max-x 10)
        (glVertex2i screen-max-x 9)
        (glEnd)
        ;;  (glBlendFunc GL_ONE GL_ZERO)
        (set-openGL-color 'black)
        (for-each (lambda (p)
                    (glBegin GL_QUADS)
                    (glVertex2i (point-x p) (+ (point-y p) 1))
                    (glVertex2i (point-x p) (+ (point-y p) 2))
                    (glVertex2i (+ (point-x p) 1) (+ (point-y p) 2))
                    (glVertex2i (+ (point-x p) 1) (+ (point-y p) 1))
                    (glEnd))
                  (game-level-wall-damage level))

        (for-each render (game-level-shields level)))
      ;; if we don't the draw the field, a black square is drawn to
      ;; cover unwanted already drawn objects.
      (begin
        (set-openGL-color 'black)
        (glBegin GL_QUADS)
        (glVertex2i 0 0)
        (glVertex2i screen-max-x 0)
        (glVertex2i screen-max-x screen-max-y)
        (glVertex2i 0 screen-max-y)
        (glEnd)
        ;; Re-draw all message over the black screen
        (for-each (lambda (msg) (render msg))
                  (level-messages level))))

  ;; Draw lives
  (let ((nb-lives (game-level-lives level)))
    (render-string 13 0 (number->string nb-lives) 'white)
    (for i 0 (< i (- nb-lives 1))
         (render-fontified-sprite 'player (+ 30 (* i 15)) 0 0 'green))))


;; Generate generic functions!
(setup-generic-functions!)
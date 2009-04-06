(declare (safe)
         (inlining-limit 150) ; avoids code exploooosion!
         (block)
         (standard-bindings)
         (extended-bindings))

(include "class.scm")
(include "scm-lib-macro.scm")
(include "thread-simulation-macro.scm")
(include "match.scm")

;; FIXME!! Not ideal, but class system does not support inter-module usage
(include "thread-simulation.scm")

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

;; (define invader-row-number 5)
;; (define invader-col-number 11)
(define invader-row-number 2)
(define invader-col-number 2)

(define invader-spacing 16)

(define invader-x-movement-speed 2)
(define invader-y-movement-speed 8)
(define player-movement-speed 2)
(define player-laser-speed 6)
(define invader-laser-speed 2)
(define mothership-movement-speed 1)

(define player-laser-last-destruction-time 0)

;; Simulation delays
(define next-invader-laser-interval 0.2)
(define manager-time-interfal 1/120)
(define redraw-interval 1/60)
(define animation-end-wait-delay 2)


(define player-laser-refresh-constraint 0.6)

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
    (new point (* x-fact (point-x dir)) (* y-fact (point-y dir)))))


;;;; Rectangle structure used in collision detection ;;;;
(define-class point () (slot: x) (slot: y))
(define-class rect (point) (slot: width) (slot: height))

;; these are translation of the pos2d lib from scm-lib into oo
(define (point-add p1 p2) (new point
                               (+ (point-x p1) (point-x p2))
                               (+ (point-y p1) (point-y p2))))
(define (point-sub p1 p2) (new point
                               (- (point-x p1) (point-x p2))
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
(define-class game-object (corout)
  ;; (slot: id) from corout
  (slot: pos)
  (slot: state)
  (slot: color)
  (slot: speed)
  (class-slot: sprite-id)
  (class-slot: bbox)
  (class-slot: state-num)
  (class-slot: score-value)
  (constructor: (lambda (obj id pos state color speed level)
                  (init! cast: '(corout * *) obj
                         id
                         (lambda ()(with-dynamic-handlers
                                    ((pause (pause obj level))
                                     (die   (die   obj level)))
                                    ((behaviour obj level)))))
                  (set-fields! obj game-object
                    ((pos pos)
                     (state state)
                     (color color)
                     (speed speed))))))

(define (cycle-state! obj)
  (define current-state (game-object-state obj))
  (if (number? current-state)
      (game-object-state-set!
       obj (modulo (+ current-state 1)
                   (game-object-state-num obj)))))

(define (get-bounding-box obj)
  (new rect
       (+ (point-x (game-object-pos obj)) (rect-x (game-object-bbox obj)))
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
  `(begin
     (,(symbol-append class-name '-sprite-id-set!)   ,sprite-id)
     (,(symbol-append class-name '-bbox-set!)        ,bbox)
     (,(symbol-append class-name '-state-num-set!)   ,state-num)
     (,(symbol-append class-name '-score-value-set!) ,score-value)))

;;;; Abstract classes ;;;;

(define-class sprite-obj ())


;;;; Specific game object descriptions ;;;;

;; Evil Invaders!
(define-class invader-ship (game-object sprite-obj) (slot: row) (slot: col)
  (constructor:
   (lambda (obj pos state color speed row col level)
     (init! cast: '(game-object * * * * * *)
            obj (gensym 'invader) pos state color speed level)
     (subscribe `(invader-row ,row) obj)
     (set-fields! obj invader-ship ((row row) (col col))))))

(define-class easy-invader   (invader-ship))
(define-class medium-invader (invader-ship))
(define-class hard-invader   (invader-ship))
(setup-static-fields! easy-invader 'easy (new rect 0 0 12 8) 2 10)
(setup-static-fields! medium-invader 'medium (new rect 0 0 12 8) 2 20)
(setup-static-fields! hard-invader 'hard (new rect 0 0 12 8) 2 30)


(define-class player-ship  (game-object sprite-obj)
  (constructor:
   (lambda (obj level)
     (let* ((pos (new point 22 (- screen-max-y 240)))
            (state 0)
            (speed (new point 0 0))
            (player-ship (new player-ship)))
       (init! cast: '(game-object * * * * * *)
              obj 'player pos state 'green speed level)
       (level-spawn-object! level obj)))))
(setup-static-fields! player-ship 'player (new rect 0 0 13 8) 1 0)

(define-class mothership   (game-object sprite-obj)
  (constructor:
   (lambda (obj level)
     (init! cast: '(game-object * * * * * *)
            obj 'mothership
            'not-init 'not-init 'not-init 'not-init
            level))
   (lambda (obj pos state color speed level)
     (init! cast: '(game-object * * * * * *)
            obj 'mothership pos state color speed level))))
(setup-static-fields! mothership 'mothership (new rect 0 0 16 7) 1 100)

(define-class laser-obj    (game-object sprite-obj))
(define-class laserA         (laser-obj))
(define-class laserB         (laser-obj))
(define-class laserC         (laser-obj))
(define-class player_laser   (laser-obj))
(setup-static-fields! laserA 'laserA (new rect 1 0 1 7) 6 0)
(setup-static-fields! laserB 'laserB (new rect 1 0 1 7) 8 0)
(setup-static-fields! laserC 'laserC (new rect 1 0 1 7) 4 0)
(setup-static-fields! player_laser 'player_laser (new rect 0 0 1 7) 1 0)

(define-class shield       (game-object) (slot: particles)
  (constructor:
   (lambda (obj id pos state color speed particles level)
     (init! cast: '(game-object * * * * * *)
            obj id pos state color speed level)
     (set-fields! obj shield ((particles particles))))))
(setup-static-fields! shield 'shield (new rect 0 0 22 16) 1 0)

(define-class explosion    (game-object sprite-obj))
(define-class invader_explosion       (explosion)
  (constructor:
   (lambda (obj inv level)
     (init! cast: '(game-object * * * * * *)
            obj
            (symbol-append (game-object-id inv) '-explosion)
            (game-object-pos inv)
            0 ; state
            (game-object-color inv)
            (game-object-speed inv)
            level))))
(define-class invader_laser_explosion (explosion))
(define-class player_explosion        (explosion)
  (constructor:
   (lambda (obj player level)
     (init! cast: '(game-object * * * * * *)
            obj
            (symbol-append (game-object-id player) '-explosion)
            (game-object-pos player)
            0 ; state
            (game-object-color player)
            (game-object-speed player)
            level))))
(define-class player_laser_explosion  (explosion))
(define-class mothership_explosion    (explosion)
  (constructor:
   (lambda (obj mother level)
     (init! cast: '(game-object * * * * * *)
            obj
            'mothership-explosion
            (game-object-pos mother)
            0 ; state
            'red
            (game-object-speed mother)
            level))))
(setup-static-fields!
 invader_explosion 'invader_explosion (new rect 0 0 13 8) 1 0)
(setup-static-fields!
 invader_laser_explosion 'invader_laser_explosion (new rect 0 0 6 8) 1 0)
(setup-static-fields!
 player_explosion 'player_explosion (new rect 0 0 16 8) 2 0)
(setup-static-fields! 
 player_laser_explosion 'player_laser_explosion (new rect 0 0 8 8) 1 0)
(setup-static-fields!
 mothership_explosion 'mothership_explosion (new rect 0 0 21 8) 1 0)

(define-class message-obj  (game-object) (slot: text)
  (constructor:
   (lambda (obj id pos state color speed level text)
     (init! cast: '(game-object * * * * * *)
            obj id pos state color speed level)
     (message-obj-text-set! obj text))))
(setup-static-fields! message-obj 'message (new rect 0 0 0 0) 0 0)

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
  ;; here the received point list are in pos2d format (translated into
  ;; point objects)
  (map (lambda (p) (new point (pos2d-x p) (pos2d-y p)))
       (rgb-pixels-to-boolean-point-list
        (parse-ppm-image-file "sprites/explodeInvL0.ppm")
        rgb-threshold? 'dont-center)))

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
                              (loop-x (+ x 1) (cons (new point x y) acc))
                              acc))
                        acc))
        acc)))

;; Particles of a player laser explosion
(define player-laser-explosion-particles
  ;; here the received point list are in pos2d format (translated into
  ;; point objects)
  (map (lambda (p) (new point (pos2d-x p) (pos2d-y p)))
       (rgb-pixels-to-boolean-point-list
        (parse-ppm-image-file "sprites/explodeL0.ppm")
        rgb-threshold? 'center)))

;; Calculate the interpenetration distance and add it to the laser's
;; position. 
(define (get-laser-penetration-pos laser-obj)
  (let* ((delta 2)
         (pos (game-object-pos laser-obj))
         (dy (point-y (game-object-speed laser-obj)))
         (delta-vect 
          (cond ((< dy 0) (new point 0 (- delta)))
                ((> dy 0) (new point 0 delta))
                (else (new point 0 0)))))
    (point-add pos delta-vect)))


;;;; Shields ;;;;
(define (generate-shields level)
  (define speed (new point 0 0))
  (define (generate-particles)
    (map (lambda (p) (new point (pos2d-x p) (pos2d-y p)))
         (rgb-pixels-to-boolean-point-list
          (parse-ppm-image-file "sprites/shield0.ppm")
          rgb-threshold?)))

  (list (new shield 'shield1 (new point  36 40) 0 'green
                     speed (generate-particles) level)
        (new shield 'shield2 (new point  81 40) 0 'green
                     speed (generate-particles) level)
        (new shield 'shield3 (new point 126 40) 0 'green
                     speed (generate-particles) level)
        (new shield 'shield4 (new point 171 40) 0 'green
                     speed (generate-particles) level)))

;; Get particle cloud associated with a certain colliding object.
(define (get-explosion-particles colliding-obj)
  (cond ((player_laser? colliding-obj) player-laser-explosion-particles)
        ((laser-obj? colliding-obj)    invader-laser-explosion-particles)
        (else (invader-ship-particles colliding-obj))))

;; Damage the givent shield such that all the particles inside the
;; colliding-obj are removed from the shield's particles.
(define (shield-explosion! shield colliding-obj)
  (define (particle-member p p-list)
    (if (not (pair? p-list))
        #f
        (if (point= p (car p-list))
            p-list
            (particle-member p (cdr p-list)))))
  (let* ((explosion-particles (get-explosion-particles colliding-obj))
         (explosion-pos       (game-object-pos colliding-obj))
         (explosion-speed     (game-object-speed colliding-obj))
         (shield-pos          (game-object-pos shield))
         (particles           (shield-particles shield))
         (relative-expl-particles
          (let ((relative-expl-pos (point-sub explosion-pos
                                              shield-pos)))
            (map (lambda (ex-part) (point-add ex-part relative-expl-pos))
                 explosion-particles)))
         (new-particles
          (filter (lambda (p) (not (particle-member p relative-expl-particles)))
                  particles)))
    (shield-particles-set! shield new-particles)))


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
(define-class wall () (slot: rect) (slot: id)
  (constructor: (lambda (obj x y width height id)
                  (set-fields! obj wall
                               ((rect (new rect x y width height))
                                (id id))))))

(define (generate-walls)
  (list (new wall wall-x-offset   screen-bottom-offset +inf.0 -inf.0 'bottom)
        (new wall wall-x-offset   screen-bottom-offset -inf.0 +inf.0 'left)
        (new wall gamefield-max-x gamefield-max-y      -inf.0 +inf.0 'top)
        (new wall gamefield-max-x gamefield-max-y      +inf.0 -inf.0 'right)))


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
  (slot: wall-collision-detected?)
  (slot: draw-game-field?)
  (slot: game-paused?))


(define-class 2p-game-level (game-level)
  (slot: finished?)
  (slot: other-finished?)
  (slot: other-score))

;; Level utilitary functions
(define (level-add-object! lvl obj)
  (table-set! (level-object-table lvl) (game-object-id obj) obj))

(define (level-spawn-object! lvl obj)
  (level-add-object! lvl obj)
  (spawn-brother obj))

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


(define (get-invaders-from-row level row-index)
  (filter (lambda (inv) (and (invader-ship? inv)
                             (= (invader-ship-row inv) row-index)))
          (level-all-objects level)))

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

(define (play-level level)
  (let ((sim-res (apply simple-boot (level-init-corouts level))))
    (pp `(sim-res ,sim-res))
    sim-res))

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
  (define speed (new point 0 0))
  (define hi-score (level-hi-score level))
  (for-each
   (lambda (m) (level-spawn-object! level m))
   (append 
    (list
     (new message-obj 'top-banner (new point 2 y) state 'white speed
          level "SCORE<1>  HI-SCORE  SCORE<2>")
     (new message-obj 'hi-score-msg
          (new point 95 (- y 17)) state 'white speed
          level (get-score-string hi-score)))
    (if (game-level? level)
        (list
         (new message-obj 'player2-score-msg 
                           (new point 175 (- y 17)) state 'white speed level "")
         (new message-obj 'player1-score-msg 
                           (new point 15 (- y 17)) state 'white speed level ""))
        '()))))

;; New game level generation. Works for both single and 2 player
;; games, but the behaviour will differ a bit in the 2 cases.
(define (new-level init-score hi-score number-of-players player-id)
  (let* ((walls (generate-walls))
         (wall-damage '())
         (init-corouts 'unbound)
         (lives 3)
         (wall-collision-detected? #f)
         (draw-game-field? #t)
         (game-paused? #f)
         (finished? #f)        ; only used for 2p games
         (other-finished? #f)  ; only used for 2p games
         (other-score 0)       ; only used for 2p games
         (level (if (= number-of-players 2)

                    (make-2p-game-level
                     screen-max-y screen-max-x (make-table)
                     hi-score init-corouts (new-mutex)
                     player-id init-score lives 
                     walls wall-damage wall-collision-detected?
                     draw-game-field? game-paused?
                     finished? other-finished? other-score)
                    
                    (make-game-level
                     screen-max-y screen-max-x (make-table)
                     hi-score init-corouts (new-mutex)
                     player-id init-score lives 
                     walls wall-damage wall-collision-detected?
                     draw-game-field? game-paused?)))
         (level-setup-corout
          (new corout
               'game-initialisation
               (compose-thunks
                ;; Intro anim
                ;;(start-of-game-animation level)
                (invader-generator level)
                ;; Setup level and start primordial game corouts
                (lambda ()
                  (new player-ship level)
                  ;(spawn-brother (new spawner-agent level))
                  (spawn-brother (new mothership level)))
                (start-game! level))))
         (redraw-corout (new redraw-agent level))
         (init-corouts
          (list level-setup-corout
                redraw-corout)))

    (add-global-score-messages! level)
    (for-each (lambda (s) (level-spawn-object! level s)) (generate-shields level))
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
        (begin (resolve-collision! level obj collision-obj)
               collision-obj)
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

(define-generic detect-collision?)

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
  (if (detect-collision? cast: '(game-object game-object) obj shield)
      (let* ((pos (game-object-pos shield)))
        (exists (lambda (particle)
                  (detect-collision? (point-add pos particle)
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

(define-generic resolve-collision!)

;; Abstraction used to simplify the resolve-laser-collision!
;; function. This will remove the laser object from the level and
;; depending on the laser type, might re-scheduled a new laser shot.
(define (destroy-laser! level laser-obj)
  (level-remove-object! level laser-obj)
  )

;; Laser collisions ;;
(define-method (resolve-collision! level (laser laser-obj) (inv invader-ship))
  (level-increase-score! level inv)
  (! laser 'die)
  (! inv   'die)
  (level-spawn-object! level (new invader_explosion inv level)))

(define-method (resolve-collision! level (laser1 laser-obj) (laser2 laser-obj))
  (let ((inv-laser (if (player_laser? laser1) laser2 laser1)))
    (explode-laser! level inv-laser)
    (! inv-laser 'die)))

(define-method (resolve-collision! level (laser laser-obj) (expl explosion))
  (explode-laser! level laser)
  (! laser 'die))

(define-method (resolve-collision! level (laser laser-obj) (player player-ship))
  (! player 'die)
  (! laser  'die))

(define-method (resolve-collision! level (laser laser-obj) (shield shield))
  (let ((penetrated-pos (get-laser-penetration-pos laser)))
    (game-object-pos-set! laser penetrated-pos)
    (explode-laser! level laser)
    (shield-explosion! shield laser))
  (! laser 'die))


(define-method (resolve-collision! level (laser laser-obj) (mother mothership))
  (! laser  'die)
  (! mother 'die)
  (level-spawn-object! level (new mothership_explosion mother level))
  (level-increase-score! level mother))

(define-method (resolve-collision! level (laser laser-obj) (wall wall))
  (damage-wall! level laser)
  (explode-laser! level laser)
  (! laser 'die))

;; Invader Collision ;;
(define-method (resolve-collision! level (invader invader-ship) (shield shield))
  (shield-explosion! shield invader))

(define-method (resolve-collision! level (invader invader-ship) (wall wall))
  (cond ((eq? (wall-id wall) 'bottom)
         (game-over! level))
        ((or (eq? (wall-id wall) 'right)
             (eq? (wall-id wall) 'left))
         (broadcast `(row-controller ,(invader-ship-row invader))
                    'go-down-warning)
         (game-level-wall-collision-detected?-set! level #t))))


(define-method (resolve-collision! level (inv invader-ship) (laser laser-obj))
  (resolve-collision! level laser inv))


;; Player collisions ;;
(define-method (resolve-collision! level (player player-ship) (wall wall))
  (let ((current-speed (game-object-speed player)))
    (game-object-speed-set! player
                            (new point
                                 (- (point-x current-speed))
                                 (point-y current-speed)))
    (move-object! level player)))
(define-method (resolve-collision! level
                                   (player player-ship)
                                   (inv    invader-ship))
  'todo
;;   (spawn-brother-thunk 'player-explosion
;;                        (lambda () (explode-player! level player)))
  (explode-invader! level inv))

(define-method (resolve-collision! level (player player-ship) (laser laser-obj))
  (resolve-collision! level laser player))



;; Mothership collisions ;;
(define-method (resolve-collision! level (mother mothership) (wall wall))
  (! mother 'die))

(define-method (resolve-collision! level (mother mothership) (laser laser-obj))
  (resolve-collision! level laser mother))

;; Fallback ;;
(define-method (resolve-collision! level (o1 game-object) (o2 game-object))
  (pp `(Warning: unresolved collision between
                 ,(get-class-id o1) and ,(get-class-id o2))))



;;*****************************************************************************
;;
;;                 Gameplay related simulation events 
;;
;;*****************************************************************************

;; *Important note*: these generic functions must be called from
;;                   *within* the obj coroutine such that (self) = obj.
(define-generic behaviour)
(define-generic die)
(define-generic pause)

;; Default behaviour
(define-method (behaviour (obj corout) level)
  (pretty-print (to-string
                 (show "Warning: no behaviour defined for object of type "
                       (get-class-id obj))))
  (recv (wait-forever....! (error 'should-no-go-here))))

(define (pause-game level)
  (let ((paused? (game-level-game-paused? level)))
    (broadcast 'redraw (if paused? 'unpause 'pause))
    (game-level-game-paused?-set! level (not paused?))))

(define-method (pause (obj game-object) level)
  (recv (unpause 'ok)))

(define-method (die (obj game-object) level)
  (pp `(object ,(corout-id (self)) died))
  (level-remove-object! level obj)
  ;; the terminate-corout unsubscribes the coroutine of its msg lists
  (terminate-corout (to-string (show (corout-id (self)) " died normally"))))

(define-method (die (obj invader-ship) level)
  
  (die cast: '(game-object *) obj level))

(define-method (die (obj laser-obj) level)
  (if (eq? (game-object-sprite-id obj) 'player_laser)
      (set! player-laser-last-destruction-time (time->seconds (current-time))))
  (die cast: '(game-object *) obj level))

(define-method (die (obj player-ship) level)
  (level-loose-1-life! level)
  (level-spawn-object! level (new player_explosion obj level))
  (die cast: '(game-object *) obj level))


;;;;;;;;;; Barrier definition and usage facilities ;;;;;;;;;;

(define-class Barrier (corout) (slot: agent-arrived)
  (constructor: (lambda (obj thunk)
                  (init! cast: '(corout * *) obj
                         (gensym 'barrier)
                         thunk)
                  (Barrier-agent-arrived-set! obj 0))))

;; Condition is actively tested, i.e. that it is executed each time a
;; message is received. This enables the use of dynamic barriers.
(define-macro (define-wait-state state-name msg condition . barrier-open-code)
  `(define (,state-name)
     (recv (,msg
            (begin
              (update! (self) Barrier agent-arrived (lambda (n) (+ n 1)))
              (pp `(,(corout-id (self)) barrier
                    (sn: ,(object->serial-number (self))) status:
                    (,(Barrier-agent-arrived (self)) / ,,condition)
                    (,(Barrier-agent-arrived (self)) / ,(msg-list-size instant-components))
                    ,(map corout-id (get-msg-list instant-components))
                    ,(msg-list-size instant-components)
                    ,(length (get-msg-list instant-components))))
              (if (>= (Barrier-agent-arrived (self)) ,condition)
                  (begin
                    ;; make sure there is no left-overs of msg 
                    (clean-mailbox ,msg)
                    (Barrier-agent-arrived-set! (self) 0)
                    ,@barrier-open-code)
                  (,state-name)))))))

(define instant-components 'instant-components)
(define (wait-for-next-instant)
;;   (pp `(waiting for instant ,(corout-id (self))
;;                 (inst list size ,(msg-list-size instant-components))
;;                 (mbox: ,(queue->list (corout-mailbox (self))))))
  (broadcast 'redraw 'redraw)
  (recv (next-instant 'ok)))


;;;;;;;;;; Invader Generation ;;;;;;;;;;

(define (invader-generator level)
  (define animation-delay 0.01)
  (define x-offset 30)
  (define y-offset (- 265 152))

  (define (generate-inv! row col)
    (let* ((x (+ x-offset (* col invader-spacing)))
           (y (+ y-offset (* row invader-spacing)))
           (pos (new point x y))
           (state 1)
           (speed (new point invader-x-movement-speed 0))
           (inv
            (cond ((< col 2) (new easy-invader
                                  pos state 'white speed row col level))
                  ((< col 4) (new medium-invader
                                  pos state 'white speed row col level))
                  (else      (new hard-invader
                                  pos state 'white speed row col level)))))
      (level-spawn-object! level inv)))
                         
  (lambda ()
    (let loop ((row 0) (col 0))
      (if (< row invader-row-number)
          (if (< col invader-col-number)
              (begin (generate-inv! row col)
                     (thread-send user-interface-thread `(redraw ,level))
                     (sleep-for animation-delay)
                     (loop row (+ col 1)))
              (begin
                (yield)
                (loop (+ row 1) 0)))))
    
    (let loop ((row 0)  (controllers '()))
      (if (< row invader-row-number)
          (begin (let ((controller
                        (spawn-brother (new Inv-Controller row))))
                   (yield)
                   (loop (+ row 1) (cons controller controllers))))))
    (thread-send user-interface-thread `(redraw ,level))
    (sleep-for animation-delay)))

(define (start-game! level)
  (lambda ()
    (broadcast '(row-controller 0) 'init)))


;;;;;;;;;; Invader behaviour ;;;;;;;;;;

(define-method (behaviour (inv invader-ship) level)
  (define (main-state)
    (recv
     (move
      (begin
        (move-object! level (self))
        (broadcast `(row-controller ,(invader-ship-row (self))) 'moved)
        (main-state)))
     ((wall-collision ,move-back? ,sync-host?)
      (begin
        (let ((dx (point-x (game-object-speed (self)))))
          (point-x-set! (game-object-speed (self))
                        (if move-back? (- (* 2 dx)) 0))
          (point-y-set! (game-object-speed (self)) (- invader-y-movement-speed))
          (move-object! level (self))
          (point-x-set! (game-object-speed (self)) (- dx))
          (point-y-set! (game-object-speed (self)) 0))
        (if sync-host? (! sync-host? 'moved))
        (main-state)))
     (player-explosion
      (begin (player-expl-state)))
     (game-paused
      (begin (paused-state)))))

  ;; init state
  main-state)

;;;;;;;;;; Invader Controller ;;;;;;;;;;

(define-class Inv-Controller (Barrier) (slot: row)
  (constructor: (lambda (obj row)
                  (init! cast: '(Barrier *) obj invader-controller)
                  (subscribe `(row-controller ,row) obj)
                  (set-fields! obj Inv-Controller
                               ((id (gensym 'Inv-Controller))
                                (row row))))))

(define (invader-controller)
  ;; Utilitaries
  (define (next-row previous-row)
    (define (inc x) (modulo (+ x 1) invader-row-number))
    (let loop ((r (inc previous-row)))
      (cond
       ((not (zero? (msg-list-size `(invader-row ,r))))
        r)
       ((= r previous-row)
        r)
       (else (loop (inc r))))))

  (define (inv-nb)
    (msg-list-size `(invader-row ,(Inv-Controller-row (self)))))

  ;; States
  (define (init-state)
    (recv
     (init
      (subscribe instant-components (self))
      (broadcast `(invader-row ,(Inv-Controller-row (self)))
                 'move)
      (wait-state))))
  (define-wait-state wait-state moved (inv-nb)
    (recv
     (go-down-warning
      (let ((row-nb (Inv-Controller-row (self))))
        (broadcast `(invader-row ,row-nb) 'wall-collision)
        (for i 0 (< i invader-row-number)
             (if (not (= i row-nb))
                 (if (not (zero? (msg-list-size `(invader-row ,i))))
                     (let ((move-back? (< i row-nb)))
                       (broadcast `(invader-row ,i)
                                  `(wall-collision ,move-back? #f))))
                 (broadcast `(invader-row ,i)
                            `(wall-collision  #t ,(self))))))
      (wait-state))
     ;; if no wall collision, then proceed to the next state
     (after 0
            (wait-for-next-instant)
            (broadcast `(row-controller
                         ,(next-row (Inv-Controller-row (self))))
                       'init)
            (unsubscribe instant-components (self))
            (init-state))))

  ;; initialization
  (init-state))

;;;;;;;;;; Mothership behaviour ;;;;;;;;;;

(define-method (behaviour (obj mothership) level)
  (define (mothership-random-delay) (+ (random-integer 10) 5))
  (define (random-score-val) (vector-ref '#(50 100 150) (random-integer 3)))
  (define (initialize-data!)
    (let ((dx-mult (vector-ref '#(1 -1) (random-integer 2))))
      (set-fields! obj mothership
                   ((pos (new point
                              (if (> dx-mult 0)
                                  gamefield-min-x
                                  (- gamefield-max-x 16))
                              201))
                    (state 0)
                    (color 'red)
                    (speed (new point
                                (* dx-mult mothership-movement-speed)
                                0))
                    (score-value (random-score-val))))))
  
  (define (main-state)
    (initialize-data!)
    (sleep-for 0) ; <- FIXME!
    (level-add-object! level (self))
    (subscribe instant-components (self))
    (let loop ()
      (move-object! level (self))
      (if (not (eq? (wait-for-next-instant) 'died))
          (loop)
          (main-state))))

  main-state)

(define-method (die (obj mothership) level)
  (level-remove-object! level (self))
  (unsubscribe instant-components (self))
  'died)


;;;;;;;;;; Lasers behaviour ;;;;;;;;;;

(define (shoot-laser! level laser-instance-creator shooter-obj dy)
  (define is-player-laser?
    (eq? laser-instance-creator make-player_laser-instance))
  ;; if the shot laser is a player laser, there must not be another
  ;; player laser in the game or the player-laser-refresh-constraint
  ;; must be elabsed before shooting a new one.
  (if (not (and is-player-laser?
                (or (level-player-laser level)
                    (< (- (time->seconds (current-time))
                          player-laser-last-destruction-time)
                       player-laser-refresh-constraint))))
                    
      (let* ((laser-obj (laser-instance-creator))
             (shooter-x (point-x (game-object-pos shooter-obj)))
             (shooter-y (point-y (game-object-pos shooter-obj)))
             (x         (+ shooter-x (floor (/ (type-width shooter-obj) 2))))
             (y         (if (< dy 0)
                            (- shooter-y
                               (type-height laser-obj )
                               invader-y-movement-speed)
                            (+ shooter-y (type-height shooter-obj))))
             (laser-id  (if is-player-laser? 'player-laser (gensym 'laser))))

        ;;(id pos state color speed level)
        (init! laser-obj laser-id (make-point x y) 0 'white (make-point 0 dy)
               level)
        (level-spawn-object! level laser-obj))))

(define (find-shooter-candidates level)
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

  ;; FIXME: Inefficient or bottleneck??
  (filter bottom-invader? (level-invaders level)))

(define (explode-laser! level laser-obj)
  (define (center-pos pos expl-type-width)
    ;; FIXME: ugly hack where only player laser's explotion are
    ;; centered in x. I'm unsure why other lasers don't require this
    ;; shift so far...
    (if (player_laser? laser-obj)
        (point-sub pos (make-point (floor (/ expl-type-width 2))
                                   0))
        pos))
  (let* ((expl-instance (if (player_laser? laser-obj)
                                    (make-player_laser_explosion-instance)
                                    (make-invader_laser_explosion-instance)))
         (obj (init! cast: '(game-object * * * * * *)
                     expl-instance
                     (gensym (symbol-append (corout-id laser-obj) '-explosion))
                     (game-object-pos laser-obj) ; not centered
                     0
                     (choose-color (game-object-pos laser-obj))
                     (make-point 0 0)
                     level)))
    (update! obj laser-obj pos (lambda (p) (center-pos p (type-width obj))))
    (level-spawn-object! level obj)))

(define-method (behaviour (obj laser-obj) level)
  (define (init-state)
    (subscribe instant-components (self))
    (main-state))
  (define (main-state)
    (move-object! level (self))
    (wait-for-next-instant)
    (main-state))
  init-state)

(define-method (behaviour (obj player_laser) level)
  (define (init-state)
    (subscribe instant-components (self))
    (main-state))
  (define (main-state)
    (move-object! level (self))
    (wait-for-next-instant)
    (main-state))
  init-state)

;;;;;;;;;; Explosions behaviour ;;;;;;;;;;

(define-method (behaviour (obj explosion) level)
  (define animation-delay 0.03)
  (lambda ()
   (sleep-for animation-delay)
   (die (self) level)))

(define-method (behaviour (obj player_explosion) level)
  (define animation-delay 0.03)
  (lambda ()
    (pause-game level)
    (sleep-for animation-delay)
    (pause-game level)
    (die (self) level)))

;;;;;;;;;; Spawner agent behaviour ;;;;;;;;;;

(define-class spawner-agent (corout)
  (constructor: (lambda (obj level)
                  (init! cast: '(corout * *)
                         obj (gensym 'spawner-agent) (behaviour obj level))
                  (subscribe 'spawner-agent obj))))

(define-method (behaviour (obj spawner-agent) level)
  (define (random-access lst) (list-ref lst (random-integer (length lst))))
  (define creators (list make-laserA-instance
                         make-laserB-instance
                         make-laserC-instance))
  (define (shoot-invader-laser!)
    (let* ((laser-creator  (random-access creators))
           (shooter        (random-access (find-shooter-candidates level))))
      (shoot-laser! level laser-creator shooter (- invader-laser-speed))))
  (define (main-state)
    (recv (shoot-laser 'ok))
    (sleep-for next-invader-laser-interval)
    (shoot-invader-laser!)
    (main-state))
  main-state)


;;;;;;;;;; Redraw agent behaviour ;;;;;;;;;;

(define-class redraw-agent (Barrier)
  (constructor: (lambda (obj level)
                  (init! cast: '(Barrier *) obj (behaviour obj level))
                  (subscribe 'redraw obj))))


(define (process-user-input level)
  (define (player-can-move?)
    (and (level-player level) (not (game-level-game-paused? level))))
  
  (let ((player (level-player level))
        (msg (thread-receive 0 #f)))
    (if msg
        (case msg
          ((space)
           (if (player-can-move?)
               (shoot-laser! level
                             make-player_laser-instance
                             (level-player level)
                             player-laser-speed)
               ))
          ((right)
           (if (player-can-move?)
               (let ((new-speed (new point player-movement-speed 0)))
                 (game-object-speed-set! player new-speed)
                 (move-object! level player))))
          
          ((left)
           (if (player-can-move?)
               (let ((new-speed (new point (- player-movement-speed) 0)))
                 (game-object-speed-set! player new-speed)
                 (move-object! level player))))
          
          ((p) (pause-game level))

          ((r)
           (stop-sfx 'all)
           (super-kill-all! 'reset))

          ((d) (error "DEBUG"))))))

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

(define-method (behaviour (obj redraw-agent) level)
  (define-wait-state main-state redraw (msg-list-size instant-components)
    (begin
      (process-user-input level)
      (update-score-msg! level)
      ;; FIXME: It would be better to either copy the level obj
      ;; before passing it to redraw or do a syncronous remote call
      ;; (in the termite !? style)
      ;; Could also be done within this thread...
      (thread-send user-interface-thread `(redraw ,level))
      (broadcast instant-components 'next-instant)
      (main-state)))
  main-state)




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


;; Main game loop that dispatch and run the different intro/game levels
(define (game-loop ui-thread)
  (define init-high-score (retrieve-hi-score))
  (set! user-interface-thread ui-thread)
    (lambda ()
      ;; infinite loop, where the result of playing a level should
      ;; either tell explicitely which level should be played next, or
      ;; return the hi-score of the last played game.
      (let inf-loop ((hi-score init-high-score)
                     (result 'restart!))
        (cond
         ((and (number? result) (> result hi-score))
          (save-hi-score result)
          (inf-loop result 'restart!))
         
         (else
          (let ((p1 (new corout 
                         'player1
                         (lambda ()
                           (play-level (new-level 0 hi-score 1 player1))))))
            (inf-loop hi-score (simple-boot p1))))))))


;;*****************************************************************************
;;
;;                              Rendering
;;
;;*****************************************************************************

(define-generic render)

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
    (render-fontified-sprite (game-object-sprite-id obj) x y state color)))

(define-method (render (level level))
  ;; Draw all objects
  (for-each render (level-all-objects level)))

(define-method (render (level game-level))
  (render cast: '(level) level)
  
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

      ;; FIXME: if we don't the draw the field, a black square is
      ;; drawn to cover unwanted already drawn objects... hehe :(
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


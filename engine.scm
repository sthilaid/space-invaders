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
(parameterize ((current-directory "oops/src"))
  (load "oops"))
(include "oops/src/oops-macros.scm")


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

;; (define invader-row-number 5)
;; (define invader-col-number 11)
(define invader-row-number 2)
(define invader-col-number 2)

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
;;             Data Structures definitions and operations
;;
;;*****************************************************************************

;;;; 2d position coordinate additionnal operations ;;;;
(define (inverse-dir dir . options)
  (let ((x-fact (if (memq 'x options) -1 1))
        (y-fact (if (memq 'y options) -1 1)))
    (<2dcoord> :x: (* x-fact (:x dir))
               :y: (* y-fact (:y dir)))))

;;;; Rectangle structure used in collision detection ;;;;
;;(define-type rect x y width height)
(define-class <rect> ()
  (:x :y :width :height))

(define-class <2dcoord> ()
  (:x :y))

(define-method (add (p1 <2dcoord>) (p2 <2dcoord>))
  (<2dcoord> :x: (+ (:x p1) (:x p2))
             :y: (+ (:y p1) (:y p2))))

(define-method (sub (p1 <2dcoord>) (p2 <2dcoord>))
  (<2dcoord> :x: (- (:x p1) (:x p2))
             :y: (- (:y p1) (:y p2))))

(define-method (instance-equal? (o1 <2dcoord>) (o2 <2dcoord>))
  (and (= (:x o1) (:x o2))
       (= (:y o1) (:y o2))))

(define-method (scalar-prod (p1 <2dcoord>) (p2 <2dcoord>))
  (+ (* (:x p1) (:x p2)) (* (:y p1) (:y p2))))

(define-method (cartesian-distance (p1 <2dcoord>) (p2 <2dcoord>))
  (sqrt (+ (expt (- (:x p1) (:x p2)) 2)
           (expt (- (:y p1) (:y p2)) 2))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main game object definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; General game object description ;;;;
;; (define-type game-object id type pos state color speed
;;   extender: define-type-of-game-object)


(define-class <game-object> ()
  (#;(:class-id allocation: class:)
   #;(:bbox allocation: class:)
   #;(:state-num allocation: class:)
   (:id)
   (:pos)))

(define-class <statefull-object> ()
  (:state))

(define-class <colored-object> ()
  (:color))

(define-class <movable-object> ()
  (:speed))

(define-class <valuable-object> ()
  (#; (score-value allocation: class: init-value: 0)))

;; Abstract class that defines the requirements for sprites
(define-class <sprite-object> (<game-object>
                               <statefull-object>
                               <colored-object>)
  ())

(define-method (type-height (obj <game-object>))
  (:height (:bbox obj)))

(define-method (type-width (obj <game-object>))
  (:width (:bbox obj)))

(define-method (cycle-state! (obj <statefull-object>))
  (define current-state (:state obj))
  (if (number? current-state)
      (:state-set! obj (modulo (+ current-state 1) (:state-num obj)))))

(define (get-absolute-bounding-box obj)
  (<rect> :x: (+ (:x (:pos obj)) (:x (:bbox obj)))
          :y: (+ (:y (:pos obj)) (:y (:bbox obj)))
          :width: (type-width obj)
          :height: (type-height obj)))

;; In function of a certain coordinate, will choose the appropriate
;; color for the object (red if in the upper-screen, white in the
;; middle and green in the lower part).
(define choose-color
  (let ((red-bottom (- screen-max-y 81))
        (normal-bottom (- screen-max-y 201))
        (green-bottom (- screen-max-y 259)))
    (lambda (pos)
      (let ((y (:y pos)))
        (cond 
         ((> y red-bottom) 'red)
         ((> y normal-bottom) 'white)
         ((> y green-bottom) 'green)
         (else 'white))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other object derived types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Specific game object descriptions ;;;;

(define-class <invader> (<sprite-object>
                         <movable-object>
                         <valuable-object>)
  ((:row)
   (:col)))

(define-class <easy> (<invader>)
  ((:class-id    allocation: class: init-value: 'easy)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                       :width: 12 :height: 8))
   (:state-num   allocation: class: init-value: 2)
   (:score-value allocation: class: init-value: 10)))

(define-class <medium> (<invader>)
  ((:class-id    allocation: class: init-value: 'medium)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                       :width: 12 :height: 8))
   (:state-num   allocation: class: init-value: 2)
   (:score-value allocation: class: init-value: 20)))

(define-class <hard> (<invader>)
  ((:class-id    allocation: class: init-value: 'hard)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                       :width: 12 :height: 8))
   (:state-num   allocation: class: init-value: 2)
   (:score-value allocation: class: init-value: 30)))

(define-class <player> (<sprite-object>
                        <movable-object>)
  ((:class-id    allocation: class: init-value: 'player)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                        :width: 13 :height: 8))
   (:state-num   allocation: class: init-value: 1)))

(define-class <mothership> (<sprite-object>
                            <movable-object>
                            <valuable-object>)
  ((:class-id    allocation: class: init-value: 'mothership)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                       :width: 16 :height: 7))
   (:state-num   allocation: class: init-value: 1)
   ;;score val is not constant, wil be initialized later
   (:score-value allocation: class: init-value: 0)))

(define-class <laser> (<sprite-object>
                       <movable-object>)
  ())

(define-class <laserA> (<laser>)
  ((:class-id    allocation: class: init-value: 'laserA)
   (:bbox        allocation: class: init-value: (<rect> :x: 1 :y: 0
                                                       :width: 1 :height: 7))
   (:state-num   allocation: class: init-value: 6)))

(define-class <laserB> (<laser>)
  ((:class-id    allocation: class: init-value: 'laserB)
   (:bbox        allocation: class: init-value: (<rect> :x: 1 :y: 0
                                                       :width: 1 :height: 7))
   (:state-num   allocation: class: init-value: 8)))

(define-class <laserC> (<laser>)
  ((:class-id    allocation: class: init-value: 'laserC)
   (:bbox        allocation: class: init-value: (<rect> :x: 1 :y: 0
                                                       :width: 1 :height: 7))
   (:state-num   allocation: class: init-value: 4)))

(define-class <player-laser> (<laser>)
  ((:class-id    allocation: class: init-value: 'player_laser)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                       :width: 1 :height: 7))
   (:state-num   allocation: class: init-value: 1)))

(define-class <shield> (<game-object>
                        <colored-object>)
  ((:class-id    allocation: class: init-value: 'shield)
   (:bbox        allocation: class:
                 init-value: (<rect> :x: 0 :y: 0 :width: 22 :height: 16))
   (:state-num   allocation: class: init-value: 1)
   (:particles)))

(define-class <explosion> (<sprite-object>
                           <movable-object>)
  ())

(define-class <invader-explosion> (<explosion>)
  ((:class-id    allocation: class: init-value: 'invader_explosion)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                       :width: 13 :height: 8))
   (:state-num   allocation: class: init-value: 1)))

(define-class <invader-laser-explosion> (<explosion>)
  ((:class-id    allocation: class: init-value: 'invader_laser_explosion)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                       :width: 6 :height: 8))
   (:state-num   allocation: class: init-value: 1)))

(define-class <player-explosion> (<explosion>)
  ((:class-id    allocation: class: init-value: 'player_explosion)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                       :width: 16 :height: 8))
   (:state-num   allocation: class: init-value: 2)))

(define-class <player-laser-explosion> (<explosion>)
  ((:class-id    allocation: class: init-value: 'player_laser_explosion)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                       :width: 8 :height: 8))
   (:state-num   allocation: class: init-value: 1)))

(define-class <mothership-explosion> (<explosion>)
  ((:class-id    allocation: class: init-value: 'mothership_explosion)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                       :width: 21 :height: 8))
   (:state-num   allocation: class: init-value: 1)))

(define-class <message> (<game-object>
                         <movable-object>
                         <colored-object>)
  ;; Class allocated slots ignored here at the moment...
  ((:class-id    allocation: class: init-value: 'message)
   (:bbox        allocation: class: init-value: (<rect> :x: 0 :y: 0
                                                        :width: 0 :height: 0))
   (:text)))

;; (define-type-of-game-object invader-ship row col)
;; (define-type-of-game-object player-ship)
;; (define-type-of-game-object mothership)
;; (define-type-of-game-object laser-obj)
;; (define-type-of-game-object shield particles)
;; (define-type-of-game-object message-obj text)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; object type definition ;;;;
;; (define-type object-type id bbox state-num score-value)
;; (define type-id object-type-id)
;; (define (type-height t) (rect-height (object-type-bbox t)))
;; (define (type-width t) (rect-width (object-type-bbox t)))

;; ;; Global associative list of all object types
;; (define types
;;   ;; Bounding boxes for all ship types must be equal such that they
;;   ;; behave the same way in the level.
;;   `( (easy ,(make-object-type 'easy (make-rect 0 0 12 8) 2 10))
;;      (medium ,(make-object-type 'medium (make-rect 0 0 12 8) 2 20))
;;      (hard ,(make-object-type 'hard (make-rect 0 0 12 8) 2 30))
;;      (mothership ,(make-object-type 'mothership (make-rect 0 0 16 7) 1 100))
;;      (player ,(make-object-type 'player (make-rect 0 0 13 8) 1 0))
;;      ;; Little bounding box hack here to facilitate the laser
;;      ;; penetration into shields.
;;      (laserA ,(make-object-type 'laserA (make-rect 1 0 1 7) 6 0))
;;      (laserB ,(make-object-type 'laserB (make-rect 1 0 1 7) 8 0))
;;      (laserC ,(make-object-type 'laserC (make-rect 1 0 1 7) 4 0))
;;      (player_laser
;;       ,(make-object-type 'player_laser (make-rect 0 0 1 7) 1 0))
;;      (shield ,(make-object-type 'shield (make-rect 0 0 22 16) 1 0))
;;      (invader_explosion
;;       ,(make-object-type 'invader_explosion (make-rect 0 0 13 8) 1 0))
;;      (invader_laser_explosion
;;       ,(make-object-type 'invader_laser_explosion (make-rect 0 0 6 8) 1 0))
;;      (player_laser_explosion
;;       ,(make-object-type 'player_laser_explosion (make-rect 0 0 8 8) 1 0))
;;      (player_explosion
;;       ,(make-object-type 'player_explosion (make-rect 0 0 16 8) 2 0))
;;      (mothership_explosion
;;       ,(make-object-type 'mothership_explosion (make-rect 0 0 21 8) 1 0))
;;      (message ,(make-object-type 'message (make-rect 0 0 0 0) 0 0))
;;    ))

;; (define (get-type type-name)
;;   (let ((type (assq type-name types)))
;;   (if type
;;       (cadr type)
;;       (error (string-append "no such type: " (symbol->string type-name))))))

;; (define (get-type-id obj)
;;   (cond ((game-object? obj)
;;          (object-type-id (game-object-type obj)))
;;         ((wall-struct? obj) 'wall)
;;         (else (error "unknown object type"))))



;; Returns #t only if the type-id corresponds to an explosion
;; (define (is-explosion? type-id)
;;   (case type-id
;;     ((invader_laser_explosion
;;       player_laser_explosion invader_explosion player_explosion) #t)
;;     (else #f)))

;; (define (get-object-score-value obj)
;;   (object-type-score-value (game-object-type obj)))
      
      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Particles objects and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Function used in the rgb-pixels-to-boolean-point-list calls to
;; filter only pixels having some color in it. This will result in a
;; point cloud corresponding to the read image.
(define (rgb-threshold? r g b)
  (> (+ r g b) 150))

;; Calculate the interpenetration distance and add it to the laser's
;; position. 
(define-method (get-laser-penetration-pos (laser <laser>))
  (let* ((delta 2)
         (pos (:pos laser))
         (dy (:y (:speed laser)))
         (delta-vect 
          (cond ((< dy 0) (<2dcoord> :x: 0 :y: (- delta)))
                ((> dy 0) (<2dcoord> :x: 0 :y: delta))
                (else (<2dcoord> :x: 0 :y: 0)))))
    (add pos delta-vect)))


;;;; Shields ;;;;
(define (generate-shields)
  (define speed (<2dcoord> :x: 0 :y: 0))
  (define (generate-particles)
    (map (lambda (p) (<2dcoord> :x: (car p) :y: (cdr p)))
         (rgb-pixels-to-boolean-point-list
          (parse-ppm-image-file "sprites/shield0.ppm")
          rgb-threshold?)))
  

  (list (<shield> :id: 'shield1
                  :pos: (<2dcoord> :x: 36 :y: 40)
                  :color: 'green
                  :particles: (generate-particles))
        (<shield> :id: 'shield2
                  :pos: (<2dcoord> :x: 81 :y: 40)
                  :color: 'green
                  :particles: (generate-particles))
        (<shield> :id: 'shield3
                  :pos: (<2dcoord> :x: 126 :y: 40)
                  :color: 'green
                  :particles: (generate-particles))
        (<shield> :id: 'shield4
                  :pos: (<2dcoord> :x: 171 :y: 40)
                  :color: 'green
                  :particles: (generate-particles))))


;; (define (get-explosion-particles colliding-obj)
;;   (let ((type-id (type-id (game-object-type colliding-obj))))
;;     (cond ((eq? type-id 'player_laser) player-laser-explosion-particles)
;;           ((or (eq? type-id 'laserA)
;;                (eq? type-id 'laserB)
;;                (eq? type-id 'laserC))
;;            invader-laser-explosion-particles)
;;           (else
;;            (invader-ship-particles colliding-obj)))))

(let ((particles (map (lambda (p) (<2dcoord> :x: (car p) :y: (cdr p)))
                      (rgb-pixels-to-boolean-point-list
                       (parse-ppm-image-file "sprites/explodeL0.ppm")
                       rgb-threshold? 'center))))
  (define-method (get-explosion-particles (plaser <player-laser>))
    particles))

;; Get particle cloud associated with a certain colliding object.
(let ((particles (map (lambda (p) (<2dcoord> :x: (car p) :y: (cdr p)))
                      (rgb-pixels-to-boolean-point-list
                       (parse-ppm-image-file "sprites/explodeInvL0.ppm")
                       rgb-threshold? 'dont-center))))
  (define-method (get-explosion-particles (colliding-obj <laser>))
    particles))

(define-method (get-explosion-particles (inv <invader>))
  (define height (type-height inv))
  (define width (type-width inv))
  (let loop-y ((y 0) (acc '()))
    (if (< y height)
        (loop-y (+ y 1)
                (append (let loop-x ((x 0) (acc '()))
                          (if (< x width)
                              (loop-x (+ x 1)
                                      (cons (<2dcoord> :x: x :y: y) acc))
                              acc))
                        acc))
        acc)))



;; Damage the givent shield such that all the particles inside the
;; colliding-obj are removed from the shield's particles.
(define (shield-explosion! shield colliding-obj)
  (define explosion-particles (get-explosion-particles colliding-obj))
  (define explosion-pos (:pos colliding-obj))
  (define explosion-speed (:speed colliding-obj))
  (define shield-pos (:pos shield))
  (define particles (:particles shield))
  
  (define relative-expl-particles
    (let ((relative-expl-pos (sub explosion-pos shield-pos)))
      (map (lambda (ex-part) (add ex-part relative-expl-pos))
         explosion-particles)))
  
  (define (particle-member p p-list)
    (if (not (pair? p-list))
        #f
        (if (instance-equal? p (car p-list))
            p-list
            (particle-member p (cdr p-list)))))
  
  (define new-particles
    (filter (lambda (p) (not (particle-member p relative-expl-particles)))
            particles))
  (:particles-set! shield new-particles))
  
  


;;;; Wall ;;;;
(define (damage-wall! level laser-obj)
  (define explosion-particles (get-explosion-particles laser-obj))
  (define pos (:pos laser-obj))
  (define wall-damage
    (map 
     (lambda (p) (add p pos))
     (filter (lambda (p) (= (:y p) 0))
             explosion-particles)))
  ;; FIXME!!
  (level-damage-wall! level wall-damage))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wall data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Wall or game boundary structure ;;;;
;; (define-type wall-struct rect id)

(define-class <wall> ()
  ((:rect)
   (:id)))

(define (new-wall x y width height id)
  (<wall> :rect: (<rect> :x: x :y: y :width: width :height: height) :id: id))

;; (define wall? wall-struct?)
;; (define wall-rect wall-struct-rect)
;; (define wall-id wall-struct-id)

(define (generate-walls)
  (list (new-wall wall-x-offset screen-bottom-offset +inf.0 -inf.0 'bottom)
        (new-wall wall-x-offset screen-bottom-offset -inf.0 +inf.0 'left)
        (new-wall gamefield-max-x gamefield-max-y -inf.0 +inf.0 'top)
        (new-wall gamefield-max-x gamefield-max-y +inf.0 -inf.0 'right)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Level related data structure and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Game level description ;;;;
;; (define-type level
;;   height width object-table hi-score sim mutex
;;   extender: define-type-of-level)

(define-class <level> ()
  ((:height)
   (:width)
   (:object-table)
   (:hi-score)
   (:sim)
   (:mutex)))

;; A word should be mentionned on the draw-game-field? parameter since
;; it is mostly a hack to let know the user interface not to draw all
;; the invaders, shields, etc... This will occur in a 2p mid-game when
;; switching context from 1 player to another and that a PLAY
;; PLAYER<X> will be redisplayed. I am conscient that this is not
;; clean, but it greatly simplifies the problem.
;; (define-type-of-level game-level
;;   player-id score lives walls wall-damage draw-game-field?
;;   extender: define-type-of-game-level)

(define-class <game-level> (<level>)
  ((:player-id)
   (:score)
   (:lives)
   (:walls)
   (:wall-damage)
   (:draw-game-field?)))

;; (define-type-of-game-level 2p-game-level
;;   other-finished? other-score)

(define-class <2p-game-level> (<game-level>)
  ((:other-finished?)
   (:other-score)))

;; FIXME? use typed methods here?
;; Level utilitary functions
(define (level-add-object! lvl obj)
  (table-set! (:object-table lvl) (:id obj) obj))

(define (level-remove-object! lvl obj)
  (table-set! (:object-table lvl) (:id obj)))

(define (level-exists lvl obj-id)
  (table-ref (:object-table lvl) obj-id #f))

(define level-get level-exists)

(define (level-all-objects lvl)
  (map cdr (table->list (:object-table lvl))))

(define (level-invaders lvl)
  (filter (lambda (o) (is-a? o <invader>))
          (level-all-objects lvl)))

(define (level-messages lvl)
  (filter (lambda (o) (is-a? o <message>))
          (level-all-objects lvl)))

(define (game-level-shields level)
  (filter (lambda (o) (is-a? o <shield>))
          (level-all-objects level)))

(define (level-loose-1-life! lvl)
  (:lives-set! lvl (- (:lives lvl) 1)))

(define (level-increase-score! level obj)
  (:score-set! level (+ (:score level) (:score-value obj))))

(define (level-damage-wall! level damage)
  (define current-damage (:wall-damage level))
  (:wall-damage-set! level (generic-union instance-equal?
                                          current-damage damage)))

(define (game-over! level)
  ;;(exit-simulation (:score level)))
  (terminate-corout (:score level)))


;; Returns (not efficiently) the list of all invaders located on the
;; specified row index or '() if none exists.
(define (get-invaders-from-row level row-index)
  (filter (lambda (inv) (= (:row inv) row-index))
          (level-invaders level)))

(define (get-all-invader-rows level)
  (let loop ((i 0) (acc '()))
    (if (< i invader-row-number)
        (loop (+ i 1) (cons (get-invaders-from-row level i) acc))
        (reverse (cleanse acc)))))
          
(define (level-player lvl)
   (table-ref (:object-table lvl) 'player #f))

(define (level-player-laser lvl)
   (table-ref (:object-table lvl) 'player-laser #f))

(define (level-mothership lvl)
   (table-ref (:object-table lvl) 'mothership #f))

(define (new-player! level)
  (let* ((pos (<2dcoord> :x: 22 :y: (- screen-max-y 240)))
         (state 0)
         (speed (<2dcoord> :x: 0 :y: 0))
         (player-ship (<player> :id: 'player
                                :pos: pos
                                :state: state
                                :color: 'green
                                :speed: speed)))
    (level-add-object! level player-ship)))

(define (play-level level)
  (start-simulation! (:sim level) +inf.0))

(define (get-score-string score)
  (cond ((= score 0) "0000")
        ((< score 10) (string-append "000" (number->string score)))
        ((< score 100) (string-append "00" (number->string score)))
        ((< score 1000) (string-append "0" (number->string score)))
        (else (number->string score))))




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
  (let ((msg (cons (:score level) finished?)))
    (if (eq? (:player-id level) 'p2)
        (! (p1-corout) msg)
        (! (p2-corout) msg))))

(define-generic (receive-update-msg-from-other! level))

(define-method (receive-update-msg-from-other! (level <2p-game-level>))
  (if (not (corout-empty-mailbox?))
      (let ((msg (?)))
        (:other-score-set! level (car msg))
        (:other-finished?-set! level (cdr msg)))))

(define-method (receive-update-msg-from-other! level)
  'nothing)




;;*****************************************************************************
;;
;;                           Game Level Creation
;;
;;*****************************************************************************

;; Abstraction that will add to the level all the usual top banner
;; messages.
(define (add-global-score-messages! level)
  (define y 254)
  (define speed (<2dcoord> :x: 0 :y: 0))
  (for-each
   (lambda (m) (level-add-object! level m))
   (append 
    (list
     (<message> :id:    'top-banner
                :pos:   (<2dcoord> :x: 2 :y: y)
                :color: 'white
                :speed: speed
                :text:  "SCORE<1>  HI-SCORE  SCORE<2>")
     (<message> :id:    'hi-score-msg
                :pos:   (<2dcoord> :x: 95 :y: (- y 17))
                :color: 'white
                :speed: speed
                :text:  (get-score-string (:hi-score level))))
    (if (is-a? level <game-level>)
        (list
         (<message> :id:    'player2-score-msg 
                    :pos:   (<2dcoord> :x: 175 :y: (- y 17))
                    :color: 'white
                    :speed: speed
                    :text:  "")
         (<message> :id:    'player1-score-msg 
                    :pos:   (<2dcoord> :x: 15 :y: (- y 17))
                    :color: 'white
                    :speed: speed
                    :text:  ""))
        '()))))


;; New game level generation. Works for both single and 2 player
;; games, but the behaviour will differ a bit in the 2 cases.
(define (new-level init-score hi-score number-of-players player-id)
  (let* ((shields (generate-shields))
         (sim (create-simulation))
         (level (if (= number-of-players 2)

                    (<2p-game-level>
                     :height:           screen-max-y
                     :width:            screen-max-x
                     :object-table:     (make-table)
                     :hi-score:         hi-score
                     :sim:              sim
                     :mutex:            (new-mutex)
                     :player-id:        player-id
                     :score:            init-score
                     :lives:            3
                     :walls:            (generate-walls)
                     :wall-damage:      '()
                     :draw-game-field?: #t
                     :other-finished?:  #f
                     :other-score:      0)

                    (<game-level>
                     :height:           screen-max-y
                     :width:            screen-max-x
                     :object-table:     (make-table)
                     :hi-score:         hi-score
                     :sim:              sim
                     :mutex:            (new-mutex)
                     :player-id:        player-id
                     :score:            init-score
                     :lives:            3
                     :walls:            (generate-walls)
                     :wall-damage:      '()
                     :draw-game-field?: #t))))
    
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
    (schedule-event! sim 0 (create-redraw-event level))
    level))

;; Creation of the default initial intro movie
(define (new-animation-level-A hi-score)
  (let* ((sim (create-simulation))
         (level (<level> :height:       screen-max-y
                         :width:        screen-max-x
                         :object-table: (make-table)
                         :hi-score:     hi-score
                         :sim:          sim
                         :mutex:        (new-mutex))))
    (add-global-score-messages! level)
    
    (schedule-event! sim 0 (create-animation-A-event level))
    (schedule-event! sim 0 (create-intro-manager-event level))
    (schedule-event! sim 0 (create-redraw-event level))
    level))

;; Creation of the instructions intro movie
(define (new-animation-level-B hi-score)
  (let* ((sim (create-simulation))
         (level (<level> :height:       screen-max-y
                         :width:        screen-max-x
                         :object-table: (make-table)
                         :hi-score:     hi-score
                         :sim:          sim
                         :mutex:        (new-mutex))))
    (add-global-score-messages! level)
    
    (schedule-event! sim 0 (create-animation-B-event level))
    (schedule-event! sim 0 (create-intro-manager-event level))
    (schedule-event! sim 0 (create-redraw-event level))
    level))

;; Creation of a new demo movie. The demo movie is a tweaked normal
;; game level where the user input manager is the same as for the
;; other intro movies, and a special AI event is scheduled to control
;; the player. The demo should stop as soon as the player dies.
(define (new-animation-level-demo hi-score)
  (let* ((shields (generate-shields))
         (sim (create-simulation))
         (level (<game-level>
                     :height:           screen-max-y
                     :width:            screen-max-x
                     :object-table:     (make-table)
                     :hi-score:         hi-score
                     :sim:              sim
                     :mutex:            (new-mutex)
                     :player-id:        'demo-ai
                     :score:            0
                     :lives:            3
                     :walls:            (generate-walls)
                     :wall-damage:      '()
                     :draw-game-field?: #t)))
    
    (add-global-score-messages! level)
    (level-add-object!
     level
     (<message> :id:    'demo-msg
                :pos:   (<2dcoord> :x: 100 :y: (- screen-max-y 60))
                :color: 'white
                :speed: (<2dcoord> :x: 0 :y: 0)
                :text:  "DEMO"))
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
    (schedule-event! sim 0 (create-redraw-event level))
    level))

;; Creation of the instructions intro movie
(define (new-credits-level hi-score)
  (let* ((sim (create-simulation))
         (level (<level> :height:       screen-max-y
                         :width:        screen-max-x
                         :object-table: (make-table)
                         :hi-score:     hi-score
                         :sim:          sim
                         :mutex:        (new-mutex))))
    (add-global-score-messages! level)
    
    (schedule-event! sim 0 (create-animation-credit-event level))
    (schedule-event! sim 0 (create-intro-manager-event level))
    (schedule-event! sim 0 (create-redraw-event level))
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
;; depending on the collision type. The objectif which gets collided
;; is returned. If no collision is detected, #f is returned.
(define (move-object! level obj)
  (define (move-object-raw! obj)
    (let* ((pos (:pos obj) )
           (px (:x pos))
           (py (:y pos))
           (speed (:speed obj))
           (dx (:x speed))
           (dy (:y speed)))
      (cycle-state! obj)
      (:color-set! obj (choose-color pos))
      (:x-set! pos (+ px dx))
      (:y-set! pos (+ py dy))))
  
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

;; Returns #f is the object is not colliding, else returns an object
;; which is in collision with obj. Only one object is return, even if
;; multiple collision are occurring.
(define (detect-collision? obj level)
  ;; exists is exptected to return the object that satisfy the condition
  (or (exists (lambda (collision-obj)
                (obj-collision? obj collision-obj))
              (level-all-objects level))
      (exists (lambda (wall) (obj-collision? obj wall))
              (:walls level))))


;; collision detection between 2 game objects
(define-method (obj-collision? (obj1 <game-object>) (obj2 <game-object>))
  (let* ((obj1-pos (:pos obj1))
         (obj2-pos (:pos obj2)))
    (and (not (eq? obj1 obj2))
         (rectangle-collision? (get-absolute-bounding-box obj1)
                               (get-absolute-bounding-box obj2)))))

(define-method (obj-collision? (obj <game-object>) (wall <wall>))
  (rectangle-collision? (get-absolute-bounding-box obj)
                        (:rect wall)))

(define-method (obj-collision? (wall <wall>) (obj <game-object>))
  (obj-collision? obj wall))

(define-method (obj-collision? (obj <game-object>) (shield <shield>))
  ;; (if (obj-obj-collision? obj shield)
  (if (next-method obj shield)
      (let* ((pos (:pos shield)))
        (exists (lambda (particle)
                  (point-rect-collision? (add pos particle)
                                         (get-absolute-bounding-box obj)))
                (:particles shield)))
      #f))
(define-method (obj-collision? (shield <shield>) (obj <game-object>))
  (obj-collision? obj shield))

;; Simple rectangular collision detection. Not optimized.
(define (rectangle-collision? r1 r2)
  (let* ((r1-x1 (:x r1))
         (r1-x2 (+ r1-x1 (:width r1)))
         (r1-x-min (min r1-x1 r1-x2))
         (r1-x-max (max r1-x1 r1-x2))
         (r1-y1 (:y r1))
         (r1-y2 (+ r1-y1 (:height r1)))
         (r1-y-min (min r1-y1 r1-y2))
         (r1-y-max (max r1-y1 r1-y2))

         (r2-x1 (:x r2))
         (r2-x2 (+ r2-x1 (:width r2)))
         (r2-x-min (min r2-x1 r2-x2))
         (r2-x-max (max r2-x1 r2-x2))
         (r2-y1 (:y r2))
         (r2-y2 (+ r2-y1 (:height r2)))
         (r2-y-min (min r2-y1 r2-y2))
         (r2-y-max (max r2-y1 r2-y2)))
    (not (or (< r1-x-max r2-x-min)
             (> r1-x-min r2-x-max)
             (< r1-y-max r2-y-min)
             (> r1-y-min r2-y-max)))))

(define (point-rect-collision? point rect)
  (let* ((rect-x1 (:x rect))
         (rect-x2 (+ rect-x1 (:width rect)))
         (rect-x-min (min rect-x1 rect-x2))
         (rect-x-max (max rect-x1 rect-x2))
         (rect-y1 (:y rect))
         (rect-y2 (+ rect-y1 (:height rect)))
         (rect-y-min (min rect-y1 rect-y2))
         (rect-y-max (max rect-y1 rect-y2))
         (point-x (:x point))
         (point-y (:y point)))
    (and (>= point-x rect-x-min)
         (<= point-x rect-x-max)
         (>= point-y rect-y-min)
         (<= point-y rect-y-max))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision resolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Abstraction used to simplify the resolve-laser-collision!
;; function. This will remove the laser object from the level and
;; depending on the laser type, might re-scheduled a new laser shot.
(define-method (destroy-laser! level (player-laser <player-laser>))
  (level-remove-object! level player-laser))

(define-method (destroy-laser! level (laser <laser>))
  (level-remove-object! level laser)
  (in next-invader-laser-interval (create-invader-laser-event level))
      (set! player-laser-last-destruction-time
            (time->seconds (current-time))))


;; (resolve-collision! A B) is of the form where A is the actor, thus
;; A collides into B.

#; (define-generic (resolve-collision! level colider colided))

;; Laser collisions
(define-method (resolve-collision! level (laser <laser>) (inv <invader>))
  (level-increase-score! level inv)
  (explode-invader! level inv)
  (destroy-laser! level laser))

;; this particular specialization gives more power to player lasers.
(define-method (resolve-collision! level
                                   (laser1 <laser>)
                                   (laser2 <player-laser>))
  (explode-laser! level laser1)
  (destroy-laser! level laser1))

(define-method (resolve-collision! level (laser1 <laser>) (laser2 <laser>))
  (explode-laser! level laser2)
  (destroy-laser! level laser2))

(define-method (resolve-collision! level (laser <laser>) (player <player>))
  (explode-player! level player)
  (destroy-laser! level laser))

(define-method (resolve-collision! level (laser <laser>) (shield <shield>))
  (let ((penetrated-pos (get-laser-penetration-pos laser)))
    (:pos-set! laser penetrated-pos)
    (explode-laser! level laser)
    (shield-explosion! shield laser))
  (destroy-laser! level laser))

(define-method (resolve-collision! level
                                   (laser <laser>)
                                   (mothership <mothership>))
  (destroy-laser! level laser)
  (explode-mothership! level mothership)
  (level-increase-score! level mothership)
  (let ((delta-t (mothership-random-delay)))
    (in delta-t (create-new-mothership-event level))))

(define-method (resolve-collision! level (laser <laser>) (shield <wall>))
  (damage-wall! level laser)
  (explode-laser! level laser)
  (destroy-laser! level laser))

;; Invader collision
(define-method (resolve-collision! level (inv <invader>) (laser <laser>))
  (resolve-collision! level laser inv))

(define-method (resolve-collision! level (inv <invader>) (shield <shield>))
  (shield-explosion! shield inv))

(define-method (resolve-collision! level (inv <invader>) (wall <wall>))
  (if (eq? (:id wall) 'bottom)
      (game-over! level)))

(define-method (resolve-collision! level (inv <invader>) (player <player>))
  (explode-player! level player)
  (explode-invader! level inv))

;; Player collisions
(define-method (resolve-collision! level (player <player>) (wall <wall>))
  (let ((current-speed (:speed player)))
    (:speed-set! player  (<2dcoord> :x: (- (:x current-speed))
                                   :y: (:y current-speed)))
    (move-object! level player)))

(define-method (resolve-collision! level (player <player>) (laser <laser>))
  (resolve-collision! level laser player))

(define-method (resolve-collision! level (player <player>) (inv <invader>))
  (resolve-collision! level inv player))


;; Mothership collisions
(define-method (resolve-collision! level
                                   (mothership <mothership>)
                                   (wall <wall>))
  (level-remove-object! level mothership)
  (let ((delta-t (mothership-random-delay)))
    (in delta-t (create-new-mothership-event level))))

(define-method (resolve-collision! level
                                   (mothership <mothership>)
                                   (laser <laser>))
  (resolve-collision! level laser mothership))

(define-method (resolve-collision! level any-obj (expl <explosion>))
  'do-nothing)

(define-method (resolve-collision! level (expl <explosion>) any-obj)
  'do-nothing)

(define-method (resolve-collision! level any-obj (expl <message>))
  'do-nothing)

(define-method (resolve-collision! level (expl <message>) any-obj)
  'do-nothing)

;; Default collision handler
(define-method (resolve-collision! level unknown-type-obj1 unknown-type-obj2)
  (error
   (with-output-to-string
     ""
     (lambda () (show "unknown collision resolution between "
                      (class-name (class-of unknown-type-obj1))
                      " and "
                      (class-name (class-of unknown-type-obj2)))))))

  


;;*****************************************************************************
;;
;;                 Gameplay related simulation events 
;;
;;*****************************************************************************

;; Macro used to synchronize certain game events, such that the game
;; may be paused.
(define-macro (synchronized-event-thunk level action . actions)
  `(lambda ()
     (critical-section! (:mutex ,level)
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
  (define player-id (:player-id level))
  (lambda ()
    (let* ((text (if (eq? player-id 'p2)
                     "PLAY  PLAYER<2>"
                     "PLAY  PLAYER<1>"))
           (msg (<message> :id: 'start-msg
                           :pos: (<2dcoord> :x: 61 :y: (- screen-max-y 136))
                           :color: 'white
                           :speed: (<2dcoord> :x: 0 :y: 0)
                           :text: text))
           (new-cont
            (lambda ()
              (level-remove-object! level msg)
              (:draw-game-field?-set! level #t)
              (in 0 continuation))))
      (:draw-game-field?-set! level #f)
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
    (cond ((< col 2) <easy>)
          ((< col 4) <medium>)
          (else <hard>)))

  (define (generate-inv! row col)
    (let* ((constructor (determine-type-id col))
           (invader
            (constructor :id: (gensym 'inv)
                         :pos: (<2dcoord>
                                :x: (+ x-offset (* col invader-spacing))
                                :y: (+ y-offset (* row invader-spacing)))
                         :state: 1
                         :color: 'white
                         :speed:  (<2dcoord> :x: invader-x-movement-speed :y: 0)
                         :row: row
                         :col: col)))
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
           (walls (:walls level))
           (wall-collision?
            (exists
             (lambda (row)
               (exists
                (lambda (inv)
                  (exists (lambda (wall) (obj-collision? inv wall))
                          walls))
                row))
             rows)))
      (if (null? rows)
          ;; Regenerate invaders when they all died
          (in 0 (generate-invaders-event
                 level (create-init-invader-move-event level)))
          (let* ((old-dx (:x (:speed (caar rows))))
                 (dt (get-invader-move-refresh-rate level)))
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
                   (lambda (inv) (let ((speed (<2dcoord> :x: dx :y: dy)))
                                   (:speed-set! inv speed)))
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
            (<mothership> :id: 'mothership
                          :pos: (<2dcoord> :x: (if (> dx-mult 0)
                                                   gamefield-min-x
                                                   (- gamefield-max-x 16))
                                           :y: 201)
                          :state: 0
                          :color: 'red
                          :speed: (<2dcoord> :x: (* dx-mult
                                                    mothership-movement-speed)
                                             :y: 0))))
      (play-sound 'mothership-sfx)
      (level-add-object! level mothership)
      (in 0 (create-mothership-event level)))))

;; Event that moves a mothership and handles its collisions.
 (define (create-mothership-event level)
  (define mothership-event
    (synchronized-event-thunk level
      (let ((mothership (level-mothership level)))
        (if mothership
            (let ((collision-occured? (move-object! level mothership)))
              ;; FIXME? not very clean. Here we have a kind of
              ;; collision resolution which does not lie with the
              ;; resolve-collision! method specializtions...
              (if (or (not collision-occured?)
                      (is-a? collision-occured? <explosion>)
                      (is-a? collision-occured? <message>))
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
      (let* ((inv-rect (<rect> :x:      (:x (:pos inv))
                               :y:      (:y (:pos inv))
                               :width:  (type-width inv)
                               :height: (type-height inv))))
        (rectangle-collision? rect inv-rect))))
                                  
  (define (bottom-invader? inv)
    (let* ((rect (<rect> :x: (:x (:pos inv))
                         :y: (- (:y (:pos inv)) 1)
                         :width: (type-width inv)
                         :height: (- (:y (:pos inv))))))
      (not (exists (rect-inv-collision? rect) (level-invaders level)))))
                       
  (define (get-candidates)
    (filter bottom-invader? (level-invaders level)))

  (synchronized-event-thunk
   level
   (if (not (exists (lambda (obj)
                      (and (is-a? obj <laser>)
                           (not (is-a? obj <player-laser>))))
                    (level-all-objects level)))
       (let* ((candidates (get-candidates))
              (canditate-nb (length candidates))
              (shooting-invader
               (if (> canditate-nb 0)
                   (list-ref candidates (random-integer (length candidates)))
                   #f)))
         (if shooting-invader
             ;; FIXME: usage of <laser> subclass introspection would
             ;; be much nicer
             (shoot-laser! level
                           ((list-ref (list <laserA> <laserB> <laserC>)
                                      (random-integer 3)))
                           shooting-invader
                           (- invader-laser-speed)))))))

;; Wrapper function over create-laser-event which will create a new
;; laser object instance of specifiex type and place it correctly next
;; to the shooting object.

(define-method (shoot-laser! level (laser <player-laser>) shooter-obj dy)
  ;; if the shot laser is a player laser, there must not be another
  ;; player laser in the game or the player-laser-refresh-constraint
  ;; must be elabsed before shooting a new one.
  (if (not (or (level-player-laser level)
               (< (- (time->seconds (current-time))
                     player-laser-last-destruction-time)
                  player-laser-refresh-constraint)))
      (next-method)))
(define-method (shoot-laser! level (laser <laser>) shooter-obj dy)
  (let* ((shooter-x (:x (:pos shooter-obj)))
         (shooter-y (:y (:pos shooter-obj)))
         (x (+ shooter-x (floor (/ (type-width shooter-obj) 2))))
         (y (if (< dy 0)
                (- shooter-y
                   (type-height laser)
                   invader-y-movement-speed)
                (+ shooter-y
                   (type-height shooter-obj))))
         (pos (<2dcoord> :x: x :y: y))
         (laser-id (if (eq? laser-type 'player_laser)
                       'player-laser
                       (gensym 'inv-laser))))
    (:id-set! laser laser-id)
    (:pos-set! laser pos)
    (:state-set! laser 0)
    (:color-set! laser (choose-color pos))
    (:speed-set! laser (<2dcoord> :x: 0 :y: dy))
    
    (level-add-object! level laser)
    (in 0 (create-laser-event laser level))))

;; Will generate the events associated with a laser object such that
;; it will be moved regularly dy pixels on the y axis. The game logic
;; of a laser is thus defined by the returned event.
(define (create-laser-event laser-obj level)
  (define laser-event
    (synchronized-event-thunk level
      ;; centered laser position (depending on the laser type...
      (let ((pos (add (:pos laser-obj)
                      (<2dcoord> :x: (floor (/ (type-width laser-obj) 2))
                                 :y: 0)))
            (collision-occured? (move-object! level laser-obj)))
        (if (or (not collision-occured?)
                (level-exists level (:id laser-obj)))
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
  (let* ((id (gensym 'inv-expl))
         (expl-obj
          (<invader-explosion> :id: id
                               :pos: (:pos inv)
                               :state: 0
                               :color: (choose-color (:pos inv))
                               :speed: (:speed inv))))
    (level-add-object! level expl-obj)
    (level-remove-object! level inv)
    (pp `(expl-obj ,expl-obj ,(level-get level (:id inv))))
    (in animation-duration
      (create-explosion-end-event!
       level expl-obj end-of-continuation-event))))


;; dispalys the explosion of the mothership was and removes it from
;; the level. The points for killing that mothership will then be
;; randomly calculated and displayed after the explosion.
(define (explode-mothership! level mothership)
  (define animation-duration 0.3)
  (define score-val (list-ref '(50 100 150) (random-integer 3)))
  (define pos (:pos mothership))
  (define expl-obj
    (<mothership-explosion> :id: (gensym 'explosion)
                            :pos: pos
                            :state: 0
                            :color: (choose-color pos)
                            :speed: (<2dcoord> :x: 0 :y: 0)))
  ;; Update the global mothership's score value to the current value...
  (:score-value-set! mothership score-val)
  (level-remove-object! level mothership)
  (level-add-object! level expl-obj)
  (in animation-duration
      (create-explosion-end-event!
       level expl-obj
       (show-points-event level pos score-val end-of-continuation-event))))

;; Will display the points value of a mothership kill at pos
(define (show-points-event level pos points continuation)
  (define duration 1)
  (define msg (<message> :id: (gensym 'mothership-points)
                         :pos: pos
                         :color: (choose-color pos)
                         :speed: (<2dcoord> :x: 0 :y: 0)
                         :text: (number->string points)))
  (synchronized-event-thunk
   level
   (level-add-object! level msg)
   (in duration
       (synchronized-event-thunk
        level
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
(define-method (explode-player-continuation (level <game-level>))
  (if (<= (:lives level) 0)
      (game-over-animation-event level (lambda () (game-over! level)))
      (lambda ()
        (begin
          (yield-corout)
          (sem-unlock! (:mutex level))
          (new-player! level)))))

(define-method (explode-player-continuation (level <2p-game-level>))
  (if (<= (:lives level) 0)
      (if (:other-finished? level)
          (begin
            (game-over-2p-animation-event
             level
             (game-over-animation-event
              level (lambda () (game-over! level)))))
          (begin
            (send-update-msg-to-other level #t)
            (game-over-2p-animation-event
             level (lambda () (game-over! level)))))
      (lambda ()
        (send-update-msg-to-other level #f)
        (in NOW! (start-of-game-animation-event
                  level (return-to-player-event level)))
        (yield-corout))))

(define (explode-player! level player)
  (define animation-duration 1.5)
  (define expl-obj
    (<player-explosion> :id: (gensym 'explosion)
                        :pos: (:pos player)
                        :state: 0
                        :color: (choose-color (:pos player))
                        :speed: (<2dcoord> :x: 0 :y: 0)))
  (level-loose-1-life! level)
  (level-add-object! level expl-obj)
  (level-remove-object! level player)
  (let ((continuation (explode-player-continuation level)))
    (in 0 (lambda ()
            (sem-lock! (:mutex level))
            (in 0 (player-explosion-animation-event
                   level expl-obj animation-duration continuation))))))

;; Event used in 2 player games, where the games returns to the
;; current game.
(define (return-to-player-event level)
  (lambda ()
    (sem-unlock! (:mutex level))
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
(define-method (explode-laser! level (laser <player-laser>))
  (define animation-duration 0.3)
  (define (center-pos pos)
    ;; FIXME: ugly hack where only player laser's explotion are
    ;; centered in x. I'm unsure why other lasers don't require this
    ;; shift so far...
    (sub pos (<2dcoord> :x: (floor (/ (type-width laser) 2))
                        :y: 0)))
  (define obj
    (<player-laser-explosion> :id: (gensym 'explosion)
                              :pos: (center-pos (:pos laser))
                              :state: 0
                              :color: (choose-color (:pos laser))
                              :speed: (<2dcoord> :x: 0 :y: 0)))
  (level-add-object! level obj)
  (in animation-duration
      (create-explosion-end-event! level obj end-of-continuation-event)))

(define-method (explode-laser! level (laser <laser>))
  (define animation-duration 0.3)
    (define obj
      (<player-laser-explosion> :id: (gensym 'explosion)
                                :pos: (:pos laser)
                                :state: 0
                                :color: (choose-color (:pos laser))
                                :speed: (<2dcoord> :x: 0 :y: 0)))
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
                (let ((current-text (:text msg-obj)))
                  (:text-set!
                   msg-obj
                   (string-append current-text (substring str 0 1)))
                  (in animation-delay
                      (anim-event (substring str 1
                                              (string-length str))))))))))
    (anim-event msg)))

;; Flashing animation where the msg-obj will flash for a certain duration.
(define (create-text-flash-animation-event level msg-obj duration continuation)
  (define animation-delay 0.2)
  (define original-color (:color msg-obj))
  (define (cycle-msg-state! msg-obj)
    (let ((current-color (:color msg-obj)))
      (:color-set! msg-obj
                  (if (eq? current-color 'black) original-color 'black))))
  (define (flash-ev dt)
    (lambda ()
      (if (< dt duration)
          (begin (cycle-msg-state! msg-obj)
                 (in animation-delay (flash-ev (+ dt animation-delay))))
          (begin
            (:color-set! msg-obj original-color)
            (in 0 continuation)))))
  (flash-ev 0))

;; Special animation that occur in the intro movie A (main intro)
(define (create-animation-A-event level)
  (define speed (<2dcoord> :x: 0 :y: 0))

  (lambda ()
    ;; messages declaration
    (let* ((play (let ((pos (<2dcoord> :x: 101 :y: (- screen-max-y 88))))
                   (<message> :id:    'play
                              :pos:   pos
                              :color: (choose-color pos)
                              :speed: speed
                              :text:  "")))
           (space (let ((pos (<2dcoord> :x: 61 :y: (- screen-max-y 112))))
                    (<message> :id:    'space
                               :pos:   pos
                               :color: (choose-color pos)
                               :speed: speed
                               :text:  "")))
           (score (let ((pos (<2dcoord> :x: 37 :y: (- screen-max-y 144))))
                    (<message> :id:    'score
                               :pos:   pos
                               :color: (choose-color pos)
                               :speed: speed
                               :text:  "")))
           (mother (let ((pos (<2dcoord> :x: 85 :y: (- screen-max-y 160))))
                     (<message> :id:    'mother
                                :pos:   pos
                                :color: (choose-color pos)
                                :speed: speed
                                :text:  "")))
           (hard (let ((pos (<2dcoord> :x: 85 :y: (- screen-max-y 176))))
                   (<message> :id:    'hard
                              :pos:   pos
                              :color: (choose-color pos)
                              :speed: speed
                              :text:  "")))
           (medium (let ((pos (<2dcoord> :x: 85 :y: (- screen-max-y 192))))
                     (<message> :id:    'medium
                                :pos:   pos
                                :color: (choose-color pos)
                                :speed: speed
                                :text:  "")))
           (easy (let ((pos (<2dcoord> :x: 85 :y: (- screen-max-y 208))))
                   (<message> :id:    'easy
                              :pos:   pos
                              :color: (choose-color pos)
                              :speed: speed
                              :text:  "")))
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
  (define speed (<2dcoord> :x: 0 :y: 0))
  (define (pos x y) (<2dcoord> :x: x :y: (- screen-max-y y)))
  (lambda ()
    ;; create the ship prototypes
    (let ((mothership (<mothership> :id: 'mothership
                                    :pos: (pos 68 160)
                                    :state: 0
                                    :color: (choose-color (pos 68 160))
                                    :speed: speed))
          (hard-ship (<hard> :id: 'hard-ship
                             :pos: (pos 72 176)
                             :state: 0
                             :color: (choose-color (pos 72 176))
                             :speed: speed
                             :row: 0 :col: 0))
          (medium-ship (<medium> :id: 'medium-ship
                                 :pos: (pos 71 192)
                                 :state: 0
                                 :color: (choose-color (pos 71 192))
                                 :speed: speed
                                 :row: 0 :col: 0))
          (easy-ship (<easy> :id: 'easy-ship
                             :pos: (pos 70 208)
                             :state: 0
                             :color: (choose-color (pos 70 208))
                             :speed: speed
                             :row: 0 :col: 0))
          (score-msg-obj (level-get level 'score)))
      ;; add all the new ships into the intro level
      (for-each (lambda (ship) (level-add-object! level ship))
                (list mothership hard-ship medium-ship easy-ship))
      ;; Display in one shot the score advance msg
      (:text-set! score-msg-obj "*SCORE ADVANCE TABLE*"))

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
  (define speed (<2dcoord> :x: 0 :y: 0))

  (lambda ()
    (let* ((instruction
            (let ((pos (<2dcoord> :x: 70 :y: (- screen-max-y 100))))
              (<message> :id:'instruction
                         :pos: pos
                         :color: (choose-color pos)
                         :speed: speed
                         :text: "")))
           (press1 (let ((pos (<2dcoord> :x: 20 :y: (- screen-max-y 130))))
                    (<message> :id: 'press1
                               :pos: pos
                               :color: (choose-color pos)
                               :speed: speed
                               :text: "")))
           (press2 (let ((pos (<2dcoord> :x: 20 :y: (- screen-max-y 154))))
                     (<message> :id: 'press2
                                :pos: pos
                                :color: (choose-color pos)
                                :speed: speed
                                :text: "")))
           (score (let ((pos (<2dcoord> :x: 37 :y: (- screen-max-y 144))))
                    (<message> :id: 'score
                               :pos: pos
                               :color: (choose-color pos)
                               :speed: speed
                               :text: "")))
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

(define (create-animation-credit-event level)
  (define speed (<2dcoord> :x: 0 :y: 0))

  (lambda ()
    (let* ((producer
            (let ((pos (<2dcoord> :x: 65 :y: (- screen-max-y 80))))
              (<message> :id: 'producer
                         :pos: pos
                         :color: 'white
                         :speed: speed
                         :text: "")))
           (dsth (let ((pos (<2dcoord> :x: 59 :y: (- screen-max-y 107))))
                    (<message> :id: 'dsth
                               :pos: pos
                               :color: 'red
                               :speed: speed
                               :text: "")))
           (support (let ((pos (<2dcoord> :x: 65 :y: (- screen-max-y 150))))
                      (<message> :id: 'support
                                 :pos: pos
                                 :color:'white
                                 :speed: speed
                                 :text: "")))
           (support-chars
            (let ((pos (<2dcoord> :x: 20 :y: (- screen-max-y 180))))
              (<message> :id: 'support-chars
                         :pos: pos
                         :color: 'blue
                         :speed: speed
                         :text: "")))
           (you (let ((pos (<2dcoord> :x: 100 :y: (- screen-max-y 220))))
                  (<message> :id: 'you
                             :pos: pos
                             :color: 'yellow
                             :speed: speed
                             :text: "")))
           (anim-messages
            (list producer dsth support support-chars you)))
      (for-each (lambda (m) (level-add-object! level m)) anim-messages)
      (play-sfx 'star-wars-op)
      (in 0 (animate-message
             producer "Game Producer:"
             (animate-message
              dsth "David St-Hilaire"
              (animate-message
               support "Support Team:"
               (animate-message
                support-chars "Julie, the LTP and ... "
                (animate-message
                 you "You!"
                 (lambda ()
                   (in 40 ;;about the end of the song?
                       (lambda ()
                         (stop-sfx 'star-wars-op)
                         (exit-simulation 'intro-A)))))))))))))


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
          (new-speed (<2dcoord> :x: dx :y: 0)))
      (:speed-set! player new-speed)
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
            (shoot-laser! level (<player-laser>)
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
    (let* ((msg-obj (<message> :id: 'game-over-msg 
                               :pos: (<2dcoord> :x: 77 :y: (- screen-max-y 60))
                               :color: 'red
                               :speed: (<2dcoord> :x: 0 :y: 0)
                               :text: "")))
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
    (let* ((text (if (eq? (:player-id level) 'p2)
                     "GAME OVER PLAYER<2>"
                     "GAME OVER PLAYER<1>"))
           (msg-obj (<message> :id: 'game-over-msg 
                               :pos: (<2dcoord> :x: 37 :y: (- screen-max-y 248))
                               :color: 'green
                               :speed: (<2dcoord> :x: 0 :y: 0)
                               :text: "")))
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
              ((space)
               (if (player-can-move?)
                   (shoot-laser! level (<player-laser>)
                                 (level-player level)
                                 player-laser-speed)))
              ((right)
               (if (player-can-move?)
                   (let ((new-speed (<2dcoord> :x: player-movement-speed
                                               :y: 0)))
                     (:speed-set! player new-speed)
                     (move-object! level player))))
              
              ((left)
               (if (player-can-move?)
                   (let ((new-speed (<2dcoord> :x: (- player-movement-speed)
                                               :y: 0)))
                     (:speed-set! player new-speed)
                     (move-object! level player))))
              
              ((p)
               (if game-paused?
                   (sem-unlock! (:mutex level))
                   (sem-lock! (:mutex level)))
               (set! game-paused? (not game-paused?)))

              ((r)
               (stop-sfx 'all)
               (corout-kill-all!))

              ((d) (error "DEBUG"))))
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
              ((1) (exit-simulation 'start-1p-game))
              ((2) (exit-simulation 'start-2p-game))
              ((c) (exit-simulation 'show-credits))
              ((r)
               (stop-sfx 'all)
               (exit-simulation 'intro-A))
              ((d) (error "DEBUG"))))
        (in manager-time-interfal manager-event))))
  manager-event)

  ;; will update the 2 top score messages if required.
(define-method (update-score-msg! (level <game-level>))
  (let* ((is-player2? (eq? (:player-id level) 'p2))
         (score-obj
          (level-get level (if is-player2?
                               'player2-score-msg
                               'player1-score-msg)))
         (other-score-obj
          (level-get level (if is-player2?
                               'player1-score-msg
                               'player2-score-msg))))
    (receive-update-msg-from-other! level)
    (:text-set! score-obj (get-score-string (:score level)))
    ;; not worth to do a separate method, would have too much
    ;; duplicated code
    (if (is-a? level <2p-game-level>)
        (:text-set! other-score-obj
                   (get-score-string (:other-score level))))))

(define-method (update-score-msg! level) 'nothing)

;; Event that will send a message to the ui asking for a redraw.
(define (create-redraw-event level)
  (define (redraw-event)
    (update-score-msg! level)
    (thread-send user-interface-thread `(redraw ,level))
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

         ((eq? result 'show-credits)
          (inf-loop hi-score
                    (play-level (new-credits-level hi-score))))

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

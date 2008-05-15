(include "scm-lib.scm")
(include "ppm-reader.scm")
(include "event-simulation.scm")

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
;; (define invader-col-number 5)

(define invader-spacing 16)

(define invader-x-movement-speed 2)
(define invader-y-movement-speed 8)
(define player-movement-speed 2)
(define player-laser-speed 2)
(define invader-laser-speed 2)
(define mothership-movement-speed (make-pos2d 1 0))

(define player-laser-last-destruction-time 0)


;; Simulation delays
(define mothership-update-interval 0.02)
(define player-laser-update-interval 0.001)
(define invader-laser-update-interval 0.02)
(define next-invader-laser-interval 0.2)
(define manager-time-interfal 0.001)
(define redraw-interval 0.01)

(define player-laser-refresh-constraint 0.6)

(define (mothership-random-delay) (+ (random-integer 10) 5))

(define get-invader-move-refresh-rate
  ;; the sleep delay is a function such that when the level is full of
  ;; invaders (55 invaders) then the delay is 0.1 and when there is
  ;; no invader left, it is 0.01. Thus the equation system:
  ;; 55x + xy = 1/10 and 0x + xy = 1/100 was solved.
  (let* ((min-delta 1/100)
         (max-delta 2/10)
         (max-inv-nb 55)
         (slope (/ (- max-delta min-delta) max-inv-nb)))
    (lambda (level)
      (let ((x (length (level-invaders level))))
        (+  (* slope x) min-delta)))))



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
(define-type game-object id type pos state speed
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
     (laserA ,(make-object-type 'laserA (make-rect 1 0 1 6) 2 0))
     (laserB ,(make-object-type 'laserB (make-rect 1 0 1 6) 3 0))
     (laserP ,(make-object-type 'laserP (make-rect 0 0 1 5) 1 0))
     (shield ,(make-object-type 'shield (make-rect 0 0 22 16) 1 0))
     (explodeI ,(make-object-type 'explodeI (make-rect 0 0 13 8) 1 0))
     (explodeInvL ,(make-object-type 'explodeInvL (make-rect 0 0 6 8) 1 0))
     (explodeS ,(make-object-type 'explodeS (make-rect 0 0 8 8) 1 0))
     (explodeP ,(make-object-type 'explodeP (make-rect 0 0 16 8) 2 0))
     (message ,(make-object-type 'message (make-rect 0 0 0 0) 0 0))
   ))

(define (get-type type-name)
  (let ((type (assq type-name types)))
  (if type
      (cadr type)
      (error (string-append "no such type: " type-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Particles objects and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; centered explosion particle positions ;;;;
(define invader-laser-explosion-particles
  (rgb-pixels-to-boolean-point-list
   (parse-ppm-image-file "sprites/explodeInvL0.ppm") 'dont-center))

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

(define player-laser-explosion-particles
  (rgb-pixels-to-boolean-point-list
   (parse-ppm-image-file "sprites/explodeL0.ppm") 'center))

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
       (parse-ppm-image-file "sprites/shield0.ppm")))

  (list (make-shield 'shield1 shield-type (make-pos2d  36 40) 0
                     speed (generate-particles))
        (make-shield 'shield2 shield-type (make-pos2d  81 40) 0
                     speed (generate-particles))
        (make-shield 'shield3 shield-type (make-pos2d 126 40) 0
                     speed (generate-particles))
        (make-shield 'shield4 shield-type (make-pos2d 171 40) 0
                     speed (generate-particles))))

(define (get-explosion-particles colliding-obj)
  (let ((type-id (type-id (game-object-type colliding-obj))))
    (cond ((eq? type-id 'laserP) player-laser-explosion-particles)
          ((or (eq? type-id 'laserA)
               (eq? type-id 'laserB))
           invader-laser-explosion-particles)
          (else
           (invader-ship-particles colliding-obj)))))

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
  height width object-table walls wall-damage shields
  score hi-score lives sim mutex)

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

(define (level-loose-1-life! lvl)
  (level-lives-set! lvl (- (level-lives lvl) 1)))

(define (level-increase-score! level obj)
  (level-score-set! level
                    (+ (level-score level)
                       (object-type-score-value (game-object-type obj)))))

(define (level-damage-wall! level damage)
  (define current-damage (level-wall-damage level))
  (level-wall-damage-set! level (union current-damage damage)))

(define (game-over! level)
  (show "Game over with " (level-score level) " points.\n")
  (exit-simulation (level-score level)))


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

(define (add-global-score-messages! level)
  (define y 254)
  (define type (get-type 'message))
  (define state 'white)
  (define speed (make-pos2d 0 0))
  (define hi-score (level-hi-score level))
  (for-each
   (lambda (m) (level-add-object! level m))
   (list
    (make-message-obj 'top-banner type (make-pos2d 13 y) state speed
                      "SCORE<1>  HI-SCORE  SCORE<2>")
    (make-message-obj 'player1-score-msg type
                      (make-pos2d 30 (- y 17)) state speed
                      (get-score-string 0))
    (make-message-obj 'hi-score-msg type
                      (make-pos2d 93 (- y 17)) state speed
                      (get-score-string hi-score))
    (make-message-obj 'player2-score-msg type
                      (make-pos2d 173 (- y 17)) state speed ""))))

(define (new-level hi-score)
  (let* ((walls (generate-walls))
         (wall-damage '())
         (shields (generate-shields))
         (sim (create-simulation))
         (lives 3)
         (level (make-level screen-max-y screen-max-x (make-table)
                          walls wall-damage shields 0 hi-score
                          lives sim (new-mutex))))
    (add-global-score-messages! level)
    
    (schedule-event!
     sim 0
     (start-of-game-animation-event
      level "PLAY  PLAYER<1>"
      (generate-invaders-event
       level
       (lambda ()
         (new-player! level)
         (schedule-event! sim 0 (create-init-invader-move-event level))
         (schedule-event! sim 1 (create-invader-laser-event level))
         (schedule-event! sim (mothership-random-delay)
                          (create-new-mothership-event level))))))
    
    (schedule-event! sim 0 (create-main-manager-event level))
    (schedule-event! sim 0 (create-redraw-event user-interface-thread level))
    level))

(define (new-animation-level-A hi-score)
  (let* ((invaders '())
         (walls '())
         (wall-damage '())
         (shields '())
         (sim (create-simulation))
         (level (make-level screen-max-y screen-max-x (make-table)
                            walls wall-damage shields 0 hi-score
                            0 sim (new-mutex))))
    (add-global-score-messages! level)
    
    (schedule-event! sim 0 (create-animation-A-event level))
    (schedule-event! sim 0 (create-intro-manager-event level))
    (schedule-event! sim 0 (create-redraw-event user-interface-thread level))
    level))


;;*****************************************************************************
;;
;;                         Movement Gameplay
;;
;;*****************************************************************************

;; Returns #t if a collision occured (and was resolved) during the
;; movement.
(define (move-object! level obj)
  (define (move-object-raw! obj)
    (let* ((pos (game-object-pos obj) )
           (x (pos2d-x pos))
           (y (pos2d-y pos))
           (speed (game-object-speed obj))
           (dx (pos2d-x speed))
           (dy (pos2d-y speed)))
      (cycle-state! obj)
      (pos2d-x-set! pos (+ x dx))
      (pos2d-y-set! pos (+ y dy))))
  
  ;; 1st move the object, then detect/respond to a collision if
  ;; required.
  (move-object-raw! obj)
  (let ((collision-obj (detect-collision? obj level)))
    (if collision-obj
        (begin
          (resolve-collision! level obj collision-obj)
          #t)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collision resolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (resolve-collision! level obj collision-obj)
  (cond
   ((player-ship? obj) (resolve-player-collision! level obj collision-obj))
   ((laser-obj? obj) (resolve-laser-collision! level obj collision-obj))
   ((invader-ship? obj) (resolve-invader-collision! level obj collision-obj))
   ((mothership? obj) (resolve-mothership-collision! level obj collision-obj))
   (else
    (error "cannot resolve object collision."))))


(define (destroy-laser! level laser-obj)
  (level-remove-object! level laser-obj)
  (if (not (eq? (object-type-id (game-object-type laser-obj)) 'laserP))
      (in next-invader-laser-interval (create-invader-laser-event level))
      (set! player-laser-last-destruction-time
            (time->seconds (current-time)))))

(define (resolve-laser-collision! level laser-obj collision-obj)
  (define type-id (object-type-id (game-object-type laser-obj)))
  
  ;;(show "collision occured with " collision-obj "\n")
  (cond ((invader-ship? collision-obj) 
         (level-increase-score! level collision-obj)
         (explode-invader! level collision-obj)
         (destroy-laser! level laser-obj))
        
        ((laser-obj? collision-obj)
         (let ((inv-laser
                (if (not (eq? type-id 'laserP)) laser-obj collision-obj)))
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
         (resolve-mothership-collision! level collision-obj laser-obj)
         (destroy-laser! level laser-obj))

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
         (level-remove-object! level mothership))
        ((laser-obj? collision-obj)
         (level-increase-score! level mothership)
         (explode-invader! level mothership)))
  ;; Schedule next mothership
  (let ((delta-t (mothership-random-delay)))
    (in delta-t (create-new-mothership-event level))))


;;*****************************************************************************
;;
;;                 Gameplay related simulation events 
;;
;;*****************************************************************************

(define-macro (synchronized-event-thunk level action . actions)
  `(lambda ()
     (critical-section! (level-mutex ,level)
        ,action
        ,@actions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of game level animations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (start-of-game-animation-event level text continuation)
  (define animation-duration 3)
  (synchronized-event-thunk level
    (let* ((pos (make-pos2d 61 (- screen-max-y 136)))
           (type (get-type 'message))
           (state 'white)
           (speed (make-pos2d 0 0))
           (msg (make-message-obj 'start-msg type pos state speed text))
           (new-cont
            (lambda () (level-remove-object! level msg)
                    (in 0 continuation))))
      (level-add-object! level msg)
      (in 0 (create-text-flash-animation-event
             level
             (level-get level 'player1-score-msg)
             animation-duration new-cont)))))

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
             (gensym 'inv) current-type pos state speed row col)))
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

    
  ;(define (generate-inv-row index)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gameplay related simulation events 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Event that will move a single row of invaders
(define (create-init-invader-move-event level)
  (synchronized-event-thunk level
    (let* ((rows (get-all-invader-rows level))
           (walls (level-walls level))
           (wall-collision?
            (exists
             (lambda (row)
               (exists (lambda (inv) (obj-wall-collision? inv walls)) row))
             rows)))
      (if (null? rows)
          (game-over! level)
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

(define (create-invader-wall-movement-continuation-event old-dx level)
  (synchronized-event-thunk level
    (let ((rows (get-all-invader-rows level)))
      (if (null? rows)
          (game-over! level)
          (let* ((dt (get-invader-move-refresh-rate level)))
            (in 0
                (create-invader-row-move-event!
                 dt (- old-dx) 0 level
                 (create-init-invader-move-event level))))))))

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
    (let ((mothership
           (make-mothership 'mothership
                            (get-type 'mothership)
                            (make-pos2d wall-x-offset 201)
                            0
                            mothership-movement-speed)))
    (level-add-object! level mothership)
    (in 0 (create-mothership-event level)))))

;; Event that moves a mothership and handles its collisions.
 (define (create-mothership-event level)
  (define mothership-event
    (synchronized-event-thunk level
      (let ((mothership (level-mothership level)))
        (if mothership
            (let ((collision-occured? (move-object! level mothership)))
              (if (not collision-occured?)
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
                                      'laserP))))
                     (level-all-objects level)))
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
                            (- invader-laser-speed)))))))

;; Wrapper function over create-laser-event which will create a new
;; laser object instance of specifiex type and place it correctly next
;; to the shooting object.
(define (shoot-laser! level laser-type shooter-obj dy)
  ;; if the shot laser is a player laser, there must not be another
  ;; player laser in the game or the player-laser-refresh-constraint
  ;; must be elabsed before shooting a new one.
  (if (not (and (eq? laser-type 'laserP)
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
             (laser-id (if (eq? laser-type 'laserP)
                           'player-laser
                           (gensym 'inv-laser)))
             (laser-obj (make-laser-obj
                         laser-id
                         (get-type laser-type)
                         (make-pos2d x y)
                         0
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
  (level-loose-1-life! level)
  (level-add-object! level expl-obj)
  (level-remove-object! level player)
  
  (let ((cont (if (<= (level-lives level) 0)
                  (game-over-animation-event level)
                  (lambda () (yield-corout)
                          (sem-unlock! (level-mutex level))
                          (new-player! level)))))
    (in 0 (lambda ()
            (sem-lock! (level-mutex level))
            (in 0 (player-explosion-animation-event
                   level expl-obj animation-duration cont))))))
        

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
                      (center-pos (game-object-pos laser-obj))
                      0 (make-pos2d 0 0)))
  (level-add-object! level obj)
  (in animation-duration (create-explosion-end-event! level obj)))


;; Event that will stop an invader explosion animation
(define (create-explosion-end-event! level inv)
  (synchronized-event-thunk level
    (level-remove-object! level inv)))



;;*****************************************************************************
;;
;;                            Animation events 
;;
;;*****************************************************************************

(define-macro (animate-message msg-obj msg cont)
  (let ((animation-delay 0.1)
        (anim-event      (gensym 'anim-event))
        (str             (gensym 'str))
        (current-text    (gensym 'current-text)))
    `(letrec
         ((,anim-event
           (lambda (,str)
             (lambda ()
               (if (string=? ,str "")
                   (in ,animation-delay ,cont)
                   (let ((,current-text (message-obj-text ,msg-obj)))
                     (message-obj-text-set!
                      ,msg-obj
                      (string-append ,current-text (substring ,str 0 1)))
                     (in ,animation-delay
                         (,anim-event (substring ,str 1
                                                 (string-length ,str))))))))))
       (,anim-event ,msg))))

    
(define (create-text-flash-animation-event level msg-obj duration continuation)
  (define animation-delay 0.2)
  (define original-color (game-object-state msg-obj))
  (define (cycle-msg-state! msg-obj)
    (let ((current-state (game-object-state msg-obj)))
      (game-object-state-set!
       msg-obj
       (if (eq? current-state 'black) original-color 'black))))
  (define (flash-ev dt)
    (synchronized-event-thunk level
      (if (< dt duration)
          (begin (cycle-msg-state! msg-obj)
                 (in animation-delay (flash-ev (+ dt animation-delay))))
          (begin
            (game-object-state-set! msg-obj original-color)
            (in 0 continuation)))))
  (flash-ev 0))

(define (create-animation-A-event level)
  (define msg-type (get-type 'message))
  (define speed (make-pos2d 0 0))
  (define state 'white)

  (lambda ()
    (let* ((play (let ((pos (make-pos2d 101 (- screen-max-y 88))))
                   (make-message-obj 'play msg-type pos state speed "")))
           (space (let ((pos (make-pos2d 61 (- screen-max-y 112))))
                    (make-message-obj 'space msg-type pos state speed "")))
           (score (let ((pos (make-pos2d 37 (- screen-max-y 144))))
                    (make-message-obj 'score msg-type pos state speed "")))
           (mother (let ((pos (make-pos2d 85 (- screen-max-y 160))))
                     (make-message-obj 'mother msg-type pos state speed "")))
           (hard (let ((pos (make-pos2d 85 (- screen-max-y 176))))
                   (make-message-obj 'hard msg-type pos state speed "")))
           (medium (let ((pos (make-pos2d 85 (- screen-max-y 192))))
                     (make-message-obj 'medium msg-type pos state speed "")))
           (easy (let ((pos (make-pos2d 85 (- screen-max-y 208))))
               (make-message-obj 'easy msg-type pos state speed "")))
           (anim-messages
            (list play space score mother hard medium easy)))
             
      (for-each (lambda (m) (level-add-object! level m)) anim-messages )
      (in 0 (animate-message
             play "PLAY"
             (animate-message
              space "SPACE   INVADERS"
              (create-animate-score-adv-table-event level)))))))

(define (create-animate-score-adv-table-event level)
  (define speed (make-pos2d 0 0))
  (define (pos x y) (make-pos2d x (- screen-max-y y)))
  (lambda ()
    (let ((mothership (make-mothership 'mothership (get-type 'mothership)
                                   (pos 68 160) 0 speed))
          (hard-ship (make-invader-ship 'hard-ship (get-type 'hard)
                                   (pos 72 176) 0 speed 0 0))
          (medium-ship (make-invader-ship 'medium-ship (get-type 'medium)
                                     (pos 71 192) 0 speed 0 0))
          (easy-ship (make-invader-ship 'easy-ship (get-type 'easy)
                                   (pos 70 208) 0 speed 0 0))
          (score-msg-obj (level-get level 'score)))
      (for-each (lambda (ship) (level-add-object! level ship))
                (list mothership hard-ship medium-ship easy-ship))
      (message-obj-text-set! score-msg-obj "*SCORE ADVANCE TABLE*"))
    
    (in 0 (animate-message
           (level-get level 'mother) "=? MYSTERY"
           (animate-message
            (level-get level 'hard) "=30 POINTS"
            (animate-message
             (level-get level 'medium) "=20 POINTS"
             (animate-message
              (level-get level 'easy) "=10 POINTS"
              (lambda () 'animation-finished))))))))

(define (game-over-animation-event level)
  (lambda ()
    (let* ((type (get-type 'message))
           (pos (make-pos2d 77 (- screen-max-y 60)))
           (speed (make-pos2d 0 0))
           (msg-obj (make-message-obj 'game-over-msg type pos 'red speed "")))
      (level-add-object! level msg-obj)
      (in 0 (animate-message
             msg-obj "GAME OVER"
             (lambda () (in 2 (lambda () (game-over! level)))))))))

  



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
                   (shoot-laser! level 'laserP
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
              
              ((#\s #\S) (pp `(score is ,(level-score level))))

              ((#\t #\T)
               (call/cc
                (lambda (k)
                  (play-level (new-animation-level-A 1010))
                  (k 'go))))
              
              ((#\p #\P)
               (if game-paused?
                   (sem-unlock! (level-mutex level))
                   (sem-lock! (level-mutex level)))
               (set! game-paused? (not game-paused?)))

              ((#\r #\R) (exit-simulation 'intro-A))
               
              (else
               (show "received keyboard input: " msg ".\n"))))
        (in manager-time-interfal manager-event))))

  manager-event)

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
              (else
               (show "received keyboard input: " msg ".\n"))))
        (in manager-time-interfal manager-event))))
  manager-event)


;; Event that will send a message to the ui asking for a redraw.
(define (create-redraw-event ui-thread level)
  ;;TODO: Dummy duplication!!
  (define (duplicate obj) obj) 
  (define (update-score-msg! level)
    (let ((msg-obj (level-get level 'player1-score-msg)))
      (message-obj-text-set! msg-obj (get-score-string (level-score level)))))
  (define (redraw-event)
    (update-score-msg! level)
    (thread-send ui-thread (duplicate level))
    (in redraw-interval redraw-event))
  
  redraw-event)



;;*****************************************************************************
;;
;;                              Game loop
;;
;;*****************************************************************************

(define hi-score-filename ".spaceinvaders")

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

(define (save-hi-score hi-score)
  (with-output-to-file `(path: ,hi-score-filename create: maybe truncate: #t)
    (lambda ()
      (write hi-score))))

;; Setup of initial game events and start the simulation.
(define (game-loop ui-thread)
  (define init-high-score (retrieve-hi-score))
  (set! user-interface-thread ui-thread)
    (lambda ()
      (let inf-loop ((hi-score init-high-score)
                     (result (play-level
                              (new-animation-level-A init-high-score))))
        (cond
         ((and (number? result) (> result hi-score))
          (save-hi-score result)
          (inf-loop result
                    (play-level (new-animation-level-A result))))
         ((eq? result 'start-1p-game)
          (let ((p1 (new-corout '
                     'p1 (lambda () (play-level (new-level hi-score))))))
            (inf-loop hi-score (corout-boot p1))))

         ((eq? result 'start-2p-game)
          (let ((p1 (new-corout '
                     'p1 (lambda () (play-level (new-level hi-score)))))
                (p2 (new-corout '
                     'p2 (lambda () (play-level (new-level hi-score))))))
            (inf-loop hi-score (corout-boot p1 p2))))
         
         (else
          (inf-loop hi-score
                    (play-level (new-animation-level-A hi-score))))))))


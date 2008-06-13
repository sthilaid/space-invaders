;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: user-interface.scm
;;
;; description: Glut user interface for space invaders, based on the
;; engine.scm space invader model. This file contains the
;; application's "main" function. During the initialization, 1 other
;; thread will be started and will contain the engine's game-loop. The
;; other thread will be controlled by glut. The user input will be
;; forwarded as needed to the engine, and in return, the engine will
;; tell to the glut thread when to execute redraw on the screen. This
;; communication is made through the gambit thread mailbox system.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(include "scm-lib-macro.scm")
(include "opengl-header.scm")


;;;;;;;;;;;;;;;;;;;;;;; Global state variables  ;;;;;;;;;;;;;;;;;;;;;;;

(define game-loop-thunk #f)
(define simulation-thread #f)
(define display-fps? #f)
(define FPS (create-simple-moving-avg))




;;;;;;;;;;;;;;;;;;;;;;; Sound rendering functions ;;;;;;;;;;;;;;;;;;;;;;;

(define (play-sfx sfx)
  'nothing)

(define (stop-sfx sfx)
  'nothing)




;;;;;;;;;;;;;;;;;;;;;;; Render-Sceneing function ;;;;;;;;;;;;;;;;;;;;;;;

;; Deprecated
;; (define (render-sprite sprite-name x y state)
;;   (if (not (number? state)) (error "sprite state must be a number."))
;;   (let ((sprite-name
;;          (string-append (symbol->string sprite-name)
;;                         (number->string state))))
;;     (draw-sprite sprite-name x y)))

(define (render-string x y str color)
  (if (not (eq? color 'black))
      (let loop ((i 0) (chars (string->list str)))
        (if (pair? chars)
            (begin
              (draw-char "bb_fonts" color (car chars) x y i)
              (loop (+ i 1) (cdr chars)))))))

(define (render-fontified-sprite sprite-name x y state color)
  (draw-char (symbol->string sprite-name) color state x y 0))

(define (render-message msg-obj)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (let* ((pos (game-object-pos msg-obj))
         (x (pos2d-x pos))
         (y (pos2d-y pos))
         (color (game-object-color msg-obj))
         (str (message-obj-text msg-obj)))
    (render-string x y str color)))
    ;;(display-message x y (message-obj-text msg-obj) state)))

(define (render-shield shield)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (set-openGL-color 'green)
  (for-each (lambda (particle)
              (let* ((shield-x (pos2d-x (game-object-pos shield)))
                     (shield-y (pos2d-y (game-object-pos shield)))
                     (x (+ shield-x (pos2d-x particle)))
                     (y (+ shield-y (pos2d-y particle))))
                (glBegin GL_QUADS)
                (glVertex2i x y)
                (glVertex2i x (- y 1))
                (glVertex2i (+ x 1) (- y 1))
                (glVertex2i (+ x 1) y)
                (glEnd)))
            (shield-particles shield)))

(define (render-object obj)
  (define x (pos2d-x (game-object-pos obj)))
  (define y (pos2d-y (game-object-pos obj)))
  (define type (type-id (game-object-type obj)))
  (define state (game-object-state obj))
  (define color (game-object-color obj))
  (case type
    ((easy)   (render-fontified-sprite 'easy x y state color))
    ((medium) (render-fontified-sprite 'medium x y state color))
    ((hard)   (render-fontified-sprite 'hard x y state color))
    ((player) (render-fontified-sprite 'player x y state color))
    ((laserA) (render-fontified-sprite 'laserA x y state color))
    ((laserB) (render-fontified-sprite 'laserB x y state color))
    ((laserC) (render-fontified-sprite 'laserC x y state color))
    ((player_laser) (render-fontified-sprite 'player_laser x y state color))
    ((invader_explosion)
     (render-fontified-sprite 'invader_explosion x y state color))
    ((invader_laser_explosion)
     (render-fontified-sprite 'invader_laser_explosion x y state color))
    ((player_laser_explosion)
     (render-fontified-sprite 'player_laser_explosion x y state color))
    ((player_explosion)
     (render-fontified-sprite 'player_explosion x y state color))
    ((mothership) (render-fontified-sprite 'mothership x y state color))
    ((mothership_explosion) (render-fontified-sprite 'mothership_explosion
                                           x y state color))
    ((message) (render-message obj))
    ((shield) (render-shield obj))
    (else (error (string-append "Cannot render unknown object type:"
                                (symbol->string type))))))

;; Simple abstraction over open-gl to set up the desired color.
(define (set-openGL-color color)
  (case color
    ;; equivalent to rgb color: 1ffe1f
    ((green)
     (glColor3f .12156862745098039 .996078431372549 .12156862745098039))
    ((white)
     (glColor3f 1. 1. 1.))
    ((red)
     (glColor3f 1. 0. 0.))
    ((black)
     (glColor3f 0. 0. 0.))
    (else (error "unknown color"))))


;; Will render a full game level
(define (render-game-level level)
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
                    (glVertex2i (pos2d-x p) (+ (pos2d-y p) 1))
                    (glVertex2i (pos2d-x p) (+ (pos2d-y p) 2))
                    (glVertex2i (+ (pos2d-x p) 1) (+ (pos2d-y p) 2))
                    (glVertex2i (+ (pos2d-x p) 1) (+ (pos2d-y p) 1))
                    (glEnd))
                  (game-level-wall-damage level))

        (for-each render-shield (game-level-shields level)))
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
        (for-each (lambda (msg) (render-object msg))
                  (level-messages level))))

  ;; Draw lives
  (let ((nb-lives (game-level-lives level)))
    (render-string 13 0 (number->string nb-lives) 'white)
    (for i 0 (< i (- nb-lives 1))
         (render-fontified-sprite 'player (+ 30 (* i 15)) 0 0 'green))))
  

(define (render-level level)
  ;; Draw all objects
  (for-each render-object (level-all-objects level))
  
  (if (game-level? level)
      (render-game-level level)))

;; Main rendering function, also calculates the redraw frame-rate
(define render-scene
  (let ((last-render-time 0))
    (lambda (level)

      (glClearColor 0. 0. 0. 0.)
      (glClear GL_COLOR_BUFFER_BIT)

      (glBlendFunc GL_SRC_ALPHA GL_ONE)
      (glColor4f .1215 .9960 .1215 0.05)
      (let ((y 65))
        (glBegin GL_QUADS)
        (glVertex2i 0 0)
        (glVertex2i screen-max-x 0)
        (glVertex2i screen-max-x y)
        (glVertex2i 0 y)
        (glEnd))

      (glBlendFunc GL_ONE GL_ZERO)
      
      ;; Draw background stuff
      (render-level level)

      (let ((now (time->seconds (current-time))))
        (if (not (= last-render-time 0))
            (FPS (/ 1 (- now last-render-time))))
        (set! last-render-time now))

      ;;draw frame-rate just over the green line
      (if display-fps?
          (render-string
           0 11 
           (with-output-to-string "" (lambda () (show "FPS: " (FPS))))
           'white))

      (glFlush)
      (glutSwapBuffers))))



  
;;;;;;;;;;;;;;;;;;;;;;; Viewport and projection ;;;;;;;;;;;;;;;;;;;;;;;

(c-define (reshape w h) (int int) void "reshape" ""
  (let* ((zoom-x (/ w screen-max-x))
         (zoom-y (/ h screen-max-y))
         (factor (exact->inexact (ceiling (max zoom-x zoom-y)))))
;;     (glPointSize factor)
;;     (glPixelZoom factor factor)
    (glViewport 0 0 w h)
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (gluOrtho2D 0.                            ;;left clip
                (exact->inexact (/ w zoom-x)) ;;right clip
                0.                            ;;bottom clip
                (exact->inexact (/ h zoom-y)));;top
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)))

;;;;;;;;;;;;;;;;;;;;;;; User I/O ;;;;;;;;;;;;;;;;;;;;;;;

(define (register-user-action action)
  (thread-send simulation-thread action))

(c-define (keyboard key x y) (unsigned-char int int) void "keyboard" ""
 (case key
   ((#\f #\F) (set! display-fps? (not display-fps?)))
   ;; On Escape, Ctl-q, Ctl-c, Ctl-w, q -> terminate the program
   ((#\x1b #\x11 #\x03 #\x17 #\q) (quit))
   ((#\space) (register-user-action 'space))
   ((#\r #\R) (register-user-action 'r))
   ((#\p #\P) (register-user-action 'p))
   ((#\d #\D) (register-user-action 'd))
   ((#\c #\C) (register-user-action 'd))
   ((#\1) (register-user-action '1))
   ((#\2) (register-user-action '2))
   ))

(c-define (special-keyboard key x y)
          (unsigned-char int int) void "special_keyboard" ""
 (case key
;;    ((#\e) (pp 'up))
;;    ((#\g) (pp 'down))
   ((#\f) (register-user-action 'right))
   ((#\d) (register-user-action 'left))))
   
;;    (else (show "received special keyboard input: " key
;;                ". Mouse is @ ("x","y")\n"))))

;;;;;;;;;;;;;;;;;;;;;;; Idle function (animation) ;;;;;;;;;;;;;;;;;;;;;;;

(c-define (dummy-render) () void "dummy_render" ""
  'dummy-renderer)

(c-define (idle-callback) () void "idle_callback" ""
  ;; receive from the game engine thread the next redraw command
  (let ((msg (thread-receive)))
    (case (car msg)
      [(redraw) (render-scene (cadr msg))])
    ))

;;;;;;;;;;;;;;;;;;;;;;; Gui Initialization ;;;;;;;;;;;;;;;;;;;;;;;

(c-declare "int argc = 0;")
(define (glut-init)
  (let ((argc ((c-lambda () (pointer int) "___result_voidstar = &argc;"))))

    (set! simulation-thread
          (make-thread (game-loop (current-thread))))
    (thread-start! simulation-thread)
    
    (glutInit argc '())
    (glutInitDisplayMode (bitwise-ior GLUT_DOUBLE GLUT_RGBA))
    (glutInitWindowSize screen-max-x screen-max-y)
    (glutCreateWindow "Space Invaders")
    
    (glPointSize 1.)
    (glDisable GL_POINT_SMOOTH)

    (glPixelStorei GL_UNPACK_ALIGNMENT 1)
    (glShadeModel GL_FLAT)

    (glEnable GL_BLEND)
    (glBlendFunc GL_SRC_ALPHA GL_ONE)

    (initialize-textures!)

    ;(create-menu)
    
    (glutReshapeFunc reshape)
    (glutKeyboardFunc keyboard)
    (glutSpecialFunc special-keyboard)
    (glutIdleFunc idle-callback)
    (glutDisplayFunc dummy-render)))

(define usage-message "USAGE: ./space-invaders\n")

(define return #f)
(define (quit) (return 0))

;; Main function which only sets up and starts the game threads
(define (main)
  (define (start)
    (glut-init)
    (call/cc (lambda (k) (set! return k) (glutMainLoop))))

  ;; Start a debug/developpement repl in a seperate thread
  ;;   (thread-start! (make-thread (lambda () (##repl))))
  (cond
   ((eqv? (length (command-line)) 1) (start))
   (else
    (display usage-message))))

 (main)
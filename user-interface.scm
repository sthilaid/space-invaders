(include "scm-lib-macro.scm")
(include "texture-macro.scm")
(include "sprite-macro.scm")
(include "font-macro.scm")
(include "opengl-header.scm")
;;(include "ppm-reader.scm")


;;;;;;;;;;;;;;;;;;;;;;; Global state variables  ;;;;;;;;;;;;;;;;;;;;;;;

(define game-loop-thunk #f)
(define simulation-thread #f)

;;;;;;;;;;;;;;;;;;;;;;; State modification functions ;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;; Menu functionnalities ;;;;;;;;;;;;;;;;;;;;;;;


;; (c-define (menu value) (int) void "menu" ""
;;   (cond
;;    ((eqv? value gray-scale-mode)   (set-display-mode! grayscalify))
;;    ((eqv? value cloud-mode)        (set-display-mode! cloudify))
;;    ((eqv? value terrain-mode)      (set-display-mode! terrainify))
;;    ((eqv? value interpolation-mode)(switch-interpolation-fun!))
;;    ((eqv? value animation-mode)    (switch-animation-mode!))
;;    ((eqv? value octaves-mode)      (switch-octave!))))

;; (define (create-menu)
;;   (glutCreateMenu menu)
;;   (glutAddMenuEntry "Gray Scale Mode" gray-scale-mode)
;;   (glutAddMenuEntry "Cloud Mode" cloud-mode)
;;   (glutAddMenuEntry "Terrain Mode" terrain-mode)
;;   (glutAddMenuEntry "Toggle Animation" animation-mode)
;;   (glutAddMenuEntry "Toggle Interpolation" interpolation-mode)
;;   (glutAddMenuEntry "Toggle Octaves Number" octaves-mode)
;;   (glutAttachMenu GLUT_RIGHT_BUTTON))


;;;;;;;;;;;;;;;;;;;;;;; Render-Sceneing function ;;;;;;;;;;;;;;;;;;;;;;;

(define-sprite "sprites/laserA0.ppm")
(define-sprite "sprites/laserA1.ppm")
(define-sprite "sprites/laserB0.ppm")
(define-sprite "sprites/laserB1.ppm")
(define-sprite "sprites/laserB2.ppm")
(define-sprite "sprites/laserB3.ppm")
(define-sprite "sprites/laserB4.ppm")
(define-sprite "sprites/laserP0.ppm")
(define-sprite "sprites/shield0.ppm")
(define-sprite "sprites/easy0.ppm")
(define-sprite "sprites/easy1.ppm")
(define-sprite "sprites/medium0.ppm")
(define-sprite "sprites/medium1.ppm")
(define-sprite "sprites/hard0.ppm")
(define-sprite "sprites/hard1.ppm")
(define-sprite "sprites/mothership0.ppm")
(define-sprite "sprites/player0.ppm")
(define-sprite "sprites/player1.ppm")
(define-sprite "sprites/explodeI0.ppm")
(define-sprite "sprites/explodeS0.ppm")
(define-sprite "sprites/explodeP0.ppm")
(define-sprite "sprites/explodeP1.ppm")
(define-sprite "sprites/explodeInvL0.ppm")

(define display-fps? #f)

(define-symmetric-font "bb_fonts" 8 8)
(define-symmetric-font "f_operationwolf" 8 8)
(define-symmetric-font "f_syvalion" 16 16)
(define current-font "bb_fonts")
(define cycle-font!
  (let* ((fonts (map car (table->list global-fonts-table)))
         (index 0)
         (fonts-nb (length fonts)))
    (lambda ()
      (set! index (modulo (+ index 1) fonts-nb))
      (set! current-font (list-ref fonts index))
      (pp `(current-font is now ,current-font)))))

(define (render-sprite sprite-name x y state)
  (if (not (number? state)) (error "sprite state must be a number."))
  (let ((sprite-name
         (string-append (symbol->string sprite-name)
                        (number->string state))))
    (draw-sprite sprite-name x y)))

(define (render-message msg-obj)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (let* ((pos (game-object-pos msg-obj))
         (x (pos2d-x pos))
         (y (pos2d-y pos))
         (color (game-object-state msg-obj))
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
                (glBegin GL_POINTS)
                (glVertex2i x y)
                (glEnd)))
            (shield-particles shield)))

(define (render-object obj)
  (define x (pos2d-x (game-object-pos obj)))
  (define y (pos2d-y (game-object-pos obj)))
  (define type (type-id (game-object-type obj)))
  (define state (game-object-state obj))
  (case type
    ((easy)   (render-sprite 'easy x y state))
    ((medium) (render-sprite 'medium x y state))
    ((hard)   (render-sprite 'hard x y state))
    ((player) (render-sprite 'player x y state))
    ((laserA) (render-sprite 'laserA x y state))
    ((laserB) (render-sprite 'laserB x y state))
    ((laserP) (render-sprite 'laserP x y state))
    ((explodeI) (render-sprite 'explodeI x y state))
    ((explodeInvL) (render-sprite 'explodeInvL x y state))
    ((explodeS) (render-sprite 'explodeS x y state))
    ((explodeP) (render-sprite 'explodeP x y state))
    ((mothership) (render-sprite 'mothership x y state))
    ((message) (render-message obj))
    ((shield) (render-shield obj))
    (else (error (string-append "Cannot render unknown object type:"
                                (symbol->string type))))))

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

(define (render-game-level level)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

  ;; Must verify if the game field should be drawn or not...
  (if (game-level-draw-game-field? level)
      (begin
        ;; Draw horizontal bottom green wall and remove damaged parts by
        ;; redrawing over it in black.
        (set-openGL-color 'green)
        (glBegin GL_LINES)
        (glVertex2i 0 9)
        (glVertex2i screen-max-x 9)
        (glEnd)
        ;;  (glBlendFunc GL_ONE GL_ZERO)
        (set-openGL-color 'black)
        (for-each (lambda (p)
                    (glBegin GL_POINTS)
                    (glVertex2i (pos2d-x p) (pos2d-y p))
                    (glEnd))
                  (game-level-wall-damage level))

        (for-each render-shield (game-level-shields level)))
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
         (render-sprite 'player (+ 30 (* i 15)) 0 0))))
  

(define (render-level level)
  ;; Draw all objects
  (for-each render-object (level-all-objects level))
  
  (if (game-level? level)
      (render-game-level level)))


;; (define (display-message x y msg color)
;;   (let ((chars (map char->integer (string->list msg)))
;;         (font GLUT_BITMAP_HELVETICA_12))
;;     (set-openGL-color color)
;;     (glRasterPos2i x y)
;;     (for-each (lambda (char) (glutBitmapCharacter font char))
;;               chars)))

(define FPS (create-simple-moving-avg))

(define (render-string x y str color)
  (if (not (eq? color 'black))
      (let loop ((i 0) (chars (string->list str)))
        (if (pair? chars)
            (begin
              (draw-char current-font color (car chars) x y i)
              (loop (+ i 1) (cdr chars)))))))

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

      ;;(render-string 20 20 "TESTING 1 - 2")
      
      (glFlush)
      (glutSwapBuffers))))
      



;;;;;;;;;;;;;;;;;;;;;;; Viewport and projection ;;;;;;;;;;;;;;;;;;;;;;;

(c-define (reshape w h) (int int) void "reshape" ""
;;   (glViewport 0 0 w h)
;;   (glMatrixMode GL_PROJECTION)
;;   (glLoadIdentity);
;;   (gluPerspective 60.0 (exact->inexact (/ w h)) 1.0 30.0)
;;   (glMatrixMode GL_MODELVIEW);
;;   (glLoadIdentity);
;;   (glTranslatef 0.0 0.0 -3.6))

;;    (glViewport 0 0 w h)
;;    (glMatrixMode GL_PROJECTION)
;;    (glLoadIdentity)
;;    (glOrtho 0. (exact->inexact w) 0. (exact->inexact h) -1.0 1.0)
;;    (glMatrixMode GL_MODELVIEW))

  (let* ((zoom-x (/ w screen-max-x))
         (zoom-y (/ h screen-max-y))
         (factor (exact->inexact (ceiling (max zoom-x zoom-y)))))
    (glPointSize factor)
    (glPixelZoom factor factor)
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
   ((#\n #\N) (cycle-font!))
   ;; On Escape, Ctl-q, Ctl-c, Ctl-w, q -> terminate the program
   ((#\x1b #\x11 #\x03 #\x17 #\q) (quit))
   (else (register-user-action key))))

(c-define (special-keyboard key x y)
          (unsigned-char int int) void "special_keyboard" ""
 (case key
;;    ((#\e) (pp 'up))
;;    ((#\g) (pp 'down))
   ((#\f) (register-user-action 'right-arrow))
   ((#\d) (register-user-action 'left-arrow))
   
   (else (show "received special keyboard input: " key
               ". Mouse is @ ("x","y")\n"))))

;;;;;;;;;;;;;;;;;;;;;;; Idle function (animation) ;;;;;;;;;;;;;;;;;;;;;;;

(c-define (idle-callback) () void "idle_callback" ""
  (let ((level (thread-receive)))
    (render-scene level)))

;;;;;;;;;;;;;;;;;;;;;;; Gui Initialization ;;;;;;;;;;;;;;;;;;;;;;;

(c-declare "int argc = 0;")
(define (glut-init height width)
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
    (glutIdleFunc idle-callback)))
;    (glutDisplayFunc render-scene)))

(define (usage-message) "TODO: Usage msg...\n\n")

(define return #f)
(define (quit) (return 0))
(define (main)
  (define (start heigth width)
    (glut-init heigth width)
      ;(display-instructions)
    (call/cc (lambda (k) (set! return k) (glutMainLoop))))

  ;; Start a debug/developpement repl in a seperate thread
  ;;   (thread-start! (make-thread (lambda () (##repl))))
  (cond
   ((eqv? (length (command-line)) 1) (start 200 200))
   ((eqv? (length (command-line)) 3)
    (start (string->number (list-ref (command-line) 1))
           (string->number (list-ref (command-line) 2))))
   (else
    (display usage-message))))


(time (main))
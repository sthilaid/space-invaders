
(include "scm-lib.scm")

;;;;;;;;;;;;;;;;;;;;;;; Global state variables  ;;;;;;;;;;;;;;;;;;;;;;;

(define image-height #f)
(define image-width #f)

(define status-message #f)

;; Game instance
(define current-level #f)
(define game-loop-thunk #f)

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

(include "opengl-header.scm")
(c-declare "#include \"bitmaps.c\"")

(define-macro (get-bitmap-param name param)
  (let ((result-type (cond ((eq? param 'pointer)     'GLubyte*)
                           ((or (eq? param 'width)
                                (eq? param 'height)) 'GLsizei)
                           (else                     'GLfloat)))
        (result (if (eq? param 'pointer)
                    "___result_voidstar"
                    "___result")))
    `((c-lambda () ,result-type
                ,(string-append result " = "
                                (symbol->string name) "."
                                (symbol->string param) ";")))))

(define-macro (render-sprite ship-type sprite-index)
  (let ((id (string->symbol (string-append (symbol->string ship-type)
                                           (number->string sprite-index)))))
    `(glBitmap (get-bitmap-param ,id width)
               (get-bitmap-param ,id height)
               (get-bitmap-param ,id xorig)
               (get-bitmap-param ,id yorig)
               (get-bitmap-param ,id xmove)
               (get-bitmap-param ,id ymove)
               (get-bitmap-param ,id pointer))))

(define-macro (ship-renderer ship-type)
  `(lambda (x y state)
     (glRasterPos2i x y)
     (case state
       ((0) (render-sprite ,ship-type 0))
       ((1) (render-sprite ,ship-type 1))
       (else
        (error "cannot draw sprite: invalid state.")))))

(define render-invader-ship
  (let ((easy-renderer (ship-renderer easy))
        (medium-renderer (ship-renderer medium))
        (hard-renderer (ship-renderer hard)))
    (lambda (invader)
      (define x (pos2d-x (invader-ship-pos invader)))
      (define y (pos2d-y (invader-ship-pos invader)))
      (define type (invader-ship-type invader))
      (define state (invader-ship-state invader))
      (case type
        ((easy)
         (glColor3f (random-real) (random-real) (random-real))
         (easy-renderer x y state))
        ((medium)
         (glColor3f (random-real) (random-real) (random-real))
         (medium-renderer x y state))
        ((hard)
         (glColor3f (random-real) (random-real) (random-real))
         (hard-renderer x y state))
        (else (error "Cannor render unknown ship type."))))))

(define render-player-ship
  (let ((player-renderer (ship-renderer player)))
    (lambda (player)
      (define x (pos2d-x (player-ship-pos player)))
      (define y (pos2d-y (player-ship-pos player)))
      (define type (player-ship-type player))
      (define state (player-ship-state player))
      (case type
        ((player)
         (glColor3f 0. 1. 0.)
         (player-renderer x y state))
        (else (error "Cannor render unknown ship type."))))))
      

(define (display-message x y msg)
  (let ((chars (map char->integer (string->list msg)))
        (font GLUT_BITMAP_HELVETICA_12))
    (glColor3f 1. 1. 1.)
    (glRasterPos2i x y)
    (for-each (lambda (char) (glutBitmapCharacter font char))
              chars)))

(c-define (render-scene) () void "render_scene" ""
  (glClearColor 0. 0. 0. 0.)
  (glClear GL_COLOR_BUFFER_BIT)

  ;; Draw invaders
  (for-each render-invader-ship (level-invaders current-level))

  (render-player-ship (level-player current-level))

;;   (if status-message
;;       (display-message 0 0 status-message))

  (glutSwapBuffers))

;;;;;;;;;;;;;;;;;;;;;;; Viewport and projection ;;;;;;;;;;;;;;;;;;;;;;;

(c-define (reshape w h) (int int) void "reshape" ""
;;    (glViewport 0 0 w h)
;;    (glMatrixMode GL_PROJECTION)
;;    (glLoadIdentity)
;;    (glOrtho 0. (exact->inexact w) 0. (exact->inexact h) -1.0 1.0)
;;    (glMatrixMode GL_MODELVIEW))

  (let ((zoom-x (/ w image-width))
        (zoom-y (/ h image-height)))
    (glPointSize (exact->inexact (+ (max zoom-x zoom-y) 1)))
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

(c-define (keyboard key x y) (unsigned-char int int) void "keyboard" ""
 (case key
   ;; On Escape, Ctl-q, Ctl-c, Ctl-w, q -> terminate the program
   ((#\x1b #\x11 #\x03 #\x17 #\q) (quit))
   (else (show "received keyboard input: " key ". Mouse is @ ("x","y")\n"))))

(c-define (special-keyboard key x y)
          (unsigned-char int int) void "special_keyboard" ""
 (let ((player (level-player current-level))
       (speed 4))
   (case key
     ((#\e) (pp 'up))
     ((#\g) (pp 'down))
     ((#\f) (move-player-ship! player speed 0))
     ((#\d) (move-player-ship! player (- speed) 0))
     
     (else (show "received special keyboard input: " key
                 ". Mouse is @ ("x","y")\n")))))

;;;;;;;;;;;;;;;;;;;;;;; Idle function (animation) ;;;;;;;;;;;;;;;;;;;;;;;

(c-define (idle-callback) () void "idle_callback" ""
  (game-loop-thunk)
  (thread-sleep! 0.05)
  (render-scene))

;;;;;;;;;;;;;;;;;;;;;;; Gui Initialization ;;;;;;;;;;;;;;;;;;;;;;;

(c-declare "int argc = 0;")
(define (glut-init height width)
  (let ((argc ((c-lambda () (pointer int) "___result_voidstar = &argc;"))))

    (set! current-level (new-level))
    (set! image-height (level-height current-level))
    (set! image-width (level-width current-level))
    (set! game-loop-thunk (create-game-loop-thunk current-level))
    
    (glutInit argc '())
    (glutInitDisplayMode (bitwise-ior GLUT_DOUBLE GLUT_RGB))
    (glutInitWindowSize image-width image-height)
    (glutCreateWindow "Space Invaders")
    
    (glPointSize 1.)
    (glDisable GL_POINT_SMOOTH)

    (glPixelStorei GL_UNPACK_ALIGNMENT 1)

    ;(create-menu)
    
    (glutReshapeFunc reshape)
    (glutKeyboardFunc keyboard)
    (glutSpecialFunc special-keyboard)
    (glutIdleFunc idle-callback)
    (glutDisplayFunc render-scene)))

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


(main)
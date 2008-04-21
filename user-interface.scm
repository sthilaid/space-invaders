
(include "scm-lib.scm")

;;;;;;;;;;;;;;;;;;;;;;; Global state variables  ;;;;;;;;;;;;;;;;;;;;;;;

(define image-height #f)
(define image-width #f)

(define status-message #f)

;; Game instance
(define current-level #f)

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

(define-macro (draw-sprite id)
  `(glBitmap (get-bitmap-param ,id width)
             (get-bitmap-param ,id height)
             (get-bitmap-param ,id xorig)
             (get-bitmap-param ,id yorig)
             (get-bitmap-param ,id xmove)
             (get-bitmap-param ,id ymove)
             (get-bitmap-param ,id pointer)))

(define ship-rendering-list
  `((easy ,(lambda (x y state)
             (glColor3f (random-real) (random-real) (random-real))
             (glRasterPos2i x y)
             (if (eq? state 'state1)
                 (draw-sprite easy1)
                 (draw-sprite easy2))))))

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
  (for-each (lambda (inv)
              (let ((x (pos2d-x (spaceship-pos inv)))
                    (y (pos2d-y (spaceship-pos inv))))
                ((cadr (assq 'easy ship-rendering-list)) x y 'state1)))
            (level-invaders current-level))

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
;;    ((#\0) (set-octave! 0))
;;    ((#\1) (set-octave! 1))
;;    ((#\2) (set-octave! 2))
;;    ((#\3) (set-octave! 3))
;;    ((#\4) (set-octave! 4))
;;    ((#\5) (set-octave! 5))
;;    ((#\6) (set-octave! 6))
;;    ((#\7) (set-octave! 7))
;;    ((#\8) (set-octave! 8))
;;    ((#\9) (set-octave! 9))

   ;; On Escape, Ctl-q, q -> terminate the program
   ((#\x1b #\x11 #\q) (quit))
   (else (pp `(received keyboard input ,key ,x ,y)))))

(c-define (special-keyboard key x y)
          (unsigned-char int int) void "special_keyboard" ""
 (case key
;;    ((#\e) (move-image! 0 animation-velocity))
;;    ((#\g) (move-image! 0 (- animation-velocity)))
;;    ((#\f) (move-image! animation-velocity 0))
;;    ((#\d) (move-image! (- animation-velocity) 0))
   
   (else (pp `(received special keyboard input ,key ,x ,y)))))

;;;;;;;;;;;;;;;;;;;;;;; Idle function (animation) ;;;;;;;;;;;;;;;;;;;;;;;

(c-define (idle-callback) () void "idle_callback" ""
  (thread-sleep! 0.05)
  (render-scene))

;;;;;;;;;;;;;;;;;;;;;;; Gui Initialization ;;;;;;;;;;;;;;;;;;;;;;;

(c-declare "int argc = 0;")
(define (glut-init height width)
  (let ((argc ((c-lambda () (pointer int) "___result_voidstar = &argc;"))))

    (set! current-level (new-level))
    (set! image-height (level-height current-level))
    (set! image-width (level-width current-level))
    
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
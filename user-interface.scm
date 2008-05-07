(include "scm-lib.scm") 
(include "sprite.scm")


;;;;;;;;;;;;;;;;;;;;;;; Global state variables  ;;;;;;;;;;;;;;;;;;;;;;;

(define image-height #f)
(define image-width #f)

(define status-message #f)

;; Game instance
(define current-level #f)
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

(include "opengl-header.scm")
(include "ppm-reader.scm")
(c-declare #<<end
  #include <GL/gl.h>
  
  typedef struct{
    GLsizei width;
    GLsizei height;
    GLubyte* pointer;} pixelmap;
end
)

(include-ppm-pixel-sprite "sprites/laserA0.ppm")
(include-ppm-pixel-sprite "sprites/laserA1.ppm")
(include-ppm-pixel-sprite "sprites/laserB0.ppm")
(include-ppm-pixel-sprite "sprites/laserB1.ppm")
(include-ppm-pixel-sprite "sprites/laserB2.ppm")
(include-ppm-pixel-sprite "sprites/laserB3.ppm")
(include-ppm-pixel-sprite "sprites/laserB4.ppm")
(include-ppm-pixel-sprite "sprites/laserP0.ppm")
(include-ppm-pixel-sprite "sprites/shield0.ppm")
(include-ppm-pixel-sprite "sprites/easy0.ppm")
(include-ppm-pixel-sprite "sprites/easy1.ppm")
(include-ppm-pixel-sprite "sprites/medium0.ppm")
(include-ppm-pixel-sprite "sprites/medium1.ppm")
(include-ppm-pixel-sprite "sprites/hard0.ppm")
(include-ppm-pixel-sprite "sprites/hard1.ppm")
(include-ppm-pixel-sprite "sprites/mothership0.ppm")
(include-ppm-pixel-sprite "sprites/player0.ppm")
(include-ppm-pixel-sprite "sprites/player1.ppm")
(include-ppm-pixel-sprite "sprites/explodeI0.ppm")
(include-ppm-pixel-sprite "sprites/explodeS0.ppm")
(include-ppm-pixel-sprite "sprites/explodeP0.ppm")
(include-ppm-pixel-sprite "sprites/explodeP1.ppm")
(include-ppm-pixel-sprite "sprites/explodeInvL0.ppm")

(define-macro (cast-pointer new-type old-type val)
  `((c-lambda (,old-type) ,new-type
              ,(string-append "___result_voidstar = ("
                              (symbol->string new-type)
                              ")___arg1;"))
    ,val))

(define-macro (get-pixelmap-param name param)
  (let ((result-type (cond ((eq? param 'pointer)     'GLubyte*)
                           ((or (eq? param 'width)
                                (eq? param 'height)) 'GLsizei)))
        (result (if (eq? param 'pointer)
                    "___result_voidstar"
                    "___result")))
    `((c-lambda () ,result-type
                ,(string-append result " = "
                                (symbol->string name) "."
                                (symbol->string param) ";")))))

(define-macro (render-pixel-sprite name sprite-index)
  (let ((id (string->symbol (string-append (symbol->string name)
                                           (number->string sprite-index)))))
    `(glDrawPixels (get-pixelmap-param ,id width)
                   (get-pixelmap-param ,id height)
                   GL_RGB
                   GL_UNSIGNED_BYTE
                   (cast-pointer GLvoid* GLubyte*
                                 (get-pixelmap-param ,id pointer)))))

(define-macro (create-single-state-renderer obj-type)
  `(lambda (x y state)
     (glRasterPos2i x y)
     (case state
       ((0) (render-pixel-sprite ,obj-type 0))
       (else
        (error "cannot draw sprite: invalid state.")))))

(define-macro (create-2-state-renderer ship-type)
  `(lambda (x y state)
     (glRasterPos2i x y)
     (case state
       ((0) (render-pixel-sprite ,ship-type 0))
       ((1) (render-pixel-sprite ,ship-type 1))
       (else
        (error "cannot draw sprite: invalid state.")))))

(define-macro (create-4-state-renderer obj-type)
  `(lambda (x y state)
     (glRasterPos2i x y)
     (case state
       ((0) (render-pixel-sprite ,obj-type 0))
       ((1) (render-pixel-sprite ,obj-type 1))
       ((2) (render-pixel-sprite ,obj-type 2))
       ((3) (render-pixel-sprite ,obj-type 3))
       (else
        (error "cannot draw sprite: invalid state.")))))

(define render-object
  (let ((easy-renderer   (create-2-state-renderer easy))
        (medium-renderer (create-2-state-renderer medium))
        (hard-renderer   (create-2-state-renderer hard))
        (player-renderer (create-single-state-renderer player))
        (laserA-renderer  (create-2-state-renderer laserA))
        (laserB-renderer  (create-4-state-renderer laserB))
        (laserP-renderer  (create-single-state-renderer laserP))
        (explodeI-renderer (create-single-state-renderer explodeI))
        (explodeInvL-renderer (create-single-state-renderer explodeInvL))
        (explodeS-renderer (create-single-state-renderer explodeS))
        (explodeP-renderer (create-2-state-renderer explodeP))
;;        (shield-renderer (create-single-state-renderer shield))
        (mothership-renderer (create-single-state-renderer mothership)))
        
    (lambda (obj)
      (define x (pos2d-x (game-object-pos obj)))
      (define y (pos2d-y (game-object-pos obj)))
      (define type (type-id (game-object-type obj)))
      (define state (game-object-state obj))
      (case type
        ((easy)   (easy-renderer x y state))
        ((medium) (medium-renderer x y state))
        ((hard)   (hard-renderer x y state))
        ((player) (player-renderer x y state))
        ((laserA) (laserA-renderer x y state))
        ((laserB) (laserB-renderer x y state))
        ((laserP) (laserP-renderer x y state))
        ((explodeI) (explodeI-renderer x y state))
        ((explodeInvL) (explodeInvL-renderer x y state))
        ((explodeS) (explodeS-renderer x y state))
        ((explodeP) (explodeP-renderer x y state))
;;        ((shield) (shield-renderer x y state))
        ((mothership) (mothership-renderer x y state))
        (else (error (string-append "Cannot render unknown object type:"
                                    (symbol->string type))))))))
(define (render-shield shield)
  ;; equivalent to rgb color: 1ffe1f
  (glColor3f .12156862745098039 .996078431372549 .12156862745098039)

  (for-each (lambda (particle)
              (let* ((shield-x (pos2d-x (game-object-pos shield)))
                     (shield-y (pos2d-y (game-object-pos shield)))
                     (x (+ shield-x (pos2d-x particle)))
                     (y (+ shield-y (pos2d-y particle))))
                (glBegin GL_POINTS)
                (glVertex2i x y)
                (glEnd)))
            (shield-particles shield)))

(define (render-level level)
  (define score (level-score level))
  (define player-lives (level-lives level))

  (glBegin GL_LINES)
  (glVertex2i 0 9)
  (glVertex2i screen-max-x 9)
  (glEnd))



(define (display-message x y msg)
  (let ((chars (map char->integer (string->list msg)))
        (font GLUT_BITMAP_HELVETICA_12))
    (glColor3f 1. 1. 1.)
    (glRasterPos2i x y)
    (for-each (lambda (char) (glutBitmapCharacter font char))
              chars)))

(define (render-scene level)
  (glClearColor 0. 0. 0. 0.)
  (glClear GL_COLOR_BUFFER_BIT)

  ;; Draw background stuff
  (render-level level)
  
  ;; Draw all objects
  (for-each render-object (level-all-objects level))
  (for-each render-shield (level-shields level))

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

  (let* ((zoom-x (/ w image-width))
         (zoom-y (/ h image-height))
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
   ((#\s #\S) (register-user-action 'show-score))
   ((#\space) (register-user-action 'shoot-laser))
   ;; On Escape, Ctl-q, Ctl-c, Ctl-w, q -> terminate the program
   ((#\x1b #\x11 #\x03 #\x17 #\q) (quit))
   (else (show "received keyboard input: " key ". Mouse is @ ("x","y")\n"))))

(c-define (special-keyboard key x y)
          (unsigned-char int int) void "special_keyboard" ""
 (case key
   ((#\e) (pp 'up))
   ((#\g) (pp 'down))
   ((#\f) (register-user-action 'move-right))
   ((#\d) (register-user-action 'move-left))
   
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

    (set! current-level (new-level))
    (set! image-height (level-height current-level))
    (set! image-width (level-width current-level))

    (set! simulation-thread
          (make-thread (game-loop (current-thread) current-level)))
    (thread-start! simulation-thread)
    
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


(main)
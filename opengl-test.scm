;; (load "opengl")
;; (load "glu")
;; (load "glut")

(define leftFirst #t)

(define (init)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glShadeModel GL_FLAT);
  (glClearColor 0.0 0.0 0.0 0.0))

(define (drawLeftTriangle)
 ; draw yellow triangle on LHS of screen
 (glBegin GL_TRIANGLES)
 (begin
   (glColor4f 1.0 1.0 0.0 0.75);
   (glVertex3f 0.1 0.9 0.0)
   (glVertex3f 0.1 0.1 0.0)
   (glVertex3f 0.7 0.5 0.0))
 (glEnd))

(define (drawRightTriangle)
 ; draw cyan triangle on RHS of screen */
 (glBegin GL_TRIANGLES)
 (begin
   (glColor4f 0.0 1.0 1.0 0.75)
   (glVertex3f 0.9 0.9 0.0)
   (glVertex3f 0.3 0.5 0.0)
   (glVertex3f 0.9 0.1 0.0))
 (glEnd))

(c-define (display) () void "display" ""
  (glClear GL_COLOR_BUFFER_BIT)
  (if leftFirst
      (begin
        (drawLeftTriangle)
        (drawRightTriangle))
      (begin
        (drawRightTriangle)
        (drawLeftTriangle)))
  (glFlush))

(c-define (reshape w h) (int int) void "reshape" ""
  (glViewport 0 0 w h)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (if (<= w h)
      (gluOrtho2D 0.0 1.0 0.0 (exact->inexact (/ h w)))
      (gluOrtho2D 0.0 (exact->inexact (/ w h)) 0.0 1.0)))

(c-define (keyboard key x y) (unsigned-char int int) void "keyboard" ""
  (case key
    ((#\t #\T)
     (set! leftFirst (if leftFirst #f #t))
     (glutPostRedisplay))
    ((#\x1b) ; Escape key
     (thread-terminate! (current-thread)))))

(c-declare "int argc = 0;")
(define (glut-init)
  (let ((argc ((c-lambda () (pointer int) "___result_voidstar = &argc;"))))
    (glutInit argc '())
    (glutInitDisplayMode (bitwise-ior GLUT_SINGLE GLUT_RGB))
    (glutInitWindowSize 200 200)
    (glutCreateWindow "test")
    (glutReshapeFunc reshape)
    (glutKeyboardFunc keyboard)
    (glutDisplayFunc display)))

;; Main Loop
;; Open window with initial window size title bar 
;; RGBA display mode and handle input events.
(define (main)
  (glut-init)
  (init)
  (glutMainLoop))

(main)
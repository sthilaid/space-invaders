
;(include "glut#.scm")
;; (load "opengl")
;; (load "glu")
;; (load "glut")

(define rotAngle 0)

;; /*  Initialize antialiasing for RGBA mode including alpha
;;  *  blending hint and line width.  Print out implementation
;;  *  specific info on line width granularity and width.
;;  */
(define (init)
;;    GLfloat values[2];
;;    glGetFloatv (GL_LINE_WIDTH_GRANULARITY values);
;;    printf ("GL_LINE_WIDTH_GRANULARITY value is %3.1f\n" values[0]);

;;    glGetFloatv (GL_LINE_WIDTH_RANGE values);
;;    printf ("GL_LINE_WIDTH_RANGE values are %3.1f %3.1f\n"
;;       values[0] values[1]);

 (glEnable GL_LINE_SMOOTH)
 (glEnable GL_BLEND)
 (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
 (glHint GL_LINE_SMOOTH_HINT GL_DONT_CARE)
 (glLineWidth 1.5)

 (glClearColor 0.0 0.0 0.0 0.0))


;; /* Draw 2 diagonal lines to form an X
;;  */
(c-define (display) () void "display" ""
 (display-body))

(define (display-body)
 (glClear GL_COLOR_BUFFER_BIT)
 (glColor3f 0.0 1.0 0.0)
 (glPushMatrix)
 (glRotatef (+ 0. (- rotAngle)) 0.0 0.0 0.1)
 (glBegin GL_LINES)
 (begin
   (glVertex2f -0.5 0.5)
   (glVertex2f 0.5 -0.5))
 (glEnd)
 (glPopMatrix)
 (glColor3f 0.0 0.0 1.0)
 (glPushMatrix)
 (glRotatef (+ 0. rotAngle) 0.0 0.0 0.1)
 (glBegin GL_LINES)
 (begin
   (glVertex2f 0.5 0.5)
   (glVertex2f -0.5 -0.5))
 (glEnd)
 (glPopMatrix)

 (glFlush))


(c-define (reshape w h) (int int) void "reshape" ""
 (glViewport 0 0 w h)
 (glMatrixMode GL_PROJECTION)
 (glLoadIdentity)
 (if (<= w h) 
     (gluOrtho2D -1.0 1.0 
                 (exact->inexact (/ (- h) w))
                 (exact->inexact (/ h w)))
     (gluOrtho2D (exact->inexact (/ (- w) h))
                 (exact->inexact (/ w h)) -1.0 1.0))
 (glMatrixMode GL_MODELVIEW)
 (glLoadIdentity))


(c-define (keyboard key x y) (unsigned-char int int) void "keyboard" ""
 (case key
   ((#\r #\R)
    (set! rotAngle (+ rotAngle 20))
    (if (>= rotAngle 360) (set! rotAngle 0))
    (glutPostRedisplay))
   ((#\x1b) (thread-terminate! (current-thread))) ; escape key
   (else (pp `(received keyboard input ,key ,x ,y)))))

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

;; /*  Main Loop
;;  *  Open window with initial window size title bar 
;;  *  RGBA display mode and handle input events.
;;  */
(define (main)
  (glut-init)
  (init)
  (glutMainLoop))

(main)

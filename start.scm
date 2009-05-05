;; temporary fix

(define primordial-thread (current-thread))

(define files
  (list "opengl" "glu" "rbtree" "scm-lib" "scm-lib-macro" "stats" "ppm-reader" "texture" "sprite" "font" "thread-simulation.scm"
        "user-interface-images" "sdl-interface" "engine.scm" "sdl-user-interface"
        ))
(for-each (lambda (lib) (load lib)) files)
(main)

(define (restart)
  (for-each
   (lambda (t)
     (if (not (eq? t primordial-thread))
         (thread-terminate! t)))
   (thread-group->thread-list (thread-thread-group (current-thread))))
  (load "engine.scm")
  (main))
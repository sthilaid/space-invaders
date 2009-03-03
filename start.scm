;; temporary fix
(define files
  (list "opengl" "glu" "rbtree" "scm-lib" "scm-lib-macro" "stats" "ppm-reader" "texture" "sprite" "font"
        "user-interface-images" "sdl-interface" "new-engine.scm" "sdl-user-interface"
        ))
(for-each (lambda (lib) (load lib)) files)
(main)


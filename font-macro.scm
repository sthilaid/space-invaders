;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: font-macro.scm
;;
;; description: This file contains abstraction related to symmertric
;; bitmap fonts. By symmertric, it is meant that all the characters
;; must have the same size.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(c-declare "#include <gl.h>\n")

;; Generates code that will load an bitmap font called font-name at
;; runtime into a C array of the same name. The array must have been
;; previously be declared. The difference between char-(width|height)
;; and good-char-(width|height) is that the good ones should be the
;; lowest power of 2 greater then the corresponding char-(width|height).
(define-macro (runtime-font-generation-code font-name char-width char-height
                                            good-char-width good-char-height)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))

  (let ((char-table  (gensym 'char-table))
        (char (gensym 'char))
        (update-fun
         `(c-lambda
           (int
            int int
            unsigned-int unsigned-int unsigned-int unsigned-int) void
            ,(with-output-to-string
               ""
               (lambda ()
                 (show font-name
                       "[___arg1][___arg2][___arg3][0] = ___arg4;\n")
                 (show font-name
                       "[___arg1][___arg2][___arg3][1] = ___arg5;\n")
                 (show font-name
                       "[___arg1][___arg2][___arg3][2] = ___arg6;\n")
                 (show font-name
                       "[___arg1][___arg2][___arg3][3] = ___arg7;\n"))))))
    `(let ((,char-table (get-font-table ,font-name ,char-width ,char-height)))
       (for-each
        (lambda (,char) (load-font-char ,font-name ,char ,update-fun
                                        ,char-width ,char-height
                                        ,good-char-width ,good-char-height))
        (table->list ,char-table)))))

;; Defines a symmetric font from a pair of 2 files.
;;
;; The first is an ascii ppm formated bitmap image called
;; font-name.ppm (here font-name is the value passed on the
;; macro-call, and not directly font-name...) containing all the
;; characters one after the other (can be on multiple row, does not
;; matter), but if there is multiple colors, the chars of a same color
;; must be adjacent one another.
;;
;; The second file, called font-name.scm, will contain some scheme
;; data of the following format:
;;
;; ((colors: (color1 color2 ...))
;;  (chars: (char1 char2 char3 ...)))
;;
;; which will describe the order in which the characters color
;; aggregations occurs, and the order in which each char inside a
;; color aggregation occurs. This forces all colors to have all the
;; characters present in chars. The values used in colors and chars
;; will later be used to reference each individual char images and
;; display them.
(define-macro (define-symmetric-font font-name
                char-width char-height . options)
  ;; This a sorting function that will ensure that a
  (define (sort-chars char-list)
    (quick-sort
     (lambda (a b)
       (< (get-char-index font-name (caar a) (cadar a))
          (get-char-index font-name (caar b) (cadar b))))
     (lambda (a b)
       (= (get-char-index font-name (caar a) (cadar a))
          (get-char-index font-name (caar b) (cadar b))))
     (lambda (a b)
       (> (get-char-index font-name (caar a) (cadar a))
          (get-char-index font-name (caar b) (cadar b))))
     char-list))
  
  (include "font.scm")
  (include "scm-lib-macro.scm")
  (include "ppm-reader.scm")
  (include "scm-lib.scm")

  ;; Ensure that the char indices will be coherent between the macro
  ;; expansion and the runtime with this call...
  (init-char-indexes! font-name)
  (let* ((font-identifier (string->symbol font-name))
         (good-char-width (next-power-of-2 char-width))
         (good-char-height (next-power-of-2 char-height))
         (font-char-table (get-font-table font-name char-width char-height))
        ;; if font loading is dynamic, an empty declaration is made.
        (dynamic-font-elements-declarations
         `(c-declare ,(to-string (show "GLubyte "font-name
                                       "["(table-length font-char-table)"]"
                                       "["good-char-height"]"
                                       "["good-char-width"]"
                                       "[4]\n;"))))
        ;; then, the array will get filled at runtime.
        (dynamic-font-elements-generation
         `((lambda ()
             (runtime-font-generation-code
              ,font-name ,char-width ,char-height
              ,good-char-width ,good-char-height))))
        ;; dummy declaration and generation code is used with the
        ;; texture abstraction. This will only register a single
        ;; texture id/number that will be used by the font the render
        ;; a character. The same texture will be reused over and over.
        (texture-declaration-code ";\n")
        (texture-generation-code "1+1;\n")
        ;; The init script contains only the texture settings.
        (texture-init-script
         (let* ((tex-id-sym (gensym 'tex-id))
               (wrap-s (if (memq 'loop-x options) 'GL_REPEAT 'GL_CLAMP))
               (wrap-t (if (memq 'loop-y options) 'GL_REPEAT 'GL_CLAMP))
               (mag 'GL_NEAREST)
               (min mag))
           (if (memq 'loop-x options) (pp `(,font-name : ,wrap-s ,wrap-t)))
           `(upload-font-to-video-card
             ,font-identifier
             wrap-s: ,(if (memq 'loop-x options) 'GL_REPEAT 'GL_CLAMP)
             wrap-t: ,(if (memq 'loop-y options) 'GL_REPEAT 'GL_CLAMP)
             mag: GL_NEAREST
             min: GL_NEAREST)))
        ;; This function returns a pointer to the image array.
        (get-pointer-code
         `(c-lambda (int) (pointer void)
           ,(with-output-to-string ""
              (lambda ()
                (show "___result_voidstar = "font-name"[___arg1];\n"))))))
    `(begin
       ;; depending on the options passed, the font loading will be
       ;; either static (at compile time) or dynamic (at runtime)
       ,dynamic-font-elements-declarations
       ,dynamic-font-elements-generation
       (define ,font-identifier
         (make-font ,font-name
                    (make-table test: equal?)
                    ,good-char-width ,good-char-height
                    ,get-pointer-code))
       (let ((tex-table (font-texture-table ,font-identifier)))
         ,@(map
            (lambda (color-char)
              (let ((color (car color-char))
                    (char  (cadr color-char))
                    (tex-id (gensym 'tex-id)))
                `(let ((texture (new-texture ,good-char-width
                                             ,good-char-height)))
                   (table-set! tex-table (list ',color ',char) texture)
                   ;;(,texture-init-script ',color ',char (texture-id texture))

                   ;; the init must come *after* opengl's initialization?
                   (set! debug-textures (cons (lambda () (,texture-init-script ',color ',char (texture-id texture))) debug-textures)))))
            (map car (table->list font-char-table)))))))

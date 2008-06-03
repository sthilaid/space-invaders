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

(c-declare "#include <GL/gl.h>\n")

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
  (let* ((good-char-width (next-power-of-2 char-width))
         (good-char-height (next-power-of-2 char-height))
         (font-char-table (get-font-table font-name char-width char-height))
         (static-font-elements-declarations
          ;; Will generate the filled array declaration that should
          ;; look like
          ;; GLubyte font[charnb][cheight][cwidth][4] =
          ;;                                   {r1,g1,b1,a1,r2,g2,b2,a2,...};
          `(c-declare
            ,(to-string
              (show "GLubyte "font-name
                    "["(table-length font-char-table)"]"
                    "["good-char-height"]"
                    "["good-char-width"]"
                    "[4] = {")
              (let ((n 0)
                    (last-n (- (table-length font-char-table) 1)))
                (for-each
                 (lambda (el)
                   (let ((color (caar el))
                         (char (cadar el))
                         (pixels (cdr el)))
                     (show "{")
                     (for y 0 (< y good-char-height)
                       (begin
                         (for x 0 (< x good-char-width)
                           (let* ((out-of-bounds?
                                   (or (>= x char-width) (>= y char-height)))
                                  (current-pix
                                   (if (not out-of-bounds?)
                                       (list-ref pixels (+ (* y char-width) x))
                                       '()))
                                  (r (if out-of-bounds? 0 (car current-pix)))
                                  (g (if out-of-bounds? 0 (cadr current-pix)))
                                  (b (if out-of-bounds? 0 (caddr current-pix)))
                                  (a (if out-of-bounds?
                                         0
                                         (if (< (+ r g b) 10) 0 255))))
                             (show r "," g "," b "," a)
                             (if (< x (- good-char-width 1)) (show ","))))
                         (if (= y (- good-char-height 1))
                             (show "}")
                             (show ",")))))
                   (if (< n last-n)
                       (begin (set! n (+ n 1)) (show ","))))
                 (sort-chars (table->list font-char-table))))
              (show "};\n"))))
         ;; sets up the correct char indices at runtime
         (static-font-elements-generation
          `((lambda () (init-char-indexes! ,font-name))))
         
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
         (let ((tex-id-sym (gensym 'tex-id)))
          `(lambda (,tex-id-sym)
             (lambda ()
               (glBindTexture GL_TEXTURE_2D ,tex-id-sym)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
               (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
               ))))
        ;; This function returns a pointer to the image array.
        (get-pointer-code
         `(c-lambda (int) (pointer void)
           ,(with-output-to-string ""
              (lambda ()
                (show "___result_voidstar = "font-name"[___arg1];\n"))))))
    `(begin
       ;; depending on the options passed, the font loading will be
       ;; either static (at compile time) or dynamic (at runtime)
       ,@(if (memq 'static options)
             (list
              static-font-elements-declarations
              static-font-elements-generation)
             (list
              dynamic-font-elements-declarations
              dynamic-font-elements-generation))
       (define-texture ,texture-declaration-code ,texture-generation-code
         ,texture-init-script ,font-name ,good-char-width ,good-char-height) 
       (add-new-font!
        ,font-name
        (make-font ,font-name
                   (texture-id (retrieve-texture ,font-name))
                   ,good-char-width ,good-char-height
                   ,get-pointer-code)))))

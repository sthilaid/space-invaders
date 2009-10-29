;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: texture-macro.scm
;;
;; description: This file contains abstractions related to the
;; creation of opengl textures.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Will take the image pointer and load the image inside the videocard.
(define-macro (attach-texture image-pointer-name width height)
  (define (show . args)
    (for-each (lambda (x) (if (string? x) (display x) (write x))) args))
  
  `((c-lambda () void
     ,(with-output-to-string ""
        (lambda ()
          (show "glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, " width ", "
                height ", 0, GL_RGBA, GL_UNSIGNED_BYTE, "
                image-pointer-name ");\n"))))))

;; Texture abastraction. Using this abstraction will enable several
;; features provided in texture.scm. All textures will be recored in a
;; hash-table and some simple drawing function are provided in
;; texture.scm.
;;
;; declaration-code should be a string that will
;; contain the declaration of the array that will contain the image
;; used for the texture.
;;
;; generation-code is expected to be scheme code that will generate or
;; load the image inside the previously declared C array.
;;
;; init-script is a scheme function which expects the texture id
;; (integer) and should return a thunk that will be executed at
;; runtime when the function initialize-textures! is called.
;;
;; tex-pointer should be the image pointer name (string) and width and
;; heigth are corresponding to the images size.
(define-macro (new-texture width height)
  `(make-texture (genTexture) ,width ,height))

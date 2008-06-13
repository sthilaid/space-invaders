(include "texture-macro.scm")
(include "sprite-macro.scm")
(include "font-macro.scm")
(include "scm-lib-macro.scm")
(include "opengl-header.scm")

;; (define-sprite "sprites/laserA0.ppm")
;; (define-sprite "sprites/laserA1.ppm")
;; (define-sprite "sprites/laserB0.ppm")
;; (define-sprite "sprites/laserB1.ppm")
;; (define-sprite "sprites/laserB2.ppm")
;; (define-sprite "sprites/laserB3.ppm")
;; (define-sprite "sprites/laserB4.ppm")
;; (define-sprite "sprites/laserP0.ppm")
;; (define-sprite "sprites/shield0.ppm")
;; (define-sprite "sprites/easy0.ppm")
;; (define-sprite "sprites/easy1.ppm")
;; (define-sprite "sprites/medium0.ppm")
;; (define-sprite "sprites/medium1.ppm")
;; (define-sprite "sprites/hard0.ppm")
;; (define-sprite "sprites/hard1.ppm")
;; (define-sprite "sprites/mothership0.ppm")
;; (define-sprite "sprites/player0.ppm")
;; (define-sprite "sprites/explodeI0.ppm")
;; (define-sprite "sprites/explodeS0.ppm")
;; (define-sprite "sprites/explodeP0.ppm")
;; (define-sprite "sprites/explodeP1.ppm")
;; (define-sprite "sprites/explodeInvL0.ppm")

(define-symmetric-font "laserA" 3 7 static)
(define-symmetric-font "laserB" 3 7 static)
(define-symmetric-font "laserC" 3 7 static)
(define-symmetric-font "player_laser" 1 7 static)
(define-symmetric-font "easy" 12 8 static)
(define-symmetric-font "medium" 11 8 static)
(define-symmetric-font "hard" 8 8 static)
(define-symmetric-font "player_laser_explosion" 8 8 static)
(define-symmetric-font "invader_laser_explosion" 6 8 static)
(define-symmetric-font "invader_explosion" 13 8 static)
(define-symmetric-font "mothership" 16 7 static)
(define-symmetric-font "mothership_explosion" 21 8 static)
(define-symmetric-font "player" 13 8 static)
(define-symmetric-font "player_explosion" 16 8 static)

(define-symmetric-font "bb_fonts" 8 8 static)
 ;;(define-symmetric-font "f_operationwolf" 8 8)
;;(define-symmetric-font "f_syvalion" 16 16)

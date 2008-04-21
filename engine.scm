(include "scm-lib.scm")



(define-type pos2d x y)

(define (inverse-dir dir . options)
  (let ((x-fact (if (memq 'x options) -1 1))
        (y-fact (if (memq 'y options) -1 1)))
    (make-pos2d (* x-fact (pos2d-x dir))
               (* y-fact (pos2d-y dir)))))

(define-type spaceship type pos state)


;;;;; todo: Should we remove this  ship-type data or not?
(define-type ship-type id height width)
(define types
  `( (easy ,(make-ship-type 'easy 8 12))
     (medium ,(make-ship-type 'medium 8 11))
     (hard ,(make-ship-type 'hard 8 8))
     (mothership ,(make-ship-type 'mothership 7 16))
     (player ,(make-ship-type 'player 8 13))
     (side-wall ,(make-ship-type 'side-wall 125 1))
     (horiz-wall ,(make-ship-type 'horiz-wall 1 200))
   ))
(define (get-type type-name)
  (let ((type (assq type-name types)))
  (if type
      (cadr type)
      (error (string-append "no such type: " type-name)))))

(define type-id ship-type-id)
(define type-height ship-type-height)
(define type-width ship-type-width)


(define-type wall-struct rect)
(define (new-wall x y width height)
  (make-wall-struct (make-rect x y width height)))
(define wall-rect wall-struct-rect)

(define-type level-struct height width invaders player walls)
(define level-height level-struct-height)
(define level-width level-struct-width)
(define level-invaders level-struct-invaders)
(define level-player level-struct-player)
(define level-walls level-struct-walls)

(define (new-level)
  (define invaders '())
  (define x-offset 30)
  (define y-offset (- 265 152))
  (define max-x 228)
  (define max-y 265)
  (define invader-spacing 16)
  
  (define (determine-type-id max-y)
    (cond ((< max-y 2) 'easy)
          ((< max-y 4) 'medium)
          (else 'hard)))

  (let loop-h ((h 0))
    (let ((current-type (determine-type-id h)))
      (if (< h 5)
          (begin
            (let loop-w ((w 0))
              (if (< w 11)
                  (let ((x (+ x-offset (* w invader-spacing)))
                        (y (+ y-offset (* h invader-spacing))))
                    (set! invaders
                          (cons (make-spaceship current-type (make-pos2d x y)
                                                'state1)
                                invaders))
                    (loop-w (+ w 1)))))
            (loop-h (+ h 1))))))
  
  (let ((walls (list (new-wall 0 -inf.0 -inf.0 +inf.0)
                     (new-wall -inf.0 0 +inf.0 -inf.0)
                     (new-wall max-x -inf.0 +inf.0 +inf.0)
                     (new-wall -inf.0 max-y +inf.0 +inf.0)))
        (player-ship (make-spaceship 'player (make-pos2d 22 (- max-y 240))
                                     'state1)))
    (make-level-struct max-y max-x invaders player-ship walls)))


(define (move-player! player delta-x delta-y)
  (let* ((pos (spaceship-pos player) )
         (x (pos2d-x pos))
         (y (pos2d-y pos)))
    (pos2d-x-set! pos (+ x delta-x))
    (pos2d-y-set! pos (+ y delta-y))))

(define (detect-collision? ship level)
  (define (detect-ship-col? ship1 ship2)
    (let* ((ship1-pos (spaceship-pos ship1))
           (ship2-pos (spaceship-pos ship2)))
      (and (not (eq? ship1 ship2))
           (rectangle-collision?
            (make-rect (pos2d-x ship1-pos) (pos2d-y ship1-pos)
                       (type-width (spaceship-type ship1))
                       (type-height (spaceship-type ship1)))
            (make-rect (pos2d-x ship2-pos) (pos2d-y ship2-pos)
                       (type-width (spaceship-type ship2))
                       (type-height (spaceship-type ship2)))))))
  
  (let ((ship-pos (spaceship-pos ship)))
    (or (exists (lambda (inv) (detect-ship-col? ship inv))
                (level-invaders level))
        
        (exists (lambda (wall) (rectangle-collision?
                                (make-rect (pos2d-x (spaceship-pos ship))
                                           (pos2d-y (spaceship-pos ship))
                                           (type-width (spaceship-type ship))
                                           (type-height (spaceship-type ship)))
                                (wall-rect wall)))
                (level-walls level)))))

(define-type rect x y width height)
(define (rectangle-collision? r1 r2)
  (let* ((r1-x-min (rect-x r1))
         (r1-x-max (+ r1-x-min (rect-width r1)))
         (r1-y-min (rect-y r1))
         (r1-y-max (+ r1-y-min (rect-height r1)))
         (r2-x-min (rect-x r2))
         (r2-x-max (+ r2-x-min (rect-width r2)))
         (r2-y-min (rect-y r2))
         (r2-y-max (+ r2-y-min (rect-height r2))))
    (if (or (< r1-x-max r2-x-min)
            (> r1-x-min r2-x-max)
            (< r1-y-max r2-y-min)
            (> r1-y-min r2-y-max))
        #f
        #t)))
                              

;; (define (step! level dir)
;;   (let ((used-dir
;;          (if (exists (lambda (inv) (detect-collision? inv level))
;;                      (level-invaders level))
;;              (inverse-dir dir 'x)
;;              dir)))
;;     (for-each (lambda (inv)
;;                 (let ((next-pos (make-pos2d (* (pos2d-x used-dir)
;;                                               (pos2d-x (spaceship-pos inv)))
;;                                            (* (pos2d-y used-dir)
;;                                               (pos2d-y (spaceship-pos inv))))))
;;                   (spaceship-set-pos! inv next-pos)))
;;               (level-invaders level))
;;     used-dir))


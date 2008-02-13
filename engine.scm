(include "scm-lib.scm")

(define (generic-member comparator el list)
  (cond
   ((not (pair? list)) #f)
   ((comparator el (car list)) list)
   (else (generic-member comparator el (cdr list)))))


(define-type 2dcoord x y)
(define (new-pos2d x y) (make-2dcoord x y))
; todo: could be optimised
(define pos2d-x 2dcoord-x)
(define pos2d-y 2dcoord-y)
(define pos2d-set-x! 2dcoord-x-set!)
(define pos2d-set-y! 2dcoord-y-set!)
(define (pos2d-eq? p1 p2) equal?)

(define (inverse-dir dir . options)
  (let ((x-fact (if (memq 'x options) -1 1))
        (y-fact (if (memq 'y options) -1 1)))
    (new-pos2d (* x-fact (pos2d-x dir))
               (* y-fact (pos2d-y dir)))))

(define-type ship type pos)
(define (new-spaceship type pos) (make-ship type pos))
(define spaceship-type ship-type)
(define spaceship-pos ship-pos)
(define spaceship-set-pos! ship-pos-set!)

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


; normally 5 x 11
(define-type level-struct height width invaders player walls)
(define (new-level height width)
  (define invaders '())
  (define walls
    ; todo: must change! un-hardcode boundaries + not use spaceship
    (list (new-spaceship (get-type 'horiz-wall) (new-pos2d 0 0))
          (new-spaceship (get-type 'horiz-wall) (new-pos2d 0 125))
          (new-spaceship (get-type 'side-wall) (new-pos2d 0 0))
          (new-spaceship (get-type 'side-wall) (new-pos2d 200 0))))
          
  (define (determine-type-id height)
    (cond ((< height 2) 'easy)
          ((< height 4) 'medium)
          (else 'hard)))

  ; todo: BAD generation of invaders... not one per pixel...
  (define invader-spacing 16)
  (let loop-h ((h 0))
    (let ((current-type (get-type (determine-type-id h))))
      (if (< h height)
          (begin
            (let loop-w ((w 0))
              (if (< w width)
                  (begin
                    (set! invaders
                          (cons (new-spaceship current-type (new-pos2d h w))
                                invaders))
                    (loop-w (+ w invader-spacing)))))
            (loop-h
             (+ h invader-spacing))))))
              
  (for h 0 (< h height)
       (let ((type (get-type (determine-type-id h))))
         (for w 0 (< w width)
              (set! invaders
                    (cons (new-spaceship type (new-pos2d h w))
                          invaders)))))
  
  ;Warning: todo... must change the new player gen...
  (let ((player-ship (new-spaceship (get-type 'player)
                                    (new-pos2d (- height 1) 3))))
    (make-level-struct height width invaders player-ship walls)))

(define level-height level-struct-height)
(define level-width level-struct-width)
(define level-invaders level-struct-invaders)
(define level-player level-struct-player)

(define (detect-collision? ship level)
  (define (detect-ship-col? ship1 ship2)
    (let* ((ship1-pos (spaceship-pos ship1))
           (ship2-pos (spaceship-pos ship2))
           (ship1-x-min (pos2d-x ship1-pos))
           (ship1-y-min (pos2d-y ship1-pos))
           (ship1-x-max (+ ship1-x-min
                           (- (type-width (spaceship-type ship1)) 1)))
           (ship1-y-may (+ ship1-y-min
                           (- (type-height (spaceship-type ship1)) 1)))
           (ship2-x-min (pos2d-x ship2-pos))
           (ship2-y-min (pos2d-y ship2-pos))
           (ship2-x-max (+ ship2-x-min
                           (- (type-width (spaceship-type ship2)) 2)))
           (ship2-y-may (+ ship2-y-min
                           (- (type-height (spaceship-type ship2)) 2))))
      #t))
                
  (let ((ship-pos (spaceship-pos ship)))
    (or (exists (lambda (inv) (detect-ship-col? ship inv))
                (level-invaders level))
        
        (exists (lambda (wall) detect-ship-col? ship wall)
                (level-walls level)))))

(define (step! level dir)
  (let ((used-dir
         (if (exists (lambda (inv) (detect-collision? inv level))
                     (level-invaders level))
             (inverse-dir dir 'x)
             dir)))
    (for-each (lambda (inv)
                (let ((next-pos (new-pos2d (* (pos2d-x used-dir)
                                              (pos2d-x (spaceship-pos inv)))
                                           (* (pos2d-y used-dir)
                                              (pos2d-y (spaceship-pos inv))))))
                  (spaceship-set-pos! inv next-pos)))
              (level-invaders level))
    used-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; filename: sdl-user-interface.scm
;;
;; description: SDL user interface for space invaders, based on the
;; engine.scm space invader model. This file contains the
;; application's "main" function. During the initialization, 2 other
;; threads will be started. The first will contain the engine's
;; game-loop and the second will contain the sdl event polling
;; loop. Thus the primordial thread will be the one which will render
;; the opengl image on the screen. Any user input will be forwarded as
;; needed to the engine, and in return, the engine will tell to the
;; primordial thread when to execute redraw on the screen, or play
;; sounds. This communication is made through the gambit thread
;; mailbox system.
;;
;; author: David St-Hilaire
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(include "scm-lib-macro.scm")
(include "opengl-header.scm")


;;;;;;;;;;;;;;;;;;;;;;; Global state variables  ;;;;;;;;;;;;;;;;;;;;;;;

(define game-loop-thunk #f)
(define sdl-screen #f)
(define simulation-thread #f)
(define event-thread #f)
(define display-fps? #f)
(define FPS (create-bounded-simple-moving-avg 10))
(define GC-dt-set '())
(define fps-set '())
(define draw-set '())

;; SDL mixer sound chunks
(define star-wars-chunk #f)
(define test-chunk #f)
(define test-chunk2 #f)




;;;;;;;;;;;;;;;;;;;;;;; Sound rendering functions ;;;;;;;;;;;;;;;;;;;;;;;

(define (play-sfx sfx)
  (case sfx
    [(mothership-sfx)
     (when test-chunk2 (SDL::Mix::play-channel 0 test-chunk2 4))]
    [(star-wars-op)
     (when star-wars-chunk (SDL::Mix::play-channel 0 star-wars-chunk 0))]))

(define (stop-sfx sfx)
  (SDL::Mix::halt-channel -1))



;;;;;;;;;;;;;;;;;;;;;;; Render-Sceneing function ;;;;;;;;;;;;;;;;;;;;;;;

(define (render-string x y str color)
  (if (not (eq? color 'black))
      (let loop ((i 0) (chars (string->list str)))
        (if (pair? chars)
            (begin
              (draw-char "bb_fonts" color (car chars) x y i)
              (loop (+ i 1) (cdr chars)))))))

(define (render-fontified-sprite sprite-name x y state color)
  (if (not (eq? color 'black))
   (draw-char (symbol->string sprite-name) color state x y 0)))

;; Simple abstraction over open-gl to set up the desired color.
(define (set-openGL-color color)
  (case color
    ;; equivalent to rgb color: 1ffe1f
    ((green)
     (glColor3f .12156862745098039 .996078431372549 .12156862745098039))
    ((white)
     (glColor3f 1. 1. 1.))
    ((red)
     (glColor3f 1. 0. 0.))
    ((black)
     (glColor3f 0. 0. 0.))
    (else (error "unknown color"))))


;; Main rendering function, also calculates the redraw frame-rate
(define render-scene
  (let ((last-render-time 0))
    (lambda (sdl-screen level)
      (let ((render-init-time (time->seconds (current-time))))
       (SDL::with-locked-surface
        sdl-screen
        (lambda ()
          (glClearColor 0. 0. 0. 0.)
          (glClear GL_COLOR_BUFFER_BIT)

          (glBlendFunc GL_SRC_ALPHA GL_ONE)
          (glColor4f .1215 .9960 .1215 0.05)
          (let ((y 65))
            (glBegin GL_QUADS)
            (glVertex2i 0 0)
            (glVertex2i screen-max-x 0)
            (glVertex2i screen-max-x y)
            (glVertex2i 0 y)
            (glEnd))

          (glBlendFunc GL_ONE GL_ZERO)
         
          ;; Draw background stuff
          (render level)

          (let* ((now (time->seconds (current-time)))
                 (this-fps (/ 1 (- now last-render-time))))
            (set! fps-set (cons this-fps fps-set))
            (set! draw-set (cons (- now render-init-time) draw-set))
            (if (not (= last-render-time 0))
                (FPS this-fps))
            (set! last-render-time now))

          ;; Accumulate last GC time
          (##gc)
          (set! GC-dt-set (cons (f64vector-ref (##process-statistics) 14)
                                GC-dt-set))

          ;;draw frame-rate just over the green line
          (if display-fps?
              (render-string
               0 11 
               (with-output-to-string "" (lambda () (show "FPS: " (FPS))))
               'white))

          (glFlush)
          (SDL::GL::SwapBuffers)))))))



  
;;;;;;;;;;;;;;;;;;;;;;; Viewport and projection ;;;;;;;;;;;;;;;;;;;;;;;

(define (reshape w h)
  (let* ((zoom-x (/ w screen-max-x))
         (zoom-y (/ h screen-max-y))
         (factor (exact->inexact (ceiling (max zoom-x zoom-y)))))
    (glViewport 0 0 w h)
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (gluOrtho2D 0.                            ;;left clip
                (exact->inexact (/ w zoom-x)) ;;right clip
                0.                            ;;bottom clip
                (exact->inexact (/ h zoom-y)));;top
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)))

;;;;;;;;;;;;;;;;;;;;;;; User I/O ;;;;;;;;;;;;;;;;;;;;;;;

(define (register-user-action action)
  (thread-send simulation-thread action))

;; (c-define (keyboard key x y) (unsigned-char int int) void "keyboard" ""
;;  (case key
;;    ((#\f #\F) (set! display-fps? (not display-fps?)))
;;    ;; On Escape, Ctl-q, Ctl-c, Ctl-w, q -> terminate the program
;;    ((#\x1b #\x11 #\x03 #\x17 #\q) (quit))
;;    (else (register-user-action key))))

;; (c-define (special-keyboard key x y)
;;           (unsigned-char int int) void "special_keyboard" ""
;;  (case key
;; ;;    ((#\e) (pp 'up))
;; ;;    ((#\g) (pp 'down))
;;    ((#\f) (register-user-action 'right-arrow))
;;    ((#\d) (register-user-action 'left-arrow))))
   
;; ;;    (else (show "received special keyboard input: " key
;; ;;                ". Mouse is @ ("x","y")\n"))))

(define (->unhandled  evt-struct)
  'todo)
;; (define (->activate   evt-struct)
;;   (make-event 'focus-change
;;               (cons (if (SDL::active-gain? evt-struct) 'gained 'lost)
;;                     (let ( [state (SDL::active-state  evt-struct)] )
;;                       (cond
;;                        ((= state SDL::app-mouse-focus) 'mouse-focus)
;;                        ((= state SDL::app-input-focus) 'keyboard-focus)
;;                        ((= state SDL::app-active)      'application-focus)
;;                        (else ;; multiple states
;;                         (let ( [states '()] )
;;                           (when (bitwise-and state SDL::app-mouse-focus)
;;                             (set! states (cons 'mouse-focus states)))
;;                           (when (bitwise-and state SDL::app-input-focus)
;;                             (set! states (cons 'keyboard-focus states)))
;;                           (when (bitwise-and state SDL::app-active)
;;                             (set! states (cons 'application-focus states)))
;;                           states))))
;;               )
;; ) )
(define (->key-down evt-struct)
  (let ((key-enum  (SDL::key-enum      evt-struct))
        (modifiers (SDL::key-modifiers evt-struct))
        (unicode   (SDL::key-unicode   evt-struct)))
    (case key-enum
      [(key-left-arrow)   (key-down-table-add!
                           'left
                           (lambda () (register-user-action 'left)))]
      [(key-right-arrow)  (key-down-table-add!
                           'right
                           (lambda () (register-user-action 'right)))]
      [(key-space)        (register-user-action 'space)]
      [(key-r)            (register-user-action 'r)]
      [(key-p)            (register-user-action 'p)]
      [(key-1)            (register-user-action '1)]
      [(key-2)            (register-user-action '2)]
      [(key-c)            (register-user-action 'c)]
      [(key-d)            (register-user-action 'd)]
      [(key-f)            (set! display-fps? (not display-fps?))]
      [(key-q)            (request-exit)])
    ))
(define (->key-up   evt-struct)
  (let ((key-enum  (SDL::key-enum      evt-struct))
        (modifiers (SDL::key-modifiers evt-struct)))
    (case key-enum
      [(key-left-arrow)   (key-down-table-reset-key 'left)]
      [(key-right-arrow)  (key-down-table-reset-key 'right)])
    ))
;; (define (->mouse-motion evt-struct)
;;   (make-event 'mouse-motion
;;               (cons 'x     (SDL::move-x     evt-struct))
;;               (cons 'y     (SDL::move-y     evt-struct))
;;               (cons 'rel-x (SDL::move-rel-x evt-struct))
;;               (cons 'rel-y (SDL::move-rel-y evt-struct))
;; ) )
;; (define (->mouse-button-down evt-struct)
;;   (make-event 'mouse-button-down
;;               (cons 'button (SDL::mouse-button evt-struct))
;;               (cons 'x      (SDL::mouse-x      evt-struct))
;;               (cons 'y      (SDL::mouse-y      evt-struct))
;; ) )
;; (define (->mouse-button-up   evt-struct)
;;   (make-event 'mouse-button-up
;;               (cons 'button (SDL::mouse-button evt-struct))
;;               (cons 'x      (SDL::mouse-x      evt-struct))
;;               (cons 'y      (SDL::mouse-y      evt-struct))
;; ) )

(define (->quit evt-struct)
  (request-exit))

(define (->video-resize evt-struct)
  (let ((w (SDL::resize-w evt-struct))
        (h (SDL::resize-h evt-struct)))
    (SDL::set-video-mode w h 32 (bitwise-ior SDL::opengl SDL::resizable))
    (init-GL w h)
    ))

(define managage-sdl-event
  ;; SDL event structure bits -> Scheme object
  (let ( [xforms (make-vector (+ 1 SDL::num-events) ->unhandled)] )
;;     (vector-set! xforms SDL::active-event         ->activate)
    (vector-set! xforms SDL::key-down             ->key-down)
    (vector-set! xforms SDL::key-up               ->key-up)
;;     (vector-set! xforms SDL::mouse-motion         ->mouse-motion)
;;     (vector-set! xforms SDL::mouse-button-down    ->mouse-button-down)
;;     (vector-set! xforms SDL::mouse-button-up      ->mouse-button-up)
    (vector-set! xforms SDL::quit                 ->quit)
    (vector-set! xforms SDL::video-resize         ->video-resize)
    
    (lambda (sdl-event-struct)
      (let ( (event-type (SDL::raw-event-type sdl-event-struct)) )
        (if (<= 0 event-type SDL::num-events)
            ((vector-ref xforms event-type) sdl-event-struct)
            (->unhandled sdl-event-struct))))
) )

(define key-down-table (make-table))
(define (key-down-table-add! key action)
  (table-set! key-down-table key action))
(define (key-down-table-reset-key key)
  (table-set! key-down-table key))
(define (key-down-table-actions)
  (map cdr (table->list key-down-table)))

(define (event-thread-thunk)
  (let ((evt-struct (SDL::malloc-event-struct)))
    (let poll-loop ((event-or-false (SDL::poll-event evt-struct)))
      (if event-or-false
          (begin
            (managage-sdl-event evt-struct)
            (poll-loop (SDL::poll-event evt-struct)))
          (begin
            (for-each (lambda (x) (x)) (key-down-table-actions))
            (thread-sleep! 0.01)
            (poll-loop (SDL::poll-event evt-struct)))))))



;;;;;;;;;;;;;;;;;;;;;;; Gui Initialization ;;;;;;;;;;;;;;;;;;;;;;;

(c-declare "int argc = 0;")
(define (init-GL w h)
  (glPointSize 1.)
  (glDisable GL_POINT_SMOOTH)

  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (glShadeModel GL_FLAT)

  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE)

  (initialize-textures!)

  (reshape w h)
  )

(define (start-threads!)
  (set! simulation-thread (make-thread (game-loop (current-thread))))
  (set! event-thread      (make-thread event-thread-thunk))
  (thread-start! simulation-thread)
  (thread-start! event-thread))

(define (redraw-loop)
  (SDL::set-window-caption "Space Invaders" "Space Invaders")
  (SDL::set-window-icon (SDL::load-bmp-file "sprites/medium1.bmp") #f)
  (let ((audio-rate 22050)
        (audio-format SDL::Mix::AUDIO_S16SYS)
        (audio-channels 2)
        (audio-buffers 4096))
    (SDL::Mix::open-audio audio-rate audio-format
                          audio-channels audio-buffers))
;;   (set! test-chunk (SDL::Mix::load-wav "sounds/25753_FreqMan_raygun01.wav"))
;;   (set! test-chunk2 (SDL::Mix::load-wav
;;                      "sounds/32562_FreqMan_chronosphere_ish.wav"))
  (set! star-wars-chunk (SDL::Mix::load-wav "sounds/starwars.wav"))
  
  (let ( (screen (SDL::set-video-mode
                    screen-max-x screen-max-y 32
                    (bitwise-ior  SDL::opengl SDL::resizable))) )
      (if screen
          (call/cc
           (lambda (k)
             (set! sdl-screen screen)
             (set! return
                   (lambda (ret-val)
                     (thread-terminate! event-thread)
                     (thread-terminate! simulation-thread)
                     (k ret-val)))
             (init-GL screen-max-x screen-max-y)
             (start-threads!)
             (let loop ((msg (thread-receive)))
               (if exit-requested? (quit))
               (case (car msg)
                 [(redraw)
                  (let ((level (cadr msg)))
                    (render-scene screen level))]
                 [(play-sfx)
                  (let ((sfx (cadr msg)))
                    (play-sfx sfx))]
                 )
               (loop (thread-receive)))))
          (display "Could not set SDL screen")))
  )

(define usage-message "USAGE: ./space-invaders\n")

(define (request-exit)
  (set! exit-requested? #t))
(define exit-requested? #f)
(define return #f)
(define (quit) (return 0))

;; Main function which only sets up and starts the game threads
(define (main)
  (define (start)
    (SDL::within-sdl-lifetime SDL::init-everything
                              redraw-loop))

  ;; Start a debug/developpement repl in a seperate thread
  ;;   (thread-start! (make-thread (lambda () (##repl))))
  (cond
   ((eqv? (length (command-line)) 1) (start))
   (else
    (display usage-message))))


(include "statprof.scm")
(profile-start!)
(main)
(profile-stop!)

(write-profile-report "profiling")


;; Creation of histogram data
(let ((histo-size 30))
  (set! fps-set   (drop-right fps-set   1))
  (set! GC-dt-set (drop-right GC-dt-set 1))
  (set! draw-set  (drop-right draw-set  1))
  (with-output-to-file "histo-fps.csv"
    (lambda () (generate-histogram
                "fps"
                histo-size
                `(("Framerate" . ,fps-set)))))
  (with-output-to-file "histo-render.csv"
    (lambda () (generate-histogram
                "rendering"
                histo-size
                `(("time to render all of a frame" .
                   ,(map (lambda (x) (/ 1 x)) fps-set))
                  ("time to draw a frame" . ,draw-set))
                0.001)))
  (with-output-to-file "histo-gc.csv"
    (lambda () (generate-histogram
                "gc"
                histo-size
                `(("Garbage collection time" . ,GC-dt-set))
                0.001))))

(if trace-coroutines?
    (output-corout-tracing-results))

(define-module (update)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (scheme write)
  #:use-module (math rect)
  #:use-module (math vector)
  #:use-module (stdlib list)
  #:use-module (gameboard)
  #:use-module (gamestate)
  #:export (start-board-evaluation!
            progress-evaluation!))

(define floating-puyos '())

(define (move hitbox velocity)
  (set-rect-x! hitbox (+ (rect-x hitbox) (vec2-x velocity)))
  (set-rect-y! hitbox (+ (rect-y hitbox) (vec2-y velocity))))


(define (find-floating-puyos)
  (let ((board-indices (range 0 board-vector-length)))
    (filter floating-puyo? board-indices)))

(define (start-board-evaluation!)
  (add-new-board-state!)
  (set! floating-puyos (find-floating-puyos))
  (switch-game-mode! 'evaluating))


(define (progress-evaluation!)
  (display (length floating-puyos))
  (newline)
  (flush-output-port)
  (switch-game-mode! 'moving))

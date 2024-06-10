(define-module (update)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (scheme write)
  #:use-module (math rect)
  #:use-module (math vector)
  #:use-module (gamestate)
  #:export (start-board-evaluation!
            progress-evaluation!))

(define evaluation-count 0)

(define (move hitbox velocity)
  (set-rect-x! hitbox (+ (rect-x hitbox) (vec2-x velocity)))
  (set-rect-y! hitbox (+ (rect-y hitbox) (vec2-y velocity))))


(define (progress-evaluation!)
  (set! evaluation-count (- evaluation-count 1))
  (display evaluation-count)
  (newline)
  (flush-output-port)
  (if (= evaluation-count 0)
      (begin
        (add-new-board-state!)
        (switch-game-mode! 'moving))))

(define (start-board-evaluation!)
  (set! evaluation-count 100)
  (switch-game-mode! 'evaluating))

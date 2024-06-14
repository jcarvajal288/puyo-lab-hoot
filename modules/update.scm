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

(define (move hitbox velocity)
  (set-rect-x! hitbox (+ (rect-x hitbox) (vec2-x velocity)))
  (set-rect-y! hitbox (+ (rect-y hitbox) (vec2-y velocity))))


(define (start-board-evaluation!)
  (add-new-board-state!)
  (set-falling-puyos!)
  (switch-game-mode! 'evaluating))


(define (progress-evaluation!)
  (switch-game-mode! 'moving))

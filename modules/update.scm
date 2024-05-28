(define-module (update)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (scheme write)
  #:use-module (hoot match)
  #:use-module (stdlib list)
  #:use-module (input)
  #:use-module (math)
  #:use-module (math rect)
  #:use-module (math vector)
  #:use-module (dom window)
  #:use-module (ball)
  #:export (update-all))


(define (move hitbox velocity)
  (set-rect-x! hitbox (+ (rect-x hitbox) (vec2-x velocity)))
  (set-rect-y! hitbox (+ (rect-y hitbox) (vec2-y velocity))))


(define (update-all ball)
  (move (ball-hitbox ball) (ball-velocity ball)))

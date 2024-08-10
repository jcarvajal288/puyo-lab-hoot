(define-module (stats)
  #:pure
  #:use-module (scheme base)
  #:use-module (dom canvas)
  #:use-module (stdlib debug)
  #:export (draw-stats set-last-chain!))

(define stats-x 400.0)
(define stats-y 100.0)

(define last-chain 0)

(define (set-last-chain! new-chain)
  (set! last-chain new-chain))

(define (draw-stats context)
  (set-fill-color! context "#FFFFFF")
  (set-font! context "24px Eraser")
  (set-text-align! context "left")
  (fill-text context "LAST CHAIN:" stats-x stats-y)
  (fill-text context (number->string last-chain) (+ stats-x 175) stats-y))

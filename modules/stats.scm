(define-module (stats)
  #:pure
  #:use-module (scheme base)
  #:use-module (dom canvas)
  #:use-module (stdlib debug)
  #:export (draw-stats
            increment-chain-counter!
            update-chain-stats!))

(define stats-x 400.0)
(define stats-y 100.0)

(define chain-counter 0)
(define last-chain 0)
(define max-chain 0)

(define (increment-chain-counter!)
  (set! chain-counter (+ chain-counter 1)))

(define (reset-chain-counter!)
  (set! chain-counter 0))

(define (update-chain-stats!)
  (if (> chain-counter 0)
      (set! last-chain chain-counter))
  (if (> chain-counter max-chain)
      (set! max-chain chain-counter))
  (reset-chain-counter!))

(define (draw-stats context)
  (set-fill-color! context "#FFFFFF")
  (set-font! context "24px Eraser")
  (set-text-align! context "left")
  (fill-text context "LAST CHAIN:" stats-x stats-y)
  (fill-text context (number->string last-chain) (+ stats-x 175) stats-y)
  (fill-text context "MAX CHAIN:" stats-x (+ stats-y 30))
  (fill-text context (number->string max-chain) (+ stats-x 175) (+ stats-y 30)))

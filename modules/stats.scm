(define-module (stats)
  #:pure
  #:use-module (scheme base)
  #:use-module (dom canvas)
  #:use-module (stdlib debug)
  #:export (draw-stats))

(define stats-x 400.0)
(define stats-y 100.0)

(define (draw-stats context)
  (set-fill-color! context "#FFFFFF")
  (set-font! context "bold 24px monospace")
  (set-text-align! context "left")
  (fill-text context "MAX CHAIN:" stats-x stats-y))

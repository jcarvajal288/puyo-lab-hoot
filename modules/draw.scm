(define-module (draw)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (math)
  #:use-module (images)
  #:use-module (screen)
  #:use-module (gameboard)
  #:export (draw-frame))

(define (draw-background context)
  (draw-image context image:background
              0 0 screen:width screen:height
              0 0 screen:width screen:height))


(define (draw-frame context prev-time)
  (draw-background context)
  (draw-gameboard context))

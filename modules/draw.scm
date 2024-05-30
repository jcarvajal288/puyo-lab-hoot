(define-module (draw)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (math)
  #:use-module (images)
  #:use-module (screen)
  #:use-module (ball)
  #:export (draw-frame))

(define (draw-background context)
  (draw-image context image:background
              0 0 screen:width screen:height
              0 0 screen:width screen:height))

(define (draw-play-border context)
  (draw-image context image:play-border
              0 0 play-border-width play-border-height
              100 68 play-border-width play-border-height))


(define (draw-frame context ball prev-time)
  (draw-background context)
  (draw-play-border context))

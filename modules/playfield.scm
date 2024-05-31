(define-module (playfield)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (math)
  #:use-module (images)
  #:export (build-playfield draw-play-border))

(define play-border-x 100.0)
(define play-border-y 68.0)
(define play-border-width 198.0)
(define play-border-height 424.0)

(define-record-type <playfield>
  (make-playfield origin-x origin-y)
  playfield?
  (origin-x playfield-origin-x)
  (origin-y playfield-origin-y))

(define (build-playfield)
  (make-playfield play-border-x play-border-y))

(define (draw-play-border playfield context)
  (draw-image context image:play-border
              0 0 play-border-width play-border-height
              play-border-x play-border-y play-border-width play-border-height))

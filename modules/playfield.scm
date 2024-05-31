(define-module (playfield)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (math)
  #:use-module (images)
  #:use-module (puyo)
  #:export (build-playfield draw-playfield))

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

(define (draw-play-border context playfield)
  (draw-image context image:play-border
              0 0 play-border-width play-border-height
              play-border-x play-border-y play-border-width play-border-height))

(define (draw-playfield context playfield)
  (draw-play-border context playfield)
  (draw-puyo context (build-puyo 'red 200.0 200.0)))

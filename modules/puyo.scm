(define-module (puyo)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (math vector)
  #:use-module (math rect)
  #:use-module (dom canvas)
  #:use-module (images)
  #:export (puyo-color
            puyo-hitbox
            build-puyo
            draw-puyo))

(define puyo-size 32)


(define-record-type <puyo>
  (make-puyo color hitbox)
  puyo?
  (color puyo-color)
  (hitbox puyo-hitbox))

(define (build-puyo color x y)
  (let* ((hitbox (make-rect x y puyo-size puyo-size)))
    (make-puyo color hitbox)))

(define (draw-puyo context puyo x y)
  (draw-image context image:puyo-sheet
              3 4 puyo-size puyo-size
              (* x puyo-size) (* y puyo-size) puyo-size puyo-size))

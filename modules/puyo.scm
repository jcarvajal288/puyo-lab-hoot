(define-module (puyo)
  #:pure
  #:use-module (scheme base)
  #:use-module (math vector)
  #:use-module (math rect)
  #:use-module (dom canvas)
  #:use-module (images)
  #:export (puyo-color
            puyo-hitbox
            build-puyo
            draw-puyo))

(define puyo-size 32.0)


(define-record-type <puyo>
  (make-puyo color hitbox)
  puyo?
  (color puyo-color)
  (hitbox puyo-hitbox))

(define (build-puyo color x y)
  (let* ((hitbox (make-rect x y puyo-size puyo-size)))
    (make-puyo color hitbox)))

(define (draw-puyo context puyo)
  (let* ((hitbox (puyo-hitbox puyo))
         (x (rect-x hitbox))
         (y (rect-y hitbox))
         (w (rect-width hitbox))
         (h (rect-height hitbox)))
    (draw-image context image:puyo-sheet
                3 4 w h
                x y w h)))

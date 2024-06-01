(define-module (puyo)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (math vector)
  #:use-module (math rect)
  #:use-module (dom canvas)
  #:use-module (images)
  #:export (puyo-size
            puyo-color
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

(define (draw-puyo context puyo-color dx dy)
  (let* ((sprite-sheet-coords (get-puyo-sprite-sheet-coords puyo-color))
         (sx (car sprite-sheet-coords))
         (sy (cdr sprite-sheet-coords)))
    (draw-image context image:puyo-sheet
                sx sy puyo-size puyo-size
                dx dy puyo-size puyo-size)))

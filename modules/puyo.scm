(define-module (puyo)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (hoot match)
  #:use-module (math)
  #:use-module (math vector)
  #:use-module (math rect)
  #:use-module (dom canvas)
  #:use-module (images)
  #:export (puyo-size
            puyo-color
            puyo-hitbox
            build-puyo
            draw-puyo
            active-pair
            puyo-pair-board-index1
            puyo-pair-board-index2
            set-puyo-pair-board-index1!
            set-puyo-pair-board-index2!
            puyo-pair-color1
            puyo-pair-color2
            new-puyo-pair!
            random-puyo-color))

(define puyo-size 32)

(define-record-type <puyo>
  (make-puyo color hitbox)
  puyo?
  (color puyo-color)
  (hitbox puyo-hitbox))

(define-record-type <puyo-pair>
  (make-puyo-pair board-index1 board-index2 color1 color2)
  puyo-pair?
  (board-index1 puyo-pair-board-index1 set-puyo-pair-board-index1!)
  (board-index2 puyo-pair-board-index2 set-puyo-pair-board-index2!)
  (color1 puyo-pair-color1)
  (color2 puyo-pair-color2))

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

(define (new-puyo-pair)
  (make-puyo-pair 1 2 (random-puyo-color) (random-puyo-color)))

(define (random-puyo-color)
  (let ((color (random-float 0 6)))
    (cond
      ((< color 1) 'red)
      ((< color 2) 'green)
      ((< color 3) 'yellow)
      ((< color 4) 'blue)
      (else 'purple))))

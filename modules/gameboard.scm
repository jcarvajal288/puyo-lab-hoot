(define-module (gameboard)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (math)
  #:use-module (images)
  #:use-module (puyo)
  #:export (build-gameboard
            draw-gameboard
            add-puyo-at))

(define play-border-x 100)
(define play-border-y 68)
(define play-border-width 212.0)
(define play-border-height 432.0)
(define board-grid-width 6)
(define board-grid-height 13)
(define grid-origin-x (+ play-border-x 9))
(define grid-origin-y (+ play-border-y 8))
(define board-vector-length (* board-grid-width board-grid-height))

(define-record-type <gameboard>
  (make-gameboard origin-x origin-y grid)
  gameboard?
  (origin-x gameboard-origin-x)
  (origin-y gameboard-origin-y)
  (grid gameboard-grid))

(define (build-gameboard)
  (let ((grid (make-vector board-vector-length #f)))
  (make-gameboard play-border-x play-border-y grid)))


(define (grid-index x y)
  (+ x (* y board-grid-width)))

(define (get-puyo-at gameboard x y)
  (let ((target-index (grid-index x y)))
    (vector-ref (gameboard-grid gameboard) target-index)))

(define (add-puyo-at gameboard puyo-color x y)
  (let ((target-index (grid-index x y)))
    (vector-set! (gameboard-grid gameboard) target-index puyo-color)))


(define (draw-play-border context gameboard)
  (draw-image context image:play-border
              0 0 play-border-width play-border-height
              play-border-x play-border-y play-border-width play-border-height))

(define (draw-grid context grid)
  (define (draw-grid-func context grid index)
    (let ((puyo-color (vector-ref grid index)))
      (if (not (eqv? puyo-color #f))
          (let* ((grid-x (floor-remainder index board-grid-width))
                 (grid-y (floor-quotient index board-grid-width))
                 (screen-x (+ (* grid-x puyo-size) grid-origin-x))
                 (screen-y (+ (* grid-y puyo-size) grid-origin-y)))
          (draw-puyo context puyo-color screen-x screen-y)))
    (if (< index (- board-vector-length 1))
        (draw-grid-func context grid (+ index 1)))))
  (draw-grid-func context grid 0))


(define (draw-gameboard context gameboard)
  (draw-play-border context gameboard)
  (draw-grid context (gameboard-grid gameboard)))



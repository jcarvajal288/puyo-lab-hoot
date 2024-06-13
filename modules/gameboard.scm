(define-module (gameboard)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (hoot match)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (gamestate)
  #:use-module (math)
  #:use-module (images)
  #:use-module (puyo)
  #:export (build-gameboard
            board-vector-length
            board-grid-width
            set-gameboard!
            draw-gameboard
            add-puyo-at
            create-puyo-sprite-at
            remove-puyo-at
            on-same-level?
            space-empty?
            floating-puyo?))

(define play-border-x 100)
(define play-border-y 68)
(define play-border-width 212.0)
(define play-border-height 432.0)
(define board-grid-width 6)
(define board-grid-height 13)
(define grid-origin-x (+ play-border-x 9))
(define grid-origin-y (+ play-border-y 8))
(define board-vector-length (* board-grid-width board-grid-height))

(define (grid-index x y)
  (+ x (* y board-grid-width)))

(define (grid-index-to-screen-coords index)
  (let* ((grid-x (floor-remainder index board-grid-width))
         (grid-y (floor-quotient index board-grid-width))
         (screen-x (+ (* grid-x puyo-size) grid-origin-x))
         (screen-y (+ (* grid-y puyo-size) grid-origin-y)))
    (cons screen-x screen-y)))

(define (get-puyo-at i)
  (vector-ref (get-game-grid) i))

;; deprecated; kept around for testing
(define (add-puyo-at puyo-color x y)
  (let ((target-index (grid-index x y)))
    (vector-set! (get-game-grid) target-index puyo-color)))

(define (remove-puyo-at index)
  (vector-set! (get-game-grid) index 'empty))


(define (draw-play-border context)
  (draw-image context image:play-border
              0 0 play-border-width play-border-height
              play-border-x play-border-y play-border-width play-border-height))

(define (draw-puyo-at-board-index context index puyo-color)
  (let ((screen-coords (grid-index-to-screen-coords index)))
    (draw-puyo context puyo-color (car screen-coords) (cdr screen-coords))))

(define (draw-grid context)
  (define (draw-grid-func context grid index)
    (let ((puyo-color (vector-ref grid index)))
      (if (not (eqv? puyo-color 'empty))
        (draw-puyo-at-board-index context index puyo-color))
    (if (< index (- board-vector-length 1))
        (draw-grid-func context grid (+ index 1)))))
  (draw-grid-func context (get-game-grid) 0))

(define (draw-active-pair context)
  (draw-puyo-at-board-index
    context
    active-pair-index1
    (car (get-active-pair)))
  (draw-puyo-at-board-index
    context
    active-pair-index2
    (cdr (get-active-pair))))


(define (draw-gameboard context)
  (draw-play-border context)
  (draw-grid context)
  (draw-active-pair context))

(define (space-empty? index)
  (eqv? (vector-ref (get-game-grid) index) 'empty))

(define (on-same-level? s d)
  (= (floor/ s board-grid-width) (floor/ d board-grid-width)))

(define (floating-puyo? index)
  (let ((space-below (+ index board-grid-width)))
    (and (not (eqv? (get-puyo-at index) 'empty))
         (< space-below board-vector-length)
         (space-empty? space-below))))

(define (create-puyo-sprite-at index)
  (let ((screen-coords (grid-index-to-screen-coords index))
        (color (vector-ref (get-game-grid) index)))
    (build-puyo color (car screen-coords) (cdr screen-coords))))

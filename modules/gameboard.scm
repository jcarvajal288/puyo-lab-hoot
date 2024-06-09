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
            set-gameboard!
            draw-gameboard
            add-puyo-at
            move-active-pair!))

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

(define (get-puyo-at x y)
  (let ((target-index (grid-index x y)))
    (vector-ref (get-game-grid) target-index)))

;; deprecated; kept around for testing
(define (add-puyo-at puyo-color x y)
  (let ((target-index (grid-index x y)))
    (vector-set! (get-game-grid) target-index puyo-color)))


(define (draw-play-border context)
  (draw-image context image:play-border
              0 0 play-border-width play-border-height
              play-border-x play-border-y play-border-width play-border-height))

(define (draw-puyo-at-board-index context index puyo-color)
  (let* ((grid-x (floor-remainder index board-grid-width))
          (grid-y (floor-quotient index board-grid-width))
          (screen-x (+ (* grid-x puyo-size) grid-origin-x))
          (screen-y (+ (* grid-y puyo-size) grid-origin-y)))
    (draw-puyo context puyo-color screen-x screen-y)))

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

(define (move-active-pair! direction)
  (let* ((i1 active-pair-index1)
         (i2 active-pair-index2)
         (move-result (match direction
                        ('left (move-active-pair-left i1 i2))
                        ('right (move-active-pair-right i1 i2))
                        ('up (move-active-pair-up i1 i2))
                        ('down (move-active-pair-down i1 i2))
                        ('counter-clockwise (rotate-active-pair-counter-clockwise i1 i2))
                        ('clockwise (rotate-active-pair-clockwise i1 i2)))))
    (if (eqv? move-result 'stick-pair)
        (add-new-board-state)
        (set-active-pair-location! move-result))))

(define (move-active-pair-left s1 s2)
  (let ((d1 (- s1 1))
        (d2 (- s2 1)))
    (if (and (on-same-level? s1 d1)
             (on-same-level? s2 d2)
             (space-empty? d1)
             (space-empty? d2))
        (cons d1 d2)
        (cons s1 s2))))

(define (move-active-pair-right s1 s2)
  (let ((d1 (+ s1 1))
        (d2 (+ s2 1)))
    (if (and (on-same-level? s1 d1)
             (on-same-level? s2 d2)
             (space-empty? d1)
             (space-empty? d2))
        (cons d1 d2)
        (cons s1 s2))))

(define (move-active-pair-up s1 s2)
  (let ((d1 (- s1 board-grid-width))
        (d2 (- s2 board-grid-width)))
    (if (and (>= d1 0)
             (>= d2 0)
             (space-empty? d1)
             (space-empty? d2))
        (cons d1 d2)
        (cons s1 s2))))

(define (move-active-pair-down s1 s2)
  (let ((d1 (+ s1 board-grid-width))
        (d2 (+ s2 board-grid-width)))
    (if (and (< d2 board-vector-length)
             (< d2 board-vector-length)
             (space-empty? d1)
             (space-empty? d2))
        (cons d1 d2)
        'stick-pair)))

(define (rotate-active-pair-counter-clockwise s1 s2)
  (define (rotate-right-to-up)
    (- s1 board-grid-width))
  (define (rotate-left-to-down)
    (+ s1 board-grid-width))
  (define (rotate-down-to-right)
    (+ s1 1))
  (define (rotate-up-to-left)
    (- s1 1))
  (let ((d2 (if (on-same-level? s1 s2)
                (if (< s1 s2)
                    (rotate-right-to-up)
                    (rotate-left-to-down))
                (if (< s1 s2)
                    (rotate-down-to-right)
                    (rotate-up-to-left)))))
    (cons s1 d2)))

(define (rotate-active-pair-clockwise s1 s2)
  (define (rotate-right-to-down)
    (+ s1 board-grid-width))
  (define (rotate-left-to-up)
    (- s1 board-grid-width))
  (define (rotate-down-to-left)
    (- s1 1))
  (define (rotate-up-to-right)
    (+ s1 1))
  (let ((d2 (if (on-same-level? s1 s2)
                (if (< s1 s2)
                    (rotate-right-to-down)
                    (rotate-left-to-up))
                (if (< s1 s2)
                    (rotate-down-to-left)
                    (rotate-up-to-right)))))
    (cons s1 d2)))



(define (space-empty? index)
  (eqv? (vector-ref (get-game-grid) index) 'empty))

(define (on-same-level? s d)
  (= (floor/ s board-grid-width) (floor/ d board-grid-width)))


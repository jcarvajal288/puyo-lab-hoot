(define-module (gameboard)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (gamestate)
  #:use-module (math)
  #:use-module (math rect)
  #:use-module (images)
  #:use-module (puyo)
  #:use-module (stdlib list)
  #:use-module (stdlib debug)
  #:export (board-vector-length
            board-grid-width
            screen-coords-to-grid-index
            draw-gameboard
            falling-puyos
            find-falling-puyos!
            remove-falling-puyo!
            remove-puyo-groups!
            get-puyo-at
            add-puyo-at!
            create-puyo-sprite-at
            on-same-level?
            empty-space?
            space-empty?))

(define play-border-x 100)
(define play-border-y 68)
(define play-border-width 212.0)
(define play-border-height 432.0)
(define board-grid-width 6)
(define board-grid-height 13)
(define grid-origin-x (+ play-border-x 9))
(define grid-origin-y (+ play-border-y 8))
(define board-vector-length (* board-grid-width board-grid-height))
(define falling-puyos '())


(define (grid-index-to-screen-coords index)
  (let* ((grid-x (floor-remainder index board-grid-width))
         (grid-y (floor-quotient index board-grid-width))
         (screen-x (+ (* grid-x puyo-size) grid-origin-x))
         (screen-y (+ (* grid-y puyo-size) grid-origin-y)))
    (cons screen-x screen-y)))

(define (screen-coords-to-grid-index x y)
  (let ((grid-x (floor/ (- x grid-origin-x) puyo-size))
        (grid-y (floor/ (- y grid-origin-y) puyo-size)))
    (exact (+ grid-x (* grid-y board-grid-width)))))

(define (get-puyo-at i)
  (vector-ref (get-game-grid) i))

(define (add-puyo-at! puyo-color index)
  (vector-set! (get-game-grid) index puyo-color))

(define (remove-puyo-at! index)
  (vector-set! (get-game-grid) index 'empty))

(define (remove-puyo-groups! groups)
  (for-each (lambda (group)
              (for-each remove-puyo-at! group))
            groups))

(define (find-falling-puyos!)
  (define (reducer temp-falling-puyos)
    (let* ((board-indices (range 0 board-vector-length))
           (falling-puyo-indices (filter floating-puyo? board-indices)))
      (if (empty? falling-puyo-indices)
          temp-falling-puyos
          (begin
            (set! falling-puyos (append falling-puyos (map create-puyo-sprite-at falling-puyo-indices)))
            (for-each remove-puyo-at! falling-puyo-indices)
            (reducer (append falling-puyo-indices temp-falling-puyos))))))
  (reducer '()))

(define (remove-falling-puyo! this-puyo)
  (define (not-this-puyo that-puyo)
    (let ((this-hitbox (puyo-hitbox this-puyo))
          (that-hitbox (puyo-hitbox that-puyo)))
      (not (and (= (rect-x this-hitbox) (rect-x that-hitbox))
                (= (rect-y this-hitbox) (rect-y that-hitbox))))))
  (set! falling-puyos (filter not-this-puyo falling-puyos)))

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

(define (draw-falling-puyos context)
  (define (draw-func puyo)
    (let ((hitbox (puyo-hitbox puyo)))
      (draw-puyo context (puyo-color puyo) (rect-x hitbox) (rect-y hitbox))))
  (for-each draw-func falling-puyos))


(define (draw-gameboard context)
  (draw-play-border context)
  (draw-grid context)
  (draw-falling-puyos context)
  (if (eqv? current-game-mode 'moving)
      (draw-active-pair context)))

;; FIX space-empty vs empty-space
(define (space-empty? index)
  (eqv? (vector-ref (get-game-grid) index) 'empty))

(define (on-same-level? s d)
  (= (floor/ s board-grid-width) (floor/ d board-grid-width)))

(define (empty-space? index)
  (and (< index board-vector-length)
       (space-empty? index)))

(define (floating-puyo? index)
  (let ((space-below (+ index board-grid-width)))
    (and (not (eqv? (get-puyo-at index) 'empty))
         (empty-space? space-below))))

(define (create-puyo-sprite-at index)
  (let ((screen-coords (grid-index-to-screen-coords index))
        (color (vector-ref (get-game-grid) index)))
    (build-puyo color (car screen-coords) (cdr screen-coords))))

(define-module (gamestate)
  #:pure
  #:use-module (scheme base)
  #:use-module (puyo)
  #:export (initialize-game-state
            revert-board-state!
            current-game-mode
            switch-game-mode!
            get-game-grid
            active-pair-index1
            active-pair-index2
            get-active-pair
            set-active-pair-location!
            add-new-board-state!))

(define current-game-mode 'moving)
(define current-state 0)
(define grid-timeline #f)
(define pair-timeline #f)
(define active-pair-index1 0)
(define active-pair-index2 0)

(define (initialize-game-state grid-length)
  (set! current-state 0)
  (set! active-pair-index1 1)
  (set! active-pair-index2 2)
  (set! grid-timeline (make-vector 1 (make-vector grid-length 'empty)))
  (set! pair-timeline (make-vector 1 (cons (random-puyo-color) (random-puyo-color)))))

(define (revert-board-state!)
  (if (> current-state 0)
      (set! current-state (- current-state 1)))
  (set! active-pair-index1 1)
  (set! active-pair-index2 2))

(define (switch-game-mode! game-mode)
  (set! current-game-mode game-mode))

(define (get-game-grid)
  (vector-ref grid-timeline current-state))

(define (get-active-pair)
  (vector-ref pair-timeline current-state))

(define (set-active-pair-location! new-location)
  (set! active-pair-index1 (car new-location))
  (set! active-pair-index2 (cdr new-location)))

(define (new-active-pair!)
  (let ((new-pair (cons (random-puyo-color) (random-puyo-color))))
    (set! pair-timeline (vector-append pair-timeline (vector new-pair)))
    (set! active-pair-index1 1)
    (set! active-pair-index2 2)))

(define (update-board!)
  (let ((new-board (vector-copy (get-game-grid)))
        (previous-boards (vector-copy grid-timeline 0 (+ current-state 1)))
        (color1 (car (get-active-pair)))
        (color2 (cdr (get-active-pair))))
    (vector-set! new-board active-pair-index1 color1)
    (vector-set! new-board active-pair-index2 color2)
    (set! grid-timeline (vector-append previous-boards (vector new-board)))))

(define (add-new-board-state!)
  (update-board!)
  (new-active-pair!)
  (set! current-state (+ current-state 1)))

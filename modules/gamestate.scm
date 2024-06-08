(define-module (gamestate)
  #:pure
  #:use-module (scheme base)
  #:use-module (puyo)
  #:export (initialize-game-state
            get-game-grid
            active-pair-index1
            active-pair-index2
            get-active-pair
            set-active-pair-location!
            new-active-pair!))

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

(define (get-game-grid)
  (vector-ref grid-timeline current-state))

(define (get-active-pair)
  (vector-ref pair-timeline current-state))

(define (set-active-pair-location! new-location)
  (set! active-pair-index1 (car new-location))
  (set! active-pair-index2 (cdr new-location)))

(define (new-active-pair!)
  (vector-set! pair-timeline 0 (cons (random-puyo-color) (random-puyo-color)))
  (set! active-pair-index1 1)
  (set! active-pair-index2 2))

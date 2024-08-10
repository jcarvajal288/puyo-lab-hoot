(define-module (gamestate)
  #:pure
  #:use-module (scheme base)
  #:use-module (puyo)
  #:use-module (stdlib debug)
  #:export (initialize-game-state
            revert-board-state!
            current-game-mode
            get-game-grid
            active-pair-position1
            active-pair-position2
            get-active-pair
            set-active-pair-location!
            add-new-board-state!
            switch-mode-to-moving!
            switch-mode-to-evaluating!
            get-last-chain
            get-max-chain
            set-last-chain!
            set-max-chain!))

(define current-game-mode 'moving)
(define current-state 0)
(define grid-timeline #f)
(define pair-timeline #f)
(define stats-timeline #f)
(define active-pair-position1 0)
(define active-pair-position2 0)


(define (initialize-game-state grid-length)
  (set! current-state 0)
  (set! active-pair-position1 1)
  (set! active-pair-position2 2)
  (set! grid-timeline (make-vector 1 (make-vector grid-length 'empty)))
  (set! pair-timeline (make-vector 1 (cons (random-puyo-color) (random-puyo-color))))
  (set! stats-timeline (make-vector 1 (make-stat-moment 0 0))))

(define (revert-board-state!)
  (if (> current-state 0)
      (set! current-state (- current-state 1)))
  (set! active-pair-position1 1)
  (set! active-pair-position2 2))

(define (get-game-grid)
  (vector-ref grid-timeline current-state))

(define (get-active-pair)
  (vector-ref pair-timeline current-state))

(define (set-active-pair-location! new-location)
  (set! active-pair-position1 (car new-location))
  (set! active-pair-position2 (cdr new-location)))

(define (update-board!)
  (let ((new-board (vector-copy (get-game-grid)))
        (previous-boards (vector-copy grid-timeline 0 (+ current-state 1)))
        (color1 (car (get-active-pair)))
        (color2 (cdr (get-active-pair))))
    (vector-set! new-board active-pair-position1 color1)
    (vector-set! new-board active-pair-position2 color2)
    (set! grid-timeline (vector-append previous-boards (vector new-board)))))

(define (new-active-pair!)
  (let ((new-pair (cons (random-puyo-color) (random-puyo-color))))
    (set! pair-timeline (vector-append pair-timeline (vector new-pair)))
    (set! active-pair-position1 1)
    (set! active-pair-position2 2)))

(define (add-new-board-state!)
  (update-board!)
  (new-active-pair!)
  (new-stat-moment!)
  (set! current-state (+ current-state 1)))

(define (switch-mode-to-evaluating!)
  (set! current-game-mode 'evaluating))

(define (switch-mode-to-moving!)
  (set! current-game-mode 'moving))


;;;; STATS

(define-record-type <stat-moment>
  (make-stat-moment last-chain max-chain)
  stat-moment?
  (last-chain stat-moment-last-chain set-stat-moment-last-chain!)
  (max-chain stat-moment-max-chain set-stat-moment-max-chain!))

(define (new-stat-moment!)
  (let* ((current-stat-moment (vector-ref stats-timeline current-state))
         (new-stat-moment (make-stat-moment
                            (stat-moment-last-chain current-stat-moment)
                            (stat-moment-max-chain current-stat-moment)))
         (previous-stat-moments (vector-copy stats-timeline 0 (+ current-state 1))))
    (set! stats-timeline (vector-append previous-stat-moments (vector new-stat-moment)))))

(define (get-last-chain)
  (let ((current-stat-moment (vector-ref stats-timeline current-state)))
    (stat-moment-last-chain current-stat-moment)))

(define (set-last-chain! new-last-chain)
  (set-stat-moment-last-chain! (vector-ref stats-timeline current-state) new-last-chain))

(define (get-max-chain)
  (let ((current-stat-moment (vector-ref stats-timeline current-state)))
    (stat-moment-max-chain current-stat-moment)))

(define (set-max-chain! new-max-chain)
  (set-stat-moment-max-chain! (vector-ref stats-timeline current-state) new-max-chain))

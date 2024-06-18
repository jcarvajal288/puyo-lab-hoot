(define-module (update)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme inexact)
  #:use-module (scheme write)
  #:use-module (hoot match)
  #:use-module (math rect)
  #:use-module (math vector)
  #:use-module (stdlib list)
  #:use-module (gameboard)
  #:use-module (gamestate)
  #:use-module (puyo)
  #:export (move-active-pair!
            start-board-evaluation!
            progress-evaluation!))

(define puyo-falling-speed 10)


(define (left-neighbor i)
  (- i 1))

(define (right-neighbor i)
  (+ i 1))

(define (up-neighbor i)
  (- i board-grid-width))

(define (down-neighbor i)
  (+ i board-grid-width))


(define (move-active-pair! direction)
  (if (eqv? current-game-mode 'moving)
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
          (start-board-evaluation!)
          (set-active-pair-location! move-result)))))

(define (move-active-pair-left s1 s2)
  (let ((d1 (left-neighbor s1))
        (d2 (left-neighbor s2)))
    (if (and (on-same-level? s1 d1)
             (on-same-level? s2 d2)
             (space-empty? d1)
             (space-empty? d2))
        (cons d1 d2)
        (cons s1 s2))))

(define (move-active-pair-right s1 s2)
  (let ((d1 (right-neighbor s1))
        (d2 (right-neighbor s2)))
    (if (and (on-same-level? s1 d1)
             (on-same-level? s2 d2)
             (space-empty? d1)
             (space-empty? d2))
        (cons d1 d2)
        (cons s1 s2))))

(define (move-active-pair-up s1 s2)
  (let ((d1 (up-neighbor s1))
        (d2 (up-neighbor s2)))
    (if (and (>= d1 0)
             (>= d2 0)
             (space-empty? d1)
             (space-empty? d2))
        (cons d1 d2)
        (cons s1 s2))))

(define (move-active-pair-down s1 s2)
  (let ((d1 (down-neighbor s1))
        (d2 (down-neighbor s2)))
    (if (and (< d1 board-vector-length)
             (< d2 board-vector-length)
             (space-empty? d1)
             (space-empty? d2))
        (cons d1 d2)
        'stick-pair)))

(define (rotate-active-pair-counter-clockwise s1 s2)
  (define (rotate-right-to-up)
    (let ((d2 (up-neighbor s1)))
      (if (>= d2 0) d2 s2)))
  (define (rotate-left-to-down)
    (let ((d2 (down-neighbor s1)))
      (if (< d2 board-vector-length) d2 s2)))
  (define (rotate-down-to-right)
    (let ((d2 (right-neighbor s1)))
      (if (= (modulo (+ s1 1) board-grid-width) 0) s2 d2)))
  (define (rotate-up-to-left)
    (let ((d2 (left-neighbor s1)))
      (if (= (modulo s1 board-grid-width) 0) s2 d2)))
  (let ((d2 (if (on-same-level? s1 s2)
                (if (< s1 s2)
                    (rotate-right-to-up)
                    (rotate-left-to-down))
                (if (< s1 s2)
                    (rotate-down-to-right)
                    (rotate-up-to-left)))))
    (if (space-empty? d2)
        (cons s1 d2)
        (cons s1 s2))))

(define (rotate-active-pair-clockwise s1 s2)
  (define (rotate-right-to-down)
    (let ((d2 (down-neighbor s1)))
      (if (< d2 board-vector-length) d2 s2)))
  (define (rotate-left-to-up)
    (let ((d2 (up-neighbor s1)))
      (if (>= d2 0) d2 s2)))
  (define (rotate-down-to-left)
    (let ((d2 (left-neighbor s1)))
      (if (= (modulo s1 board-grid-width) 0) s2 d2)))
  (define (rotate-up-to-right)
    (let ((d2 (right-neighbor s1)))
      (if (= (modulo (+ s1 1) board-grid-width) 0) s2 d2)))
  (let ((d2 (if (on-same-level? s1 s2)
                (if (< s1 s2)
                    (rotate-right-to-down)
                    (rotate-left-to-up))
                (if (< s1 s2)
                    (rotate-down-to-left)
                    (rotate-up-to-right)))))
    (if (space-empty? d2)
        (cons s1 d2)
        (cons s1 s2))))


(define (start-board-evaluation!)
  (add-new-board-state!)
  (find-falling-puyos!)
  (switch-mode-to-evaluating!))

(define (fall puyo)
  (let ((hitbox (puyo-hitbox puyo)))
    (set-rect-y! hitbox (+ (rect-y hitbox) puyo-falling-speed))))

(define (check-landing puyo)
  (let* ((hitbox (puyo-hitbox puyo))
         (current-index (screen-coords-to-grid-index (rect-x hitbox) (rect-y hitbox)))
         (space-below (down-neighbor current-index)))
    (if (not (empty-space? space-below))
        (land-puyo! puyo current-index))))

(define (land-puyo! puyo index)
  (add-puyo-at! (puyo-color puyo) index)
  (remove-falling-puyo! puyo))


(define (find-scoring-groups)
  (let ((occupied-spaces (filter (lambda (i)
                                   (not (empty-space? i)))
                                 (range 0 board-vector-length))))
    ;(foldl find-group '() occupied-spaces)))
    (display occupied-spaces) (newline) (flush-output-port)
    (map get-neighbor-indices-for-index occupied-spaces)))

(define (find-group groups remaining-indices)
  (let* ((this-index (car remaining-indices))
         (this-color (get-puyo-at this-index))
         (neighbors (get-neighbor-indices-for-index this-index)))
         ;(group-members (find-group-members neighbors)))
    (display neighbors)))

(define (get-neighbor-indices-for-index index)
  (list (left-neighbor index)
        (right-neighbor index)
        (up-neighbor index)
        (down-neighbor index)))

(define (score-groups! groups)
  (display "Scoring groups: ")
  (display groups)
  (newline)
  (flush-output-port))

(define (progress-evaluation!)
  (for-each fall falling-puyos)
  (for-each check-landing falling-puyos)
  (if (= (length falling-puyos) 0)
      (let ((scoring-groups (find-scoring-groups)))
        (if (> (length scoring-groups) 0)
            (score-groups! scoring-groups)
            (switch-mode-to-moving!)))))

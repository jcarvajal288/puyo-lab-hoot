(define-module (ball)
  #:pure
  #:use-module (scheme base)
  #:use-module (math vector)
  #:use-module (math rect)
  #:use-module (dom canvas)
  #:use-module (images)
  #:export (ball-width
            ball-height
            ball-velocity
            ball-hitbox
            build-ball
            draw-ball))


(define-record-type <ball>
  (make-ball width height velocity hitbox)
  ball?
  (width ball-width)
  (height ball-height)
  (velocity ball-velocity)
  (hitbox ball-hitbox))

(define (build-ball level-width level-height)
  (let* ((width 22)
         (height 22)
         (velocity (vec2 1.0 2.0))
         (hitbox (make-rect (- (/ level-width 2) (/ width 2))
                            (- (/ level-height 2) (/ height 2))
                            width 
                            height)))
    (make-ball width height velocity hitbox)))

(define (draw-ball context ball)
  (let ((x (rect-x (ball-hitbox ball)))
        (y (rect-y (ball-hitbox ball)))
        (w (ball-width ball))
        (h (ball-height ball)))
    (draw-image context image:ball
                0 0 w h
                x y w h)))

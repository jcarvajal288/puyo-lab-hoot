(define-module (draw)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (dom image)
  #:use-module (dom canvas)
  #:use-module (math)
  #:use-module (screen)
  #:use-module (ball)
  #:export (draw-frame))

(define (draw-with-rotation context image angle sx sy sw sh dx dy dw dh)
  (let ((center-x (+ dx (/ dw 2)))
        (center-y (+ dy (/ dh 2))))
    (save context)
    (translate! context center-x center-y) 
    (rotate! context (to-radians angle))
    (translate! context (- center-x) (- center-y))
    (draw-image context image
                sx sy sw sh
                dx dy dw dh)
    (restore! context)))

(define (draw-background context)
  (set-fill-color! context "#140c1c")
  (fill-rect context 0.0 0.0 screen:width screen:height))


(define (draw-frame context ball prev-time)
  (draw-background context)
  (draw-ball context ball))

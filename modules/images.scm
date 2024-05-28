(define-module (images)
  #:pure
  #:use-module (scheme base)
  #:use-module (dom image)
  #:export (load-all-images
            image:ball))

(define image:ball #f)

(define (load-all-images)
  (set! image:ball (make-image "assets/images/ball.png")))

(define-module (images)
  #:pure
  #:use-module (scheme base)
  #:use-module (dom image)
  #:export (load-all-images
            image:background
            image:play-border
            play-border-width
            play-border-height))

(define image:background #f)
(define image:play-border #f)

(define play-border-width 198.0)
(define play-border-height 424.0)

(define (load-all-images)
  (set! image:background (make-image "assets/images/chalkboard_800x600.jpg"))
  (set! image:play-border (make-image "assets/images/playBorder.png")))

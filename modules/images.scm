(define-module (images)
  #:pure
  #:use-module (scheme base)
  #:use-module (dom image)
  #:export (load-all-images
            image:background
            image:play-border
            image:puyo-sheet))

(define image:background #f)
(define image:play-border #f)
(define image:puyo-sheet #f)

(define (load-all-images)
  (set! image:background (make-image "assets/images/chalkboard_800x600.jpg"))
  (set! image:play-border (make-image "assets/images/playBorder.png"))
  (set! image:puyo-sheet (make-image "assets/images/chalkpuyo_sprites.png")))

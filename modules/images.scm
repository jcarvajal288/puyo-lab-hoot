(define-module (images)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot match)
  #:use-module (dom image)
  #:export (image:background
            image:play-border
            image:puyo-sheet
            load-all-images
            get-puyo-sprite-sheet-coords))

(define image:background #f)
(define image:play-border #f)
(define image:puyo-sheet #f)

(define (load-all-images)
  (set! image:background (make-image "assets/images/chalkboard_800x600.jpg"))
  (set! image:play-border (make-image "assets/images/playBorder.png"))
  (set! image:puyo-sheet (make-image "assets/images/chalkpuyo_sprites.png")))

(define (get-puyo-sprite-sheet-coords puyo-color)
  (match puyo-color
    ('red (cons 3 4))
    ('green (cons 3 35))
    ('blue (cons 4 68))
    ('yellow (cons 5 99))
    ('purple (cons 3 132))))

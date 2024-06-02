(define-module (images)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme time)
  #:use-module (scheme write)
  #:use-module (hoot match)
  #:use-module (dom image)
  #:use-module (screen)
  #:export (image:background
            image:play-border
            image:puyo-sheet
            load-all-images
            get-puyo-sprite-sheet-coords))

(define image:background #f)
(define image:play-border #f)
(define image:puyo-sheet #f)

(define animation-speed-ms 500)

(define red-puyo-coords (vector '(3 . 4) '(36 . 4) '(68 . 2) '(100 . 3)))
(define green-puyo-coords (vector '(3 . 35) '(35 . 35) '(68 . 35) '(100 . 36)))
(define blue-puyo-coords (vector '(4 . 68) '(35 . 66) '(67 . 69) '(100 . 67)))
(define yellow-puyo-coords (vector '(4 . 99) '(35 . 100) '(68 . 99) '(100 . 99)))
(define purple-puyo-coords (vector '(3 . 132) '(35 . 132) '(69 . 131) '(101 . 131)))


(define (load-all-images)
  (set! image:background (make-image "assets/images/chalkboard_800x600.jpg"))
  (set! image:play-border (make-image "assets/images/playBorder.png"))
  (set! image:puyo-sheet (make-image "assets/images/chalkpuyo_sprites.png")))

(define (current-time-ms)
  (let ((jiffies-per-ms (/ (jiffies-per-second) 1000)))
    (floor/ (current-jiffy) jiffies-per-ms)))

(define (get-sprite-index)
  (modulo (floor/ (current-time-ms) animation-speed-ms) 4))

(define (get-puyo-sprite-sheet-coords puyo-color)
  (let ((sprite-index (get-sprite-index)))
    (match puyo-color
      ('red (vector-ref red-puyo-coords sprite-index))
      ('green (vector-ref green-puyo-coords sprite-index))
      ('blue (vector-ref blue-puyo-coords sprite-index))
      ('yellow (vector-ref yellow-puyo-coords sprite-index))
      ('purple (vector-ref purple-puyo-coords sprite-index)))))

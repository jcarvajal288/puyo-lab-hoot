(define-module (screen)
  #:pure
  #:use-module (scheme base)
  #:export (screen:width screen:height screen:ms-per-frame))

(define screen:width 800.0)
(define screen:height 600.0)
(define screen:refresh-rate 60)

(define screen:ms-per-frame (/ 1000.0 screen:refresh-rate)) ; aim for updating at 60Hz

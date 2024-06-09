(define-module (input)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:use-module (hoot ffi)
  #:use-module (dom event)
  #:use-module (math vector)
  #:use-module (gameboard)
  #:use-module (gamestate)
  #:export (on-key-down))

(define key:up "ArrowUp")
(define key:down "ArrowDown")
(define key:left "ArrowLeft")
(define key:right "ArrowRight")
(define key:revert "KeyR")
(define key:rotate-left "KeyZ")
(define key:rotate-right "KeyX")

(define (on-key-down event)
  (let ((key (keyboard-event-code event)))
    (cond ((string=? key key:left)         (move-active-pair! 'left))
          ((string=? key key:right)        (move-active-pair! 'right))
          ((string=? key key:up)           (move-active-pair! 'up))
          ((string=? key key:down)         (move-active-pair! 'down))
          ((string=? key key:rotate-left)  (move-active-pair! 'counter-clockwise))
          ((string=? key key:rotate-right) (move-active-pair! 'clockwise))
          ((string=? key key:revert)       (revert-board-state!)))))

;; (define (on-key-up event)
;;   (let ((key (keyboard-event-code event)))
;;     (cond ((string=? key key:left)  (set! command:rotate-left #f))
;;           ((string=? key key:right) (set! command:rotate-right #f))
;;           ((string=? key key:up)    (set! command:accelerate-forward #f))
;;           ((string=? key key:down)  (set! command:accelerate-backward #f))
;;           ((string=? key key:fire)  (set! command:fire-missile #f)))))

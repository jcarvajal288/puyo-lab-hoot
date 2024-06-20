(import (scheme base)
        (scheme inexact)
        (scheme write)
        (hoot debug)
        (hoot ffi)
        (math)
        (math rect)
        (math vector)
        (dom canvas)
        (dom document)
        (dom element)
        (dom event)
        (dom window)
        (draw)
        (gameboard)
        (gamestate)
        (images)
        (input)
        (puyo)
        (screen)
        (update))

(initialize-game-state board-vector-length)

(define (update)
  (if (eqv? current-game-mode 'evaluating)
      (progress-evaluation!))
  (timeout update-callback screen:ms-per-frame))
(define update-callback (procedure->external update))

(define (draw prev-time)
  (draw-frame context prev-time)
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;; Canvas and event loop setup
(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))

(load-all-images)

(set-element-width! canvas (exact screen:width))
(set-element-height! canvas (exact screen:height))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
;; (add-event-listener! (current-document) "keyup"
;;                      (procedure->external on-key-up))

(request-animation-frame draw-callback)
(timeout update-callback screen:ms-per-frame)

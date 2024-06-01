(import (scheme base)
        (scheme inexact)
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
        (screen)
        (draw)
        (images)
        (input)
        (gameboard)
        (update))

(define gameboard (build-gameboard))
(add-puyo-at gameboard 'red 0 0)
(add-puyo-at gameboard 'red 2 0)
(add-puyo-at gameboard 'red 0 5)
(add-puyo-at gameboard 'red 3 6)

(define (update)
  ;(update-all ball)
  (timeout update-callback screen:ms-per-frame))
(define update-callback (procedure->external update))

(define (draw prev-time)
  (draw-frame context gameboard prev-time)
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
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))

(request-animation-frame draw-callback)
(timeout update-callback screen:ms-per-frame)

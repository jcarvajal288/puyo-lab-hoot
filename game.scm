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
(add-puyo-at gameboard 'red 1 0)
(add-puyo-at gameboard 'red 2 0)
(add-puyo-at gameboard 'red 3 0)
(add-puyo-at gameboard 'red 4 0)
(add-puyo-at gameboard 'red 5 0)
(add-puyo-at gameboard 'red 0 12)
(add-puyo-at gameboard 'red 1 12)
(add-puyo-at gameboard 'red 2 12)
(add-puyo-at gameboard 'red 3 12)
(add-puyo-at gameboard 'red 4 12)
(add-puyo-at gameboard 'red 5 12)
(add-puyo-at gameboard 'red 0 1)
(add-puyo-at gameboard 'red 0 2)
(add-puyo-at gameboard 'red 0 3)
(add-puyo-at gameboard 'red 0 4)
(add-puyo-at gameboard 'red 0 5)
(add-puyo-at gameboard 'red 0 6)
(add-puyo-at gameboard 'red 0 7)
(add-puyo-at gameboard 'red 0 8)
(add-puyo-at gameboard 'red 0 9)
(add-puyo-at gameboard 'red 0 10)
(add-puyo-at gameboard 'red 0 11)
(add-puyo-at gameboard 'red 0 12)
(add-puyo-at gameboard 'red 5 12)
(add-puyo-at gameboard 'red 5 1)
(add-puyo-at gameboard 'red 5 2)
(add-puyo-at gameboard 'red 5 3)
(add-puyo-at gameboard 'red 5 4)
(add-puyo-at gameboard 'red 5 5)
(add-puyo-at gameboard 'red 5 6)
(add-puyo-at gameboard 'red 5 7)
(add-puyo-at gameboard 'red 5 8)
(add-puyo-at gameboard 'red 5 9)
(add-puyo-at gameboard 'red 5 10)
(add-puyo-at gameboard 'red 5 11)
(add-puyo-at gameboard 'red 5 12)

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

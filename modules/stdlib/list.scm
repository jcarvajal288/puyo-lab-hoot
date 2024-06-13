(define-module (stdlib list)
  #:pure
  #:use-module (scheme base)
  #:export (filter range))

(define (filter fn lst)
  (cond ((null? lst) '())
        ((fn (car lst)) 
           (cons (car lst)
                 (filter fn (cdr lst))))
        (else (filter fn (cdr lst)))))

(define (range min max)
  (if (>= min max)
      '()
      (cons min (range (+ min 1) max))))

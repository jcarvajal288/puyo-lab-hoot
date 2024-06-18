(define-module (stdlib list)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:export (filter
            range
            empty?
            foldl))

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

(define (empty? lst)
  (= (length lst) 0))

(define (foldl fn initial lst)
  (display lst) (newline) (flush-output-port)
  (if (empty? lst)
      initial
      (fn (car lst)
          (foldl fn initial (cdr lst)))))

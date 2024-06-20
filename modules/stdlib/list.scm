(define-module (stdlib list)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:export (filter
            range
            empty?
            foldl
            contains?
            get-longest-sublist))

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
  (display "fold: ")
  (display lst) (newline) (flush-output-port)
  (if (empty? lst)
      initial
      (foldl fn (fn initial (car lst)) (cdr list))))

(define (contains? lst obj)
  (not (not (memq obj lst))))

(define (get-longest-sublist lists)
  (define (reducer current-longest lists)
    (cond ((empty? lists)
            current-longest)
          ((> (length (car lists)) (length current-longest))
            (reducer (car lists) (cdr lists)))
          (else
            (reducer current-longest (cdr lists)))))
  (reducer '() lists))

(define-module (stdlib debug)
  #:pure
  #:use-module (scheme base)
  #:use-module (scheme write)
  #:export (println print-var))

  (define (println str)
    (display str)
    (newline)
    (flush-output-port))

  (define (print-var label var)
    (display label)
    (display var)
    (newline)
    (flush-output-port))

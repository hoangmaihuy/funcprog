#lang racket

(define (print-list lst)
  (for-each (lambda (x) (displayln x)) lst)
)

(define (read-list lst)
  (define elem (read))

  (if (eq? elem eof)
    (print-list (map abs lst))
    (read-list (append lst (list elem)))
  )
)

(define lst (list))
(read-list lst)
#lang racket

(define (read-list lst)
  (define elem (read))

  (if (eq? elem eof)
    (display (length lst))
    (read-list (append lst (list elem)))
  )
)

(define lst (list))
(read-list lst)
#lang racket

(define (print-even-pos i)
  (define (even? x) (= 0 (remainder x 2)))

  (define x (read))

  (if (eq? x eof)
    (void)
    (begin
      (cond ((even? i) (displayln x)))
      (print-even-pos (+ i 1))
    )
  )
)

(print-even-pos 1)
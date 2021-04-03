#lang racket

(define (main)
  (define a (read))
  (define b (read))
  (if (eq? a eof)
    (void)
    (begin
      (set! a (remove-duplicates a))
      (set! b (remove-duplicates b))
      (display (sort (set-subtract a b) <))
      (display (sort (set-symmetric-difference a b) <))
      (newline)
      (main)
    )
  )
)

(main)
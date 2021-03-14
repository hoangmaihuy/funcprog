#lang racket

(define (filter-smaller-than x)
  (define y (read))
  (if (eq? y eof)
    (void)
    (begin
      (cond ((< y x) (displayln y)))
      (filter-smaller-than x)
    )
  )
)

(filter-smaller-than (read))

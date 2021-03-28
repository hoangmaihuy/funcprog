#lang racket

(define (even? x) (= 0 (remainder x 2)))

(define (sum-list sum)
  (define elem (read))

  (if (eq? elem eof)
    (display sum)
    (begin
      (if (even? elem)
        (set! elem 0)
        (void)
      )
      (sum-list (+ sum elem))
    )
  )
)

(sum-list 0)
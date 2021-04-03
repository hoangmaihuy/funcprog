#lang racket

(define (main) 
  (define x (read))
  (if (eq? eof x)
    (void)
    (begin
      (displayln (reverse x))
      (main)
    )
  )
)

(main)
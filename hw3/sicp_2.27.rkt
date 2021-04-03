#lang racket

(define (reverse-tree x)
  (if (number? x)
    x
    (map reverse-tree (reverse x))
  )
)

(define (main)
  (define x (read))
  (if (eq? x eof)
    (void)
    (begin
      (displayln (reverse-tree x))
      (main)
    )
  )
)

(main)
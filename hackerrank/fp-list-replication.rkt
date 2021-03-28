#lang racket

(define (replicate n x)
  (define (print n x) 
    (if (= n 0)
      (void)
      (begin (displayln x) (print (- n 1) x))
    )
  )

  (if (eq? x eof) 
    (void)
    (begin (print n x) (replicate n (read)))
  )
)

(replicate (read) (read))

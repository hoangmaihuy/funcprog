#lang racket

(define (print-n-times str n)
  (if (= n 0) 
      (void)
      (begin (displayln str) (print-n-times str (- n 1)))
  )
)

(print-n-times "Hello World" (read))

#lang racket

(define (double f)
  (lambda (x) (f (f x)))
)

(define (inc x) (+ x 1))

((double inc) 5) ; >> 7
(((double double) inc) 5) ; >> 9
(((double (double double)) inc) 5) ;>> 21

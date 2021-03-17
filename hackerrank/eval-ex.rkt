#lang racket

(define (eval-ex x n)
  (define (eval-iter i exp fact)
    (if (> i n)
      0
      (+ (/ exp fact) (eval-iter (+ i 1) (* exp x) (* fact i)))
    )
  )

  (eval-iter 1 1 1)
)

(define (main ntest)
  (if (= ntest 0)
    (void)
    (begin
      (displayln (eval-ex (read) 10))
      (main (- ntest 1))
    )
  )
)
(main (read))
#lang racket

(define (f n)
  (define (square x) (* x x))
  (define (cube x) (* x x x))
  (define (f-iter i f1 f2 f3 f4 f5)
    (if (> i n)
      f1
      (f-iter (+ i 1) 
              (+ f1 
                 (* 4 f2) 
                 (* 5 f3) 
                 (* -2 (square f4))
                 (cube f5)
              )
              f1 f2 f3 f4)
    )
  )

  (if (< n 5)
    1
    (f-iter 5 1 1 1 1 1)
  )
)

(define (read-iter n)
  (if (eq? n eof)
    (void)
    (begin (displayln (f n)) (read-iter (read))) 
  )
)

(read-iter (read))
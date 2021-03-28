#lang racket

(define (function? n)

  (define (match-func? lst x y)
    (if (null? lst)
      #t
      (if (and (= x (car (car lst))) (not (= y (cdr (car lst)))))
        #f
        (match-func? (cdr lst) x y)
      )
    )
  )

  (define (func-iter values n)
    (define x (read))
    (define y (read))
    (if (= n 1)
      (match-func? values x y)
      (if (match-func? values x y)
        (func-iter (append values (list (cons x y))) (- n 1))
        #f
      )
    )
  )
  (func-iter (list) n)
)

(define (main ntest)
  (if (= ntest 0)
    (void)
    (begin 
      (if (function? (read))
        (displayln "YES")
        (displayln "NO")
      )
      (main (- ntest 1))
    )
  )
)

(main (read))
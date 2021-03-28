#lang racket

(define (max a b)
    (if (> a b) a b)
)

(define (max-value n)
    (if (= n 1)
        (read)
        (max (read) (max-value (- n 1)))
    )
)

(define (main ntest) 
    (if (= ntest 0)
        (void)
        (begin (displayln (max-value (read))) (main (- ntest 1)))
    )
)

(main (read))

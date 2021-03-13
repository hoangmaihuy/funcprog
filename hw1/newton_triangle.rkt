#lang racket

(define (print-line n) ; return list
    (define l (list 1))

    (define (build-line i n v)
        (if (= i n)
            (set! l (append l (list 1)))
            (begin 
                (set! l (append l (list (+ (car v) (cadr v)))))
                (build-line (+ i 1) n (cdr v))
            )
        )
    )

    (define (print n l)
        (if (= n 1)
            (displayln (car l))
            (begin
                (display (car l))
                (display " ")
                (print (- n 1) (cdr l))
            )
        )
    )

    (if (> n 1)
        (build-line 2 n (print-line (- n 1)))
        (void)
    )

    (print n l)
    l
)

(define (main n) 
    (if (eq? n eof)
        (void)
        (begin
            (print-line n)
            (main (read))
        )
    )
)

(main (read))
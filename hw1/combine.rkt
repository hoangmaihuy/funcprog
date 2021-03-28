#lang racket

(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (db x) (* x 2))

(define (combine f1 f2 n)
    (define (g f1 f2) (lambda (x) (f1 (f2 x))))

    (define (pow f n)
        (if (= n 0)
            (lambda (x) x)
            (lambda (x) (f ((pow f (- n 1)) x)))
        )
    )

    (pow (g f1 f2) n)
)


((combine square inc 1) 2)
((combine square inc 2) 3)
((combine db inc 3) 2)
((combine inc inc 4) 3)

(display "********") (newline)

(define (myloop)
  (let ((n (read))
        (x (read)))
    (if (eq? n eof)
        (void)
        (begin (display ((combine inc square n) x)) 
               (newline) (myloop)))))

(myloop)

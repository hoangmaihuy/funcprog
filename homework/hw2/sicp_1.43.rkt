#lang racket
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (db x) (* x 2))

(define (repeated f n)
; submit from here
  (if (= n 1)
    (lambda (x) (f x))
    (lambda (x) (f ((repeated f (- n 1)) x)))
  )
)
; to here
((repeated square 2) 5)
((repeated inc 4) 6)
((repeated db 4) 6)

(display "********") (newline)

(define (myloop)
  (let ((n (read)))
    (if (eq? n eof)
        (void)
        (begin (display ((repeated square n) 2)) 
               (newline) (myloop)))))

(myloop)
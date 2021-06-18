#lang racket

(define (display-vector v)
	(for ([x v])
		(display x) (display " ")
	)
	(newline)
)

(define (pascal n)
	(define v (make-vector n))
	(vector-set! v 0 1)
	(vector-set! v (- n 1) 1)
	(if (> n 1)
		(let ([u (pascal (- n 1))])
			(for ([i (in-range 1 (- n 1))])
				(vector-set! v i
					(+ (vector-ref u i) (vector-ref u (- i 1)))
				)
			)
		)
		(void)
	)
	(display-vector v)
	v
)

(define (loop)
	(let ([n (read)])
		(if (eq? n eof)
			(void)
			(begin
				(pascal n)
				(loop)
			)
		)
	)
)

(loop)
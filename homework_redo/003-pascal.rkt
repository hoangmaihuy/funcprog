#lang racket

(define (display-vector v)
	(for ([x v])
		(display x)
		(display " ")
	)
	(newline)
)

(define (draw-pascal n)
	(let ([v (make-vector n)])
		(vector-set! v 0 1)
		(vector-set! v (- n 1) 1)
		(if (> n 1)
			(let ([u (draw-pascal (- n 1))])
				(for ([i (in-range 1 (- n 1))])
					(vector-set! v i
						(+ (vector-ref u (- i 1)) (vector-ref u i))
					)
				)
			)
			(void)
		)
		(display-vector v)
		v
	)
)

(define (loop)
	(let ([input (read)])
		(if (eq? input eof)
			(void)
			(begin
				(draw-pascal input)
				(loop)
			)
		)
	)
)

(loop)
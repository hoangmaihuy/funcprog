#lang racket

(define (cal exp)
	(if (number? exp)
		exp
		(let ([op (car exp)]
					[operands (map cal (cdr exp))])
			(cond 
				[(eq? op '+) (apply + operands)]
				[(eq? op '*) (apply * operands)]
				[(eq? op '-) (apply - operands)]
				[(eq? op '/) (apply / operands)]
				[else (error "unknown op" op)]
			)
		)
	)
)

(define (loop)
	(let ([input (read)])
		(if (eq? input eof)
			(void)
			(begin 
				(display (cal input))
				(newline)
				(loop)
			)
		)
	)
)
(loop)
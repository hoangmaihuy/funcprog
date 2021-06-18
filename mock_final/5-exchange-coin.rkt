#lang racket 

(define (exchange x y z)
	(let ([ans 0])
		(for ([i (in-range 1 (/ x y) 1)])
			(let ([tmp (remainder (- x (* i y)) z)])
				(if (= tmp 0)
					(set! ans (+ ans 1))
					(void)
				)
			)
		)	
		ans
	)
)

(exchange (* (read) 10) (read) (read))
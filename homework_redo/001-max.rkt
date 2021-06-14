#lang racket

(define (get-max n)
	(let ([x (read)])
		(if (= n 1)
			x
			(max x (get-max (- n 1)))
		)
	)	
)

(define (loop ntest)
	(if (= ntest 0)
		(void)
		(begin 
			(display (get-max (read)))
			(newline)
			(loop (- ntest 1))
		)
	)
)

(loop (read))
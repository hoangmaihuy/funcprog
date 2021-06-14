#lang racket

(define (power-set lst n)
	(define (generate s)
		(if (null? s)
			'(())
			(let ([tmp (generate (cdr s))]
						[first (car s)])
				(append
					(map (lambda (rest) (cons first rest)) tmp)
					tmp
				)
			)
		)
	)
	(if (= n 1)
		(generate lst)
		(generate (power-set lst (- n 1)))
	)
)

(define (loop)
	(let ([lst (read)]
				[n (read)])
		(if (eq? lst eof)
			(void)
			(begin 
				(display (power-set (remove-duplicates (sort lst <)) n))
				(newline)
				(loop)
			)
		)
	)
)

(loop)
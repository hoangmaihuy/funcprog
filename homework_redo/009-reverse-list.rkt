#lang racket

(define (loop)
	(let ([input (read)])
		(if (eq? input eof)
			(void)
			(begin 
				(display (reverse input))
				(newline)
				(loop)
			)
		)
	)
)

(loop)

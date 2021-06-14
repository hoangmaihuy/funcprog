#lang racket

(define (reverse-tree input)
	(if (list? input)
		(map reverse-tree (reverse input))
		input
	)
)

(define (loop)
	(let ([input (read)])
		(if (eq? input eof)
			(void)
			(begin 
				(display (reverse-tree input))
				(newline)
				(loop)
			)
		)
	)
)

(loop)
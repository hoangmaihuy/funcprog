#lang racket

(define (flat-map lst)
	(let ([flat-list '()])
		(for-each 
			(lambda (x)
				(if (list? x)
					(set! flat-list (append flat-list (flat-map x)))
					(set! flat-list (append flat-list (list x)))
				)
			)
			lst
		)
		flat-list
	)
)

(define (loop)
	(let ([input (read)])
		(if (eq? input eof)
			(void)
			(begin
				(display (flat-map input))
				(newline)
				(loop)
			)
		)
	)
)

(loop)
#lang racket

(define lst '())

(define (loop)
	(let ([input (read)])
		(if (eq? input eof)
			(for-each (lambda (x) (display x) (display " ")) (remove-duplicates (sort lst <)))
			(begin
				(set! lst (cons input lst))
				(loop)
			)
		)
	)
)

(loop)
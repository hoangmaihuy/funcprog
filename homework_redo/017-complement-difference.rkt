#lang racket

(define (relative-complement a b)
	(remove-duplicates (sort
		(filter
			(lambda (x)
				(not (member x b))
			)
			a
		)
		<
	))
)

(define (symmetric-difference a b)
	(remove-duplicates (sort
		(filter
			(lambda (x)
				(not (and (member x a) (member x b)))
			)
			(append a b)
		)
		<
	))
)

(define (loop)
	(let ([a (read)]
				[b (read)])
		(if (eq? a eof)
			(void)
			(begin
				(display (relative-complement a b))
				(display (symmetric-difference a b))
				(newline)
				(loop)
			)
		)
	)
)

(loop)
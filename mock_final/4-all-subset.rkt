#lang racket 

(define (subset lst)
	(if (null? lst)
		'(())
		(let ([tmp (subset (cdr lst))]
					[first (car lst)])
			(append
				(map (lambda (rest) (cons first rest)) tmp)
				tmp
			)
		)
	)
)

(define (cmp-list l1 l2)
	(cond
		[(eq? l1 l2) #t]
		[(null? l1) #t]
		[(null? l2) #f]
		[(= (car l1) (car l2)) (cmp-list (cdr l1) (cdr l2))]
		[else (< (car l1) (car l2))]
	)
)

(define (loop)
	(let ([input (read)])
		(if (eq? input eof)
			(void)
			(begin 
				(display (sort (subset (sort input <)) cmp-list))
				(newline)
				(loop)
			)
		)
	)
)
(loop)
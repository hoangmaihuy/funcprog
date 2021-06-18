#lang racket

(define (flat-map-n lst n)
	; (display lst) (display " ") (display n) (newline)
	(if (pair? lst)
		(if (= n 0)
			lst
			(let ([a '()])
				(for-each
					(lambda (x)
						(set! a (append a (flat-map-n x (- n 1))))
					)
					lst 
				)
				a
			)
		)
		(if (null? lst) '() (list lst))
	)
)

(define (loop)
	(let ([lst (read)]
				[n (read)])
		(if (eq? lst eof)
			(void)
			(begin
				(display (flat-map-n lst n))
				(newline)
				(loop)
			)
		)			
	)
)


(loop)
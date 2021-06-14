#lang racket

(define max-n 51)

(define f (make-vector max-n))

(define (fib n)
	(cond 
		[(< n 5) 1]
		[(eq? (vector-ref f n) 0) 
			(let ([f1 (fib (- n 1))]
						[f2 (fib (- n 2))]
						[f3 (fib (- n 3))]
						[f4 (fib (- n 4))]
						[f5 (fib (- n 5))])
				(vector-set! f n
					(+
						f1 
						(* 4 f2)
						(* 5 f3)
						(* -2 f4 f4)
						(* f5 f5 f5)
					)
				)
				(vector-ref f n)
			)
		]
		[else (vector-ref f n)]
	)
)

(define (loop)
	(let ([n (read)])
		(if (eq? n eof)
			(void)
			(begin (displayln (fib n)) (loop))
		)
	)
)

(loop)

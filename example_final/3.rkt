#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your my-map, the program in the input can use my-map
 '
(define (my-map proc . args)
;--- starts here
	; (display "args: ")(display args)(newline)
	(define (get-operands a)
		(cond
			[(null? a) '()]
			[(null? (car a)) (get-operands (cdr a))]
			[else (cons (car (car a)) (get-operands (cdr a)))]
		)
	)

	(define (get-rest a)
		(cond 
			[(null? a) '()]
			[(null? (car a)) (get-rest (cdr a))]
			[else (cons (cdr (car a)) (get-rest (cdr a)))]
		)
	)
	(if (null? args)
		'()
		(let ([operands (get-operands args)])
			; (display operands) (newline)
			(if (null? operands)
				'()
				(cons
					(apply proc operands)
					(apply my-map (cons proc (get-rest args)))
				)
			)
		)
	)
)
;--- ends here
env)

(define (myloop)
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin  (displayln (eval codes env)) (myloop)))))


(myloop)
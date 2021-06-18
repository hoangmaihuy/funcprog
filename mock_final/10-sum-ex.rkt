#lang racket
(require r5rs)
(define env (scheme-report-environment 5))
(define exit 5)


(eval '(define (power-sum 
	. args)
		(define sum (apply + args))
		(define (power-sum-helper . args)
			(if (null? args)
				sum
				(begin
					(set! sum (apply + (cons sum args)))
					power-sum-helper
				)
			)
		)
		power-sum-helper
	)
	env
)
(define (myloop)
  (let ((exp (read)))
    (if (eq? eof exp)
        (void)
        (let ((val (eval exp env)))
          (if (eq? val (void))
              (begin (void) (myloop))
              (begin (displayln val) (myloop)))))))
(myloop)
#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '
;--- starts here
(define (count-pairs lst)
	(define visited '())
	(define (count-iter x)
		(cond
			[(null? x) 0]
			[(pair? x)
				(if (memq x visited)
					0
					(begin
						(set! visited (cons x visited))
						(+ 1 (count-iter (car x)) (count-iter (cdr x)))
					)
				)
			]
			[else 0]
		)
	)
	(count-iter lst)
)
;--- ends here
env)

(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)
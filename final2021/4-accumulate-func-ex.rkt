#lang racket
(require r5rs)
(define (exit) #f)
(define env (scheme-report-environment 5))

(eval '(define (acc-func . argf)
	(define (combine f1 f2)
		(lambda (x)
			(f1 (f2 x))
		)
	)


	(define (build-combine funcs)
		(if (null? funcs)
			(lambda (x) x)
			(lambda (x)
				((build-combine (cdr funcs)) ((car funcs) x))
			)
		)
	)

	(define (build flist)
		(let ([f (build-combine flist)])
			(define (helper . args)
				(if (null? args)
					helper
					(f (car args))
				)
			)
			helper
		)
	)
	(if (null? argf)
		acc-func
		(let ([flist argf])
			(define (helper . argf2)
				(if (null? argf2)
					(build flist)
					(begin 
						(set! flist (append flist argf2))
						helper
					)
				)	
			)
			helper
		)
	)

)
env)


(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
		; (display codes)
		; (newline)
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))
(myloop)
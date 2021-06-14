#lang racket
(require r5rs)
(define (exit) #f)
(define env (scheme-report-environment 5))

(eval '(define (last-pair lst)
         (if (null? (cdr lst))
             lst
             (last-pair (cdr lst))))
      env)

(eval '(define (make-cycle lst)
         (set-cdr! (last-pair lst) lst)
         lst)
      env)

(eval '
;--- starts here
(define (check-cycle lst)
	(define visited '())
	(define (check-iter x)
		(cond
			[(null? x) #f]
			[(not (pair? x)) #f]
			[(memq (car x) visited) #t]
			[else 
				(set! visited (cons (car x) visited))
				(check-iter (cdr x))
			]
		)
	)
	(check-iter lst)
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
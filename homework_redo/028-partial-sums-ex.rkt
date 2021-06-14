#lang racket
(require r5rs)
(define env (scheme-report-environment 5))
(eval '(define (stream-car stream) (car stream)) env)
(eval '(define (stream-cdr stream) (force (cdr stream))) env)
(eval '(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))])) env)

(eval '(define the-empty-stream '()) env)
(eval '(define (stream-null? stream) (null? stream)) env)
         
(eval '(define (stream-ref s n)  ;get the nth item from s. n starts from 
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
      env)

(eval '(define (display-stream s n) ;display first n items of s
  (if (= n 0)
      (displayln "")
      (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))
      env)


(eval '
(define (partial-sums-ex op s)
;--- starts here
	(define (accumulate t1 t2 stream)

		(let ([first (stream-ref stream 0)]
					[second (stream-ref stream 1)]
					[rest (stream-cdr (stream-cdr stream))])
			(let ([new-t1 (op t1 first)]
						[new-t2 (op t2 second)])
				(cons-stream
					new-t1
					(cons-stream
						new-t2
						(accumulate new-t1 new-t2 rest)
					)
				)
			)
		)
	)

	(let ([first (stream-ref s 0)]
				[second (stream-ref s 1)]
				[rest (stream-cdr (stream-cdr s))])
		(cons-stream
			first
			(cons-stream
				second
				(accumulate first second rest)
			)
		)
	)
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
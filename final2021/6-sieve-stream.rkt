#lang racket
(define (exit) #f)
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))

(define the-empty-stream '())
(define (stream-null? stream) (null? stream))
         
(define (stream-ref s n)  ;get the nth item from s. n starts from 
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))

(define (display-stream s n) ;display first n items of s
  (if (= n 0)
      (displayln "")
      (begin (display (stream-car s)) (display " ") (display-stream (stream-cdr s) (- n 1)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (sieve stream) 
  (cons-stream
;--- start
		(stream-car stream)
		(sieve
			(stream-filter
				(lambda (x)
					(not (= (remainder x (stream-car stream)) 0))
				)
				stream
			)
		)
	)
)
;--- end

(define primes (sieve (integers-starting-from 2)))
(display-stream primes (read))
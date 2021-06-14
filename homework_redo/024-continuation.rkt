#lang racket

(define exit #f)

(define cont-list '())
(define len (read))

(define (set-cont-list n)
;--- starts here
	(define (make-cont k)
		(define saved-cc #t)
		(if (call/cc (lambda (cc)
									 (set! saved-cc cc)
									 #t
								 ))
			(void)
			(for-each (lambda (x) (displayln x)) (range k 0 -1))
		)
		saved-cc
	)
	(set! cont-list (map make-cont (range 1 (+ n 1) 1)))
)
;--- ends here
(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)
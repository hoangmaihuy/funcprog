#lang racket
(define (cont-frac-iter N D k)
;--- starts here
	(let ([ans 0])
		(for ([i (in-range k 0 -1)])
			(set! ans (/ (N i) (+ (D i) ans)))
		)
		ans
	)
)
;--- ends here
(cont-frac-iter (lambda (x) x) 
           (lambda (x) 1.0)
           30)
 
(cont-frac-iter (lambda (x) (* 2 x))
           (lambda (x) (* 1.0 x))
           30)

(display "********") (newline)
(define (myloop)
  (let ((k (read)))
    (if (eq? k eof)
        (void)
        (begin (display (cont-frac-iter (lambda (x) 1.0) (lambda (x) 1.0) k)) 
(newline) (myloop)))))

(myloop)